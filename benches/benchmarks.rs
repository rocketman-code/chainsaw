use std::collections::VecDeque;
use std::fmt::Write as _;
use std::hint::black_box;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use chainsaw::cache::ParseCache;
use chainsaw::lang::python::PythonSupport;
use chainsaw::lang::typescript::TypeScriptSupport;
use chainsaw::lang::LanguageSupport;
use chainsaw::query;
use clap::Parser;
use stats::{
    cv, format_time, mean, noise_aware_welch_t_test, noise_floor, session_bias_adjust, trim,
    trimmed_mean, NOISE_FLOOR_MIN, REGRESSION_THRESHOLD, TRIM_FRACTION, VERDICT_P,
};

mod corpus;

// --- Constants ---
//
// Shared constants (VERDICT_P, REGRESSION_THRESHOLD, TRIM_FRACTION, NOISE_FLOOR_MIN)
// are defined in the stats crate so both the harness and the attestation gate agree.
// Below are harness-only constants with derivation documented inline.

// Root: macOS mach_absolute_time resolution is ~41ns; 1ms = 24,000x headroom
const TARGET_MEASUREMENT_NS: u64 = 1_000_000;

// Derived: warmup convergence.
// Threshold must be achievable by all benchmarks (fast benchmarks inherently have
// 1-2% CV from OS scheduling jitter). 2% is achievable; 1% causes fast benchmarks
// to exhaust MAX_WARMUP_ITERATIONS without converging.
const WARMUP_CV_THRESHOLD: f64 = 0.02;
// Want P(false convergence | true_CV > 2 * threshold) < 5%.
// At K=7: P ≈ 4.2% via normal approximation of CV sampling distribution.
const WARMUP_WINDOW: usize = 7;
// Safety bound: empirically ~10x typical convergence iterations
const MAX_WARMUP_ITERATIONS: usize = 100;

// Derived: sampling.
// MIN_SAMPLES: minimum for t-test with meaningful df (≥3 after trimming)
const MIN_SAMPLES: usize = 5;
// MAX_SAMPLES: the knee of the MDE (minimum detectable effect) curve.
// MDE(n) ∝ sqrt(CV^2 * 2/n + sigma_env^2). At n=20, marginal improvement drops
// below 5% per doubling — sigma_env dominates and more samples barely help.
// The noise-aware test accounts for both SE_sampling and SE_env in quadrature,
// so correctness doesn't require SE_sampling to be negligible.
const MAX_SAMPLES: usize = 20;

// Derived: early stopping.
// Must not false-positive under session drift (not yet corrected during early stop).
// At p=0.001, drift tolerance is 3.8x sigma_env (verified via noise_aware_welch_t_test).
// At sigma_env=2%: tolerates 7.5% drift. Session drift rarely exceeds 5%.
const EARLY_STOP_P: f64 = 0.001;

// Overhead warning: both thresholds must be exceeded (AND condition).
// Fast benchmarks have high ratio variance but tiny absolute variance,
// slow benchmarks have the opposite. Empirically validated against 25 runs.
const OVERHEAD_WARN_RATIO: f64 = 3.0;
const OVERHEAD_WARN_MIN_INCREASE_NS: f64 = 500_000_000.0;

// --- Benchmark definition ---

struct Benchmark {
    name: &'static str,
    run: Box<dyn Fn()>,
}

// --- Harness core ---

fn calibrate(f: &dyn Fn()) -> u64 {
    let mut batch = 1u64;
    loop {
        let start = Instant::now();
        for _ in 0..batch {
            f();
            black_box(());
        }
        if start.elapsed() >= Duration::from_nanos(TARGET_MEASUREMENT_NS) {
            return batch;
        }
        batch *= 2;
    }
}

#[allow(clippy::cast_precision_loss)]
fn warmup(f: &dyn Fn(), batch: u64) -> usize {
    let mut window: VecDeque<f64> = VecDeque::with_capacity(WARMUP_WINDOW + 1);
    let mut count = 0;
    loop {
        let (_, elapsed) = measure(f, batch);
        let per_iter = elapsed / batch as f64;
        window.push_back(per_iter);
        if window.len() > WARMUP_WINDOW {
            window.pop_front();
        }
        count += 1;
        if window.len() == WARMUP_WINDOW && cv(window.make_contiguous()) < WARMUP_CV_THRESHOLD {
            return count;
        }
        if count >= MAX_WARMUP_ITERATIONS {
            return count;
        }
    }
}

#[allow(clippy::cast_precision_loss)]
fn measure(f: &dyn Fn(), batch: u64) -> (u64, f64) {
    let start = Instant::now();
    for _ in 0..batch {
        f();
        black_box(());
    }
    (batch, start.elapsed().as_nanos() as f64)
}

enum StopReason {
    EarlyStop,  // strong signal detected via noise-aware test
    MaxSamples, // reached MAX_SAMPLES
    NoBaseline, // save-baseline mode
}

#[allow(clippy::cast_precision_loss)]
fn sample(
    f: &dyn Fn(),
    batch: u64,
    baseline: Option<&[f64]>,
    sigma_env: Option<f64>,
) -> (Vec<(u64, f64)>, StopReason) {
    let mut samples: Vec<(u64, f64)> = Vec::new();

    loop {
        samples.push(measure(f, batch));

        // Early stop only when sigma_env is available (subsequent comparisons).
        // First comparison runs to MAX_SAMPLES to establish sigma_env.
        // Uses noise_aware_welch_t_test so environmental shifts don't trigger
        // false early stops. Only catches strong signals (p < 0.001).
        if let (Some(base), Some(sigma)) = (baseline, sigma_env) {
            if samples.len() >= MIN_SAMPLES {
                let per_iter: Vec<f64> =
                    samples.iter().map(|(b, t)| t / *b as f64).collect();
                let base_trimmed = trim(base, TRIM_FRACTION);
                let sample_trimmed = trim(&per_iter, TRIM_FRACTION);
                let p = noise_aware_welch_t_test(&base_trimmed, &sample_trimmed, sigma);
                let change_pct =
                    (mean(&sample_trimmed) - mean(&base_trimmed)) / mean(&base_trimmed);

                if p < EARLY_STOP_P && change_pct.abs() > REGRESSION_THRESHOLD {
                    return (samples, StopReason::EarlyStop);
                }
            }
        }

        if samples.len() >= MAX_SAMPLES {
            return if baseline.is_some() {
                (samples, StopReason::MaxSamples)
            } else {
                (samples, StopReason::NoBaseline)
            };
        }
    }
}

// --- I/O ---

#[allow(clippy::cast_precision_loss)]
fn save_samples(name: &str, slot: &str, samples: &[(u64, f64)], overhead_ns: f64) {
    let dir = PathBuf::from("target/criterion").join(name).join(slot);
    if let Err(e) = std::fs::create_dir_all(&dir) {
        eprintln!("error: failed to create {}: {e}", dir.display());
        std::process::exit(1);
    }

    let iters: Vec<f64> = samples.iter().map(|(b, _)| *b as f64).collect();
    let times: Vec<f64> = samples.iter().map(|(_, t)| *t).collect();

    let json = format!(
        "{{\"sampling_mode\":\"Linear\",\"iters\":{},\"times\":{},\"overhead_ns\":{overhead_ns}}}",
        format_array(&iters),
        format_array(&times),
    );
    if let Err(e) = std::fs::write(dir.join("sample.json"), json) {
        eprintln!("error: failed to write {}/sample.json: {e}", dir.display());
        std::process::exit(1);
    }
}

fn format_array(data: &[f64]) -> String {
    let mut s = String::from("[");
    for (i, v) in data.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        let _ = write!(s, "{v}");
    }
    s.push(']');
    s
}

struct Baseline {
    per_iter: Vec<f64>,
    overhead_ns: Option<f64>,
}

fn load_baseline(name: &str, baseline_name: &str) -> Option<Baseline> {
    #[derive(serde::Deserialize)]
    struct Sample {
        iters: Vec<f64>,
        times: Vec<f64>,
        #[serde(default)]
        overhead_ns: Option<f64>,
    }

    let path = PathBuf::from("target/criterion")
        .join(name)
        .join(baseline_name)
        .join("sample.json");
    let content = std::fs::read_to_string(&path).ok()?;
    let sample: Sample = serde_json::from_str(&content).ok()?;
    Some(Baseline {
        per_iter: sample
            .iters
            .iter()
            .zip(sample.times.iter())
            .map(|(i, t)| t / i)
            .collect(),
        overhead_ns: sample.overhead_ns,
    })
}

fn sigma_env_path(slot: &str) -> PathBuf {
    PathBuf::from("target/criterion").join(format!("sigma_env_{slot}.json"))
}

fn load_sigma_env(slot: &str) -> Option<f64> {
    #[derive(serde::Deserialize)]
    struct SigmaEnv {
        sigma_env: f64,
    }

    let content = std::fs::read_to_string(sigma_env_path(slot)).ok()?;
    let data: SigmaEnv = serde_json::from_str(&content).ok()?;
    Some(data.sigma_env)
}

fn save_sigma_env(slot: &str, sigma_env: f64) {
    let path = sigma_env_path(slot);
    let json = format!("{{\"sigma_env\":{sigma_env}}}");
    let _ = std::fs::write(path, json);
}

// --- CLI ---

#[derive(Parser)]
#[command(
    name = "chainsaw-bench",
    about = "Adaptive benchmark harness for chainsaw",
    version
)]
struct Args {
    /// Save results to a named baseline slot
    #[arg(long)]
    save_baseline: Option<String>,

    /// Compare against a named baseline
    #[arg(long)]
    baseline: Option<String>,

    /// List available benchmarks
    #[arg(long)]
    list: bool,

    /// Ignored (passed by cargo bench)
    #[arg(long, hide = true)]
    bench: bool,

    /// Benchmark name filters (multiple allowed, matched with OR)
    filter: Vec<String>,
}

fn matches_filter(name: &str, filter: &str) -> bool {
    filter.split('|').any(|part| name.contains(part))
}

// --- Benchmark registration ---

fn ts_entry() -> (PathBuf, PathBuf) {
    std::env::var("TS_BENCH_ROOT").map_or_else(
        |_| corpus::ts_corpus(),
        |root| {
            let root = PathBuf::from(root);
            let entry = root.join("packages/wrangler/src/index.ts");
            (root, entry)
        },
    )
}

fn py_entry() -> (PathBuf, PathBuf) {
    std::env::var("PY_BENCH_ROOT").map_or_else(
        |_| corpus::py_corpus(),
        |root| {
            let root = PathBuf::from(root);
            let entry = root.join("awscli/__init__.py");
            (root, entry)
        },
    )
}

#[allow(clippy::too_many_lines)]
fn register_benchmarks() -> Vec<Benchmark> {
    let mut benches = Vec::new();

    let (ts, ts_entry_path) = ts_entry();
    let (py, py_entry_path) = py_entry();

    // ts_parse_file
    match std::fs::read_to_string(&ts_entry_path) {
        Ok(source) => {
            let lang = TypeScriptSupport::new(&ts);
            let entry = ts_entry_path.clone();
            benches.push(Benchmark {
                name: "ts_parse_file",
                run: Box::new(move || {
                    let _ = black_box(lang.parse(black_box(&entry), black_box(&source)));
                }),
            });
        }
        Err(_) => eprintln!("Skipping ts_parse_file: {} not found", ts_entry_path.display()),
    }

    // py_parse_file
    match std::fs::read_to_string(&py_entry_path) {
        Ok(source) => {
            let lang = PythonSupport::new(&py);
            let entry = py_entry_path.clone();
            benches.push(Benchmark {
                name: "py_parse_file",
                run: Box::new(move || {
                    let _ = black_box(lang.parse(black_box(&entry), black_box(&source)));
                }),
            });
        }
        Err(_) => eprintln!("Skipping py_parse_file: {} not found", py_entry_path.display()),
    }

    // ts_resolve
    if ts.join("package.json").exists() {
        let lang = TypeScriptSupport::new(&ts);
        let from_dir = ts_entry_path.parent().unwrap().to_path_buf();
        benches.push(Benchmark {
            name: "ts_resolve",
            run: Box::new(move || {
                lang.resolve(black_box(&from_dir), black_box("./index"));
            }),
        });
    } else {
        eprintln!("Skipping ts_resolve: {} not found", ts.display());
    }

    // py_resolve
    if py.join("pyproject.toml").exists() || py.join("setup.py").exists() {
        let lang = PythonSupport::new(&py);
        let root = py.clone();
        // Resolve the top-level package: env var override means real codebase (awscli),
        // otherwise synthetic corpus uses "app"
        let specifier = if std::env::var("PY_BENCH_ROOT").is_ok() {
            "awscli"
        } else {
            "app"
        };
        benches.push(Benchmark {
            name: "py_resolve",
            run: Box::new(move || {
                lang.resolve(black_box(&root), black_box(specifier));
            }),
        });
    } else {
        eprintln!("Skipping py_resolve: {} not found", py.display());
    }

    // cache_load_validate_ts — must run before build_graph benchmarks to avoid
    // OS page cache contamination: build_graph reads full file contents for 3200+
    // files, which warms the kernel page cache. cache_load does parallel stat()
    // calls that run ~10% faster on warm pages. Since save-baseline takes 50
    // build_graph iterations (21s of I/O) but comparison early-stops at 5, the
    // cache warming differs between modes, producing false positive regressions.
    if ts_entry_path.exists() {
        let lang = TypeScriptSupport::new(&ts);
        let root = ts.clone();
        let entry = ts_entry_path.clone();
        let mut cache = ParseCache::new();
        let result = chainsaw::walker::build_graph(&entry, &root, &lang, &mut cache);
        let unresolvable_count: usize =
            result.unresolvable_dynamic.iter().map(|(_, c)| c).sum();
        cache.save(
            &root,
            &entry,
            &result.graph,
            result.unresolved_specifiers,
            unresolvable_count,
        );
        benches.push(Benchmark {
            name: "cache_load_validate_ts",
            run: Box::new(move || {
                let mut loaded = ParseCache::load(black_box(&root));
                let resolve_fn = |_: &str| false;
                loaded.try_load_graph(black_box(&entry), &resolve_fn);
            }),
        });
    }

    // build_graph/ts_cold
    if ts_entry_path.exists() {
        let lang = TypeScriptSupport::new(&ts);
        let root = ts.clone();
        let entry = ts_entry_path.clone();
        benches.push(Benchmark {
            name: "build_graph/ts_cold",
            run: Box::new(move || {
                let mut cache = ParseCache::new();
                chainsaw::walker::build_graph(
                    black_box(&entry),
                    black_box(&root),
                    &lang,
                    &mut cache,
                );
            }),
        });
    }

    // build_graph/py_cold
    if py_entry_path.exists() {
        let lang = PythonSupport::new(&py);
        let root = py.clone();
        let entry = py_entry_path.clone();
        benches.push(Benchmark {
            name: "build_graph/py_cold",
            run: Box::new(move || {
                let mut cache = ParseCache::new();
                chainsaw::walker::build_graph(
                    black_box(&entry),
                    black_box(&root),
                    &lang,
                    &mut cache,
                );
            }),
        });
    }

    // query_trace_ts
    if ts_entry_path.exists() {
        let lang = TypeScriptSupport::new(&ts);
        let entry = ts_entry_path;
        let mut cache = ParseCache::new();
        let result = chainsaw::walker::build_graph(&entry, &ts, &lang, &mut cache);
        let graph = result.graph;
        let entry_id = graph.path_to_id[&entry];
        let opts = query::TraceOptions::default();
        benches.push(Benchmark {
            name: "query_trace_ts",
            run: Box::new(move || {
                black_box(query::trace(black_box(&graph), black_box(entry_id), black_box(&opts)));
            }),
        });
    }

    // query_trace_py
    if py_entry_path.exists() {
        let lang = PythonSupport::new(&py);
        let entry = py_entry_path;
        let mut cache = ParseCache::new();
        let result = chainsaw::walker::build_graph(&entry, &py, &lang, &mut cache);
        let graph = result.graph;
        let entry_id = graph.path_to_id[&entry];
        let opts = query::TraceOptions::default();
        benches.push(Benchmark {
            name: "query_trace_py",
            run: Box::new(move || {
                black_box(query::trace(black_box(&graph), black_box(entry_id), black_box(&opts)));
            }),
        });
    }

    benches
}

// --- Main ---

struct BenchResult {
    name: &'static str,
    avg_ns: f64,
    samples_count: usize,
    stop_reason: StopReason,
    cal_time: Duration,
    warm_time: Duration,
    warm_iters: usize,
    sample_time: Duration,
    total_time: Duration,
    overhead_ns: f64,
    baseline_overhead_ns: Option<f64>,
    baseline_trimmed: Option<Vec<f64>>,
    current_trimmed: Vec<f64>,
}

#[allow(clippy::too_many_lines, clippy::cast_precision_loss)]
fn main() {
    let args = Args::parse();
    let benchmarks = register_benchmarks();

    if args.list {
        for bench in &benchmarks {
            println!("{}: benchmark", bench.name);
        }
        return;
    }

    let filter = if args.filter.is_empty() {
        None
    } else {
        Some(args.filter.join("|"))
    };
    let filtered: Vec<&Benchmark> = filter.as_ref().map_or_else(
        || benchmarks.iter().collect(),
        |filter| {
            benchmarks
                .iter()
                .filter(|b| matches_filter(b.name, filter))
                .collect()
        },
    );

    if filtered.is_empty() {
        eprintln!("No benchmarks matched the filter");
        return;
    }

    let slot = args.save_baseline.as_deref().unwrap_or("new");
    let baseline_name = args.baseline.as_deref();
    let comparing = baseline_name.is_some();

    // Load stored sigma_env for noise-aware early stopping.
    // None on first comparison — all benchmarks run to MAX_SAMPLES.
    let stored_sigma = baseline_name.and_then(load_sigma_env);

    let suite_start = Instant::now();
    let mut results: Vec<BenchResult> = Vec::new();

    for bench in &filtered {
        let bench_start = Instant::now();

        let cal_start = Instant::now();
        let batch = calibrate(&bench.run);
        let cal_time = cal_start.elapsed();

        let warm_start = Instant::now();
        let warm_iters = warmup(&bench.run, batch);
        let warm_time = warm_start.elapsed();

        let baseline = baseline_name.and_then(|name| load_baseline(bench.name, name));

        let sample_start = Instant::now();
        let (samples, stop_reason) = sample(
            &bench.run,
            batch,
            baseline.as_ref().map(|b| b.per_iter.as_slice()),
            stored_sigma,
        );
        let sample_time = sample_start.elapsed();

        let overhead_ns = (cal_time + warm_time).as_nanos() as f64;
        save_samples(bench.name, slot, &samples, overhead_ns);

        let per_iter: Vec<f64> = samples.iter().map(|(b, t)| t / *b as f64).collect();
        let avg_ns = trimmed_mean(&per_iter, TRIM_FRACTION);
        let current_trimmed = trim(&per_iter, TRIM_FRACTION);
        let baseline_trimmed = baseline.as_ref().map(|b| trim(&b.per_iter, TRIM_FRACTION));

        results.push(BenchResult {
            name: bench.name,
            avg_ns,
            samples_count: samples.len(),
            stop_reason,
            cal_time,
            warm_time,
            warm_iters,
            sample_time,
            total_time: bench_start.elapsed(),
            overhead_ns,
            baseline_overhead_ns: baseline.as_ref().and_then(|b| b.overhead_ns),
            baseline_trimmed,
            current_trimmed,
        });
    }

    // Unified verdict pipeline: session bias → noise floor → noise-aware test.
    // Same path for every benchmark regardless of early stopping.
    let (session_drift, effective_sigma) = if comparing {
        let change_pcts: Vec<f64> = results
            .iter()
            .filter_map(|r| {
                r.baseline_trimmed
                    .as_ref()
                    .map(|base| (mean(&r.current_trimmed) - mean(base)) / mean(base))
            })
            .collect();

        if change_pcts.is_empty() {
            (0.0, 0.0)
        } else {
            let (adjusted, drift) = session_bias_adjust(&change_pcts);
            let fresh_sigma = noise_floor(&adjusted, NOISE_FLOOR_MIN);
            // Use the more conservative of stored vs fresh sigma_env
            let effective = stored_sigma.map_or(fresh_sigma, |stored| stored.max(fresh_sigma));
            // Store fresh sigma_env for future comparisons
            if let Some(name) = baseline_name {
                save_sigma_env(name, fresh_sigma);
            }
            (drift, effective)
        }
    } else {
        (0.0, 0.0)
    };

    // Report: single verdict path for all benchmarks
    for result in &results {
        let verdict_str = result.baseline_trimmed.as_ref().map_or_else(
            || "baseline saved".to_string(),
            |base| {
                // Subtract session drift from candidate samples (doesn't change variance)
                let drift_ns = session_drift * mean(base);
                let adjusted_candidate: Vec<f64> =
                    result.current_trimmed.iter().map(|x| x - drift_ns).collect();
                let p = noise_aware_welch_t_test(base, &adjusted_candidate, effective_sigma);
                let adjusted_change = (mean(&adjusted_candidate) - mean(base)) / mean(base);
                let raw_change = (mean(&result.current_trimmed) - mean(base)) / mean(base);

                if p < VERDICT_P && adjusted_change > REGRESSION_THRESHOLD {
                    format!(
                        "REGRESSION +{:.1}%, p={:.4} (raw {:+.1}%)",
                        adjusted_change * 100.0,
                        p,
                        raw_change * 100.0,
                    )
                } else if p < VERDICT_P && adjusted_change < -REGRESSION_THRESHOLD {
                    format!(
                        "faster {:.1}%, p={:.4} (raw {:+.1}%)",
                        adjusted_change * 100.0,
                        p,
                        raw_change * 100.0,
                    )
                } else {
                    format!(
                        "clean {:+.1}%, p={:.2} (raw {:+.1}%)",
                        adjusted_change * 100.0,
                        p,
                        raw_change * 100.0,
                    )
                }
            },
        );

        let samples_label = match result.stop_reason {
            StopReason::EarlyStop => format!("{} samples (early stop)", result.samples_count),
            _ => format!("{} samples", result.samples_count),
        };

        eprintln!(
            "{}: {} | calibrate {}ms, warmup {}ms ({} iters), {} in {}ms [{}]",
            result.name,
            format_time(result.avg_ns),
            result.cal_time.as_millis(),
            result.warm_time.as_millis(),
            result.warm_iters,
            samples_label,
            result.sample_time.as_millis(),
            verdict_str,
        );

        let overhead_ms = result.cal_time.as_millis() + result.warm_time.as_millis();
        let useful_ms = result.sample_time.as_millis();
        if useful_ms > 0 {
            eprintln!(
                "  overhead: {}ms / {}ms useful = {:.0}%",
                overhead_ms,
                useful_ms,
                overhead_ms as f64 / useful_ms as f64 * 100.0,
            );
        }

        if let Some(baseline_overhead) = result.baseline_overhead_ns {
            let ratio = result.overhead_ns / baseline_overhead;
            let increase = result.overhead_ns - baseline_overhead;
            if ratio > OVERHEAD_WARN_RATIO && increase > OVERHEAD_WARN_MIN_INCREASE_NS {
                eprintln!(
                    "  WARNING: overhead increased {:.1}x ({} -> {})",
                    ratio,
                    format_time(baseline_overhead),
                    format_time(result.overhead_ns),
                );
            }
        }

        eprintln!("  total: {}ms", result.total_time.as_millis());
    }

    if comparing {
        eprintln!(
            "\nSession drift: {:+.1}%, noise floor: {:.1}% (sigma_env {})",
            session_drift * 100.0,
            effective_sigma * 100.0,
            if stored_sigma.is_some() {
                "stored"
            } else {
                "calibrated"
            },
        );
    }

    eprintln!(
        "\nSuite complete: {} benchmarks in {:.1}s",
        results.len(),
        suite_start.elapsed().as_secs_f64(),
    );
}
