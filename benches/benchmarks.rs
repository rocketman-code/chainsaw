use std::collections::VecDeque;
use std::hint::black_box;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use chainsaw::cache::ParseCache;
use chainsaw::lang::python::PythonSupport;
use chainsaw::lang::typescript::TypeScriptSupport;
use chainsaw::lang::LanguageSupport;
use chainsaw::query;
use stats::{cv, mean, trim, trimmed_mean, welch_t_test};

// --- Constants ---

const MIN_SAMPLES: usize = 5;
const MAX_SAMPLES: usize = 50;
const WARMUP_WINDOW: usize = 5;
const WARMUP_CV_THRESHOLD: f64 = 0.02;
const MAX_WARMUP_ITERATIONS: usize = 100;
const TARGET_MEASUREMENT_NS: u64 = 1_000_000; // 1ms
const EARLY_STOP_P_REGRESSION: f64 = 0.001;
const EARLY_STOP_P_CLEAN: f64 = 0.10;
const REGRESSION_THRESHOLD: f64 = 0.02;
const TRIM_FRACTION: f64 = 0.10;
// Both thresholds must be exceeded to warn (AND condition). This exploits
// a natural asymmetry: fast benchmarks have high ratio variance but tiny
// absolute variance, slow benchmarks have the opposite. Empirically validated
// against 25 runs — zero false positives, catches warmup regressions > 500ms.
const OVERHEAD_WARN_RATIO: f64 = 3.0;
const OVERHEAD_WARN_MIN_INCREASE_NS: f64 = 500_000_000.0; // 500ms

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
            black_box(f());
        }
        if start.elapsed() >= Duration::from_nanos(TARGET_MEASUREMENT_NS) {
            return batch;
        }
        batch *= 2;
    }
}

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
        if window.len() == WARMUP_WINDOW && cv(&Vec::from(window.clone())) < WARMUP_CV_THRESHOLD {
            return count;
        }
        if count >= MAX_WARMUP_ITERATIONS {
            return count;
        }
    }
}

fn measure(f: &dyn Fn(), batch: u64) -> (u64, f64) {
    let start = Instant::now();
    for _ in 0..batch {
        black_box(f());
    }
    (batch, start.elapsed().as_nanos() as f64)
}

enum StopReason {
    Regression(f64, f64), // (change_pct, p_value)
    Faster(f64, f64),     // (change_pct, p_value)
    Clean(f64, f64),      // (change_pct, p_value)
    MaxSamples(f64, f64), // (change_pct, p_value)
    NoBaseline,
}

fn sample(
    f: &dyn Fn(),
    batch: u64,
    baseline: Option<&[f64]>,
) -> (Vec<(u64, f64)>, StopReason) {
    let mut samples: Vec<(u64, f64)> = Vec::new();

    loop {
        samples.push(measure(f, batch));

        if let Some(base) = baseline {
            if samples.len() >= MIN_SAMPLES {
                let per_iter: Vec<f64> =
                    samples.iter().map(|(b, t)| t / *b as f64).collect();
                let base_trimmed = trim(base, TRIM_FRACTION);
                let sample_trimmed = trim(&per_iter, TRIM_FRACTION);
                let p = welch_t_test(&base_trimmed, &sample_trimmed);
                let change_pct =
                    (mean(&sample_trimmed) - mean(&base_trimmed)) / mean(&base_trimmed);

                if p < EARLY_STOP_P_REGRESSION && change_pct > REGRESSION_THRESHOLD {
                    return (samples, StopReason::Regression(change_pct, p));
                }
                if p < EARLY_STOP_P_REGRESSION && change_pct < -REGRESSION_THRESHOLD {
                    return (samples, StopReason::Faster(change_pct, p));
                }
                if p > EARLY_STOP_P_CLEAN {
                    return (samples, StopReason::Clean(change_pct, p));
                }
            }
        }

        if samples.len() >= MAX_SAMPLES {
            if let Some(base) = baseline {
                let per_iter: Vec<f64> =
                    samples.iter().map(|(b, t)| t / *b as f64).collect();
                let base_trimmed = trim(base, TRIM_FRACTION);
                let sample_trimmed = trim(&per_iter, TRIM_FRACTION);
                let p = welch_t_test(&base_trimmed, &sample_trimmed);
                let change_pct =
                    (mean(&sample_trimmed) - mean(&base_trimmed)) / mean(&base_trimmed);
                return (samples, StopReason::MaxSamples(change_pct, p));
            }
            return (samples, StopReason::NoBaseline);
        }
    }
}

// --- I/O ---

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
        s.push_str(&format!("{v}"));
    }
    s.push(']');
    s
}

struct Baseline {
    per_iter: Vec<f64>,
    overhead_ns: Option<f64>,
}

fn load_baseline(name: &str, baseline_name: &str) -> Option<Baseline> {
    let path = PathBuf::from("target/criterion")
        .join(name)
        .join(baseline_name)
        .join("sample.json");
    let content = std::fs::read_to_string(&path).ok()?;

    #[derive(serde::Deserialize)]
    struct Sample {
        iters: Vec<f64>,
        times: Vec<f64>,
        #[serde(default)]
        overhead_ns: Option<f64>,
    }
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

fn format_time(nanos: f64) -> String {
    if nanos < 1_000.0 {
        format!("{nanos:.0}ns")
    } else if nanos < 1_000_000.0 {
        format!("{:.1}us", nanos / 1_000.0)
    } else if nanos < 1_000_000_000.0 {
        format!("{:.1}ms", nanos / 1_000_000.0)
    } else {
        format!("{:.2}s", nanos / 1_000_000_000.0)
    }
}

// --- CLI ---

struct Args {
    save_baseline: Option<String>,
    baseline: Option<String>,
    list: bool,
    filter: Option<String>,
}

fn parse_args() -> Args {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut save_baseline = None;
    let mut baseline = None;
    let mut list = false;
    let mut filter_parts: Vec<String> = Vec::new();

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--save-baseline" => {
                i += 1;
                save_baseline = Some(args.get(i).unwrap_or_else(|| {
                    eprintln!("error: --save-baseline requires a value");
                    std::process::exit(1);
                }).clone());
            }
            "--baseline" => {
                i += 1;
                baseline = Some(args.get(i).unwrap_or_else(|| {
                    eprintln!("error: --baseline requires a value");
                    std::process::exit(1);
                }).clone());
            }
            "--list" => list = true,
            "--bench" => {} // ignored (cargo passes this)
            other if !other.starts_with('-') => filter_parts.push(other.to_string()),
            _ => {} // ignore unknown flags (cargo bench passes extras)
        }
        i += 1;
    }

    let filter = if filter_parts.is_empty() {
        None
    } else {
        Some(filter_parts.join("|"))
    };

    Args {
        save_baseline,
        baseline,
        list,
        filter,
    }
}

fn matches_filter(name: &str, filter: &str) -> bool {
    filter.split('|').any(|part| name.contains(part))
}

// --- Benchmark registration ---

fn ts_root() -> PathBuf {
    std::env::var("TS_BENCH_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/Users/hlal/dev/cloudflare/workers-sdk"))
}

fn py_root() -> PathBuf {
    std::env::var("PY_BENCH_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/Users/hlal/dev/aws/aws-cli"))
}

fn register_benchmarks() -> Vec<Benchmark> {
    let mut benches = Vec::new();

    let ts = ts_root();
    let py = py_root();

    // ts_parse_file
    let entry = ts.join("packages/wrangler/src/index.ts");
    match std::fs::read_to_string(&entry) {
        Ok(source) => {
            let lang = TypeScriptSupport::new(&ts);
            let entry = entry.clone();
            benches.push(Benchmark {
                name: "ts_parse_file",
                run: Box::new(move || {
                    let _ = black_box(lang.parse(black_box(&entry), black_box(&source)));
                }),
            });
        }
        Err(_) => eprintln!("Skipping ts_parse_file: {} not found", entry.display()),
    }

    // py_parse_file
    let entry = py.join("awscli/__init__.py");
    match std::fs::read_to_string(&entry) {
        Ok(source) => {
            let lang = PythonSupport::new(&py);
            let entry = entry.clone();
            benches.push(Benchmark {
                name: "py_parse_file",
                run: Box::new(move || {
                    let _ = black_box(lang.parse(black_box(&entry), black_box(&source)));
                }),
            });
        }
        Err(_) => eprintln!("Skipping py_parse_file: {} not found", entry.display()),
    }

    // ts_resolve
    if ts.join("package.json").exists() {
        let lang = TypeScriptSupport::new(&ts);
        let from_dir = ts.join("packages/wrangler/src");
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
        benches.push(Benchmark {
            name: "py_resolve",
            run: Box::new(move || {
                lang.resolve(black_box(&root), black_box("awscli"));
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
    let entry = ts.join("packages/wrangler/src/index.ts");
    if entry.exists() {
        let lang = TypeScriptSupport::new(&ts);
        let root = ts.clone();
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
    let entry = ts.join("packages/wrangler/src/index.ts");
    if entry.exists() {
        let lang = TypeScriptSupport::new(&ts);
        let root = ts.clone();
        let entry = entry.clone();
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
    let entry = py.join("awscli/__init__.py");
    if entry.exists() {
        let lang = PythonSupport::new(&py);
        let root = py.clone();
        let entry = entry.clone();
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
    let entry = ts.join("packages/wrangler/src/index.ts");
    if entry.exists() {
        let lang = TypeScriptSupport::new(&ts);
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
    let entry = py.join("awscli/__init__.py");
    if entry.exists() {
        let lang = PythonSupport::new(&py);
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

fn main() {
    let args = parse_args();
    let benchmarks = register_benchmarks();

    if args.list {
        for bench in &benchmarks {
            println!("{}: benchmark", bench.name);
        }
        return;
    }

    let filtered: Vec<&Benchmark> = if let Some(ref filter) = args.filter {
        benchmarks
            .iter()
            .filter(|b| matches_filter(b.name, filter))
            .collect()
    } else {
        benchmarks.iter().collect()
    };

    if filtered.is_empty() {
        eprintln!("No benchmarks matched the filter");
        return;
    }

    let slot = args.save_baseline.as_deref().unwrap_or("new");

    let suite_start = Instant::now();

    for bench in &filtered {
        let bench_start = Instant::now();

        // Calibrate
        let cal_start = Instant::now();
        let batch = calibrate(&bench.run);
        let cal_time = cal_start.elapsed();

        // Warmup
        let warm_start = Instant::now();
        let warm_iters = warmup(&bench.run, batch);
        let warm_time = warm_start.elapsed();

        // Load baseline if comparing
        let baseline = args
            .baseline
            .as_ref()
            .and_then(|name| load_baseline(bench.name, name));

        // Sample
        let sample_start = Instant::now();
        let (samples, stop_reason) =
            sample(&bench.run, batch, baseline.as_ref().map(|b| b.per_iter.as_slice()));
        let sample_time = sample_start.elapsed();

        // Save
        let overhead_ns = (cal_time + warm_time).as_nanos() as f64;
        save_samples(bench.name, slot, &samples, overhead_ns);

        // Report
        let per_iter: Vec<f64> = samples.iter().map(|(b, t)| t / *b as f64).collect();
        let avg = trimmed_mean(&per_iter, TRIM_FRACTION);
        let total = bench_start.elapsed();

        let reason_str = match stop_reason {
            StopReason::Regression(pct, p) => {
                format!("REGRESSION +{:.1}%, p={:.4}", pct * 100.0, p)
            }
            StopReason::Faster(pct, p) => {
                format!("faster {:.1}%, p={:.4}", pct * 100.0, p)
            }
            StopReason::Clean(pct, p) => {
                format!("clean {:+.1}%, p={:.2}", pct * 100.0, p)
            }
            StopReason::MaxSamples(pct, p) => {
                format!("max samples {:+.1}%, p={:.4}", pct * 100.0, p)
            }
            StopReason::NoBaseline => "baseline saved".to_string(),
        };

        eprintln!(
            "{}: {} | calibrate {}ms, warmup {}ms ({} iters), {} samples in {}ms [{}]",
            bench.name,
            format_time(avg),
            cal_time.as_millis(),
            warm_time.as_millis(),
            warm_iters,
            samples.len(),
            sample_time.as_millis(),
            reason_str,
        );

        // Also report overhead ratio
        let overhead_ms = cal_time.as_millis() + warm_time.as_millis();
        let useful_ms = sample_time.as_millis();
        if useful_ms > 0 {
            eprintln!(
                "  overhead: {}ms / {}ms useful = {:.0}%",
                overhead_ms,
                useful_ms,
                overhead_ms as f64 / useful_ms as f64 * 100.0,
            );
        }

        // Warn if overhead increased significantly vs baseline
        if let Some(baseline_overhead) = baseline.as_ref().and_then(|b| b.overhead_ns) {
            let ratio = overhead_ns / baseline_overhead;
            let increase = overhead_ns - baseline_overhead;
            if ratio > OVERHEAD_WARN_RATIO && increase > OVERHEAD_WARN_MIN_INCREASE_NS {
                eprintln!(
                    "  WARNING: overhead increased {:.1}x ({} -> {})",
                    ratio,
                    format_time(baseline_overhead),
                    format_time(overhead_ns),
                );
            }
        }

        eprintln!(
            "  total: {}ms",
            total.as_millis(),
        );
    }

    eprintln!(
        "\nSuite complete: {} benchmarks in {:.1}s",
        filtered.len(),
        suite_start.elapsed().as_secs_f64(),
    );
}
