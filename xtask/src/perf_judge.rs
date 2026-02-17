use serde::Deserialize;
use stats::{
    format_time, mean, noise_aware_welch_t_test, noise_floor, session_bias_adjust, trim,
    NOISE_FLOOR_MIN, REGRESSION_THRESHOLD, TRIM_FRACTION, VERDICT_P,
};
use std::path::Path;

#[derive(Deserialize)]
struct CriterionSample {
    iters: Vec<f64>,
    times: Vec<f64>,
}

pub struct BenchResult {
    pub name: String,
    pub baseline_mean: f64,
    pub candidate_mean: f64,
    pub raw_change_pct: f64,
    pub adjusted_change_pct: f64,
    pub p_value: f64,
    pub verdict: Verdict,
}

#[derive(Debug)]
pub enum Verdict {
    Pass,
    Faster,
    Fail,
}

impl Verdict {
    pub fn is_fail(&self) -> bool {
        matches!(self, Verdict::Fail)
    }
}

impl std::fmt::Display for Verdict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Verdict::Pass => write!(f, "pass"),
            Verdict::Faster => write!(f, "faster"),
            Verdict::Fail => write!(f, "FAIL"),
        }
    }
}

struct LoadedBench {
    name: String,
    baseline_trimmed: Vec<f64>,
    candidate_trimmed: Vec<f64>,
    baseline_mean: f64,
    candidate_mean: f64,
    raw_change_pct: f64,
}

/// Judge criterion benchmark directories using the unified noise-aware pipeline.
/// Same algorithm as benchmarks.rs: session bias → noise floor → noise-aware t-test.
pub fn judge(dirs: &[String], baseline_name: &str, criterion_dir: &Path) -> Vec<BenchResult> {
    // Phase 1: load all samples, compute raw changes
    let mut loaded = Vec::new();
    for dir in dirs {
        let path = Path::new(dir);
        let name = extract_bench_name(path);

        let baseline_path = path.join(format!("{baseline_name}/sample.json"));
        let candidate_path = path.join("new/sample.json");

        if !baseline_path.exists() || !candidate_path.exists() {
            eprintln!("  SKIP {name}: missing baseline or candidate data");
            continue;
        }

        let baseline = match load_samples(&baseline_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("  ERROR {name}: {e}");
                continue;
            }
        };
        let candidate = match load_samples(&candidate_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("  ERROR {name}: {e}");
                continue;
            }
        };

        let baseline_trimmed = trim(&baseline, TRIM_FRACTION);
        let candidate_trimmed = trim(&candidate, TRIM_FRACTION);
        let baseline_mean = mean(&baseline_trimmed);
        let candidate_mean = mean(&candidate_trimmed);
        let raw_change_pct = (candidate_mean - baseline_mean) / baseline_mean;

        loaded.push(LoadedBench {
            name,
            baseline_trimmed,
            candidate_trimmed,
            baseline_mean,
            candidate_mean,
            raw_change_pct,
        });
    }

    if loaded.is_empty() {
        return Vec::new();
    }

    // Phase 2: session bias correction + noise floor estimation
    let raw_changes: Vec<f64> = loaded.iter().map(|l| l.raw_change_pct).collect();
    let (adjusted_changes, drift) = session_bias_adjust(&raw_changes);
    let fresh_sigma = noise_floor(&adjusted_changes, NOISE_FLOOR_MIN);
    let stored_sigma = load_sigma_env(criterion_dir, baseline_name);
    let effective_sigma = match stored_sigma {
        Some(s) => s.max(fresh_sigma),
        None => fresh_sigma,
    };

    if loaded.len() >= 3 {
        println!(
            "Session drift: {:+.1}%, noise floor: {:.1}% (sigma_env {})",
            drift * 100.0,
            effective_sigma * 100.0,
            if stored_sigma.is_some() { "stored" } else { "calibrated" },
        );
    }

    // Phase 3: per-benchmark verdicts with drift-adjusted candidates
    let mut results = Vec::new();
    for (i, l) in loaded.iter().enumerate() {
        let drift_ns = drift * l.baseline_mean;
        let adjusted_candidate: Vec<f64> =
            l.candidate_trimmed.iter().map(|x| x - drift_ns).collect();
        let p_value = noise_aware_welch_t_test(
            &l.baseline_trimmed,
            &adjusted_candidate,
            effective_sigma,
        );
        let adjusted_change = adjusted_changes[i];

        let verdict = if p_value < VERDICT_P && adjusted_change > REGRESSION_THRESHOLD {
            Verdict::Fail
        } else if p_value < VERDICT_P && adjusted_change < -REGRESSION_THRESHOLD {
            Verdict::Faster
        } else {
            Verdict::Pass
        };

        results.push(BenchResult {
            name: l.name.clone(),
            baseline_mean: l.baseline_mean,
            candidate_mean: l.candidate_mean,
            raw_change_pct: l.raw_change_pct,
            adjusted_change_pct: adjusted_change,
            p_value,
            verdict,
        });
    }

    results
}

fn load_sigma_env(criterion_dir: &Path, baseline_name: &str) -> Option<f64> {
    let path = criterion_dir.join(format!("sigma_env_{baseline_name}.json"));
    let content = std::fs::read_to_string(path).ok()?;
    #[derive(Deserialize)]
    struct SigmaEnv {
        sigma_env: f64,
    }
    let data: SigmaEnv = serde_json::from_str(&content).ok()?;
    Some(data.sigma_env)
}

/// Print a results table to stdout.
pub fn print_results(results: &[BenchResult]) {
    if results.is_empty() {
        return;
    }
    println!(
        "{:<35} {:>12} {:>12} {:>8} {:>8} {:>8}  {}",
        "Benchmark", "Baseline", "Candidate", "Adj", "Raw", "p-value", "Verdict"
    );
    println!("{}", "-".repeat(95));
    for r in results {
        println!(
            "{:<35} {:>12} {:>12} {:>+7.1}% {:>+7.1}% {:>8.4}  {}",
            r.name,
            format_time(r.baseline_mean),
            format_time(r.candidate_mean),
            r.adjusted_change_pct * 100.0,
            r.raw_change_pct * 100.0,
            r.p_value,
            r.verdict,
        );
    }
}

fn extract_bench_name(path: &Path) -> String {
    let s = path.to_string_lossy();
    if let Some(pos) = s.find("criterion/") {
        s[pos + "criterion/".len()..].trim_end_matches('/').to_string()
    } else {
        path.file_name()
            .map(|f| f.to_string_lossy().to_string())
            .unwrap_or_else(|| s.to_string())
    }
}

fn load_samples(path: &Path) -> Result<Vec<f64>, String> {
    let content =
        std::fs::read_to_string(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    let sample: CriterionSample =
        serde_json::from_str(&content).map_err(|e| format!("failed to parse {}: {e}", path.display()))?;

    if sample.iters.len() != sample.times.len() {
        return Err(format!(
            "mismatched array lengths: {} iters vs {} times",
            sample.iters.len(),
            sample.times.len()
        ));
    }

    Ok(sample
        .iters
        .iter()
        .zip(sample.times.iter())
        .map(|(iters, time)| time / iters)
        .collect())
}


#[cfg(test)]
mod tests {
    use super::*;

    /// Create synthetic criterion sample data.
    /// Returns per-iteration times (ns).
    fn synthetic_samples(mean_ns: f64, noise: f64, n: usize) -> Vec<f64> {
        // Deterministic pseudo-noise: use a simple LCG
        let mut rng = 12345_u64;
        (0..n)
            .map(|_| {
                rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                let uniform = (rng >> 33) as f64 / (1u64 << 31) as f64; // 0..1
                mean_ns + (uniform - 0.5) * 2.0 * noise
            })
            .collect()
    }

    #[test]
    fn judge_returns_structured_results() {
        // Write synthetic criterion data to temp dirs
        let tmp = tempfile::tempdir().unwrap();
        let bench_a = tmp.path().join("bench_a");
        let bench_b = tmp.path().join("bench_b");

        write_criterion_sample(&bench_a, "main", 100.0, 1.0, 50);
        write_criterion_sample(&bench_a, "new", 100.0, 1.0, 50); // no change
        write_criterion_sample(&bench_b, "main", 100.0, 1.0, 50);
        write_criterion_sample(&bench_b, "new", 115.0, 1.0, 50); // 15% regression

        let dirs = vec![
            bench_a.to_string_lossy().to_string(),
            bench_b.to_string_lossy().to_string(),
        ];
        let results = judge(&dirs, "main", tmp.path());

        assert_eq!(results.len(), 2);

        // bench_a: no change -> pass or faster
        assert!(
            !results[0].verdict.is_fail(),
            "no-change benchmark should not fail"
        );

        // bench_b: 15% regression -> fail
        assert!(
            results[1].verdict.is_fail(),
            "15% regression should fail, got {:?}",
            results[1].verdict
        );
        assert!(results[1].raw_change_pct > 0.10);
    }

    #[test]
    fn judge_failed_names_returns_only_failures() {
        let tmp = tempfile::tempdir().unwrap();
        let pass_dir = tmp.path().join("fast_bench");
        let fail_dir = tmp.path().join("slow_bench");

        write_criterion_sample(&pass_dir, "main", 100.0, 1.0, 50);
        write_criterion_sample(&pass_dir, "new", 95.0, 1.0, 50); // faster
        write_criterion_sample(&fail_dir, "main", 100.0, 1.0, 50);
        write_criterion_sample(&fail_dir, "new", 110.0, 1.0, 50); // 10% regression

        let dirs = vec![
            pass_dir.to_string_lossy().to_string(),
            fail_dir.to_string_lossy().to_string(),
        ];
        let results = judge(&dirs, "main", tmp.path());
        let failed: Vec<&str> = results
            .iter()
            .filter(|r| r.verdict.is_fail())
            .map(|r| r.name.as_str())
            .collect();

        assert_eq!(failed, vec!["slow_bench"]);
    }

    #[test]
    fn outlier_skewed_baseline_does_not_false_positive() {
        // Reproduce the ts_resolve bug: 50-sample baseline with fast outliers
        // pulls raw mean from ~134ns to ~133ns, making normal 136ns candidate
        // look like a 2.3% regression. With trimmed mean, both are ~134ns → clean.
        let tmp = tempfile::tempdir().unwrap();
        let bench = tmp.path().join("ts_resolve");

        // Baseline: 45 normal + 5 fast outliers
        let mut baseline = vec![134.0; 45];
        baseline.extend_from_slice(&[123.0, 124.0, 125.0, 126.0, 127.0]);
        write_raw_sample(&bench, "main", &baseline);

        // Candidate: 5 normal samples
        let candidate = vec![136.0; 5];
        write_raw_sample(&bench, "new", &candidate);

        let dirs = vec![bench.to_string_lossy().to_string()];
        let results = judge(&dirs, "main", tmp.path());

        assert_eq!(results.len(), 1);
        assert!(
            !results[0].verdict.is_fail(),
            "outlier-skewed baseline should not cause false positive, \
             got change={:.1}%, p={:.4}",
            results[0].adjusted_change_pct * 100.0,
            results[0].p_value,
        );
    }

    /// Write raw per-iteration times directly (iters=1 for each).
    fn write_raw_sample(dir: &std::path::Path, slot: &str, per_iter_ns: &[f64]) {
        let slot_dir = dir.join(slot);
        std::fs::create_dir_all(&slot_dir).unwrap();
        let iters: Vec<f64> = vec![1.0; per_iter_ns.len()];
        let json = serde_json::json!({
            "sampling_mode": "Linear",
            "iters": iters,
            "times": per_iter_ns,
        });
        std::fs::write(
            slot_dir.join("sample.json"),
            serde_json::to_string(&json).unwrap(),
        )
        .unwrap();
    }

    /// Write a synthetic criterion sample.json to dir/{slot}/sample.json.
    fn write_criterion_sample(
        dir: &std::path::Path,
        slot: &str,
        mean_ns: f64,
        noise: f64,
        n: usize,
    ) {
        let slot_dir = dir.join(slot);
        std::fs::create_dir_all(&slot_dir).unwrap();

        let samples = synthetic_samples(mean_ns, noise, n);
        // criterion format: iters=[1,1,...], times=[t1,t2,...]
        let iters: Vec<f64> = vec![1.0; n];
        let json = serde_json::json!({
            "sampling_mode": "Linear",
            "iters": iters,
            "times": samples,
        });
        std::fs::write(
            slot_dir.join("sample.json"),
            serde_json::to_string(&json).unwrap(),
        )
        .unwrap();
    }
}
