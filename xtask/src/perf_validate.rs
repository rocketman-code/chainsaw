use crate::perf_judge::{self, BenchResult};
use crate::registry::Registry;
use serde::Serialize;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Serialize)]
struct Attestation {
    commit_sha: String,
    timestamp: String,
    required_benchmarks: Vec<String>,
    overall: String,
}

/// Run perf-validate.
///
/// Two modes:
/// - Gate mode (no --baseline): check files changed since origin/main,
///   validate affected benchmarks, write attestation on pass.
/// - Ad-hoc mode (--baseline NAME): compare all benchmarks (or specified ones)
///   against the named baseline. No attestation written.
///
/// Both modes use confirmation runs to eliminate false positives.
pub fn run(baseline: Option<&str>, benchmark_args: &[String]) -> i32 {
    let root = project_root();

    let registry = match Registry::load(&root) {
        Some(r) => r,
        None => {
            eprintln!("No perf.toml found. Nothing to validate.");
            return 0;
        }
    };

    let baseline_name = baseline.unwrap_or("main");

    // Determine which benchmarks to check
    let required: BTreeSet<String> = if baseline.is_some() {
        // Ad-hoc mode: use specified benchmarks or all from registry
        if benchmark_args.is_empty() {
            registry.all_benchmarks()
        } else {
            benchmark_args.iter().cloned().collect()
        }
    } else {
        // Gate mode: only benchmarks affected by changed files
        let changed = changed_files(&root);
        if changed.is_empty() {
            println!("No files changed since origin/main. Nothing to validate.");
            return 0;
        }
        let required = registry.required_benchmarks(&changed);
        if required.is_empty() {
            println!("No perf-sensitive files changed. Nothing to validate.");
            return 0;
        }
        required
    };

    println!("Benchmarks to validate (baseline: {baseline_name}):");
    for bench in &required {
        println!("  - {bench}");
    }
    println!();

    // Check criterion data exists
    let criterion_dir = root.join("target/criterion");
    let mut dirs = Vec::new();
    let mut missing = Vec::new();

    for bench in &required {
        let bench_dir = criterion_dir.join(bench);
        let has_baseline = bench_dir.join(format!("{baseline_name}/sample.json")).exists();
        let has_candidate = bench_dir.join("new/sample.json").exists();

        if !has_baseline || !has_candidate {
            missing.push((bench.clone(), has_baseline, has_candidate));
        } else {
            dirs.push(bench_dir.to_string_lossy().to_string());
        }
    }

    if !missing.is_empty() {
        eprintln!("Missing criterion data:");
        for (bench, has_baseline, has_candidate) in &missing {
            let what = match (has_baseline, has_candidate) {
                (false, false) => format!("no baseline or candidate"),
                (false, true) => format!("no baseline (run --save-baseline {baseline_name})"),
                (true, false) => format!("no candidate (run cargo bench)"),
                _ => unreachable!(),
            };
            eprintln!("  - {bench}: {what}");
        }
        eprintln!();
        eprintln!(
            "Run: cargo bench --bench benchmarks -- --save-baseline {baseline_name}  (to set baseline)"
        );
        eprintln!(
            "Run: cargo bench --bench benchmarks -- --baseline {baseline_name}       (to test candidate)"
        );
        return 1;
    }

    // Judge
    let results = perf_judge::judge(&dirs, baseline_name, &criterion_dir);
    perf_judge::print_results(&results);

    // Confirmation runs for any failures
    if let Some(exit) = confirm_failures(&results, &criterion_dir, &root, baseline_name) {
        return exit;
    }

    println!("\nAll benchmarks passed.");

    // Only write attestation in gate mode
    if baseline.is_none() {
        if let Err(e) = write_attestation(&root, &required) {
            eprintln!("Failed to write attestation: {e}");
            return 1;
        }
        println!("Attestation written to .git/perf-attestation.json");
        println!("You can now push.");
    }

    0
}

/// If there are failures, re-bench and re-judge to confirm.
/// Returns Some(exit_code) if regression confirmed or bench failed,
/// None if all clear (either no failures or noise dismissed).
fn confirm_failures(
    results: &[BenchResult],
    criterion_dir: &Path,
    root: &Path,
    baseline_name: &str,
) -> Option<i32> {
    let failed: Vec<&BenchResult> = results.iter().filter(|r| r.verdict.is_fail()).collect();
    if failed.is_empty() {
        return None;
    }

    let failed_names: Vec<&str> = failed.iter().map(|r| r.name.as_str()).collect();
    println!(
        "\n{} regression(s) detected. Running confirmation...",
        failed.len()
    );
    for name in &failed_names {
        println!("  - {name}");
    }
    println!();

    // Re-run criterion for just the failed benchmarks
    let filter = failed_names.join("|");
    let status = Command::new("cargo")
        .args([
            "bench",
            "--bench",
            "benchmarks",
            "--",
            "--baseline",
            baseline_name,
            &filter,
        ])
        .current_dir(root)
        .status();

    match status {
        Ok(s) if s.success() => {}
        Ok(s) => {
            eprintln!("Confirmation bench exited with {s}");
            return Some(1);
        }
        Err(e) => {
            eprintln!("Failed to run confirmation bench: {e}");
            return Some(1);
        }
    }

    // Re-judge only the failed dirs
    let failed_dirs: Vec<String> = failed
        .iter()
        .map(|r| criterion_dir.join(&r.name).to_string_lossy().to_string())
        .collect();
    let confirm_results = perf_judge::judge(&failed_dirs, baseline_name, criterion_dir);

    println!("\nConfirmation results:");
    perf_judge::print_results(&confirm_results);

    let still_failing: Vec<&BenchResult> = confirm_results
        .iter()
        .filter(|r| r.verdict.is_fail())
        .collect();

    if !still_failing.is_empty() {
        eprintln!(
            "\nRegression confirmed ({}/{} still failing).",
            still_failing.len(),
            failed.len()
        );
        return Some(1);
    }

    println!("\nInitial regression(s) not reproducible. Treating as noise.");
    None
}

fn project_root() -> PathBuf {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .expect("failed to run git");
    PathBuf::from(String::from_utf8(output.stdout).unwrap().trim())
}

fn changed_files(root: &Path) -> Vec<String> {
    let output = Command::new("git")
        .args(["diff", "--name-only", "origin/main...HEAD"])
        .current_dir(root)
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .map(String::from)
        .collect()
}

fn commit_sha(root: &Path) -> String {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(root)
        .output()
        .expect("failed to run git rev-parse");
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

fn write_attestation(
    root: &Path,
    required_benchmarks: &BTreeSet<String>,
) -> Result<(), String> {
    let attestation = Attestation {
        commit_sha: commit_sha(root),
        timestamp: now_utc(),
        required_benchmarks: required_benchmarks.iter().cloned().collect(),
        overall: "pass".to_string(),
    };

    let json = serde_json::to_string_pretty(&attestation).map_err(|e| format!("json: {e}"))?;
    let path = root.join(".git/perf-attestation.json");
    std::fs::write(&path, json).map_err(|e| format!("write {}: {e}", path.display()))?;

    Ok(())
}

fn now_utc() -> String {
    let output = Command::new("date")
        .args(["-u", "+%Y-%m-%dT%H:%M:%SZ"])
        .output()
        .expect("failed to run date");
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}
