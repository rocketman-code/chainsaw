use crate::perf_judge::{self, BenchResult};
use crate::registry::Registry;
use serde::Serialize;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Serialize)]
struct Attestation {
    commit_sha: String,
    timestamp: String,
    required_benchmarks: Vec<String>,
    overall: String,
}

/// Run perf-validate: check that criterion benchmarks pass for changed files.
/// Returns exit code.
pub fn run() -> i32 {
    let root = project_root();

    let registry = match Registry::load(&root) {
        Some(r) => r,
        None => {
            eprintln!("No perf.toml found. Nothing to validate.");
            return 0;
        }
    };

    let changed = changed_files(&root);
    if changed.is_empty() {
        println!("No files changed. Nothing to validate.");
        return 0;
    }

    let required = registry.required_benchmarks(&changed);
    if required.is_empty() {
        println!("No perf-sensitive files changed. Nothing to validate.");
        return 0;
    }

    println!("Perf-sensitive files changed. Required benchmarks:");
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
        let has_baseline = bench_dir.join("main/sample.json").exists();
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
                (false, false) => "no baseline or candidate",
                (false, true) => "no baseline (run --save-baseline main)",
                (true, false) => "no candidate (run cargo bench)",
                _ => unreachable!(),
            };
            eprintln!("  - {bench}: {what}");
        }
        eprintln!();
        eprintln!("Run: cargo bench -- --save-baseline main  (to set baseline)");
        eprintln!("Run: cargo bench -- --baseline main       (to test candidate)");
        return 1;
    }

    // Run perf-judge
    let results = perf_judge::judge(&dirs);
    perf_judge::print_results(&results);

    let failed: Vec<&BenchResult> = results.iter().filter(|r| r.verdict.is_fail()).collect();

    if !failed.is_empty() {
        // Confirmation run: re-bench only the failures, re-judge
        let failed_names: Vec<&str> = failed.iter().map(|r| r.name.as_str()).collect();
        println!("\n{} regression(s) detected. Running confirmation...", failed.len());
        for name in &failed_names {
            println!("  - {name}");
        }
        println!();

        // Re-run criterion for just the failed benchmarks
        // criterion accepts a single filter regex; join with |
        let filter = failed_names.join("|");
        let status = Command::new("cargo")
            .args(["bench", "--bench", "benchmarks", "--", "--baseline", "main", &filter])
            .current_dir(&root)
            .status();

        match status {
            Ok(s) if s.success() => {}
            Ok(s) => {
                eprintln!("Confirmation bench exited with {s}");
                return 1;
            }
            Err(e) => {
                eprintln!("Failed to run confirmation bench: {e}");
                return 1;
            }
        }

        // Re-judge only the failed dirs
        let failed_dirs: Vec<String> = failed
            .iter()
            .map(|r| criterion_dir.join(&r.name).to_string_lossy().to_string())
            .collect();
        let confirm_results = perf_judge::judge(&failed_dirs);

        println!("\nConfirmation results:");
        perf_judge::print_results(&confirm_results);

        let still_failing: Vec<&BenchResult> =
            confirm_results.iter().filter(|r| r.verdict.is_fail()).collect();

        if !still_failing.is_empty() {
            eprintln!(
                "\nRegression confirmed ({}/{} still failing).",
                still_failing.len(),
                failed.len()
            );
            return 1;
        }

        println!("\nInitial regression(s) not reproducible. Treating as noise.");
    } else {
        println!("\nAll benchmarks passed.");
    }

    // Write attestation
    if let Err(e) = write_attestation(&root, &required) {
        eprintln!("Failed to write attestation: {e}");
        return 1;
    }
    println!("Attestation written to .git/perf-attestation.json");
    println!("You can now push.");

    0
}

fn project_root() -> PathBuf {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .expect("failed to run git");
    PathBuf::from(String::from_utf8(output.stdout).unwrap().trim())
}

fn changed_files(root: &Path) -> Vec<String> {
    // Check what changed since origin/main (same view as pre-push hook)
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
    required_benchmarks: &std::collections::BTreeSet<String>,
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
