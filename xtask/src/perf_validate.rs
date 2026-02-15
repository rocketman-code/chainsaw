use crate::perf_judge;
use crate::registry::Registry;
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Serialize)]
struct Attestation {
    tree_sha: String,
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
    let exit_code = perf_judge::run(&dirs);

    if exit_code == 0 {
        // Write attestation
        if let Err(e) = write_attestation(&root, &required) {
            eprintln!("Failed to write attestation: {e}");
            return 1;
        }
        println!("\nAttestation written to perf/results.json");
        println!("Stage it: git add perf/results.json");
    }

    exit_code
}

fn project_root() -> PathBuf {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .expect("failed to run git");
    PathBuf::from(String::from_utf8(output.stdout).unwrap().trim())
}

fn changed_files(root: &Path) -> Vec<String> {
    let mut files = BTreeMap::new();

    // Staged changes
    if let Ok(output) = Command::new("git")
        .args(["diff", "--cached", "--name-only"])
        .current_dir(root)
        .output()
    {
        for line in String::from_utf8_lossy(&output.stdout).lines() {
            if !line.is_empty() {
                files.insert(line.to_string(), ());
            }
        }
    }

    // Unstaged changes
    if let Ok(output) = Command::new("git")
        .args(["diff", "--name-only"])
        .current_dir(root)
        .output()
    {
        for line in String::from_utf8_lossy(&output.stdout).lines() {
            if !line.is_empty() {
                files.insert(line.to_string(), ());
            }
        }
    }

    files.into_keys().collect()
}

fn tree_sha(root: &Path) -> String {
    let output = Command::new("git")
        .args(["write-tree"])
        .current_dir(root)
        .output()
        .expect("failed to run git write-tree");

    let sha = String::from_utf8(output.stdout).unwrap().trim().to_string();
    if sha.is_empty() {
        // Fallback to HEAD
        let output = Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(root)
            .output()
            .expect("failed to run git rev-parse");
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    } else {
        sha
    }
}

fn write_attestation(
    root: &Path,
    required_benchmarks: &std::collections::BTreeSet<String>,
) -> Result<(), String> {
    let perf_dir = root.join("perf");
    std::fs::create_dir_all(&perf_dir).map_err(|e| format!("mkdir perf/: {e}"))?;

    let attestation = Attestation {
        tree_sha: tree_sha(root),
        timestamp: now_utc(),
        required_benchmarks: required_benchmarks.iter().cloned().collect(),
        overall: "pass".to_string(),
    };

    let json = serde_json::to_string_pretty(&attestation).map_err(|e| format!("json: {e}"))?;
    let path = perf_dir.join("results.json");
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
