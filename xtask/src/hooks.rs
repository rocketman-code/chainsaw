use crate::registry::Registry;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Pre-commit hook logic: blocks commits to main without perf attestation.
/// Returns exit code.
pub fn pre_commit() -> i32 {
    let root = project_root();

    let branch = current_branch(&root);
    if branch != "main" {
        return 0; // Not on main, no gate
    }

    let registry = match Registry::load(&root) {
        Some(r) => r,
        None => return 0, // No registry, no gate
    };

    let staged = staged_files(&root);
    if staged.is_empty() {
        return 0;
    }

    let required = registry.required_benchmarks(&staged);
    if required.is_empty() {
        return 0; // No perf-sensitive files staged
    }

    let results_path = root.join("perf/results.json");
    if !results_path.exists() {
        blocked("Perf-sensitive files changed but no perf/results.json found.", &root);
        return 1;
    }

    let json = match std::fs::read_to_string(&results_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("BLOCKED: failed to read perf/results.json: {e}");
            return 1;
        }
    };

    let staged_tree = staged_tree_sha(&root);
    match verify_attestation(&json, &staged_tree, &required) {
        Ok(()) => 0,
        Err(reason) => {
            eprintln!();
            eprintln!("BLOCKED: {reason}");
            eprintln!();
            eprintln!("Re-run: cargo bench -- --baseline main && cargo xtask perf-validate");
            1
        }
    }
}

/// Pre-push hook logic: blocks pushes without perf attestation.
/// Returns exit code.
pub fn pre_push() -> i32 {
    let root = project_root();

    let registry = match Registry::load(&root) {
        Some(r) => r,
        None => return 0,
    };

    let changed = files_since_main(&root);
    if changed.is_empty() {
        return 0;
    }

    let required = registry.required_benchmarks(&changed);
    if required.is_empty() {
        return 0;
    }

    let results_path = root.join("perf/results.json");
    if !results_path.exists() {
        blocked("Perf-sensitive files changed but no perf/results.json found.", &root);
        return 1;
    }

    let json = match std::fs::read_to_string(&results_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("BLOCKED: failed to read perf/results.json: {e}");
            return 1;
        }
    };

    let head_tree = head_tree_sha(&root);
    match verify_attestation(&json, &head_tree, &required) {
        Ok(()) => 0,
        Err(reason) => {
            eprintln!();
            eprintln!("BLOCKED: {reason}");
            eprintln!();
            eprintln!("Re-run: cargo bench --bench benchmarks -- --baseline main");
            eprintln!("  then: cargo xtask perf-validate");
            eprintln!("  then: git add perf/results.json && git commit --amend --no-edit");
            1
        }
    }
}

/// Install git hooks by writing thin shell stubs to .git/hooks/.
pub fn install_hooks() -> i32 {
    let root = project_root();
    let hooks_dir = root.join(".git/hooks");

    if !hooks_dir.exists() {
        eprintln!("Not a git repository: {}", root.display());
        return 1;
    }

    let hooks = [
        ("pre-commit", "#!/bin/sh\ncargo xtask pre-commit \"$@\"\n"),
        ("pre-push", "#!/bin/sh\ncargo xtask pre-push \"$@\"\n"),
    ];

    for (name, content) in &hooks {
        let path = hooks_dir.join(name);
        if path.exists() && !is_our_hook(&path) {
            eprintln!("WARNING: {name} hook exists and wasn't installed by us. Backing up.");
            let backup = hooks_dir.join(format!("{name}.bak"));
            std::fs::rename(&path, &backup).unwrap_or_else(|e| {
                panic!("failed to backup {}: {e}", path.display());
            });
        }
        std::fs::write(&path, content)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));

        // Make executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let perms = std::fs::Permissions::from_mode(0o755);
            std::fs::set_permissions(&path, perms)
                .unwrap_or_else(|e| panic!("failed to chmod {}: {e}", path.display()));
        }

        println!("Installed {name} hook");
    }

    println!("Done.");
    0
}

fn is_our_hook(path: &Path) -> bool {
    std::fs::read_to_string(path)
        .map(|c| c.contains("cargo xtask"))
        .unwrap_or(false)
}

fn blocked(reason: &str, root: &Path) {
    let _ = root; // available for future use
    eprintln!();
    eprintln!("BLOCKED: {reason}");
    eprintln!();
    eprintln!("Run:");
    eprintln!("  cargo bench -- --baseline main");
    eprintln!("  cargo xtask perf-validate");
    eprintln!("  git add perf/results.json");
    eprintln!();
    eprintln!("Or bypass with: git commit --no-verify");
}

fn project_root() -> PathBuf {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .expect("failed to run git");
    PathBuf::from(String::from_utf8(output.stdout).unwrap().trim())
}

fn current_branch(root: &Path) -> String {
    let output = Command::new("git")
        .args(["symbolic-ref", "--short", "HEAD"])
        .current_dir(root)
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8(output.stdout)
        .unwrap_or_default()
        .trim()
        .to_string()
}

fn staged_files(root: &Path) -> Vec<String> {
    let output = Command::new("git")
        .args(["diff", "--cached", "--name-only"])
        .current_dir(root)
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .map(String::from)
        .collect()
}

fn staged_tree_sha(root: &Path) -> String {
    let output = Command::new("git")
        .args(["write-tree"])
        .current_dir(root)
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

fn files_since_main(root: &Path) -> Vec<String> {
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

fn head_tree_sha(root: &Path) -> String {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD^{tree}"])
        .current_dir(root)
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8(output.stdout)
        .unwrap_or_default()
        .trim()
        .to_string()
}


/// Verify that an attestation file is valid for the given tree SHA and required benchmarks.
/// Returns Ok(()) if valid, Err(reason) if not.
fn verify_attestation(
    json: &str,
    expected_tree_sha: &str,
    required_benchmarks: &std::collections::BTreeSet<String>,
) -> Result<(), String> {
    let parsed: serde_json::Value =
        serde_json::from_str(json).map_err(|e| format!("failed to parse attestation: {e}"))?;

    let overall = parsed["overall"]
        .as_str()
        .ok_or("attestation missing 'overall' field")?;
    if overall != "pass" {
        return Err(format!("attestation verdict: {overall}"));
    }

    let tree_sha = parsed["tree_sha"]
        .as_str()
        .ok_or("attestation missing 'tree_sha' field")?;
    if tree_sha != expected_tree_sha {
        return Err(format!(
            "attestation tree SHA mismatch (attested: {tree_sha}, expected: {expected_tree_sha})"
        ));
    }

    let attested_benchmarks: std::collections::BTreeSet<String> = parsed["required_benchmarks"]
        .as_array()
        .ok_or("attestation missing 'required_benchmarks' field")?
        .iter()
        .filter_map(|v| v.as_str().map(String::from))
        .collect();

    let missing: Vec<&String> = required_benchmarks
        .iter()
        .filter(|b| !attested_benchmarks.contains(*b))
        .collect();

    if !missing.is_empty() {
        let names: Vec<&str> = missing.iter().map(|s| s.as_str()).collect();
        return Err(format!(
            "attestation missing required benchmarks: {}",
            names.join(", ")
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    fn make_attestation(tree_sha: &str, overall: &str, benchmarks: &[&str]) -> String {
        let bench_json: Vec<String> = benchmarks.iter().map(|b| format!("\"{}\"", b)).collect();
        format!(
            r#"{{"tree_sha":"{}","timestamp":"2026-01-01T00:00:00Z","required_benchmarks":[{}],"overall":"{}"}}"#,
            tree_sha,
            bench_json.join(","),
            overall,
        )
    }

    #[test]
    fn verify_attestation_valid() {
        let required: BTreeSet<String> =
            ["ts_parse_file", "build_graph/ts_cold"].into_iter().map(String::from).collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file", "build_graph/ts_cold"]);

        assert!(verify_attestation(&json, "abc123", &required).is_ok());
    }

    #[test]
    fn verify_attestation_wrong_tree_sha() {
        let required: BTreeSet<String> =
            ["ts_parse_file"].into_iter().map(String::from).collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file"]);

        let err = verify_attestation(&json, "def456", &required).unwrap_err();
        assert!(err.contains("tree"), "expected tree SHA error, got: {err}");
    }

    #[test]
    fn verify_attestation_missing_benchmark() {
        let required: BTreeSet<String> =
            ["ts_parse_file", "build_graph/ts_cold"].into_iter().map(String::from).collect();
        // Attestation only covers ts_parse_file, missing build_graph/ts_cold
        let json = make_attestation("abc123", "pass", &["ts_parse_file"]);

        let err = verify_attestation(&json, "abc123", &required).unwrap_err();
        assert!(err.contains("build_graph/ts_cold"), "expected missing benchmark error, got: {err}");
    }

    #[test]
    fn verify_attestation_failed_verdict() {
        let required: BTreeSet<String> =
            ["ts_parse_file"].into_iter().map(String::from).collect();
        let json = make_attestation("abc123", "fail", &["ts_parse_file"]);

        let err = verify_attestation(&json, "abc123", &required).unwrap_err();
        assert!(err.contains("fail"), "expected verdict error, got: {err}");
    }

    #[test]
    fn verify_attestation_superset_is_ok() {
        // Attestation covers MORE benchmarks than required — that's fine
        let required: BTreeSet<String> =
            ["ts_parse_file"].into_iter().map(String::from).collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file", "build_graph/ts_cold"]);

        assert!(verify_attestation(&json, "abc123", &required).is_ok());
    }

    #[test]
    fn verify_attestation_invalid_json() {
        let required: BTreeSet<String> =
            ["ts_parse_file"].into_iter().map(String::from).collect();

        let err = verify_attestation("not json", "abc123", &required).unwrap_err();
        assert!(err.contains("parse") || err.contains("invalid"), "expected parse error, got: {err}");
    }
}
