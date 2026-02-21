use crate::registry::Registry;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Env vars that git sets during hook execution. Cleared from all
/// subprocesses (git helpers, cargo xtask check, check::run() children)
/// to prevent interference with tests that create their own repos.
pub const GIT_HOOK_ENV_VARS: &[&str] = &["GIT_DIR", "GIT_INDEX_FILE", "GIT_WORK_TREE"];

/// Pre-commit hook: run checks on feature branches, perf attestation gate on main.
/// Returns exit code.
pub fn pre_commit() -> i32 {
    let root = project_root();

    let Some(branch) = current_branch(&root) else {
        // Detached HEAD (rebase, bisect, CI checkout) — skip checks.
        return 0;
    };
    if branch != "main" {
        // Feature branch: run fmt + clippy + test.
        // Defense-in-depth: clear hook env vars here even though check::run()
        // also clears them on each child subprocess.
        let mut cmd = Command::new("cargo");
        cmd.args(["xtask", "check"]).current_dir(&root);
        for var in GIT_HOOK_ENV_VARS {
            cmd.env_remove(var);
        }
        let status = cmd
            .status()
            .unwrap_or_else(|e| panic!("failed to run cargo xtask check: {e}"));
        if !status.success() {
            return 1;
        }
        warn_unpushed_branch(&root, &branch);
        return 0;
    }

    let Some(registry) = Registry::load(&root) else {
        return 0; // No registry, no gate
    };

    let staged = staged_files(&root);
    if staged.is_empty() {
        return 0;
    }

    let required = registry.required_benchmarks(&staged);
    if required.is_empty() {
        return 0; // No perf-sensitive files staged
    }

    // Pre-commit can't verify commit SHA (commit doesn't exist yet).
    // Just check that an attestation exists — pre-push does the full verification.
    let attestation_path = root.join(".git/perf-attestation.json");
    if !attestation_path.exists() {
        blocked(
            "Perf-sensitive files staged but no attestation found.",
            "git commit --no-verify",
        );
        return 1;
    }

    0
}

/// Pre-push hook logic: blocks pushes without perf attestation.
/// Returns exit code.
pub fn pre_push() -> i32 {
    let root = project_root();

    let Some(registry) = Registry::load(&root) else {
        return 0;
    };

    let changed = files_since_main(&root);
    if changed.is_empty() {
        return 0;
    }

    let required = registry.required_benchmarks(&changed);
    if required.is_empty() {
        return 0;
    }

    let attestation_path = root.join(".git/perf-attestation.json");
    if !attestation_path.exists() {
        blocked(
            "Perf-sensitive files changed but no attestation found.",
            "git push --no-verify",
        );
        return 1;
    }

    let json = match std::fs::read_to_string(&attestation_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("BLOCKED: failed to read .git/perf-attestation.json: {e}");
            return 1;
        }
    };

    let head_commit = head_commit_sha(&root);
    match verify_attestation(&json, &head_commit, &required) {
        Ok(()) => 0,
        Err(reason) => {
            eprintln!();
            eprintln!("BLOCKED: {reason}");
            eprintln!();
            eprintln!("Re-run: cargo xtask perf-validate");
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

/// Warn when commits are accumulating on a branch with no remote tracking ref.
/// Does not block the commit — just makes it impossible to miss.
fn warn_unpushed_branch(root: &Path, branch: &str) {
    // Check if this branch has a remote tracking branch.
    let upstream = git_in(root, &["rev-parse", "--abbrev-ref", "@{upstream}"])
        .output()
        .ok()
        .filter(|o| o.status.success());
    if upstream.is_some() {
        return; // Already tracking a remote branch.
    }

    // Count commits ahead of origin/main.
    let output = git_in(root, &["rev-list", "--count", "origin/main..HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success());
    let Some(output) = output else { return };
    let count: usize = String::from_utf8_lossy(&output.stdout)
        .trim()
        .parse()
        .unwrap_or(0);
    if count == 0 {
        return;
    }

    let s = if count == 1 { "" } else { "s" };
    eprintln!();
    eprintln!("WARNING: {count} commit{s} on '{branch}' with no remote tracking branch.");
    eprintln!("         Push to avoid losing work: git push -u origin {branch}");
    eprintln!();
}

fn blocked(reason: &str, bypass: &str) {
    eprintln!();
    eprintln!("BLOCKED: {reason}");
    eprintln!();
    eprintln!("Run: cargo xtask perf-validate");
    eprintln!();
    eprintln!("Or bypass with: {bypass}");
}

fn git_in(root: &Path, args: &[&str]) -> Command {
    let mut cmd = Command::new("git");
    cmd.args(args).current_dir(root);
    for var in GIT_HOOK_ENV_VARS {
        cmd.env_remove(var);
    }
    cmd
}

fn project_root() -> PathBuf {
    // Can't use git_in() — we don't know root yet.
    let mut cmd = Command::new("git");
    cmd.args(["rev-parse", "--show-toplevel"]);
    for var in GIT_HOOK_ENV_VARS {
        cmd.env_remove(var);
    }
    let output = cmd.output().expect("failed to run git");
    assert!(
        output.status.success(),
        "git rev-parse --show-toplevel failed (not in a git repo?)"
    );
    PathBuf::from(String::from_utf8(output.stdout).unwrap().trim())
}

fn current_branch(root: &Path) -> Option<String> {
    let output = git_in(root, &["symbolic-ref", "--short", "HEAD"])
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    if !output.status.success() {
        return None;
    }
    Some(
        String::from_utf8(output.stdout)
            .unwrap_or_default()
            .trim()
            .to_string(),
    )
}

fn staged_files(root: &Path) -> Vec<String> {
    let output = git_in(root, &["diff", "--cached", "--name-only"])
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .map(String::from)
        .collect()
}

fn files_since_main(root: &Path) -> Vec<String> {
    let output = git_in(root, &["diff", "--name-only", "origin/main...HEAD"])
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .map(String::from)
        .collect()
}

fn head_commit_sha(root: &Path) -> String {
    let output = git_in(root, &["rev-parse", "HEAD"])
        .output()
        .unwrap_or_else(|e| panic!("failed to run git: {e}"));
    String::from_utf8(output.stdout)
        .unwrap_or_default()
        .trim()
        .to_string()
}

/// Verify that an attestation file is valid for the given commit SHA and required benchmarks.
/// Returns Ok(()) if valid, Err(reason) if not.
fn verify_attestation(
    json: &str,
    expected_commit_sha: &str,
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

    let commit_sha = parsed["commit_sha"]
        .as_str()
        .ok_or("attestation missing 'commit_sha' field")?;
    if commit_sha != expected_commit_sha {
        return Err(format!(
            "attestation commit SHA mismatch (attested: {commit_sha}, expected: {expected_commit_sha})"
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

    fn make_attestation(commit_sha: &str, overall: &str, benchmarks: &[&str]) -> String {
        let bench_json: Vec<String> = benchmarks.iter().map(|b| format!("\"{b}\"")).collect();
        format!(
            r#"{{"commit_sha":"{}","timestamp":"2026-01-01T00:00:00Z","required_benchmarks":[{}],"overall":"{}"}}"#,
            commit_sha,
            bench_json.join(","),
            overall,
        )
    }

    #[test]
    fn verify_attestation_valid() {
        let required: BTreeSet<String> = ["ts_parse_file", "build_graph/ts_cold"]
            .into_iter()
            .map(String::from)
            .collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file", "build_graph/ts_cold"]);

        assert!(verify_attestation(&json, "abc123", &required).is_ok());
    }

    #[test]
    fn verify_attestation_wrong_commit_sha() {
        let required: BTreeSet<String> =
            std::iter::once("ts_parse_file").map(String::from).collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file"]);

        let err = verify_attestation(&json, "def456", &required).unwrap_err();
        assert!(
            err.contains("commit"),
            "expected commit SHA error, got: {err}"
        );
    }

    #[test]
    fn verify_attestation_missing_benchmark() {
        let required: BTreeSet<String> = ["ts_parse_file", "build_graph/ts_cold"]
            .into_iter()
            .map(String::from)
            .collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file"]);

        let err = verify_attestation(&json, "abc123", &required).unwrap_err();
        assert!(
            err.contains("build_graph/ts_cold"),
            "expected missing benchmark error, got: {err}"
        );
    }

    #[test]
    fn verify_attestation_failed_verdict() {
        let required: BTreeSet<String> =
            std::iter::once("ts_parse_file").map(String::from).collect();
        let json = make_attestation("abc123", "fail", &["ts_parse_file"]);

        let err = verify_attestation(&json, "abc123", &required).unwrap_err();
        assert!(err.contains("fail"), "expected verdict error, got: {err}");
    }

    #[test]
    fn verify_attestation_superset_is_ok() {
        let required: BTreeSet<String> =
            std::iter::once("ts_parse_file").map(String::from).collect();
        let json = make_attestation("abc123", "pass", &["ts_parse_file", "build_graph/ts_cold"]);

        assert!(verify_attestation(&json, "abc123", &required).is_ok());
    }

    #[test]
    fn verify_attestation_invalid_json() {
        let required: BTreeSet<String> =
            std::iter::once("ts_parse_file").map(String::from).collect();

        let err = verify_attestation("not json", "abc123", &required).unwrap_err();
        assert!(
            err.contains("parse") || err.contains("invalid"),
            "expected parse error, got: {err}"
        );
    }
}
