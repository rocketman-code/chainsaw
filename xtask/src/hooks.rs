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

    // Check attestation exists
    let results_path = root.join("perf/results.json");
    if !results_path.exists() {
        blocked("Perf-sensitive files changed but no perf/results.json found.", &root);
        return 1;
    }

    // Check tree SHA matches staged tree
    let staged_tree = staged_tree_sha(&root);
    let attested_tree = read_attested_tree_sha(&results_path);

    if staged_tree != attested_tree {
        eprintln!();
        eprintln!("BLOCKED: perf/results.json is stale (tested different code).");
        eprintln!("  Attested: {attested_tree}");
        eprintln!("  Staged:   {staged_tree}");
        eprintln!();
        eprintln!("Re-run: cargo bench -- --baseline main && cargo xtask perf-validate");
        return 1;
    }

    0
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

    // Check overall verdict
    let overall = read_overall_verdict(&results_path);
    if overall != "pass" {
        eprintln!();
        eprintln!("BLOCKED: perf/results.json shows verdict: {overall}");
        eprintln!("Fix the regression before pushing.");
        return 1;
    }

    0
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

fn read_attested_tree_sha(path: &Path) -> String {
    let content = std::fs::read_to_string(path).unwrap_or_default();
    extract_json_string(&content, "tree_sha").unwrap_or_default()
}

fn read_overall_verdict(path: &Path) -> String {
    let content = std::fs::read_to_string(path).unwrap_or_default();
    extract_json_string(&content, "overall").unwrap_or_default()
}

/// Extract a string value from JSON without pulling in serde for a simple read.
fn extract_json_string(json: &str, key: &str) -> Option<String> {
    let pattern = format!("\"{key}\"");
    let pos = json.find(&pattern)?;
    let after = &json[pos + pattern.len()..];
    // Skip : and whitespace, find opening quote
    let start = after.find('"')? + 1;
    let rest = &after[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}
