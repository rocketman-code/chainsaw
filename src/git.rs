//! Git integration for ref-based diffs.

use std::path::Path;

/// What a diff argument resolved to.
#[derive(Debug, PartialEq, Eq)]
pub enum DiffArg {
    /// An existing snapshot file on disk.
    Snapshot(std::path::PathBuf),
    /// A valid git ref (branch, tag, SHA).
    GitRef(String),
}

/// Classify a diff argument as a snapshot file or git ref.
///
/// Detection order:
/// 1. Existing file on disk -> Snapshot
/// 2. Path-like arg that doesn't exist (contains `/` or `.json`) -> error (file not found)
/// 3. `git rev-parse --verify <arg>` succeeds -> `GitRef`
/// 4. Neither -> error
pub fn classify_diff_arg(arg: &str, repo_root: &Path) -> Result<DiffArg, crate::error::Error> {
    let path = Path::new(arg);

    // 1. Existing file on disk?
    if path.is_file() {
        return Ok(DiffArg::Snapshot(path.to_path_buf()));
    }

    // 2. Path-like arg that doesn't exist? Error as "file not found".
    let looks_like_path = arg.contains('/')
        || arg.contains(std::path::MAIN_SEPARATOR)
        || path
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("json"));
    if looks_like_path {
        return Err(crate::error::Error::DiffFileNotFound(arg.to_string()));
    }

    // 3. Valid git ref?
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--verify", arg])
        .current_dir(repo_root)
        .output()
        .map_err(|e| crate::error::Error::GitError(format!("failed to run git: {e}")))?;

    if output.status.success() {
        return Ok(DiffArg::GitRef(arg.to_string()));
    }

    // 4. Neither
    Err(crate::error::Error::NotSnapshotOrRef(arg.to_string()))
}

/// A temporary git worktree that cleans up on drop.
pub struct TempWorktree {
    dir: tempfile::TempDir,
    repo_root: std::path::PathBuf,
}

impl TempWorktree {
    /// Path to the worktree checkout.
    pub fn path(&self) -> &Path {
        self.dir.path()
    }
}

impl Drop for TempWorktree {
    fn drop(&mut self) {
        // Best-effort cleanup: remove worktree from git's tracking.
        // If this fails, `git worktree prune` will clean it up later.
        let _ = std::process::Command::new("git")
            .args(["worktree", "remove", "--force"])
            .arg(self.dir.path())
            .current_dir(&self.repo_root)
            .output();
    }
}

/// Create a temporary worktree checked out at the given git ref.
pub fn create_worktree(
    repo_root: &Path,
    git_ref: &str,
) -> Result<TempWorktree, crate::error::Error> {
    let dir = tempfile::tempdir()
        .map_err(|e| crate::error::Error::GitError(format!("failed to create temp dir: {e}")))?;

    let output = std::process::Command::new("git")
        .args(["worktree", "add", "--detach"])
        .arg(dir.path())
        .arg(git_ref)
        .current_dir(repo_root)
        .output()
        .map_err(|e| crate::error::Error::GitError(format!("failed to run git: {e}")))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(crate::error::Error::GitError(format!(
            "git worktree add failed: {}",
            stderr.trim(),
        )));
    }

    Ok(TempWorktree {
        dir,
        repo_root: repo_root.to_path_buf(),
    })
}

/// Check whether the given path is inside a git repository.
pub fn is_git_repo(path: &Path) -> bool {
    std::process::Command::new("git")
        .args(["rev-parse", "--git-dir"])
        .current_dir(path)
        .output()
        .is_ok_and(|o| o.status.success())
}

/// Find the git repository root from any path inside it.
pub fn repo_root(path: &Path) -> Result<std::path::PathBuf, crate::error::Error> {
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .current_dir(path)
        .output()
        .map_err(|e| crate::error::Error::GitError(format!("failed to run git: {e}")))?;

    if !output.status.success() {
        return Err(crate::error::Error::NotAGitRepo);
    }

    let root = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(std::path::PathBuf::from(root))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    /// Create a git repo in a tempdir with one commit, return the tempdir and the SHA.
    fn git_repo() -> (tempfile::TempDir, String) {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path();
        Command::new("git")
            .args(["init"])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["config", "user.email", "test@test.com"])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["config", "user.name", "Test"])
            .current_dir(dir)
            .output()
            .unwrap();
        std::fs::write(dir.join("file.txt"), "hello").unwrap();
        Command::new("git")
            .args(["add", "."])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["commit", "-m", "init"])
            .current_dir(dir)
            .output()
            .unwrap();
        let sha = String::from_utf8(
            Command::new("git")
                .args(["rev-parse", "HEAD"])
                .current_dir(dir)
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_string();
        (tmp, sha)
    }

    #[test]
    fn existing_file_is_snapshot() {
        let (tmp, _) = git_repo();
        let snap = tmp.path().join("snap.json");
        std::fs::write(&snap, "{}").unwrap();
        let result = classify_diff_arg(snap.to_str().unwrap(), tmp.path());
        assert_eq!(result.unwrap(), DiffArg::Snapshot(snap));
    }

    #[test]
    fn branch_name_is_git_ref() {
        let (tmp, _) = git_repo();
        // Determine actual default branch name
        let branch = String::from_utf8(
            Command::new("git")
                .args(["branch", "--show-current"])
                .current_dir(tmp.path())
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_string();
        let result = classify_diff_arg(&branch, tmp.path());
        assert!(matches!(result, Ok(DiffArg::GitRef(_))));
    }

    #[test]
    fn sha_is_git_ref() {
        let (tmp, sha) = git_repo();
        let result = classify_diff_arg(&sha, tmp.path());
        assert_eq!(result.unwrap(), DiffArg::GitRef(sha));
    }

    #[test]
    fn short_sha_is_git_ref() {
        let (tmp, sha) = git_repo();
        let short = &sha[..7];
        let result = classify_diff_arg(short, tmp.path());
        assert!(matches!(result, Ok(DiffArg::GitRef(_))));
    }

    #[test]
    fn head_tilde_is_git_ref() {
        let (tmp, _) = git_repo();
        let result = classify_diff_arg("HEAD~0", tmp.path());
        assert!(matches!(result, Ok(DiffArg::GitRef(_))));
    }

    #[test]
    fn path_like_nonexistent_is_file_not_found() {
        let (tmp, _) = git_repo();
        let result = classify_diff_arg("./missing.json", tmp.path());
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("file not found"), "got: {err}");
    }

    #[test]
    fn json_extension_nonexistent_is_file_not_found() {
        let (tmp, _) = git_repo();
        let result = classify_diff_arg("missing.json", tmp.path());
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("file not found"), "got: {err}");
    }

    #[test]
    fn path_with_slash_nonexistent_is_file_not_found() {
        let (tmp, _) = git_repo();
        let result = classify_diff_arg("some/path/file", tmp.path());
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("file not found"), "got: {err}");
    }

    #[test]
    fn nonsense_is_not_snapshot_or_ref() {
        let (tmp, _) = git_repo();
        let result = classify_diff_arg("xyzzy-not-a-ref", tmp.path());
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("not a snapshot file or a valid git ref"),
            "got: {err}"
        );
    }

    #[test]
    fn file_named_main_beats_branch() {
        let (tmp, _) = git_repo();
        let main_file = tmp.path().join("main");
        std::fs::write(&main_file, "{}").unwrap();
        // Pass absolute path — file exists, should be Snapshot
        let result = classify_diff_arg(main_file.to_str().unwrap(), tmp.path());
        assert!(matches!(result, Ok(DiffArg::Snapshot(_))));
    }

    #[test]
    fn tag_is_git_ref() {
        let (tmp, _) = git_repo();
        Command::new("git")
            .args(["tag", "v1.0.0"])
            .current_dir(tmp.path())
            .output()
            .unwrap();
        let result = classify_diff_arg("v1.0.0", tmp.path());
        assert_eq!(result.unwrap(), DiffArg::GitRef("v1.0.0".to_string()));
    }

    #[test]
    fn worktree_roundtrip() {
        let (tmp, sha) = git_repo();
        std::fs::write(tmp.path().join("marker.txt"), "original").unwrap();
        Command::new("git")
            .args(["add", "."])
            .current_dir(tmp.path())
            .output()
            .unwrap();
        Command::new("git")
            .args(["commit", "-m", "marker"])
            .current_dir(tmp.path())
            .output()
            .unwrap();

        let wt = create_worktree(tmp.path(), &sha).unwrap();
        // Worktree should have file.txt from the first commit but NOT marker.txt
        assert!(wt.path().join("file.txt").exists());
        assert!(!wt.path().join("marker.txt").exists());
        // Cleanup should not panic
        drop(wt);
    }

    #[test]
    fn integration_diff_two_refs() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path();

        // Init repo
        Command::new("git")
            .args(["init"])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["config", "user.email", "t@t.com"])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["config", "user.name", "T"])
            .current_dir(dir)
            .output()
            .unwrap();

        // Commit 1: index.ts imports one file
        std::fs::write(dir.join("index.ts"), "import './a';\n").unwrap();
        std::fs::write(dir.join("a.ts"), "export const a = 1;\n").unwrap();
        Command::new("git")
            .args(["add", "."])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["commit", "-m", "v1"])
            .current_dir(dir)
            .output()
            .unwrap();
        let sha1 = String::from_utf8(
            Command::new("git")
                .args(["rev-parse", "HEAD"])
                .current_dir(dir)
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_string();

        // Commit 2: index.ts imports two files (more weight)
        std::fs::write(dir.join("index.ts"), "import './a';\nimport './b';\n").unwrap();
        std::fs::write(
            dir.join("b.ts"),
            "export const b = 'hello world this is extra weight';\n",
        )
        .unwrap();
        Command::new("git")
            .args(["add", "."])
            .current_dir(dir)
            .output()
            .unwrap();
        Command::new("git")
            .args(["commit", "-m", "v2"])
            .current_dir(dir)
            .output()
            .unwrap();
        let sha2 = String::from_utf8(
            Command::new("git")
                .args(["rev-parse", "HEAD"])
                .current_dir(dir)
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_string();

        // Build snapshots from each ref via worktrees
        let entry = std::path::Path::new("index.ts");
        let opts = crate::query::TraceOptions {
            include_dynamic: false,
            top_n: 0,
            ignore: vec![],
        };

        let wt1 = create_worktree(dir, &sha1).unwrap();
        let (loaded1, _cw1) = crate::loader::load_graph(&wt1.path().join(entry), true).unwrap();
        let eid1 = *loaded1.graph.path_to_id.get(&loaded1.entry).unwrap();
        let snap1 = crate::query::trace(&loaded1.graph, eid1, &opts).to_snapshot("v1");

        let wt2 = create_worktree(dir, &sha2).unwrap();
        let (loaded2, _cw2) = crate::loader::load_graph(&wt2.path().join(entry), true).unwrap();
        let eid2 = *loaded2.graph.path_to_id.get(&loaded2.entry).unwrap();
        let snap2 = crate::query::trace(&loaded2.graph, eid2, &opts).to_snapshot("v2");

        // Diff: v2 should be heavier than v1 (added b.ts)
        let diff = crate::query::diff_snapshots(&snap1, &snap2);
        assert!(
            diff.weight_delta > 0,
            "v2 should be heavier than v1, delta={}",
            diff.weight_delta
        );
    }
}
