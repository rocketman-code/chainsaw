use std::process::Command;

use chainsaw::git::{DiffArg, classify_diff_arg};

/// Create a minimal git repo with one commit. Returns (TempDir, SHA).
fn git_repo() -> (tempfile::TempDir, String) {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path();
    let git = |args: &[&str]| {
        Command::new("git")
            .args(args)
            .current_dir(d)
            .output()
            .unwrap()
    };
    git(&["init"]);
    git(&["config", "user.email", "t@t.com"]);
    git(&["config", "user.name", "T"]);
    std::fs::write(d.join("f.txt"), "x").unwrap();
    git(&["add", "."]);
    git(&["commit", "-m", "init"]);
    let sha = String::from_utf8(git(&["rev-parse", "HEAD"]).stdout)
        .unwrap()
        .trim()
        .to_string();
    (tmp, sha)
}

// --- Lightweight tags ---

#[test]
fn lightweight_tag() {
    let (tmp, _) = git_repo();
    Command::new("git")
        .args(["tag", "v1.0"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(matches!(
        classify_diff_arg("v1.0", tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

// --- Annotated tags ---

#[test]
fn annotated_tag() {
    let (tmp, _) = git_repo();
    Command::new("git")
        .args(["tag", "-a", "v2.0", "-m", "release"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(matches!(
        classify_diff_arg("v2.0", tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

// --- Branch names with slashes ---

#[test]
fn branch_with_single_slash() {
    let (tmp, _) = git_repo();
    Command::new("git")
        .args(["branch", "feature/auth"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(matches!(
        classify_diff_arg("feature/auth", tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

#[test]
fn branch_with_multiple_slashes() {
    let (tmp, _) = git_repo();
    Command::new("git")
        .args(["branch", "fix/bug/123"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(matches!(
        classify_diff_arg("fix/bug/123", tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

// --- SHA variants ---

#[test]
fn full_sha() {
    let (tmp, sha) = git_repo();
    assert_eq!(
        classify_diff_arg(&sha, tmp.path()).unwrap(),
        DiffArg::GitRef(sha)
    );
}

#[test]
fn short_sha_7() {
    let (tmp, sha) = git_repo();
    let short = &sha[..7];
    assert!(matches!(
        classify_diff_arg(short, tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

#[test]
fn short_sha_4() {
    let (tmp, sha) = git_repo();
    let short = &sha[..4];
    assert!(matches!(
        classify_diff_arg(short, tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

// --- HEAD variants ---

#[test]
fn head_tilde_0() {
    let (tmp, _) = git_repo();
    assert!(matches!(
        classify_diff_arg("HEAD~0", tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

#[test]
fn head_caret() {
    let (tmp, _) = git_repo();
    // HEAD^0 peels to the commit itself
    assert!(matches!(
        classify_diff_arg("HEAD^0", tmp.path()),
        Ok(DiffArg::GitRef(_))
    ));
}

// --- Ambiguity: file vs ref ---

#[test]
fn file_beats_branch_when_both_exist() {
    let (tmp, _) = git_repo();
    // Create a file named "main" -- should resolve as Snapshot
    let main_file = tmp.path().join("main");
    std::fs::write(&main_file, "{}").unwrap();
    assert!(matches!(
        classify_diff_arg(main_file.to_str().unwrap(), tmp.path()),
        Ok(DiffArg::Snapshot(_))
    ));
}

// --- Error cases ---

#[test]
fn nonexistent_path_with_slash_is_file_not_found() {
    let (tmp, _) = git_repo();
    let result = classify_diff_arg("some/nonexistent/path", tmp.path());
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
fn nonsense_string_is_not_snapshot_or_ref() {
    let (tmp, _) = git_repo();
    let result = classify_diff_arg("xyzzy-nope", tmp.path());
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("not a snapshot file"), "got: {err}");
}

#[test]
fn empty_string_is_error() {
    let (tmp, _) = git_repo();
    let result = classify_diff_arg("", tmp.path());
    assert!(result.is_err());
}
