mod common;

use chainsaw::error::Error;
use chainsaw::repl::Command;
use chainsaw::session::Session;

// --- parse_flags edge cases (via Command::parse) ---

#[test]
fn unknown_flag_produces_error_command() {
    let cmd = Command::parse("trace --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unknown_flag_in_chain_produces_error() {
    let cmd = Command::parse("chain lodash --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unknown_flag_in_imports_produces_error() {
    let cmd = Command::parse("imports ./a.ts --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unknown_flag_in_importers_produces_error() {
    let cmd = Command::parse("importers ./a.ts --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unknown_flag_in_cut_produces_error() {
    let cmd = Command::parse("cut lodash --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unknown_flag_in_packages_produces_error() {
    let cmd = Command::parse("packages --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unknown_flag_in_diff_produces_error() {
    let cmd = Command::parse("diff src/other.ts --bogus");
    assert!(matches!(cmd, Command::Unknown(_)));
}

// --- Command::parse edge cases ---

#[test]
fn empty_input_is_help() {
    assert!(matches!(Command::parse(""), Command::Help));
}

#[test]
fn whitespace_only_is_help() {
    assert!(matches!(Command::parse("   "), Command::Help));
}

#[test]
fn unknown_command() {
    let cmd = Command::parse("frobnicate");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn chain_without_target() {
    let cmd = Command::parse("chain");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn cut_without_target() {
    let cmd = Command::parse("cut");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn imports_without_target() {
    let cmd = Command::parse("imports");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn importers_without_target() {
    let cmd = Command::parse("importers");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn entry_without_path() {
    let cmd = Command::parse("entry");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn diff_without_path() {
    let cmd = Command::parse("diff");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn info_without_name() {
    let cmd = Command::parse("info");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn set_without_option() {
    let cmd = Command::parse("set");
    assert!(matches!(cmd, Command::Unknown(_)));
}

#[test]
fn unset_without_option() {
    let cmd = Command::parse("unset");
    assert!(matches!(cmd, Command::Unknown(_)));
}

// --- Session query edge cases ---

#[test]
fn chain_report_self_referential() {
    let p = common::TestProject::new();
    let session = Session::open(&p.entry, true).unwrap();
    // Chain to the entry itself -- the BFS finds the trivial path [entry].
    // The REPL dispatch guards against this ("target is the entry point
    // itself"), but chain_report itself just returns what BFS finds.
    let entry_name = p.entry.file_name().unwrap().to_str().unwrap();
    let report = session.chain_report(entry_name, false);
    assert!(report.found_in_graph);
    // Trivial chain: just the entry node, so 0 hops
    assert_eq!(report.hop_count, 0);
}

#[test]
fn chain_report_nonexistent_target() {
    let p = common::TestProject::new();
    let session = Session::open(&p.entry, true).unwrap();
    let report = session.chain_report("nonexistent-pkg", false);
    assert!(!report.found_in_graph);
    assert_eq!(report.chain_count, 0);
    assert!(report.chains.is_empty());
}

#[test]
fn cut_report_nonexistent_target() {
    let p = common::TestProject::new();
    let mut session = Session::open(&p.entry, true).unwrap();
    let report = session.cut_report("nonexistent-pkg", 10, false);
    assert!(!report.found_in_graph);
    assert_eq!(report.chain_count, 0);
    assert!(report.cut_points.is_empty());
    assert!(
        !report.direct_import,
        "vacuous truth: no chains should not mean direct_import"
    );
}

#[test]
fn imports_nonexistent_file() {
    let p = common::TestProject::new();
    let session = Session::open(&p.entry, true).unwrap();
    let result = session.imports(&p.root().join("nonexistent.ts"));
    assert!(result.is_err());
}

#[test]
fn importers_nonexistent_file() {
    let p = common::TestProject::new();
    let session = Session::open(&p.entry, true).unwrap();
    let result = session.importers(&p.root().join("nonexistent.ts"));
    assert!(result.is_err());
}

// --- Error Display and hints ---

#[test]
fn all_error_variants_display_without_panic() {
    use std::path::PathBuf;

    let errors: Vec<Error> = vec![
        Error::EntryNotFound(
            PathBuf::from("/tmp/x.ts"),
            std::io::Error::new(std::io::ErrorKind::NotFound, "not found"),
        ),
        Error::EntryIsDirectory(PathBuf::from("/tmp")),
        Error::UnsupportedFileType(Some("rs".to_string())),
        Error::UnsupportedFileType(None),
        Error::EntryNotInGraph(PathBuf::from("index.ts")),
        Error::SnapshotRead(
            PathBuf::from("snap.json"),
            std::io::Error::new(std::io::ErrorKind::NotFound, "not found"),
        ),
        Error::SnapshotParse(
            PathBuf::from("x"),
            serde_json::from_str::<serde_json::Value>("invalid").unwrap_err(),
        ),
        Error::SnapshotWrite(
            PathBuf::from("snap.json"),
            std::io::Error::new(std::io::ErrorKind::PermissionDenied, "denied"),
        ),
        Error::MutuallyExclusiveFlags("--chain and --cut".to_string()),
        Error::TargetIsEntryPoint("--chain".to_string()),
        Error::EntryRequired,
        Error::NotAGitRepo,
        Error::NotSnapshotOrRef("xyzzy".to_string()),
        Error::DiffFileNotFound("missing.json".to_string()),
        Error::GitError("failed".to_string()),
        Error::InvalidTopValue("--top", -5),
        Error::Readline("init failed".to_string()),
        Error::MaxWeightExceeded {
            kind: "static",
            weight: 5_000_000,
            module_count: 100,
            threshold: 1_000_000,
        },
    ];
    for err in &errors {
        let msg = err.to_string();
        assert!(!msg.is_empty(), "empty display for {err:?}");
        // hint() should not panic for any variant
        let _ = err.hint();
    }
}

#[test]
fn error_hints_are_present_where_expected() {
    use std::path::PathBuf;

    // Variants that SHOULD have hints
    assert!(
        Error::UnsupportedFileType(Some("rs".to_string()))
            .hint()
            .is_some()
    );
    assert!(Error::UnsupportedFileType(None).hint().is_some());
    assert!(
        Error::EntryNotInGraph(PathBuf::from("x.ts"))
            .hint()
            .is_some()
    );
    assert!(
        Error::TargetIsEntryPoint("--chain".to_string())
            .hint()
            .is_some()
    );
    assert!(
        Error::TargetIsEntryPoint("--cut".to_string())
            .hint()
            .is_some()
    );
    assert!(Error::EntryRequired.hint().is_some());
    assert!(
        Error::EntryIsDirectory(PathBuf::from("/tmp"))
            .hint()
            .is_some()
    );

    // Variants that should NOT have hints
    assert!(Error::NotAGitRepo.hint().is_none());
    assert!(Error::GitError("x".to_string()).hint().is_none());
    assert!(Error::Readline("x".to_string()).hint().is_none());
}

// --- Open session with bad entry ---

#[test]
fn session_open_nonexistent_entry() {
    let result = Session::open(std::path::Path::new("/nonexistent/entry.ts"), true);
    assert!(result.is_err());
}

#[test]
fn session_open_directory_entry() {
    let p = common::TestProject::new();
    let result = Session::open(p.root(), true);
    assert!(result.is_err());
}

#[test]
fn session_open_unsupported_extension() {
    let p = common::TestProject::new();
    let rs_file = p.root().join("main.rs");
    std::fs::write(&rs_file, "fn main() {}").unwrap();
    let result = Session::open(&rs_file, true);
    assert!(result.is_err());
}
