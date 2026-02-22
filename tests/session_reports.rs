mod common;

use chainsaw::query::TraceOptions;
use chainsaw::repl::{CommandOptions, ReplSettings};
use chainsaw::session::Session;

fn open_session() -> (common::TestProject, Session) {
    let p = common::TestProject::new();
    let entry = p.entry.clone();
    let session = Session::open(&entry, true).unwrap();
    (p, session)
}

// --- trace_report ---

#[test]
fn trace_report_returns_valid_json() {
    let (_p, mut session) = open_session();
    let report = session.trace_report(&TraceOptions::default(), 20);
    let json = report.to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(v["static_weight_bytes"].is_number());
    assert!(v["static_module_count"].is_number());
    assert!(v["heavy_packages"].is_array());
    assert!(v["modules_by_cost"].is_array());
    assert!(v["total_modules_with_cost"].is_number());
    // Skipped fields must not appear in JSON
    assert!(v.get("include_dynamic").is_none());
    assert!(v.get("top").is_none());
}

#[test]
fn trace_report_static_only_shows_dynamic_separately() {
    let (_p, mut session) = open_session();
    // With include_dynamic: false (default), c.ts is reported as dynamic-only
    let report = session.trace_report(&TraceOptions::default(), 20);
    assert!(
        report.dynamic_only_module_count > 0,
        "c.ts should be reported as dynamic-only when include_dynamic is false"
    );
    assert!(report.dynamic_only_weight_bytes > 0);
}

#[test]
fn trace_report_include_dynamic_folds_into_static() {
    let (_p, mut session) = open_session();
    let static_report = session.trace_report(&TraceOptions::default(), 20);
    let static_count = static_report.static_module_count;

    let opts = TraceOptions {
        include_dynamic: true,
        ..TraceOptions::default()
    };
    let dyn_report = session.trace_report(&opts, 20);
    // When include_dynamic is true, dynamic modules fold into static count
    assert_eq!(dyn_report.dynamic_only_module_count, 0);
    assert_eq!(dyn_report.dynamic_only_weight_bytes, 0);
    // Total module count should increase (c.ts is now included)
    assert!(
        dyn_report.static_module_count > static_count,
        "include_dynamic should increase module count"
    );
}

#[test]
fn trace_report_top_zero_hides_packages() {
    let (_p, mut session) = open_session();
    let opts = TraceOptions {
        top_n: 0,
        ..TraceOptions::default()
    };
    let report = session.trace_report(&opts, 20);
    assert!(report.heavy_packages.is_empty());
}

#[test]
fn trace_report_top_modules_zero_hides_modules() {
    let (_p, mut session) = open_session();
    let report = session.trace_report(&TraceOptions::default(), 0);
    assert!(report.modules_by_cost.is_empty());
    assert!(
        report.total_modules_with_cost > 0,
        "count should still be set"
    );
}

// --- chain_report ---

#[test]
fn chain_report_finds_lodash() {
    let (_p, session) = open_session();
    let report = session.chain_report("lodash", false);
    assert!(report.found_in_graph);
    assert!(report.chain_count > 0);
    assert_eq!(report.chains.len(), report.chain_count);
}

#[test]
fn chain_report_json_has_expected_fields() {
    let (_p, session) = open_session();
    let report = session.chain_report("lodash", false);
    let json = report.to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(v["target"].is_string());
    assert!(v["found_in_graph"].is_boolean());
    assert!(v["chain_count"].is_number());
    assert!(v["hop_count"].is_number());
    assert!(v["chains"].is_array());
}

#[test]
fn chain_report_nonexistent_target() {
    let (_p, session) = open_session();
    let report = session.chain_report("nonexistent-pkg", false);
    assert!(!report.found_in_graph);
    assert_eq!(report.chain_count, 0);
    assert!(report.chains.is_empty());
}

// --- cut_report ---

#[test]
fn cut_report_finds_cut_for_lodash() {
    let (_p, mut session) = open_session();
    let report = session.cut_report("lodash", 10, false);
    assert!(report.found_in_graph);
    assert!(!report.cut_points.is_empty());
    // a.ts is the cut point -- all chains to lodash pass through it
    let names: Vec<&str> = report
        .cut_points
        .iter()
        .map(|c| c.module.as_str())
        .collect();
    assert!(
        names.iter().any(|n| n.contains("a.ts")),
        "a.ts should be a cut point, got: {names:?}"
    );
}

#[test]
fn cut_report_nonexistent_target_not_direct() {
    let (_p, mut session) = open_session();
    let report = session.cut_report("nonexistent-pkg", 10, false);
    assert!(!report.found_in_graph);
    assert!(
        !report.direct_import,
        "vacuous truth: no chains should not mean direct_import"
    );
}

#[test]
fn cut_report_json_has_expected_fields() {
    let (_p, mut session) = open_session();
    let report = session.cut_report("lodash", 10, false);
    let json = report.to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(v["target"].is_string());
    assert!(v["found_in_graph"].is_boolean());
    assert!(v["chain_count"].is_number());
    assert!(v["direct_import"].is_boolean());
    assert!(v["cut_points"].is_array());
}

// --- packages_report ---

#[test]
fn packages_report_lists_lodash() {
    let (_p, session) = open_session();
    let report = session.packages_report(10);
    assert!(report.package_count > 0);
    let names: Vec<&str> = report.packages.iter().map(|p| p.name.as_str()).collect();
    assert!(names.contains(&"lodash"));
}

#[test]
fn packages_report_json_field_names() {
    let (_p, session) = open_session();
    let report = session.packages_report(10);
    let json = report.to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(v["package_count"].is_number());
    let pkg = &v["packages"][0];
    assert!(pkg["name"].is_string());
    assert!(pkg["total_size_bytes"].is_number());
    assert!(pkg["file_count"].is_number());
    // Old field names must NOT appear
    assert!(
        pkg.get("size").is_none(),
        "field should be total_size_bytes, not size"
    );
    assert!(
        pkg.get("files").is_none(),
        "field should be file_count, not files"
    );
}

#[test]
fn packages_report_top_zero() {
    let (_p, session) = open_session();
    let report = session.packages_report(0);
    assert!(report.packages.is_empty());
    assert!(report.package_count > 0, "count should still reflect total");
}

// --- CommandOptions::resolve wiring ---

#[test]
fn command_options_resolve_defaults_to_settings() {
    let opts = CommandOptions::default();
    let settings = ReplSettings::default();
    let (trace_opts, top_modules) = opts.resolve(&settings);
    assert!(!trace_opts.include_dynamic);
    assert_eq!(trace_opts.top_n, chainsaw::report::DEFAULT_TOP);
    assert_eq!(top_modules, chainsaw::report::DEFAULT_TOP_MODULES);
}

#[test]
fn command_options_resolve_overrides_settings() {
    let mut opts = CommandOptions::default();
    opts.include_dynamic = Some(true);
    opts.top = Some(5);
    opts.top_modules = Some(3);
    opts.json = true;
    let settings = ReplSettings::default();
    let (trace_opts, top_modules) = opts.resolve(&settings);
    assert!(trace_opts.include_dynamic);
    assert_eq!(trace_opts.top_n, 5);
    assert_eq!(top_modules, 3);
}

// --- diff_report ---

#[test]
fn diff_report_against_self_is_zero() {
    let p = common::TestProject::new();
    let mut session = Session::open(&p.entry, true).unwrap();
    let report = session
        .diff_report(&p.entry, &TraceOptions::default(), 10)
        .unwrap();
    // Diffing entry against itself: delta should be 0
    assert_eq!(report.weight_delta, 0);
}

#[test]
fn diff_report_different_entry_has_delta() {
    let p = common::TestProject::new();
    let b_entry = p.root().join("b.ts");
    let mut session = Session::open(&p.entry, true).unwrap();
    let report = session
        .diff_report(&b_entry, &TraceOptions::default(), 10)
        .unwrap();
    // b.ts has fewer dependencies than index.ts -- delta should be nonzero
    assert_ne!(report.weight_delta, 0);
}

#[test]
fn diff_report_json_fields() {
    let p = common::TestProject::new();
    let b_entry = p.root().join("b.ts");
    let mut session = Session::open(&p.entry, true).unwrap();
    let report = session
        .diff_report(&b_entry, &TraceOptions::default(), 10)
        .unwrap();
    let json = report.to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(v["weight_delta"].is_number());
    assert!(v["weight_a"].is_number());
    assert!(v["weight_b"].is_number());
    assert!(v["only_in_a"].is_array());
    assert!(v["only_in_b"].is_array());
    // Skipped fields
    assert!(v.get("limit").is_none());
}

// --- imports / importers ---

#[test]
fn imports_lists_direct_dependencies() {
    let p = common::TestProject::new();
    let session = Session::open(&p.entry, true).unwrap();
    let imports = session.imports(&p.entry).unwrap();
    // index.ts imports a.ts and b.ts statically
    let names: Vec<String> = imports
        .iter()
        .map(|(p, _)| p.file_name().unwrap().to_string_lossy().to_string())
        .collect();
    assert!(names.contains(&"a.ts".to_string()));
    assert!(names.contains(&"b.ts".to_string()));
}

#[test]
fn importers_of_a_includes_index_and_b() {
    let p = common::TestProject::new();
    let session = Session::open(&p.entry, true).unwrap();
    let a_path = p.root().join("a.ts");
    let importers = session.importers(&a_path).unwrap();
    let names: Vec<String> = importers
        .iter()
        .map(|(p, _)| p.file_name().unwrap().to_string_lossy().to_string())
        .collect();
    assert!(names.contains(&"index.ts".to_string()));
    assert!(names.contains(&"b.ts".to_string()));
}
