mod common;

use chainsaw::query::TraceOptions;
use chainsaw::report::FormatVersion;
use chainsaw::session::Session;

/// Parse JSON, assert a field exists with the expected type.
fn assert_field(v: &serde_json::Value, key: &str, check: fn(&serde_json::Value) -> bool) {
    assert!(
        v.get(key).is_some_and(check),
        "field '{key}' missing or wrong type in: {v}"
    );
}

fn assert_no_field(v: &serde_json::Value, key: &str) {
    assert!(
        v.get(key).is_none(),
        "field '{key}' should not be present (serde(skip)): {v}"
    );
}

fn open() -> (common::TestProject, Session) {
    let p = common::TestProject::new();
    let entry = p.entry.clone();
    let session = Session::open(&entry, true).unwrap();
    (p, session)
}

// --- TraceReport ---

#[test]
fn trace_report_json_schema() {
    let (_p, mut session) = open();
    let json = session.trace_report(&TraceOptions::default(), 20).to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();

    assert_field(&v, "entry", |v| v.is_string());
    assert_field(&v, "static_weight_bytes", |v| v.is_number());
    assert_field(&v, "static_module_count", |v| v.is_number());
    assert_field(&v, "dynamic_only_weight_bytes", |v| v.is_number());
    assert_field(&v, "dynamic_only_module_count", |v| v.is_number());
    assert_field(&v, "heavy_packages", |v| v.is_array());
    assert_field(&v, "modules_by_cost", |v| v.is_array());
    assert_field(&v, "total_modules_with_cost", |v| v.is_number());

    // serde(skip) fields
    assert_no_field(&v, "include_dynamic");
    assert_no_field(&v, "top");

    // Nested: heavy_packages entries
    let pkg = &v["heavy_packages"][0];
    assert_field(pkg, "name", |v| v.is_string());
    assert_field(pkg, "total_size_bytes", |v| v.is_number());
    assert_field(pkg, "file_count", |v| v.is_number());
    assert_field(pkg, "chain", |v| v.is_array());
    assert_field(pkg, "edge_kinds", |v| v.is_array());
    assert_field(pkg, "classification", |v| v.is_object());

    // Nested: modules_by_cost entries
    let mod_entry = &v["modules_by_cost"][0];
    assert_field(mod_entry, "path", |v| v.is_string());
    assert_field(mod_entry, "exclusive_size_bytes", |v| v.is_number());
}

// --- ChainReport ---

#[test]
fn chain_report_json_schema() {
    let (_p, session) = open();
    let json = session.chain_report("lodash", false).to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();

    assert_field(&v, "target", |v| v.is_string());
    assert_field(&v, "found_in_graph", |v| v.is_boolean());
    assert_field(&v, "chain_count", |v| v.is_number());
    assert_field(&v, "hop_count", |v| v.is_number());
    assert_field(&v, "chains", |v| v.is_array());

    // Each chain is an object with modules, edge_kinds, classification
    let chain = &v["chains"][0];
    assert!(chain.is_object());
    assert_field(chain, "modules", |v| v.is_array());
    assert_field(chain, "edge_kinds", |v| v.is_array());
    assert_field(chain, "classification", |v| v.is_object());
    assert!(chain["modules"][0].is_string());
}

// --- CutReport ---

#[test]
fn cut_report_json_schema() {
    let (_p, mut session) = open();
    let json = session.cut_report("lodash", 10, false).to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();

    assert_field(&v, "target", |v| v.is_string());
    assert_field(&v, "found_in_graph", |v| v.is_boolean());
    assert_field(&v, "chain_count", |v| v.is_number());
    assert_field(&v, "direct_import", |v| v.is_boolean());
    assert_field(&v, "cut_points", |v| v.is_array());

    let cut = &v["cut_points"][0];
    assert_field(cut, "module", |v| v.is_string());
    assert_field(cut, "exclusive_size_bytes", |v| v.is_number());
    assert_field(cut, "chains_broken", |v| v.is_number());
}

// --- DiffReport ---

#[test]
fn diff_report_json_schema() {
    let (p, mut session) = open();
    let b = p.root().join("b.ts");
    let json = session
        .diff_report(&b, &TraceOptions::default(), 10)
        .unwrap()
        .to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();

    assert_field(&v, "entry_a", |v| v.is_string());
    assert_field(&v, "entry_b", |v| v.is_string());
    assert_field(&v, "weight_a", |v| v.is_number());
    assert_field(&v, "weight_b", |v| v.is_number());
    assert_field(&v, "weight_delta", |v| v.is_number());
    assert_field(&v, "dynamic_weight_a", |v| v.is_number());
    assert_field(&v, "dynamic_weight_b", |v| v.is_number());
    assert_field(&v, "dynamic_weight_delta", |v| v.is_number());
    assert_field(&v, "shared_count", |v| v.is_number());
    assert_field(&v, "only_in_a", |v| v.is_array());
    assert_field(&v, "only_in_b", |v| v.is_array());
    assert_field(&v, "dynamic_only_in_a", |v| v.is_array());
    assert_field(&v, "dynamic_only_in_b", |v| v.is_array());

    assert_no_field(&v, "limit");
}

// --- PackagesReport ---

#[test]
fn packages_report_json_schema() {
    let (_p, session) = open();
    let json = session.packages_report(10).to_json();
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();

    assert_field(&v, "package_count", |v| v.is_number());
    assert_field(&v, "packages", |v| v.is_array());

    let pkg = &v["packages"][0];
    assert_field(pkg, "name", |v| v.is_string());
    assert_field(pkg, "total_size_bytes", |v| v.is_number());
    assert_field(pkg, "file_count", |v| v.is_number());

    // Old field names must NOT appear
    assert_no_field(pkg, "size");
    assert_no_field(pkg, "files");
}

// --- FormatVersion round-trips ---

#[test]
fn chain_report_format_version_1() {
    let (_p, session) = open();
    let report = session.chain_report("lodash", false);
    let json: serde_json::Value =
        serde_json::from_str(&report.to_json_versioned(FormatVersion::V1)).unwrap();
    assert_eq!(json["format_version"], 1);
    let chain = &json["chains"][0];
    assert!(chain.is_array(), "V1 chains should be string arrays");
    assert!(chain[0].is_string());
}

#[test]
fn chain_report_format_version_2() {
    let (_p, session) = open();
    let report = session.chain_report("lodash", false);
    let json: serde_json::Value =
        serde_json::from_str(&report.to_json_versioned(FormatVersion::V2)).unwrap();
    assert_eq!(json["format_version"], 2);
    let chain = &json["chains"][0];
    assert!(chain.is_object(), "V2 chains should be annotated objects");
    assert_field(chain, "modules", |v| v.is_array());
    assert_field(chain, "edge_kinds", |v| v.is_array());
    assert_field(chain, "classification", |v| v.is_object());
}

#[test]
fn trace_report_format_version_1_no_edge_fields() {
    let (_p, mut session) = open();
    let report = session.trace_report(
        &TraceOptions::default(),
        chainsaw::report::DEFAULT_TOP_MODULES,
    );
    let json: serde_json::Value =
        serde_json::from_str(&report.to_json_versioned(FormatVersion::V1)).unwrap();
    assert_eq!(json["format_version"], 1);
    if let Some(pkg) = json["heavy_packages"].as_array().and_then(|a| a.first()) {
        assert!(pkg.get("edge_kinds").is_none(), "V1 should omit edge_kinds");
        assert!(
            pkg.get("classification").is_none(),
            "V1 should omit classification"
        );
    }
}
