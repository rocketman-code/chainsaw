mod common;

use assert_cmd::Command;
use predicates::prelude::*;

fn chainsaw() -> Command {
    assert_cmd::cargo_bin_cmd!("chainsaw")
}

// --- trace subcommand ---

#[test]
fn trace_produces_output() {
    let p = common::TestProject::new();
    chainsaw()
        .args(["trace", "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success()
        .stdout(predicate::str::contains("lodash"));
}

#[test]
fn trace_json_produces_valid_json() {
    let p = common::TestProject::new();
    let output = chainsaw()
        .args(["trace", "--json", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let v: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(v["static_weight_bytes"].is_number());
    assert!(v["static_module_count"].is_number());
    assert!(v["heavy_packages"].is_array());
    assert!(v["modules_by_cost"].is_array());
}

#[test]
fn trace_chain_finds_lodash() {
    let p = common::TestProject::new();
    chainsaw()
        .args(["trace", "--chain", "lodash", "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success()
        .stdout(predicate::str::contains("lodash"));
}

#[test]
fn trace_chain_json() {
    let p = common::TestProject::new();
    let output = chainsaw()
        .args(["trace", "--chain", "lodash", "--json", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let v: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(v["chain_count"].is_number());
    assert!(v["chains"].is_array());
}

#[test]
fn trace_cut_finds_cut_points() {
    let p = common::TestProject::new();
    chainsaw()
        .args(["trace", "--cut", "lodash", "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success()
        .stdout(predicate::str::contains("a.ts"));
}

#[test]
fn trace_cut_json() {
    let p = common::TestProject::new();
    let output = chainsaw()
        .args(["trace", "--cut", "lodash", "--json", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let v: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(v["cut_points"].is_array());
    assert!(v["direct_import"].is_boolean());
}

#[test]
fn trace_include_dynamic() {
    let p = common::TestProject::new();
    // Without --include-dynamic: c.ts is dynamic-only, not in static count
    let without = chainsaw()
        .args(["trace", "--json", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(without.status.success());
    let v_without: serde_json::Value = serde_json::from_slice(&without.stdout).unwrap();
    let static_without = v_without["static_module_count"].as_u64().unwrap();
    let dyn_without = v_without["dynamic_only_module_count"].as_u64().unwrap();
    assert!(
        dyn_without > 0,
        "c.ts should be dynamic-only without --include-dynamic"
    );

    // With --include-dynamic: dynamic modules merge into static count
    let with = chainsaw()
        .args(["trace", "--json", "--include-dynamic", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(with.status.success());
    let v_with: serde_json::Value = serde_json::from_slice(&with.stdout).unwrap();
    let static_with = v_with["static_module_count"].as_u64().unwrap();
    assert!(
        static_with > static_without,
        "include-dynamic should merge c.ts into static count: {static_with} vs {static_without}"
    );
}

#[test]
fn trace_top_zero_hides_packages() {
    let p = common::TestProject::new();
    let output = chainsaw()
        .args(["trace", "--json", "--top", "0", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let v: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(v["heavy_packages"].as_array().unwrap().len(), 0);
}

#[test]
fn trace_save_creates_snapshot() {
    let p = common::TestProject::new();
    let snap = p.root().join("snapshot.json");
    chainsaw()
        .args(["trace", "--save", snap.to_str().unwrap(), "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success();
    assert!(snap.exists());
    let content: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&snap).unwrap()).unwrap();
    assert!(content["static_weight"].is_number());
}

#[test]
fn trace_diff_from_snapshot() {
    let p = common::TestProject::new();
    let snap = p.root().join("snap.json");
    // First, save a snapshot
    chainsaw()
        .args(["trace", "--save", snap.to_str().unwrap(), "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success();
    // Then diff against it
    chainsaw()
        .args(["trace", "--diff-from", snap.to_str().unwrap(), "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success();
}

#[test]
fn trace_diff_from_json() {
    let p = common::TestProject::new();
    let snap = p.root().join("snap.json");
    chainsaw()
        .args(["trace", "--save", snap.to_str().unwrap(), "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success();
    let output = chainsaw()
        .args([
            "trace",
            "--diff-from",
            snap.to_str().unwrap(),
            "--json",
            "--no-cache",
        ])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let v: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(v["weight_delta"].is_number());
}

#[test]
fn trace_max_weight_exceeded() {
    let p = common::TestProject::new();
    // Threshold of 1 byte -- guaranteed to exceed
    chainsaw()
        .args(["trace", "--max-weight", "1", "--no-cache"])
        .arg(&p.entry)
        .assert()
        .failure()
        .stderr(predicate::str::contains("exceeds --max-weight"));
}

#[test]
fn trace_max_weight_ok() {
    let p = common::TestProject::new();
    // Threshold of 100MB -- guaranteed to be under
    chainsaw()
        .args(["trace", "--max-weight", "100MB", "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success();
}

#[test]
fn trace_mutually_exclusive_chain_cut() {
    let p = common::TestProject::new();
    chainsaw()
        .args([
            "trace",
            "--chain",
            "lodash",
            "--cut",
            "lodash",
            "--no-cache",
        ])
        .arg(&p.entry)
        .assert()
        .failure()
        .stderr(predicate::str::contains("cannot be used together"));
}

#[test]
fn trace_chain_target_is_entry() {
    let p = common::TestProject::new();
    chainsaw()
        .args(["trace", "--chain", p.entry.to_str().unwrap(), "--no-cache"])
        .arg(&p.entry)
        .assert()
        .failure();
}

// --- packages subcommand ---

#[test]
fn packages_lists_lodash() {
    let p = common::TestProject::new();
    chainsaw()
        .args(["packages", "--no-cache"])
        .arg(&p.entry)
        .assert()
        .success()
        .stdout(predicate::str::contains("lodash"));
}

#[test]
fn packages_json() {
    let p = common::TestProject::new();
    let output = chainsaw()
        .args(["packages", "--json", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let v: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(v["package_count"].is_number());
    assert!(v["packages"].is_array());
    let pkg = &v["packages"][0];
    assert!(pkg["name"].is_string());
    assert!(pkg["total_size_bytes"].is_number());
    assert!(pkg["file_count"].is_number());
}

// --- error cases ---

#[test]
fn missing_entry_file() {
    chainsaw()
        .args(["trace", "/nonexistent/file.ts"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("cannot find entry file"));
}

#[test]
fn entry_is_directory() {
    let p = common::TestProject::new();
    chainsaw()
        .args(["trace", "--no-cache"])
        .arg(p.root())
        .assert()
        .failure()
        .stderr(predicate::str::contains("directory"));
}

#[test]
fn unsupported_file_type() {
    let p = common::TestProject::new();
    let rs_file = p.root().join("main.rs");
    std::fs::write(&rs_file, "fn main() {}").unwrap();
    chainsaw()
        .args(["trace", "--no-cache"])
        .arg(&rs_file)
        .assert()
        .failure()
        .stderr(predicate::str::contains("unsupported file type"));
}

// --- quiet flag ---

#[test]
fn quiet_suppresses_timing() {
    let p = common::TestProject::new();
    let output = chainsaw()
        .args(["trace", "--quiet", "--no-cache"])
        .arg(&p.entry)
        .output()
        .unwrap();
    assert!(output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("ms"),
        "quiet should suppress timing, got: {stderr}"
    );
}
