use std::io::Write;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use super::PythonSupport;
use crate::lang::LanguageSupport;

const ORACLE_SCRIPT: &str = r#"
import importlib.util
import json
import sys
import os

def get_package(file_path, project_root):
    """Derive the dotted package name for a file from its path."""
    try:
        rel = os.path.relpath(file_path, project_root)
    except ValueError:
        return None
    parts = rel.replace(os.sep, '/').split('/')
    parts = parts[:-1]
    package_parts = []
    current = project_root
    for part in parts:
        current = os.path.join(current, part)
        if os.path.exists(os.path.join(current, '__init__.py')):
            package_parts.append(part)
        else:
            break
    return '.'.join(package_parts) if package_parts else None

def classify(spec, project_root):
    if spec is None:
        return "not_found", None
    if spec.origin is None:
        if spec.submodule_search_locations is not None:
            return "namespace", None
        return "builtin", None
    origin = spec.origin
    if origin in ("built-in", "frozen"):
        return "builtin", None
    origin = os.path.realpath(origin)
    pr = os.path.realpath(project_root)
    if origin.startswith(pr + os.sep):
        return "local", origin
    if "/site-packages/" in origin or "\\site-packages\\" in origin:
        return "third_party", origin
    return "stdlib", origin

def main():
    data = json.load(sys.stdin)
    project_root = data['project_root']
    if project_root not in sys.path:
        sys.path.insert(0, project_root)
    results = []
    for item in data['imports']:
        try:
            spec_name = item['specifier']
            if spec_name.startswith('.'):
                package = get_package(item['file'], project_root)
                if package is None:
                    results.append({"type": "not_found", "resolved": None})
                    continue
                spec = importlib.util.find_spec(spec_name, package=package)
            else:
                spec = importlib.util.find_spec(spec_name)
            typ, resolved = classify(spec, project_root)
            results.append({"type": typ, "resolved": resolved})
        except Exception:
            results.append({"type": "error", "resolved": None})
    json.dump(results, sys.stdout)

if __name__ == '__main__':
    main()
"#;

#[derive(Deserialize)]
struct OracleResult {
    #[serde(rename = "type")]
    typ: String,
    resolved: Option<String>,
}

/// Compare our Python resolver against Python's own importlib as ground truth.
///
/// Run with: PYTHON_PROJECT=/path/to/project cargo test python::conformance -- --ignored --nocapture
#[test]
#[ignore]
fn resolver_conformance() {
    let project = std::env::var("PYTHON_PROJECT")
        .expect("Set PYTHON_PROJECT env var to a Python project root");
    let root = PathBuf::from(&project).canonicalize().unwrap();

    let support = PythonSupport::new(&root);

    // Walk all .py files
    let files = find_python_files(&root);

    // Parse and collect all (file, specifier) pairs
    let mut imports: Vec<(PathBuf, String)> = Vec::new();
    for file in &files {
        if let Ok(parsed) = support.parse(file) {
            for imp in parsed {
                imports.push((file.clone(), imp.specifier));
            }
        }
    }

    eprintln!("Parsed {} files, found {} imports", files.len(), imports.len());

    // Resolve each with our resolver
    let our_results: Vec<Option<PathBuf>> = imports
        .iter()
        .map(|(file, spec)| {
            let from_dir = file.parent().unwrap();
            support.resolve(from_dir, spec)
        })
        .collect();

    // Ask Python oracle
    let python_results = run_oracle(&root, &imports);
    assert_eq!(imports.len(), python_results.len(), "oracle returned wrong count");

    // Compare and categorize
    let mut matches = 0u32;
    let mut both_none = 0u32;
    let mut oracle_errors = 0u32;
    let mut local_misses: Vec<(String, String, String)> = Vec::new();
    let mut third_party_misses: Vec<(String, String, String)> = Vec::new();
    let mut wrong_answers: Vec<(String, String, String, String)> = Vec::new();
    let mut phantoms: Vec<(String, String, String)> = Vec::new();

    for (i, (file, spec)) in imports.iter().enumerate() {
        let ours = &our_results[i];
        let oracle = &python_results[i];

        let file_rel = file
            .strip_prefix(&root)
            .unwrap_or(file)
            .to_string_lossy()
            .to_string();

        match (ours, &oracle.resolved, oracle.typ.as_str()) {
            // Both resolved — check if they agree
            (Some(our_path), Some(their_path), _) => {
                let our_canon = our_path.canonicalize().unwrap_or_else(|_| our_path.clone());
                let their_canon = PathBuf::from(their_path);
                if our_canon == their_canon {
                    matches += 1;
                } else {
                    wrong_answers.push((
                        file_rel,
                        spec.clone(),
                        our_canon.to_string_lossy().to_string(),
                        their_canon.to_string_lossy().to_string(),
                    ));
                }
            }

            // Both failed to resolve
            (None, None, _) | (None, _, "not_found" | "namespace") => {
                both_none += 1;
            }

            // Stdlib/builtin — we intentionally don't resolve these
            (None, _, "stdlib" | "builtin") => both_none += 1,

            // We missed, Python found locally — real bug
            (None, Some(path), "local") => {
                local_misses.push((file_rel, spec.clone(), path.clone()));
            }

            // We missed, Python found in site-packages — real bug
            (None, Some(path), "third_party") => {
                third_party_misses.push((file_rel, spec.clone(), path.clone()));
            }

            // We found something, Python didn't — phantom resolution
            (Some(our_path), _, "not_found" | "error") => {
                phantoms.push((
                    file_rel,
                    spec.clone(),
                    our_path.to_string_lossy().to_string(),
                ));
            }

            // Oracle error
            (_, _, "error") => oracle_errors += 1,

            // Catch-all
            _ => oracle_errors += 1,
        }
    }

    // Print report
    eprintln!("\n=== Conformance Report ===");
    eprintln!("Total imports:              {}", imports.len());
    eprintln!("Matches:                    {matches}");
    eprintln!("Skipped (stdlib/unresolved): {both_none}");
    eprintln!("Oracle errors:              {oracle_errors}");

    if !local_misses.is_empty() {
        eprintln!(
            "\n--- Local misses ({}): we=None, Python=project file ---",
            local_misses.len()
        );
        for (file, spec, path) in &local_misses {
            eprintln!("  {file}: import {spec}");
            eprintln!("    python -> {path}");
        }
    }

    if !third_party_misses.is_empty() {
        eprintln!(
            "\n--- Third-party misses ({}) : we=None, Python=site-packages ---",
            third_party_misses.len()
        );
        for (file, spec, path) in &third_party_misses {
            eprintln!("  {file}: import {spec}");
            eprintln!("    python -> {path}");
        }
    }

    if !wrong_answers.is_empty() {
        eprintln!(
            "\n--- Wrong answers ({}): we disagree with Python ---",
            wrong_answers.len()
        );
        for (file, spec, ours, theirs) in &wrong_answers {
            eprintln!("  {file}: import {spec}");
            eprintln!("    ours:   {ours}");
            eprintln!("    python: {theirs}");
        }
    }

    if !phantoms.is_empty() {
        eprintln!(
            "\n--- Phantoms ({}): we resolved, Python didn't ---",
            phantoms.len()
        );
        for (file, spec, path) in &phantoms {
            eprintln!("  {file}: import {spec} -> {path}");
        }
    }

    let real_bugs =
        local_misses.len() + third_party_misses.len() + wrong_answers.len() + phantoms.len();
    eprintln!("\nReal bugs: {real_bugs}");
}

fn find_python_files(root: &Path) -> Vec<PathBuf> {
    let skip: Vec<String> = [
        "__pycache__",
        ".git",
        ".venv",
        "venv",
        ".mypy_cache",
        ".pytest_cache",
        ".tox",
        ".eggs",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let mut files = Vec::new();
    let walker = ignore::WalkBuilder::new(root)
        .hidden(false)
        .git_ignore(true)
        .filter_entry(move |entry| {
            let path = entry.path();
            if path.is_dir() {
                return !path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| skip.iter().any(|s| s == n));
            }
            true
        })
        .build();

    for entry in walker.flatten() {
        let path = entry.into_path();
        if path.is_file() && path.extension().is_some_and(|e| e == "py") {
            files.push(path);
        }
    }
    files
}

fn run_oracle(root: &Path, imports: &[(PathBuf, String)]) -> Vec<OracleResult> {
    let input = serde_json::json!({
        "project_root": root.to_string_lossy(),
        "imports": imports.iter().map(|(file, spec)| {
            serde_json::json!({
                "file": file.to_string_lossy(),
                "specifier": spec
            })
        }).collect::<Vec<_>>()
    });

    // Write to temp file to avoid pipe deadlock on large inputs
    let mut tmp = tempfile::NamedTempFile::new().expect("failed to create temp file");
    serde_json::to_writer(&mut tmp, &input).expect("failed to write oracle input");
    tmp.flush().unwrap();

    let input_file = std::fs::File::open(tmp.path()).unwrap();
    let output = std::process::Command::new("python3")
        .args(["-c", ORACLE_SCRIPT])
        .stdin(input_file)
        .output()
        .expect("failed to run python3");

    assert!(
        output.status.success(),
        "Python oracle failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    serde_json::from_slice(&output.stdout).expect("failed to parse oracle output")
}
