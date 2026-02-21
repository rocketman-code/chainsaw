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

def resolve_relative(spec_name, package):
    """Resolve a relative import to its absolute module name."""
    dots = len(spec_name) - len(spec_name.lstrip('.'))
    tail = spec_name[dots:]
    parts = package.split('.')
    if dots > len(parts):
        return spec_name
    base = '.'.join(parts[:len(parts) - dots + 1])
    return base + '.' + tail if tail else base

def get_package(file_path, source_roots):
    """Derive the dotted package name for a file from its path.

    Tries each source root to find one where the file sits inside a
    chain of directories with __init__.py files (i.e. a proper package).
    """
    for root in source_roots:
        try:
            rel = os.path.relpath(file_path, root)
        except ValueError:
            continue
        if rel.startswith('..'):
            continue
        parts = rel.replace(os.sep, '/').split('/')
        parts = parts[:-1]
        package_parts = []
        current = root
        for part in parts:
            current = os.path.join(current, part)
            if os.path.exists(os.path.join(current, '__init__.py')):
                package_parts.append(part)
            else:
                break
        if package_parts and len(package_parts) == len(parts):
            return '.'.join(package_parts)
    return None

def classify_path(origin, project_root):
    """Classify a resolved file path."""
    origin = os.path.realpath(origin)
    pr = os.path.realpath(project_root)
    if origin.startswith(pr + os.sep):
        return "local", origin
    if "/site-packages/" in origin or "\\site-packages\\" in origin:
        return "third_party", origin
    return "stdlib", origin

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
    return classify_path(origin, project_root)

def find_on_filesystem(full_name):
    """Fallback when find_spec crashes: walk sys.path to find the module file.

    find_spec imports parent packages, which can crash if they have missing
    C extensions (e.g. cryptography._openssl). This bypasses the import
    machinery entirely and just checks if the file exists.
    """
    rel = full_name.replace('.', os.sep)
    for base in sys.path:
        # Package: directory with __init__.py
        init = os.path.join(base, rel, '__init__.py')
        if os.path.isfile(init):
            return init
        # Module file
        mod = os.path.join(base, rel + '.py')
        if os.path.isfile(mod):
            return mod
    return None

def main():
    data = json.load(sys.stdin)
    project_root = data['project_root']
    source_roots = data.get('source_roots', [project_root])
    for root in source_roots:
        if root not in sys.path:
            sys.path.insert(0, root)
    results = []
    for item in data['imports']:
        # Save sys.path before each find_spec call. find_spec internally
        # __import__s parent packages, which can execute __init__.py that
        # modifies sys.path (e.g. poetry-core injecting _vendor/).
        # Restoring sys.path ensures modifications don't leak across calls.
        saved_path = sys.path[:]
        try:
            spec_name = item['specifier']
            if spec_name.startswith('.'):
                package = get_package(item['file'], source_roots)
                if package is None:
                    results.append({"type": "not_found", "resolved": None})
                    continue
                full_name = resolve_relative(spec_name, package)
            else:
                full_name = spec_name
            # Detect sys.modules contamination: some __init__.py files
            # replace themselves via sys.modules[name] = other_module
            # (e.g. ansible.module_utils.distro). find_spec checks
            # sys.modules first, so it would return the wrong spec.
            # Only evict when we detect replacement (__name__ mismatch).
            cached = sys.modules.get(full_name)
            evicted = False
            if cached is not None and getattr(cached, '__name__', full_name) != full_name:
                del sys.modules[full_name]
                evicted = True
            try:
                spec = importlib.util.find_spec(spec_name, package=package if spec_name.startswith('.') else None)
            finally:
                if evicted and cached is not None:
                    sys.modules[full_name] = cached
            typ, resolved = classify(spec, project_root)
            results.append({"type": typ, "resolved": resolved})
        except Exception:
            # find_spec crashed (e.g. missing C extension in parent package).
            # Fall back to filesystem lookup so we can still compare results.
            path = find_on_filesystem(full_name)
            if path:
                typ, resolved = classify_path(path, project_root)
                results.append({"type": typ, "resolved": resolved})
            else:
                results.append({"type": "not_found", "resolved": None})
        finally:
            sys.path[:] = saved_path
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
/// Run with: `PYTHON_PROJECT=/path/to/project cargo test python::conformance -- --ignored --nocapture`
#[test]
#[ignore = "requires PYTHON_PROJECT env var"]
#[allow(clippy::too_many_lines)]
fn resolver_conformance() {
    let project = std::env::var("PYTHON_PROJECT")
        .expect("Set PYTHON_PROJECT env var to a Python project root");
    let root = PathBuf::from(&project).canonicalize().unwrap();

    let support = PythonSupport::new(&root);

    // Walk all .py files
    let files = find_python_files(&root);

    // Parse and collect all (file, specifier) pairs
    let source_roots: Vec<&Path> = support
        .source_roots()
        .iter()
        .map(PathBuf::as_path)
        .collect();
    let mut imports: Vec<(PathBuf, String)> = Vec::new();
    for file in &files {
        let Ok(source) = std::fs::read_to_string(file) else {
            continue;
        };
        let Ok(parsed) = support.parse(file, &source) else {
            continue;
        };
        let in_package = has_package_chain(file, &source_roots);
        for imp in parsed.imports {
            // Skip relative imports from files not in a valid package
            // chain — Python can't resolve them and neither can the oracle.
            if imp.specifier.starts_with('.') && !in_package {
                continue;
            }
            imports.push((file.clone(), imp.specifier));
        }
    }

    let python = find_python(&root);
    eprintln!("Python: {}", python.display());
    eprintln!(
        "Parsed {} files, found {} imports",
        files.len(),
        imports.len()
    );

    // Resolve each with our resolver
    let our_results: Vec<Option<PathBuf>> = imports
        .iter()
        .map(|(file, spec)| {
            let from_dir = file.parent().unwrap();
            support.resolve(from_dir, spec)
        })
        .collect();

    // Ask Python oracle
    let python_results = run_oracle(&root, &source_roots, &imports);
    assert_eq!(
        imports.len(),
        python_results.len(),
        "oracle returned wrong count"
    );

    // Compare and categorize
    let mut matches = 0u32;
    let mut stdlib = 0u32;
    let mut both_unresolved = 0u32;
    let mut oracle_errors = 0u32;
    let mut local_misses: Vec<(String, String, String)> = Vec::new();
    let mut third_party_misses: Vec<(String, String, String)> = Vec::new();
    let mut wrong_answers: Vec<(String, String, String, String)> = Vec::new();
    let mut phantoms: Vec<(String, String, String)> = Vec::new();
    let mut oracle_crashes = 0u32;

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
                let their_canon = PathBuf::from(their_path)
                    .canonicalize()
                    .unwrap_or_else(|_| PathBuf::from(their_path));
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
                both_unresolved += 1;
            }

            // Stdlib/builtin — we intentionally don't resolve these
            (None, _, "stdlib" | "builtin") => stdlib += 1,

            // We missed, Python found locally — real bug
            (None, Some(path), "local") => {
                local_misses.push((file_rel, spec.clone(), path.clone()));
            }

            // We missed, Python found in site-packages — real bug
            (None, Some(path), "third_party") => {
                third_party_misses.push((file_rel, spec.clone(), path.clone()));
            }

            // We found something, Python said not_found — true phantom
            (Some(our_path), _, "not_found") => {
                phantoms.push((
                    file_rel,
                    spec.clone(),
                    our_path.to_string_lossy().to_string(),
                ));
            }

            // Oracle crashed (find_spec threw) — not our fault
            (_, _, "error") => oracle_crashes += 1,

            // Unexpected category from oracle
            _ => {
                oracle_errors += 1;
            }
        }
    }

    // Print report
    eprintln!("\n=== Conformance Report ===");
    eprintln!("Total imports:              {}", imports.len());
    eprintln!("Matches:                    {matches}");
    eprintln!("Stdlib/builtin:             {stdlib}");
    eprintln!("Both unresolved:            {both_unresolved}");
    eprintln!("Oracle crashes:             {oracle_crashes}");
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
    let skip = [
        "__pycache__",
        ".git",
        ".venv",
        "venv",
        ".mypy_cache",
        ".pytest_cache",
        ".tox",
        ".eggs",
    ];

    let root_owned = root.to_path_buf();
    let mut files = Vec::new();
    let walker = ignore::WalkBuilder::new(root)
        .hidden(false)
        .git_ignore(true)
        .filter_entry(move |entry| {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                if skip.contains(&name) {
                    return false;
                }
                // Skip nested git repos (embedded subprojects like pydantic-core/)
                if path != root_owned && path.join(".git").exists() {
                    return false;
                }
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

/// Check if a file sits inside a valid package chain from any source root.
/// A valid chain means every directory between the source root and the file's
/// parent has an `__init__.py`. Without this, Python can't derive a package
/// name for relative imports, so they're untestable by the oracle.
fn has_package_chain(file: &Path, source_roots: &[&Path]) -> bool {
    let Some(parent) = file.parent() else {
        return false;
    };
    'roots: for &root in source_roots {
        let Ok(rel) = parent.strip_prefix(root) else {
            continue;
        };
        let mut current = root.to_path_buf();
        for component in rel.components() {
            current.push(component);
            if !current.join("__init__.py").exists() {
                continue 'roots;
            }
        }
        return true;
    }
    false
}

fn find_python(root: &Path) -> PathBuf {
    let venv_python = root.join(".venv/bin/python");
    if venv_python.exists() {
        return venv_python;
    }
    PathBuf::from("python3")
}

fn run_oracle(
    root: &Path,
    source_roots: &[&Path],
    imports: &[(PathBuf, String)],
) -> Vec<OracleResult> {
    let input = serde_json::json!({
        "project_root": root.to_string_lossy(),
        "source_roots": source_roots.iter().map(|p| p.to_string_lossy()).collect::<Vec<_>>(),
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

    let python = find_python(root);
    let input_file = tmp.reopen().expect("failed to reopen temp file");
    let output = std::process::Command::new(&python)
        .args(["-c", ORACLE_SCRIPT])
        .stdin(input_file)
        .output()
        .unwrap_or_else(|e| panic!("failed to run {}: {e}", python.display()));

    assert!(
        output.status.success(),
        "Python oracle failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    serde_json::from_slice(&output.stdout).expect("failed to parse oracle output")
}
