use std::collections::HashSet;
use std::fs;
use std::path::Path;

#[test]
fn perf_registry_matches_benchmarks() {
    let bench_names = extract_benchmark_names();
    let (registry_benchmarks, registry_files) = parse_perf_toml();

    // Check 1: every benchmark in benchmarks.rs has a perf.toml entry
    for name in &bench_names {
        assert!(
            registry_benchmarks.contains(name),
            "Benchmark '{name}' exists in benches/benchmarks.rs but is not registered in perf.toml. \
             Add it to perf.toml so regressions are caught.",
        );
    }

    // Check 2: every benchmark in perf.toml exists in benchmarks.rs
    for name in &registry_benchmarks {
        assert!(
            bench_names.contains(name),
            "Benchmark '{name}' is registered in perf.toml but does not exist in benches/benchmarks.rs. \
             Remove it from perf.toml or add the benchmark.",
        );
    }

    // Check 3: every source file in perf.toml exists on disk
    for file in &registry_files {
        assert!(
            Path::new(file).exists(),
            "Source file '{file}' is listed in perf.toml but does not exist on disk. \
             Update perf.toml after file renames or deletions.",
        );
    }

    // Check 4: every .rs file in the workspace appears in perf.toml
    // (either with benchmarks or with an empty list as explicit exemption)
    let dirs: &[&str] = &["src", "benches", "stats/src", "xtask/src", "tests"];
    for dir in dirs {
        let files = collect_rs_files(Path::new(dir));
        for file in &files {
            assert!(
                registry_files.contains(file),
                "'{file}' is not registered in perf.toml. \
                 Add it with benchmarks if perf-sensitive, or with benchmarks = [] if exempt.",
            );
        }
    }
}

/// Benchmarks that do parallel `stat()` must run before benchmarks that read full
/// file contents on the same tree. `build_graph` reads 3200+ files, warming the OS
/// page cache. In save-baseline mode (50 iterations) this warming is heavy; in
/// comparison mode (early-stop at 5) it's light. `cache_load_validate_ts` does
/// `stat()` calls that are ~10% faster on warm pages, producing false positive
/// regressions when comparing across modes.
#[test]
fn io_sensitive_benchmarks_run_before_io_heavy() {
    let names = extract_benchmark_names_ordered();
    let cache_pos = names
        .iter()
        .position(|n| n == "cache_load_validate_ts")
        .expect("cache_load_validate_ts not found in benchmarks");
    let build_graph_pos = names
        .iter()
        .position(|n| n.starts_with("build_graph/"))
        .expect("no build_graph benchmark found");
    assert!(
        cache_pos < build_graph_pos,
        "cache_load_validate_ts (position {cache_pos}) must run before build_graph benchmarks \
         (position {build_graph_pos}) to avoid OS page cache contamination between modes",
    );
}

fn extract_benchmark_names_ordered() -> Vec<String> {
    let content = fs::read_to_string("benches/benchmarks.rs")
        .expect("Failed to read benches/benchmarks.rs");

    let mut names = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if let Some(start) = trimmed.find("name: \"") {
            let start_idx = start + "name: \"".len();
            if let Some(end_idx) = trimmed[start_idx..].find('"') {
                let name = &trimmed[start_idx..start_idx + end_idx];
                names.push(name.to_string());
            }
        }
    }

    names
}

fn extract_benchmark_names() -> HashSet<String> {
    let content = fs::read_to_string("benches/benchmarks.rs")
        .expect("Failed to read benches/benchmarks.rs");

    let mut names = HashSet::new();

    for line in content.lines() {
        let trimmed = line.trim();

        // Match: name: "bench_name",
        if let Some(start) = trimmed.find("name: \"") {
            let start_idx = start + "name: \"".len();
            if let Some(end_idx) = trimmed[start_idx..].find('"') {
                let name = &trimmed[start_idx..start_idx + end_idx];
                names.insert(name.to_string());
            }
        }
    }

    names
}

fn parse_perf_toml() -> (HashSet<String>, HashSet<String>) {
    let content = fs::read_to_string("perf.toml")
        .expect("Failed to read perf.toml");

    let mut benchmarks = HashSet::new();
    let mut files = HashSet::new();
    let mut in_benchmarks_array = false;
    let mut in_files_array = false;

    for line in content.lines() {
        let trimmed = line.trim();

        // Detect array start
        if trimmed.starts_with("benchmarks = [") {
            in_benchmarks_array = true;
            in_files_array = false;
            extract_array_values(trimmed, &mut benchmarks);
            if trimmed.ends_with(']') {
                in_benchmarks_array = false;
            }
        } else if trimmed.starts_with("files = [") {
            in_files_array = true;
            in_benchmarks_array = false;
            extract_array_values(trimmed, &mut files);
            if trimmed.ends_with(']') {
                in_files_array = false;
            }
        } else if in_benchmarks_array || in_files_array {
            // Continue parsing multi-line array
            if trimmed.ends_with(']') {
                in_benchmarks_array = false;
                in_files_array = false;
            }
            if in_benchmarks_array {
                extract_array_values(trimmed, &mut benchmarks);
            } else if in_files_array {
                extract_array_values(trimmed, &mut files);
            }
        }
    }

    (benchmarks, files)
}

fn collect_rs_files(dir: &Path) -> HashSet<String> {
    let mut files = HashSet::new();
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            files.extend(collect_rs_files(&path));
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
            // Normalize to forward-slash relative path matching perf.toml format
            let rel = path.to_string_lossy().replace('\\', "/");
            files.insert(rel);
        }
    }
    files
}

fn extract_array_values(line: &str, set: &mut HashSet<String>) {
    // Extract quoted strings from array notation
    // Handles: ["value1", "value2"] or individual "value" lines
    let mut in_quote = false;
    let mut current = String::new();

    for ch in line.chars() {
        if ch == '"' {
            if in_quote {
                // End of quoted string
                if !current.is_empty() {
                    set.insert(current.clone());
                    current.clear();
                }
                in_quote = false;
            } else {
                // Start of quoted string
                in_quote = true;
            }
        } else if in_quote {
            current.push(ch);
        }
    }
}
