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
            "Benchmark '{}' exists in benches/benchmarks.rs but is not registered in perf.toml. \
             Add it to perf.toml so regressions are caught.",
            name
        );
    }

    // Check 2: every benchmark in perf.toml exists in benchmarks.rs
    for name in &registry_benchmarks {
        assert!(
            bench_names.contains(name),
            "Benchmark '{}' is registered in perf.toml but does not exist in benches/benchmarks.rs. \
             Remove it from perf.toml or add the benchmark.",
            name
        );
    }

    // Check 3: every source file in perf.toml exists on disk
    for file in &registry_files {
        assert!(
            Path::new(file).exists(),
            "Source file '{}' is listed in perf.toml but does not exist on disk. \
             Update perf.toml after file renames or deletions.",
            file
        );
    }
}

fn extract_benchmark_names() -> HashSet<String> {
    let content = fs::read_to_string("benches/benchmarks.rs")
        .expect("Failed to read benches/benchmarks.rs");

    let mut names = HashSet::new();
    let mut current_group: Option<String> = None;

    for line in content.lines() {
        let trimmed = line.trim();

        // Check for benchmark_group("group_name")
        if let Some(group_start) = trimmed.find("benchmark_group(\"") {
            let start_idx = group_start + "benchmark_group(\"".len();
            if let Some(end_idx) = trimmed[start_idx..].find('"') {
                let group_name = &trimmed[start_idx..start_idx + end_idx];
                current_group = Some(group_name.to_string());
            }
        }

        // Check for group.finish() to end the group
        if trimmed.contains("group.finish()") {
            current_group = None;
        }

        // Check for bench_function("bench_name")
        if let Some(func_start) = trimmed.find("bench_function(\"") {
            let start_idx = func_start + "bench_function(\"".len();
            if let Some(end_idx) = trimmed[start_idx..].find('"') {
                let bench_name = &trimmed[start_idx..start_idx + end_idx];
                let full_name = if let Some(ref group) = current_group {
                    format!("{}/{}", group, bench_name)
                } else {
                    bench_name.to_string()
                };
                names.insert(full_name);
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
            // Handle single-line array
            if trimmed.ends_with(']') {
                extract_array_values(trimmed, &mut benchmarks);
                in_benchmarks_array = false;
            } else {
                // Start of multi-line array
                extract_array_values(trimmed, &mut benchmarks);
            }
        } else if trimmed.starts_with("files = [") {
            in_files_array = true;
            in_benchmarks_array = false;
            // Handle single-line array
            if trimmed.ends_with(']') {
                extract_array_values(trimmed, &mut files);
                in_files_array = false;
            } else {
                // Start of multi-line array
                extract_array_values(trimmed, &mut files);
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
