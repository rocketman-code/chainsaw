use serde::Deserialize;
use std::collections::BTreeSet;
use std::path::Path;

#[derive(Deserialize)]
struct PerfToml {
    entry: Vec<Entry>,
}

#[derive(Deserialize)]
struct Entry {
    files: Vec<String>,
    benchmarks: Vec<String>,
}

/// Parsed perf registry from perf.toml.
pub struct Registry {
    entries: Vec<(Vec<String>, Vec<String>)>, // (files, benchmarks)
}

impl Registry {
    /// Load and parse perf.toml from the project root.
    /// Returns None if the file doesn't exist.
    pub fn load(project_root: &Path) -> Option<Self> {
        let path = project_root.join("perf.toml");
        let content = std::fs::read_to_string(&path).ok()?;
        let parsed: PerfToml = toml::from_str(&content)
            .unwrap_or_else(|e| panic!("failed to parse perf.toml: {e}"));
        Some(Self {
            entries: parsed
                .entry
                .into_iter()
                .map(|e| (e.files, e.benchmarks))
                .collect(),
        })
    }

    /// Given a list of changed file paths, return the set of benchmarks
    /// that need to be validated. Returns empty set if no perf-sensitive
    /// files were changed.
    pub fn required_benchmarks(&self, changed_files: &[String]) -> BTreeSet<String> {
        let mut benchmarks = BTreeSet::new();
        for (files, benches) in &self.entries {
            for changed in changed_files {
                if files.iter().any(|f| f == changed) {
                    benchmarks.extend(benches.iter().cloned());
                    break;
                }
            }
        }
        benchmarks
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_perf_toml() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        let registry = Registry::load(root).expect("perf.toml should exist");
        assert!(!registry.entries.is_empty());
    }

    #[test]
    fn matches_changed_files() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        let registry = Registry::load(root).unwrap();

        let benchmarks =
            registry.required_benchmarks(&["src/lang/typescript/parser.rs".to_string()]);
        assert!(benchmarks.contains("ts_parse_file"));
        assert!(!benchmarks.contains("py_parse_file"));
    }

    #[test]
    fn no_match_for_non_perf_files() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        let registry = Registry::load(root).unwrap();

        let benchmarks = registry.required_benchmarks(&[
            "src/main.rs".to_string(),
            "src/report.rs".to_string(),
        ]);
        assert!(benchmarks.is_empty());
    }

    #[test]
    fn multi_file_entry_triggers_on_any() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        let registry = Registry::load(root).unwrap();

        // walker.rs and lang/mod.rs share an entry
        let benchmarks = registry.required_benchmarks(&["src/lang/mod.rs".to_string()]);
        assert!(benchmarks.contains("build_graph/ts_cold"));
        assert!(benchmarks.contains("build_graph/py_cold"));
    }
}
