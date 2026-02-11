use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ignore::WalkBuilder;
use rayon::prelude::*;

use crate::graph::{EdgeKind, ModuleGraph, ModuleId, PackageInfo};
use crate::lang::RawImport;
use crate::lang::typescript::parser;
use crate::lang::typescript::resolver::{package_name_from_path, ImportResolver};

const PARSEABLE_EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mjs", "cjs", "mts", "cts"];

fn is_parseable(path: &Path) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| PARSEABLE_EXTENSIONS.contains(&ext))
}

/// Discover source files using the ignore crate (respects .gitignore).
fn discover_source_files(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let walker = WalkBuilder::new(root)
        .hidden(false)
        .git_ignore(true)
        .filter_entry(|entry| {
            let path = entry.path();
            // Skip node_modules during initial discovery — we'll follow into it via imports
            if path.is_dir() {
                return !path
                    .file_name()
                    .is_some_and(|n| n == "node_modules" || n == ".git");
            }
            true
        })
        .build();

    for entry in walker.flatten() {
        let path = entry.into_path();
        if path.is_file() && is_parseable(&path) {
            files.push(path);
        }
    }
    files
}

/// Parse a batch of files in parallel, returning (path, imports) pairs.
fn parse_files_parallel(files: &[PathBuf]) -> Vec<(PathBuf, Vec<RawImport>)> {
    files
        .par_iter()
        .filter_map(|path| match parser::parse_file(path) {
            Ok(imports) => Some((path.clone(), imports)),
            Err(e) => {
                eprintln!("warning: {e}");
                None
            }
        })
        .collect()
}

/// Read the "name" field from a package.json file.
fn read_package_name(pkg_json: &Path) -> Option<String> {
    let content = fs::read_to_string(pkg_json).ok()?;
    let parsed: serde_json::Value = serde_json::from_str(&content).ok()?;
    parsed.get("name")?.as_str().map(|s| s.to_string())
}

/// Cache for workspace package name lookups.
/// For resolved paths outside node_modules, walks up to find package.json
/// and uses its "name" field as the package name.
struct WorkspacePackageCache {
    project_root: PathBuf,
    cache: HashMap<PathBuf, Option<String>>,
}

impl WorkspacePackageCache {
    fn new(project_root: &Path) -> Self {
        Self {
            project_root: project_root.to_path_buf(),
            cache: HashMap::new(),
        }
    }

    /// Try to find a workspace package name for the given file path.
    /// Returns None for files within the project root or without a package.json ancestor.
    fn lookup(&mut self, file_path: &Path) -> Option<String> {
        let mut dir = file_path.parent()?;
        loop {
            if let Some(cached) = self.cache.get(dir) {
                return cached.clone();
            }

            let pkg_json = dir.join("package.json");
            if pkg_json.exists() {
                let result = if dir == self.project_root {
                    // Project root itself — not a workspace dep
                    None
                } else {
                    read_package_name(&pkg_json)
                };
                self.cache.insert(dir.to_path_buf(), result.clone());
                return result;
            }

            match dir.parent() {
                Some(parent) if parent != dir => dir = parent,
                _ => break,
            }
        }
        None
    }
}

/// Build a complete ModuleGraph starting from the given root directory.
/// Walks source files, parses them, resolves imports (following into node_modules),
/// and builds the graph iteratively.
pub fn build_graph(root: &Path) -> ModuleGraph {
    let graph = Mutex::new(ModuleGraph::new());
    let resolver = ImportResolver::new(root);

    // Phase 1: Discover and parse source files
    let source_files = discover_source_files(root);
    let parsed = parse_files_parallel(&source_files);

    // Track files that failed to parse so we don't retry them in phase 2
    let parsed_paths: HashSet<&PathBuf> = parsed.iter().map(|(p, _)| p).collect();
    let mut parse_failures: HashSet<PathBuf> = source_files
        .iter()
        .filter(|p| !parsed_paths.contains(p))
        .cloned()
        .collect();

    // Register all discovered source modules
    for (path, _) in &parsed {
        let size = fs::metadata(path).map(|m| m.len()).unwrap_or(0);
        graph.lock().unwrap().add_module(path.clone(), size, None);
    }

    // Phase 2: Resolve imports and follow into node_modules iteratively
    // Use a work queue for files that need their imports resolved
    let mut pending_resolutions: VecDeque<(PathBuf, Vec<RawImport>)> =
        parsed.into_iter().collect();
    let mut workspace_cache = WorkspacePackageCache::new(root);

    while let Some((source_path, imports)) = pending_resolutions.pop_front() {
        let source_dir = source_path.parent().unwrap_or(Path::new("."));
        let source_id = graph.lock().unwrap().path_to_id[&source_path];

        let mut new_files_to_parse: Vec<PathBuf> = Vec::new();

        for raw_import in &imports {
            let resolved = match resolver.resolve(source_dir, &raw_import.specifier) {
                Some(p) => p,
                None => continue,
            };

            let package = package_name_from_path(&resolved)
                .or_else(|| workspace_cache.lookup(&resolved));
            let size = fs::metadata(&resolved).map(|m| m.len()).unwrap_or(0);

            let mut g = graph.lock().unwrap();
            let is_new = !g.path_to_id.contains_key(&resolved);
            let target_id = g.add_module(resolved.clone(), size, package);
            g.add_edge(source_id, target_id, raw_import.kind, raw_import.specifier.clone());

            // If this is a new file and it's parseable (and hasn't already failed), queue it
            if is_new && is_parseable(&resolved) && !parse_failures.contains(&resolved) {
                new_files_to_parse.push(resolved);
            }
        }

        // Parse newly discovered files (e.g. node_modules entries) in parallel
        if !new_files_to_parse.is_empty() {
            let newly_parsed = parse_files_parallel(&new_files_to_parse);
            let newly_parsed_paths: HashSet<&PathBuf> =
                newly_parsed.iter().map(|(p, _)| p).collect();
            for path in &new_files_to_parse {
                if !newly_parsed_paths.contains(path) {
                    parse_failures.insert(path.clone());
                }
            }
            for item in newly_parsed {
                pending_resolutions.push_back(item);
            }
        }
    }

    // Phase 3: Compute package info
    let mut g = graph.into_inner().unwrap();
    compute_package_info(&mut g);
    g
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn setup_workspace(tmp: &Path) {
        // Project root: tmp/packages/app (has package.json with name "my-app")
        // Workspace sibling: tmp/packages/lib (has package.json with name "@my/lib")
        let app = tmp.join("packages/app");
        let lib = tmp.join("packages/lib/src");
        fs::create_dir_all(&app).unwrap();
        fs::create_dir_all(&lib).unwrap();
        fs::write(
            app.join("package.json"),
            r#"{"name": "my-app"}"#,
        ).unwrap();
        fs::write(
            tmp.join("packages/lib/package.json"),
            r#"{"name": "@my/lib"}"#,
        ).unwrap();
        fs::write(lib.join("index.ts"), "export const x = 1;").unwrap();
    }

    #[test]
    fn read_package_name_valid() {
        let tmp = tempfile::tempdir().unwrap();
        fs::write(
            tmp.path().join("package.json"),
            r#"{"name": "@scope/pkg", "version": "1.0.0"}"#,
        ).unwrap();
        assert_eq!(
            read_package_name(&tmp.path().join("package.json")),
            Some("@scope/pkg".to_string())
        );
    }

    #[test]
    fn read_package_name_missing_name_field() {
        let tmp = tempfile::tempdir().unwrap();
        fs::write(
            tmp.path().join("package.json"),
            r#"{"version": "1.0.0"}"#,
        ).unwrap();
        assert_eq!(read_package_name(&tmp.path().join("package.json")), None);
    }

    #[test]
    fn read_package_name_nonexistent_file() {
        assert_eq!(read_package_name(Path::new("/nonexistent/package.json")), None);
    }

    #[test]
    fn workspace_cache_detects_sibling_package() {
        let tmp = tempfile::tempdir().unwrap();
        setup_workspace(tmp.path());

        let project_root = tmp.path().join("packages/app");
        let mut cache = WorkspacePackageCache::new(&project_root);

        // File in workspace sibling should resolve to package name
        let sibling_file = tmp.path().join("packages/lib/src/index.ts");
        assert_eq!(cache.lookup(&sibling_file), Some("@my/lib".to_string()));
    }

    #[test]
    fn workspace_cache_skips_project_root() {
        let tmp = tempfile::tempdir().unwrap();
        setup_workspace(tmp.path());

        let project_root = tmp.path().join("packages/app");
        let mut cache = WorkspacePackageCache::new(&project_root);

        // File in project root should NOT get a package name
        let own_file = project_root.join("src/cli.ts");
        assert_eq!(cache.lookup(&own_file), None);
    }

    #[test]
    fn workspace_cache_caches_lookups() {
        let tmp = tempfile::tempdir().unwrap();
        setup_workspace(tmp.path());

        let project_root = tmp.path().join("packages/app");
        let mut cache = WorkspacePackageCache::new(&project_root);

        let file1 = tmp.path().join("packages/lib/src/index.ts");
        let file2 = tmp.path().join("packages/lib/src/utils.ts");

        // First lookup populates cache
        assert_eq!(cache.lookup(&file1), Some("@my/lib".to_string()));
        // Second lookup for same package should hit cache
        assert_eq!(cache.lookup(&file2), Some("@my/lib".to_string()));
        // Verify cache was populated
        assert!(cache.cache.contains_key(&tmp.path().join("packages/lib")));
    }

    #[test]
    fn workspace_cache_no_package_json() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("some/random/dir");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("file.ts"), "").unwrap();

        let mut cache = WorkspacePackageCache::new(tmp.path());
        assert_eq!(cache.lookup(&dir.join("file.ts")), None);
    }

    #[test]
    fn parse_failure_not_retried() {
        let tmp = tempfile::tempdir().unwrap();
        // Canonicalize to avoid macOS /var vs /private/var symlink differences
        let root = tmp.path().canonicalize().unwrap();

        // Create a valid file that imports a broken file
        fs::write(
            root.join("entry.ts"),
            r#"import { x } from "./broken";"#,
        ).unwrap();

        // Create a binary/unparseable file with .ts extension
        fs::write(root.join("broken.ts"), &[0xFF, 0xFE, 0x00, 0x01]).unwrap();

        let graph = super::build_graph(&root);

        // The broken file should appear at most once in the graph
        let entry_count = graph.path_to_id.keys()
            .filter(|p| p.file_name().is_some_and(|n| n == "broken.ts"))
            .count();
        assert!(entry_count <= 1, "broken.ts should appear at most once, found {entry_count}");
    }
}

/// Compute aggregated package info (total reachable size + file count).
/// For each package, BFS from its entry module following only edges within the same package.
fn compute_package_info(graph: &mut ModuleGraph) {
    // Group modules by package
    let mut package_entries: HashMap<String, Vec<ModuleId>> = HashMap::new();
    for module in &graph.modules {
        if let Some(ref pkg) = module.package {
            package_entries.entry(pkg.clone()).or_default().push(module.id);
        }
    }

    for (pkg_name, module_ids) in &package_entries {
        let mut total_size: u64 = 0;
        let mut total_files: u32 = 0;
        let mut visited: HashSet<u32> = HashSet::new();

        // BFS from all entry points into this package
        let mut queue: VecDeque<ModuleId> = module_ids.iter().copied().collect();
        for &id in module_ids {
            visited.insert(id.0);
        }

        while let Some(mid) = queue.pop_front() {
            let module = &graph.modules[mid.0 as usize];
            // Only count modules in the same package
            if module.package.as_deref() == Some(pkg_name) {
                total_size += module.size_bytes;
                total_files += 1;
            }

            for &edge_id in &graph.forward_adj[mid.0 as usize] {
                let edge = &graph.edges[edge_id.0 as usize];
                if edge.kind == EdgeKind::Static {
                    let target = &graph.modules[edge.to.0 as usize];
                    if target.package.as_deref() == Some(pkg_name)
                        && visited.insert(edge.to.0)
                    {
                        queue.push_back(edge.to);
                    }
                }
            }
        }

        let entry_module = module_ids[0];
        graph.package_map.insert(
            pkg_name.clone(),
            PackageInfo {
                name: pkg_name.clone(),
                entry_module,
                total_reachable_size: total_size,
                total_reachable_files: total_files,
            },
        );
    }
}
