use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ignore::WalkBuilder;
use rayon::prelude::*;

use crate::graph::{EdgeKind, ModuleGraph, ModuleId, PackageInfo};
use crate::lang::{LanguageSupport, RawImport};

fn is_parseable(path: &Path, extensions: &[&str]) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| extensions.contains(&ext))
}

/// Discover source files using the ignore crate (respects .gitignore).
/// Also collects the set of directories visited during the walk (for cache invalidation).
fn discover_source_files(root: &Path, lang: &dyn LanguageSupport) -> (Vec<PathBuf>, Vec<PathBuf>) {
    let extensions = lang.extensions();
    let skip: Vec<String> = lang.skip_dirs().iter().map(|s| s.to_string()).collect();
    let mut files = Vec::new();
    let mut dirs = Vec::new();
    let walker = WalkBuilder::new(root)
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
        if path.is_dir() {
            dirs.push(path);
        } else if path.is_file() && is_parseable(&path, extensions) {
            files.push(path);
        }
    }
    (files, dirs)
}

/// Parse a batch of files in parallel, returning (path, imports) pairs.
fn parse_files_parallel(
    files: &[PathBuf],
    lang: &dyn LanguageSupport,
) -> Vec<(PathBuf, Vec<RawImport>)> {
    files
        .par_iter()
        .filter_map(|path| match lang.parse(path) {
            Ok(imports) => Some((path.clone(), imports)),
            Err(e) => {
                eprintln!("warning: {e}");
                None
            }
        })
        .collect()
}

/// Result of building a module graph, including metadata for cache invalidation.
pub struct BuildResult {
    pub graph: ModuleGraph,
    /// Unique import specifiers that could not be resolved during graph building.
    /// These are tracked so the cache can re-check them on load — if any become
    /// resolvable (e.g. after `pip install`), the cache is invalidated.
    pub unresolved_specifiers: Vec<String>,
    /// Directories visited during source file discovery. Stored in the cache so
    /// that on load we can check directory mtimes to detect new/removed files
    /// without re-walking the entire tree.
    pub walked_dirs: Vec<PathBuf>,
}

/// Build a complete ModuleGraph starting from the given root directory.
/// Walks source files, parses them, resolves imports (following into node_modules),
/// and builds the graph iteratively.
pub fn build_graph(root: &Path, lang: &dyn LanguageSupport) -> BuildResult {
    let graph = Mutex::new(ModuleGraph::new());
    let mut unresolved: HashSet<String> = HashSet::new();

    // Phase 1: Discover and parse source files
    let (source_files, walked_dirs) = discover_source_files(root, lang);
    let parsed = parse_files_parallel(&source_files, lang);

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

    while let Some((source_path, imports)) = pending_resolutions.pop_front() {
        let source_dir = source_path.parent().unwrap_or(Path::new("."));
        let source_id = graph.lock().unwrap().path_to_id[&source_path];

        let mut new_files_to_parse: Vec<PathBuf> = Vec::new();

        for raw_import in &imports {
            let resolved = match lang.resolve(source_dir, &raw_import.specifier) {
                Some(p) => p,
                None => {
                    unresolved.insert(raw_import.specifier.clone());
                    continue;
                }
            };

            let package = lang
                .package_name(&resolved)
                .or_else(|| lang.workspace_package_name(&resolved, root));
            let size = fs::metadata(&resolved).map(|m| m.len()).unwrap_or(0);

            let mut g = graph.lock().unwrap();
            let is_new = !g.path_to_id.contains_key(&resolved);
            let target_id =
                g.add_module(resolved.clone(), size, package);
            g.add_edge(
                source_id,
                target_id,
                raw_import.kind,
                raw_import.specifier.clone(),
            );

            // If this is a new file and it's parseable (and hasn't already failed), queue it
            if is_new
                && is_parseable(&resolved, lang.extensions())
                && !parse_failures.contains(&resolved)
            {
                new_files_to_parse.push(resolved);
            }
        }

        // Parse newly discovered files (e.g. node_modules entries) in parallel
        if !new_files_to_parse.is_empty() {
            let newly_parsed = parse_files_parallel(&new_files_to_parse, lang);
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

    // Filter to specifiers that also fail to resolve from the project root.
    // During graph building, specifiers are resolved from each source file's directory.
    // During cache loading, they're re-resolved from root. Without this filter,
    // workspace-internal packages (resolvable from root but not from a subdirectory)
    // cause false cache invalidation on every run.
    let mut unresolved_specifiers: Vec<String> = unresolved
        .into_iter()
        .filter(|spec| lang.resolve(root, spec).is_none())
        .collect();
    unresolved_specifiers.sort();

    BuildResult {
        graph: g,
        unresolved_specifiers,
        walked_dirs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::typescript::TypeScriptSupport;
    use std::fs;

    #[test]
    fn parse_failure_not_retried() {
        let tmp = tempfile::tempdir().unwrap();
        // Canonicalize to avoid macOS /var vs /private/var symlink differences
        let root = tmp.path().canonicalize().unwrap();

        // Create a valid file that imports a broken file
        fs::write(
            root.join("entry.ts"),
            r#"import { x } from "./broken";"#,
        )
        .unwrap();

        // Create a binary/unparseable file with .ts extension
        fs::write(root.join("broken.ts"), &[0xFF, 0xFE, 0x00, 0x01]).unwrap();

        let lang = TypeScriptSupport::new(&root);
        let result = build_graph(&root, &lang);
        let graph = result.graph;

        // The broken file should appear at most once in the graph
        let entry_count = graph
            .path_to_id
            .keys()
            .filter(|p| p.file_name().is_some_and(|n| n == "broken.ts"))
            .count();
        assert!(
            entry_count <= 1,
            "broken.ts should appear at most once, found {entry_count}"
        );
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
