use std::collections::VecDeque;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ignore::WalkBuilder;
use rayon::prelude::*;

use crate::graph::{EdgeKind, ModuleGraph};
use crate::parser::{self, RawImport};
use crate::resolver::{self, package_name_from_path};

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

/// Build a complete ModuleGraph starting from the given root directory.
/// Walks source files, parses them, resolves imports (following into node_modules),
/// and builds the graph iteratively.
pub fn build_graph(root: &Path) -> ModuleGraph {
    let graph = Mutex::new(ModuleGraph::new());
    let resolver = resolver::create_resolver();

    // Phase 1: Discover and parse source files
    let source_files = discover_source_files(root);
    let parsed = parse_files_parallel(&source_files);

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
            let resolved = match resolver::resolve_import(&resolver, source_dir, &raw_import.specifier) {
                Some(p) => p,
                None => continue,
            };

            let package = package_name_from_path(&resolved);
            let size = fs::metadata(&resolved).map(|m| m.len()).unwrap_or(0);

            let mut g = graph.lock().unwrap();
            let is_new = !g.path_to_id.contains_key(&resolved);
            let target_id = g.add_module(resolved.clone(), size, package);
            g.add_edge(source_id, target_id, raw_import.kind, raw_import.specifier.clone());

            // If this is a new file and it's parseable, queue it for parsing
            if is_new && is_parseable(&resolved) {
                new_files_to_parse.push(resolved);
            }
        }

        // Parse newly discovered files (e.g. node_modules entries) in parallel
        if !new_files_to_parse.is_empty() {
            let newly_parsed = parse_files_parallel(&new_files_to_parse);
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

/// Compute aggregated package info (total reachable size + file count).
/// For each package, BFS from its entry module following only edges within the same package.
fn compute_package_info(graph: &mut ModuleGraph) {
    use std::collections::{HashMap, HashSet};
    use crate::graph::PackageInfo;

    // Group modules by package
    let mut package_entries: HashMap<String, Vec<crate::graph::ModuleId>> = HashMap::new();
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
        let mut queue: VecDeque<crate::graph::ModuleId> = module_ids.iter().copied().collect();
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
