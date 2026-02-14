use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

use rayon::prelude::*;

use crate::cache::ParseCache;
use crate::graph::{EdgeKind, ModuleGraph, ModuleId, PackageInfo};
use crate::lang::{LanguageSupport, ParseResult, RawImport};

fn is_parseable(path: &Path, extensions: &[&str]) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| extensions.contains(&ext))
}

/// Result of building a module graph.
pub struct BuildResult {
    pub graph: ModuleGraph,
    /// Files containing dynamic imports with non-literal arguments, with counts.
    pub unresolvable_dynamic: Vec<(PathBuf, usize)>,
    /// Import specifiers that failed to resolve (for cache invalidation).
    pub unresolved_specifiers: Vec<String>,
}

/// Build a complete ModuleGraph via BFS from the given entry point.
/// Uses the parse cache for acceleration: unchanged files are not re-parsed.
pub fn build_graph(
    entry: &Path,
    root: &Path,
    lang: &dyn LanguageSupport,
    cache: &mut ParseCache,
) -> BuildResult {
    let mut graph = ModuleGraph::new();
    let mut unresolvable_files: Vec<(PathBuf, usize)> = Vec::new();
    let mut unresolved: HashSet<String> = HashSet::new();
    let mut parse_failures: HashSet<PathBuf> = HashSet::new();
    let mut pending: VecDeque<(PathBuf, Vec<RawImport>)> = VecDeque::new();

    // Parse entry file
    let entry_size = fs::metadata(entry).map(|m| m.len()).unwrap_or(0);
    graph.add_module(entry.to_path_buf(), entry_size, None);

    match parse_or_cache(entry, cache, lang) {
        Some(result) => {
            if result.unresolvable_dynamic > 0 {
                unresolvable_files.push((entry.to_path_buf(), result.unresolvable_dynamic));
            }
            pending.push_back((entry.to_path_buf(), result.imports));
        }
        None => {
            parse_failures.insert(entry.to_path_buf());
        }
    }

    // BFS: resolve imports in parallel, discover and parse new files
    while !pending.is_empty() {
        // Drain entire BFS frontier
        let frontier: Vec<_> = pending.drain(..).collect();

        // Collect all (source_dir, source_id, import) tuples across the frontier
        let all_imports: Vec<_> = frontier
            .iter()
            .flat_map(|(path, imports)| {
                let dir = path.parent().unwrap_or(Path::new("."));
                let source_id = graph.path_to_id[path];
                imports
                    .iter()
                    .map(move |imp| (dir, source_id, imp))
            })
            .collect();

        // Resolve all imports in parallel
        let resolved: Vec<_> = all_imports
            .par_iter()
            .map(|(dir, source_id, imp)| {
                let path = lang.resolve(dir, &imp.specifier);
                (*source_id, *imp, path)
            })
            .collect();

        // Serial graph mutations with resolved results
        let mut new_files: Vec<PathBuf> = Vec::new();
        for (source_id, raw_import, resolved_path) in resolved {
            let resolved = match resolved_path {
                Some(p) => p,
                None => {
                    unresolved.insert(raw_import.specifier.clone());
                    continue;
                }
            };

            // Fast path: module already in graph, just add the edge
            if let Some(&target_id) = graph.path_to_id.get(&resolved) {
                graph.add_edge(
                    source_id,
                    target_id,
                    raw_import.kind,
                    raw_import.specifier.clone(),
                );
                continue;
            }

            let package = lang
                .package_name(&resolved)
                .or_else(|| lang.workspace_package_name(&resolved, root));
            let size = fs::metadata(&resolved).map(|m| m.len()).unwrap_or(0);

            let target_id = graph.add_module(resolved.clone(), size, package);
            graph.add_edge(
                source_id,
                target_id,
                raw_import.kind,
                raw_import.specifier.clone(),
            );

            if is_parseable(&resolved, lang.extensions())
                && !parse_failures.contains(&resolved)
            {
                new_files.push(resolved);
            }
        }

        if !new_files.is_empty() {
            let (results, failures) = parse_batch(new_files, cache, lang);
            parse_failures.extend(failures);
            for (path, result) in results {
                if result.unresolvable_dynamic > 0 {
                    unresolvable_files.push((path.clone(), result.unresolvable_dynamic));
                }
                pending.push_back((path, result.imports));
            }
        }
    }

    compute_package_info(&mut graph);
    BuildResult {
        graph,
        unresolvable_dynamic: unresolvable_files,
        unresolved_specifiers: unresolved.into_iter().collect(),
    }
}

fn parse_or_cache(
    path: &Path,
    cache: &mut ParseCache,
    lang: &dyn LanguageSupport,
) -> Option<ParseResult> {
    if let Some(result) = cache.lookup(path) {
        return Some(result);
    }
    match lang.parse(path) {
        Ok(result) => {
            cache.insert(path.to_path_buf(), &result);
            Some(result)
        }
        Err(e) => {
            eprintln!("warning: {e}");
            None
        }
    }
}

fn parse_batch(
    files: Vec<PathBuf>,
    cache: &mut ParseCache,
    lang: &dyn LanguageSupport,
) -> (Vec<(PathBuf, ParseResult)>, HashSet<PathBuf>) {
    let mut results = Vec::new();
    let mut to_parse = Vec::new();

    for path in files {
        if let Some(result) = cache.lookup(&path) {
            results.push((path, result));
        } else {
            to_parse.push(path);
        }
    }

    // Parse cache misses in parallel
    let parsed: Vec<_> = to_parse
        .par_iter()
        .filter_map(|path| match lang.parse(path) {
            Ok(result) => Some((path.clone(), result)),
            Err(e) => {
                eprintln!("warning: {e}");
                None
            }
        })
        .collect();

    let parsed_paths: HashSet<_> = parsed.iter().map(|(p, _)| p.clone()).collect();
    let mut failures: HashSet<PathBuf> = HashSet::new();
    for path in &to_parse {
        if !parsed_paths.contains(path) {
            failures.insert(path.clone());
        }
    }

    for (path, result) in parsed {
        cache.insert(path.clone(), &result);
        results.push((path, result));
    }

    (results, failures)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::typescript::TypeScriptSupport;
    use std::fs;

    #[test]
    fn parse_failure_not_retried() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        fs::write(
            root.join("entry.ts"),
            r#"import { x } from "./broken";"#,
        )
        .unwrap();

        fs::write(root.join("broken.ts"), &[0xFF, 0xFE, 0x00, 0x01]).unwrap();

        let lang = TypeScriptSupport::new(&root);
        let mut cache = ParseCache::new();
        let result = build_graph(&root.join("entry.ts"), &root, &lang, &mut cache);
        let graph = result.graph;

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
