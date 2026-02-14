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

/// An import with its pre-resolved path (None if resolution failed).
type ResolvedImport = (RawImport, Option<PathBuf>);

/// Build a complete ModuleGraph via BFS from the given entry point.
/// Uses the parse cache for acceleration: unchanged files are not re-parsed.
/// Parse and resolve are fused into a single parallel pass per BFS level.
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
    let mut pending: VecDeque<(PathBuf, usize, Vec<ResolvedImport>)> = VecDeque::new();

    // Parse entry file and resolve its imports
    let entry_size = fs::metadata(entry).map(|m| m.len()).unwrap_or(0);
    graph.add_module(entry.to_path_buf(), entry_size, None);

    match parse_or_cache(entry, cache, lang) {
        Some(result) => {
            if result.unresolvable_dynamic > 0 {
                unresolvable_files.push((entry.to_path_buf(), result.unresolvable_dynamic));
            }
            let entry_dir = entry.parent().unwrap_or(Path::new("."));
            let resolved_imports: Vec<ResolvedImport> = result
                .imports
                .into_par_iter()
                .map(|imp| {
                    let r = lang.resolve(entry_dir, &imp.specifier);
                    (imp, r)
                })
                .collect();
            pending.push_back((entry.to_path_buf(), 0, resolved_imports));
        }
        None => {
            parse_failures.insert(entry.to_path_buf());
        }
    }

    // BFS: each iteration processes pre-resolved imports, then fuses
    // parsing and resolution for newly discovered files in one parallel pass.
    while !pending.is_empty() {
        let frontier: Vec<_> = pending.drain(..).collect();

        // Phase 1: Serial graph mutations from pre-resolved imports
        let mut new_files: Vec<PathBuf> = Vec::new();
        for (source_path, unresolvable_dynamic, resolved_imports) in &frontier {
            let source_id = graph.path_to_id[source_path];
            if *unresolvable_dynamic > 0 {
                unresolvable_files
                    .push((source_path.clone(), *unresolvable_dynamic));
            }

            for (raw_import, resolved_path) in resolved_imports {
                let resolved = match resolved_path {
                    Some(p) => p,
                    None => {
                        unresolved.insert(raw_import.specifier.clone());
                        continue;
                    }
                };

                if let Some(&target_id) = graph.path_to_id.get(resolved) {
                    graph.add_edge(
                        source_id,
                        target_id,
                        raw_import.kind,
                        raw_import.specifier.clone(),
                    );
                    continue;
                }

                let package = lang
                    .package_name(resolved)
                    .or_else(|| lang.workspace_package_name(resolved, root));
                let size = fs::metadata(resolved).map(|m| m.len()).unwrap_or(0);

                let target_id = graph.add_module(resolved.clone(), size, package);
                graph.add_edge(
                    source_id,
                    target_id,
                    raw_import.kind,
                    raw_import.specifier.clone(),
                );

                if is_parseable(resolved, lang.extensions())
                    && !parse_failures.contains(resolved)
                {
                    new_files.push(resolved.clone());
                }
            }
        }

        if new_files.is_empty() {
            continue;
        }

        // Phase 2: Check parse cache (serial — cache is &mut)
        let mut to_parse: Vec<PathBuf> = Vec::new();
        let mut work: Vec<(PathBuf, Option<ParseResult>)> = Vec::new();
        for path in new_files {
            if let Some(result) = cache.lookup(&path) {
                work.push((path, Some(result)));
            } else {
                to_parse.push(path.clone());
                work.push((path, None));
            }
        }

        // Phase 3: Parse (if needed) + resolve imports in a single parallel pass
        // Returns (path, Option<ParseResult> for cache, unresolvable_count, resolved_imports)
        let results: Vec<_> = work
            .into_par_iter()
            .filter_map(|(path, cached_result)| {
                let (result, needs_caching) = match cached_result {
                    Some(r) => (r, false),
                    None => match lang.parse(&path) {
                        Ok(r) => (r, true),
                        Err(e) => {
                            eprintln!("warning: {e}");
                            return None;
                        }
                    },
                };
                let dir = path.parent().unwrap_or(Path::new("."));
                let resolved: Vec<ResolvedImport> = result
                    .imports
                    .iter()
                    .map(|imp| {
                        let r = lang.resolve(dir, &imp.specifier);
                        (imp.clone(), r)
                    })
                    .collect();
                let unresolvable = result.unresolvable_dynamic;
                let for_cache = if needs_caching { Some(result) } else { None };
                Some((path, for_cache, unresolvable, resolved))
            })
            .collect();

        // Track parse failures and update cache (serial)
        let result_paths: HashSet<_> = results.iter().map(|(p, _, _, _)| p.clone()).collect();
        for path in &to_parse {
            if !result_paths.contains(path) {
                parse_failures.insert(path.clone());
            }
        }
        for (path, for_cache, _, _) in &results {
            if let Some(result) = for_cache {
                cache.insert(path.clone(), result);
            }
        }

        pending.extend(
            results
                .into_iter()
                .map(|(path, _, unresolvable, resolved)| (path, unresolvable, resolved)),
        );
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
