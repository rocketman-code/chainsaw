use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use crossbeam_queue::SegQueue;
use dashmap::DashSet;
use rayon::prelude::*;

use crate::cache::ParseCache;
use crate::graph::{ModuleGraph, ModuleId};
use crate::lang::{LanguageSupport, RawImport};

fn is_parseable(path: &Path, extensions: &[&str]) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| extensions.contains(&ext))
}

/// Result of discovering a single file during concurrent traversal.
struct FileResult {
    path: PathBuf,
    size: u64,
    package: Option<String>,
    imports: Vec<(RawImport, Option<PathBuf>)>,
    unresolvable_dynamic: usize,
}

/// Phase 1: Concurrent file discovery using a lock-free work queue.
/// Returns all discovered files with their parsed imports and resolved paths.
fn concurrent_discover(
    entry: &Path,
    root: &Path,
    lang: &dyn LanguageSupport,
) -> Vec<FileResult> {
    let queue: SegQueue<PathBuf> = SegQueue::new();
    let seen: DashSet<PathBuf> = DashSet::new();
    let results: Mutex<Vec<FileResult>> = Mutex::new(Vec::new());
    let active = AtomicUsize::new(1); // entry file is active
    let extensions = lang.extensions();

    queue.push(entry.to_path_buf());
    seen.insert(entry.to_path_buf());

    rayon::scope(|s| {
        for _ in 0..rayon::current_num_threads() {
            s.spawn(|_| {
                loop {
                    if let Some(path) = queue.pop() {
                        // Parse
                        let result = match lang.parse(&path) {
                            Ok(r) => r,
                            Err(e) => {
                                eprintln!("warning: {e}");
                                active.fetch_sub(1, Ordering::AcqRel);
                                continue;
                            }
                        };

                        let size = fs::metadata(&path).map(|m| m.len()).unwrap_or(0);
                        let package = lang
                            .package_name(&path)
                            .or_else(|| lang.workspace_package_name(&path, root));

                        // Resolve imports and discover new files
                        let dir = path.parent().unwrap_or(Path::new("."));
                        let imports: Vec<(RawImport, Option<PathBuf>)> = result
                            .imports
                            .into_iter()
                            .map(|imp| {
                                let resolved = lang.resolve(dir, &imp.specifier);
                                if let Some(ref p) = resolved {
                                    if is_parseable(p, extensions) && seen.insert(p.clone()) {
                                        active.fetch_add(1, Ordering::AcqRel);
                                        queue.push(p.clone());
                                    }
                                }
                                (imp, resolved)
                            })
                            .collect();

                        let file_result = FileResult {
                            path,
                            size,
                            package,
                            imports,
                            unresolvable_dynamic: result.unresolvable_dynamic,
                        };
                        results.lock().unwrap().push(file_result);

                        if active.fetch_sub(1, Ordering::AcqRel) == 1 {
                            // This was the last active item; all work is done
                            return;
                        }
                    } else if active.load(Ordering::Acquire) == 0 {
                        return;
                    } else {
                        std::hint::spin_loop();
                    }
                }
            });
        }
    });

    let mut results = results.into_inner().unwrap();
    results.sort_by(|a, b| a.path.cmp(&b.path));
    results
}

/// An import with its pre-resolved path (None if resolution failed).
type ResolvedImport = (RawImport, Option<PathBuf>);

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
    let mut parse_failures: HashSet<u32> = HashSet::new();
    let mut pending: VecDeque<(ModuleId, usize, Vec<ResolvedImport>)> = VecDeque::new();

    // Parse entry file and resolve its imports
    let entry_size = fs::metadata(entry).map(|m| m.len()).unwrap_or(0);
    let entry_id = graph.add_module(entry.to_path_buf(), entry_size, None);

    match cache.lookup(entry) {
        Some((result, resolved_paths)) => {
            if result.unresolvable_dynamic > 0 {
                unresolvable_files.push((entry.to_path_buf(), result.unresolvable_dynamic));
            }
            let resolved_imports: Vec<ResolvedImport> = result
                .imports
                .into_iter()
                .zip(resolved_paths)
                .collect();
            pending.push_back((entry_id, 0, resolved_imports));
        }
        None => match lang.parse(entry) {
            Ok(result) => {
                if result.unresolvable_dynamic > 0 {
                    unresolvable_files.push((entry.to_path_buf(), result.unresolvable_dynamic));
                }
                let entry_dir = entry.parent().unwrap_or(Path::new("."));
                let resolved_imports: Vec<ResolvedImport> = result
                    .imports
                    .par_iter()
                    .map(|imp| {
                        let r = lang.resolve(entry_dir, &imp.specifier);
                        (imp.clone(), r)
                    })
                    .collect();
                let resolved_paths: Vec<Option<PathBuf>> =
                    resolved_imports.iter().map(|(_, p)| p.clone()).collect();
                cache.insert(entry.to_path_buf(), &result, &resolved_paths);
                pending.push_back((entry_id, 0, resolved_imports));
            }
            Err(e) => {
                eprintln!("warning: {e}");
                parse_failures.insert(entry_id.0);
            }
        },
    }

    // BFS: each iteration processes pre-resolved imports, then fuses
    // parsing and resolution for newly discovered files in one parallel pass.
    while !pending.is_empty() {
        let frontier: Vec<_> = pending.drain(..).collect();

        // Phase 1: Serial graph mutations from pre-resolved imports
        let mut new_files: Vec<ModuleId> = Vec::new();
        for (source_id, unresolvable_dynamic, resolved_imports) in &frontier {
            let source_id = *source_id;
            if *unresolvable_dynamic > 0 {
                unresolvable_files
                    .push((graph.module(source_id).path.clone(), *unresolvable_dynamic));
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
                        &raw_import.specifier,
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
                    &raw_import.specifier,
                );

                if is_parseable(resolved, lang.extensions())
                    && !parse_failures.contains(&target_id.0)
                {
                    new_files.push(target_id);
                }
            }
        }

        if new_files.is_empty() {
            continue;
        }

        // Phase 2: Check parse cache (serial — cache is &mut)
        // Cache hits include resolved paths, so they skip Phase 3 entirely.
        let mut to_parse: Vec<ModuleId> = Vec::new();
        for mid in new_files {
            let path = &graph.module(mid).path;
            if let Some((result, resolved_paths)) = cache.lookup(path) {
                let resolved: Vec<ResolvedImport> = result
                    .imports
                    .into_iter()
                    .zip(resolved_paths)
                    .collect();
                pending.push_back((mid, result.unresolvable_dynamic, resolved));
            } else {
                to_parse.push(mid);
            }
        }

        if to_parse.is_empty() {
            continue;
        }

        // Phase 3: Parse + resolve only for cache misses
        let results: Vec<_> = to_parse
            .par_iter()
            .filter_map(|&mid| {
                let path = &graph.module(mid).path;
                let result = match lang.parse(path) {
                    Ok(r) => r,
                    Err(e) => {
                        eprintln!("warning: {e}");
                        return None;
                    }
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
                Some((mid, result, unresolvable, resolved))
            })
            .collect();

        // Track parse failures and update cache with resolutions (serial)
        let parsed_ids: HashSet<u32> = results.iter().map(|(mid, ..)| mid.0).collect();
        for &mid in &to_parse {
            if !parsed_ids.contains(&mid.0) {
                parse_failures.insert(mid.0);
            }
        }
        for &(mid, ref result, _, ref resolved) in &results {
            let path = &graph.module(mid).path;
            let resolved_paths: Vec<Option<PathBuf>> =
                resolved.iter().map(|(_, p)| p.clone()).collect();
            cache.insert(path.to_path_buf(), result, &resolved_paths);
        }

        pending.extend(
            results
                .into_iter()
                .map(|(mid, _, unresolvable, resolved)| (mid, unresolvable, resolved)),
        );
    }

    graph.compute_package_info();
    BuildResult {
        graph,
        unresolvable_dynamic: unresolvable_files,
        unresolved_specifiers: unresolved.into_iter().collect(),
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
