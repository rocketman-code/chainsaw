//! Concurrent dependency graph construction.
//!
//! Starting from an entry file, discovers all reachable modules by parsing
//! imports and resolving them against the filesystem in parallel using a
//! lock-free work queue and rayon thread pool.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use crossbeam_queue::SegQueue;
use dashmap::DashSet;
use rayon::slice::ParallelSliceMut;

use crate::cache::ParseCache;
use crate::graph::ModuleGraph;
use crate::lang::{LanguageSupport, RawImport};
use crate::vfs::Vfs;

fn is_parseable(path: &Path, extensions: &[&str]) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| extensions.contains(&ext))
}

/// Result of discovering a single file during concurrent traversal.
struct FileResult {
    path: PathBuf,
    size: u64,
    /// File modification time captured during read (avoids re-stat in cache insert).
    mtime_nanos: Option<u128>,
    package: Option<String>,
    imports: Vec<(RawImport, Option<PathBuf>)>,
    unresolvable_dynamic: usize,
}

struct DiscoverResult {
    files: Vec<FileResult>,
    warnings: Vec<String>,
}

/// Phase 1: Concurrent file discovery using a lock-free work queue.
/// Returns all discovered files with their parsed imports and resolved paths.
#[allow(clippy::too_many_lines)]
fn concurrent_discover(
    entry: &Path,
    root: &Path,
    lang: &dyn LanguageSupport,
    vfs: &dyn Vfs,
) -> DiscoverResult {
    let queue: SegQueue<PathBuf> = SegQueue::new();
    let seen: DashSet<PathBuf> = DashSet::new();
    let results: Mutex<Vec<FileResult>> = Mutex::new(Vec::new());
    let warnings: SegQueue<String> = SegQueue::new();
    let active = AtomicUsize::new(1); // entry file is active
    let extensions = lang.extensions();

    queue.push(entry.to_path_buf());
    seen.insert(entry.to_path_buf());

    rayon::scope(|s| {
        for _ in 0..rayon::current_num_threads() {
            s.spawn(|_| {
                let mut spin_count: u32 = 0;
                loop {
                    if let Some(path) = queue.pop() {
                        spin_count = 0;
                        let (source, meta) = match vfs.read_with_metadata(&path) {
                            Ok(r) => r,
                            Err(e) => {
                                warnings.push(format!("{}: {e}", path.display()));
                                active.fetch_sub(1, Ordering::AcqRel);
                                continue;
                            }
                        };
                        let mtime_nanos = meta.mtime_nanos;
                        let size = meta.len;

                        let result = match lang.parse(&path, &source) {
                            Ok(r) => r,
                            Err(e) => {
                                warnings.push(e.to_string());
                                active.fetch_sub(1, Ordering::AcqRel);
                                continue;
                            }
                        };
                        let package = if path == entry {
                            None
                        } else {
                            lang.package_name(&path)
                                .or_else(|| lang.workspace_package_name(&path, root))
                        };

                        // Resolve imports and discover new files
                        #[allow(clippy::or_fun_call)]
                        let dir = path.parent().unwrap_or(Path::new("."));
                        let imports: Vec<(RawImport, Option<PathBuf>)> = result
                            .imports
                            .into_iter()
                            .map(|imp| {
                                let resolved = lang.resolve(dir, &imp.specifier);
                                if let Some(ref p) = resolved
                                    && is_parseable(p, extensions)
                                    && seen.insert(p.clone())
                                {
                                    active.fetch_add(1, Ordering::AcqRel);
                                    queue.push(p.clone());
                                }
                                (imp, resolved)
                            })
                            .collect();

                        let file_result = FileResult {
                            path,
                            size,
                            mtime_nanos,
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
                    } else if spin_count < 64 {
                        spin_count += 1;
                        std::hint::spin_loop();
                    } else {
                        spin_count = 0;
                        std::thread::yield_now();
                    }
                }
            });
        }
    });

    let mut files = results.into_inner().unwrap();
    files.par_sort_unstable_by(|a, b| a.path.cmp(&b.path));
    let warnings = std::iter::from_fn(|| warnings.pop()).collect();
    DiscoverResult { files, warnings }
}

/// Result of building a module graph.
#[derive(Debug)]
#[non_exhaustive]
pub struct BuildResult {
    pub graph: ModuleGraph,
    /// Files containing dynamic imports with non-literal arguments, with counts.
    pub unresolvable_dynamic: Vec<(PathBuf, usize)>,
    /// Import specifiers that failed to resolve (for cache invalidation).
    pub unresolved_specifiers: Vec<String>,
    /// Warnings from files that could not be opened, read, or parsed.
    pub file_warnings: Vec<String>,
}

/// Build a complete `ModuleGraph` from the given entry point.
/// Phase 1 concurrently discovers files using a lock-free work queue.
/// Phase 2 serially constructs the graph from sorted discovery results.
pub fn build_graph(
    entry: &Path,
    root: &Path,
    lang: &dyn LanguageSupport,
    cache: &mut ParseCache,
    vfs: &dyn Vfs,
) -> BuildResult {
    // Phase 1: Concurrent discovery (lock-free work queue)
    let discovered = concurrent_discover(entry, root, lang, vfs);
    let file_results = discovered.files;

    // Phase 2: Serial graph construction from sorted results
    let mut graph = ModuleGraph::new();
    let mut unresolvable_files: Vec<(PathBuf, usize)> = Vec::new();
    let mut unresolved: HashSet<String> = HashSet::new();

    // First pass: add all modules (deterministic order from sorted results)
    for fr in &file_results {
        graph.add_module(fr.path.clone(), fr.size, fr.package.clone());
    }

    // Second pass: add edges, collect diagnostics, and populate parse cache.
    // Consumes file_results by value to avoid redundant clones.
    for fr in file_results {
        let source_id = graph.path_to_id[&fr.path];

        if fr.unresolvable_dynamic > 0 {
            unresolvable_files.push((fr.path.clone(), fr.unresolvable_dynamic));
        }

        // Separate imports into raw imports and resolved paths.
        // Edge processing borrows from these; cache takes ownership.
        let (raw_imports, resolved_paths): (Vec<RawImport>, Vec<Option<PathBuf>>) =
            fr.imports.into_iter().unzip();

        for (raw_import, resolved_path) in raw_imports.iter().zip(resolved_paths.iter()) {
            match resolved_path {
                Some(p) => {
                    if let Some(&target_id) = graph.path_to_id.get(p) {
                        graph.add_edge(
                            source_id,
                            target_id,
                            raw_import.kind,
                            &raw_import.specifier,
                        );
                    }
                    // Target not in graph = unparseable leaf (e.g. .json, .css)
                    // Add it as a leaf module
                    else {
                        let size = vfs.metadata(p).map(|m| m.len).unwrap_or(0);
                        let package = lang
                            .package_name(p)
                            .or_else(|| lang.workspace_package_name(p, root));
                        let target_id = graph.add_module(p.clone(), size, package);
                        graph.add_edge(
                            source_id,
                            target_id,
                            raw_import.kind,
                            &raw_import.specifier,
                        );
                    }
                }
                None => {
                    unresolved.insert(raw_import.specifier.clone());
                }
            }
        }

        let result = crate::lang::ParseResult {
            imports: raw_imports,
            unresolvable_dynamic: fr.unresolvable_dynamic,
        };
        if let Some(mtime) = fr.mtime_nanos {
            cache.insert(fr.path, fr.size, mtime, result, resolved_paths);
        }
    }

    graph.compute_package_info();
    BuildResult {
        graph,
        unresolvable_dynamic: unresolvable_files,
        unresolved_specifiers: unresolved.into_iter().collect(),
        file_warnings: discovered.warnings,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::typescript::TypeScriptSupport;
    use crate::vfs::OsVfs;
    use std::fs;

    #[test]
    fn parse_failure_not_retried() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        fs::write(root.join("entry.ts"), r#"import { x } from "./broken";"#).unwrap();

        fs::write(root.join("broken.ts"), [0xFF, 0xFE, 0x00, 0x01]).unwrap();

        let lang = TypeScriptSupport::new(&root);
        let mut cache = ParseCache::new();
        let result = build_graph(&root.join("entry.ts"), &root, &lang, &mut cache, &OsVfs);
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
