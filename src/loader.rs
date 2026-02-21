//! Entry point for building or loading a cached dependency graph.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::cache::{self, CacheWriteHandle, ParseCache};
use crate::error::Error;
use crate::graph::ModuleGraph;
use crate::lang::{self, LanguageSupport};
use crate::walker;

/// Result of loading or building a dependency graph.
#[derive(Debug)]
pub struct LoadedGraph {
    pub graph: ModuleGraph,
    /// Project root directory.
    pub root: PathBuf,
    /// Canonicalized entry point path.
    pub entry: PathBuf,
    /// File extensions recognized by the detected language.
    pub valid_extensions: &'static [&'static str],
    /// Whether the graph was loaded from cache (true) or built fresh (false).
    pub from_cache: bool,
    /// Total count of unresolvable dynamic imports.
    pub unresolvable_dynamic_count: usize,
    /// Files containing unresolvable dynamic imports, with per-file counts.
    pub unresolvable_dynamic_files: Vec<(PathBuf, usize)>,
    /// Warnings from files that could not be opened, read, or parsed.
    pub file_warnings: Vec<String>,
}

/// Load a dependency graph from the given entry point.
///
/// Validates the entry path, detects the project kind, and either loads
/// a cached graph or builds one from scratch using BFS discovery.
///
/// The returned [`CacheWriteHandle`] must be kept alive until you are done
/// with the graph — it joins a background cache-write thread on drop.
#[must_use = "the CacheWriteHandle joins a background thread on drop"]
pub fn load_graph(entry: &Path, no_cache: bool) -> Result<(LoadedGraph, CacheWriteHandle), Error> {
    let entry = entry
        .canonicalize()
        .map_err(|e| Error::EntryNotFound(entry.to_path_buf(), e))?;

    if entry.is_dir() {
        return Err(Error::EntryIsDirectory(entry));
    }

    let (root, kind) = lang::detect_project(&entry).ok_or_else(|| {
        let ext = entry
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("(none)");
        Error::UnsupportedFileType(ext.to_string())
    })?;

    let lang_support: Box<dyn LanguageSupport> = match kind {
        lang::ProjectKind::TypeScript => Box::new(lang::typescript::TypeScriptSupport::new(&root)),
        lang::ProjectKind::Python => Box::new(lang::python::PythonSupport::new(&root)),
    };

    let valid_extensions = lang_support.extensions();
    let (result, handle) = build_or_load(&entry, &root, no_cache, lang_support.as_ref());

    Ok((
        LoadedGraph {
            graph: result.graph,
            root,
            entry,
            valid_extensions,
            from_cache: result.from_cache,
            unresolvable_dynamic_count: result.unresolvable_dynamic_count,
            unresolvable_dynamic_files: result.unresolvable_dynamic_files,
            file_warnings: result.file_warnings,
        },
        handle,
    ))
}

// ---------------------------------------------------------------------------
// Internal helpers (moved from main.rs)
// ---------------------------------------------------------------------------

struct BuildResult {
    graph: ModuleGraph,
    unresolvable_dynamic_count: usize,
    unresolvable_dynamic_files: Vec<(PathBuf, usize)>,
    file_warnings: Vec<String>,
    from_cache: bool,
}

fn build_or_load(
    entry: &Path,
    root: &Path,
    no_cache: bool,
    lang: &dyn LanguageSupport,
) -> (BuildResult, CacheWriteHandle) {
    let mut cache = if no_cache {
        ParseCache::new()
    } else {
        ParseCache::load(root)
    };

    // Tier 1: try whole-graph cache
    if !no_cache {
        let resolve_fn = |spec: &str| lang.resolve(root, spec).is_some();
        match cache.try_load_graph(entry, &resolve_fn) {
            cache::GraphCacheResult::Hit {
                graph,
                unresolvable_dynamic,
                unresolved_specifiers,
                needs_resave,
            } => {
                let handle = if needs_resave {
                    cache.save(
                        root,
                        entry,
                        &graph,
                        unresolved_specifiers,
                        unresolvable_dynamic,
                    )
                } else {
                    CacheWriteHandle::none()
                };
                return (
                    BuildResult {
                        graph,
                        unresolvable_dynamic_count: unresolvable_dynamic,
                        unresolvable_dynamic_files: Vec::new(),
                        file_warnings: Vec::new(),
                        from_cache: true,
                    },
                    handle,
                );
            }
            cache::GraphCacheResult::Stale {
                mut graph,
                unresolvable_dynamic,
                changed_files,
            } => {
                // Tier 1.5: incremental update — re-parse only changed files,
                // reuse the cached graph if imports haven't changed.
                if let Some(result) = try_incremental_update(
                    &mut cache,
                    &mut graph,
                    &changed_files,
                    unresolvable_dynamic,
                    lang,
                ) {
                    graph.compute_package_info();
                    let handle = cache.save_incremental(
                        root,
                        entry,
                        &graph,
                        &changed_files,
                        result.unresolvable_dynamic,
                    );
                    return (
                        BuildResult {
                            graph,
                            unresolvable_dynamic_count: result.unresolvable_dynamic,
                            unresolvable_dynamic_files: Vec::new(),
                            file_warnings: Vec::new(),
                            from_cache: true,
                        },
                        handle,
                    );
                }
                // Imports changed — fall through to full BFS
            }
            cache::GraphCacheResult::Miss => {}
        }
    }

    // Tier 2: BFS walk with per-file parse cache
    let result = walker::build_graph(entry, root, lang, &mut cache);
    let unresolvable_count: usize = result.unresolvable_dynamic.iter().map(|(_, c)| c).sum();
    let handle = cache.save(
        root,
        entry,
        &result.graph,
        result.unresolved_specifiers,
        unresolvable_count,
    );
    (
        BuildResult {
            graph: result.graph,
            unresolvable_dynamic_count: unresolvable_count,
            unresolvable_dynamic_files: result.unresolvable_dynamic,
            file_warnings: result.file_warnings,
            from_cache: false,
        },
        handle,
    )
}

struct IncrementalResult {
    unresolvable_dynamic: usize,
}

/// Try to incrementally update the cached graph when only a few files changed.
/// Re-parses the changed files and checks if their imports match the old parse.
/// Returns None if imports changed (caller should fall back to full BFS).
#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
fn try_incremental_update(
    cache: &mut ParseCache,
    graph: &mut ModuleGraph,
    changed_files: &[PathBuf],
    old_unresolvable_total: usize,
    lang: &dyn LanguageSupport,
) -> Option<IncrementalResult> {
    let mut unresolvable_delta: isize = 0;

    for path in changed_files {
        // Get old imports without mtime check
        let old_result = cache.lookup_unchecked(path)?;
        let old_import_count = old_result.imports.len();
        let old_unresolvable = old_result.unresolvable_dynamic;
        let old_imports: Vec<_> = old_result
            .imports
            .iter()
            .map(|i| (i.specifier.as_str(), i.kind))
            .collect();

        // Re-parse the changed file
        let source = std::fs::read_to_string(path).ok()?;
        let new_result = lang.parse(path, &source).ok()?;

        // Compare import lists — if anything changed, bail out
        if new_result.imports.len() != old_import_count
            || new_result.imports.iter().zip(old_imports.iter()).any(
                |(new, &(old_spec, old_kind))| new.specifier != old_spec || new.kind != old_kind,
            )
        {
            return None;
        }

        // Track unresolvable dynamic count changes
        unresolvable_delta += new_result.unresolvable_dynamic as isize - old_unresolvable as isize;

        // Update file size in graph
        let mid = *graph.path_to_id.get(path)?;
        let new_size = source.len() as u64;
        graph.modules[mid.0 as usize].size_bytes = new_size;

        // Update parse cache entry
        #[allow(clippy::or_fun_call)]
        let dir = path.parent().unwrap_or(Path::new("."));
        let resolved_paths: Vec<Option<PathBuf>> = new_result
            .imports
            .iter()
            .map(|imp| lang.resolve(dir, &imp.specifier))
            .collect();
        if let Ok(meta) = fs::metadata(path) {
            let mtime = meta
                .modified()
                .ok()
                .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
                .map(|d: std::time::Duration| d.as_nanos());
            if let Some(mtime) = mtime {
                cache.insert(path.clone(), new_size, mtime, new_result, resolved_paths);
            }
        }
    }

    let new_total = (old_unresolvable_total as isize + unresolvable_delta).max(0) as usize;
    Some(IncrementalResult {
        unresolvable_dynamic: new_total,
    })
}
