use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::SystemTime;

use crate::graph::ModuleGraph;
use crate::lang::ParseResult;

const CACHE_FILE: &str = ".chainsaw.cache";
const CACHE_VERSION: u32 = 7;
// 16-byte header: magic (4) + version (4) + graph_len (8)
const CACHE_MAGIC: u32 = 0x4348_5357; // "CHSW"
const HEADER_SIZE: usize = 16;

pub fn cache_path(root: &Path) -> PathBuf {
    root.join(CACHE_FILE)
}

fn mtime_of(meta: &fs::Metadata) -> Option<u128> {
    meta.modified()
        .ok()
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_nanos())
}

// --- Per-file parse cache (tier 2) ---

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedParse {
    mtime_nanos: u128,
    size: u64,
    result: ParseResult,
    resolved_paths: Vec<Option<PathBuf>>,
}

// --- Whole-graph cache (tier 1) ---

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedMtime {
    mtime_nanos: u128,
    size: u64,
}

#[derive(Debug, Serialize, Deserialize)]
struct CachedGraph {
    entry: PathBuf,
    graph: ModuleGraph,
    file_mtimes: HashMap<PathBuf, CachedMtime>,
    unresolved_specifiers: Vec<String>,
    unresolvable_dynamic: usize,
    /// Lockfile mtimes — if unchanged, skip re-resolving unresolved specifiers.
    dep_sentinels: Vec<(PathBuf, u128)>,
}

const LOCKFILES: &[&str] = &[
    "package-lock.json",
    "pnpm-lock.yaml",
    "yarn.lock",
    "bun.lockb",
    "poetry.lock",
    "Pipfile.lock",
    "uv.lock",
    "requirements.txt",
];

fn find_dep_sentinels(root: &Path) -> Vec<(PathBuf, u128)> {
    LOCKFILES
        .iter()
        .filter_map(|name| {
            let path = root.join(name);
            let meta = fs::metadata(&path).ok()?;
            let mtime = mtime_of(&meta)?;
            Some((path, mtime))
        })
        .collect()
}

#[derive(Debug)]
#[non_exhaustive]
pub enum GraphCacheResult {
    /// All files unchanged — graph is valid.
    Hit {
        graph: ModuleGraph,
        unresolvable_dynamic: usize,
        unresolved_specifiers: Vec<String>,
        /// True if the graph is valid but sentinel mtimes need updating.
        needs_resave: bool,
    },
    /// Some files have different mtimes — incremental update possible.
    Stale {
        graph: ModuleGraph,
        unresolvable_dynamic: usize,
        changed_files: Vec<PathBuf>,
    },
    /// Cache miss — wrong entry, no cache, file deleted, or new imports resolve.
    Miss,
}

/// Handle for a background cache write. Joins the write thread on drop
/// to ensure the cache file is fully written before process exit.
#[derive(Debug)]
#[repr(transparent)]
pub struct CacheWriteHandle(Option<thread::JoinHandle<()>>);

impl CacheWriteHandle {
    pub fn none() -> Self {
        Self(None)
    }
}

impl Drop for CacheWriteHandle {
    fn drop(&mut self) {
        if let Some(handle) = self.0.take() {
            let _ = handle.join();
        }
    }
}

#[derive(Debug)]
pub struct ParseCache {
    entries: HashMap<PathBuf, CachedParse>,
    deferred_parse_data: Option<Vec<u8>>,
    cached_graph: Option<CachedGraph>,
    /// Preserved from Stale result for incremental save.
    stale_file_mtimes: Option<HashMap<PathBuf, CachedMtime>>,
    stale_unresolved: Option<Vec<String>>,
}

impl Default for ParseCache {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            deferred_parse_data: None,
            cached_graph: None,
            stale_file_mtimes: None,
            stale_unresolved: None,
        }
    }

    /// Load cache from disk. The graph cache is deserialized immediately;
    /// parse entries are deferred until first access (saves ~2.5ms on cache hit).
    #[allow(clippy::cast_possible_truncation)]
    pub fn load(root: &Path) -> Self {
        let path = cache_path(root);
        let Ok(data) = fs::read(&path) else {
            return Self::new();
        };
        if data.len() < HEADER_SIZE {
            return Self::new();
        }
        let magic = u32::from_le_bytes(data[0..4].try_into().unwrap());
        let version = u32::from_le_bytes(data[4..8].try_into().unwrap());
        if magic != CACHE_MAGIC || version != CACHE_VERSION {
            return Self::new();
        }
        let graph_len = u64::from_le_bytes(data[8..16].try_into().unwrap()) as usize;
        let graph_end = HEADER_SIZE + graph_len;
        if data.len() < graph_end {
            return Self::new();
        }

        let cached_graph: Option<CachedGraph> =
            bitcode::deserialize(&data[HEADER_SIZE..graph_end]).ok();

        let deferred = if data.len() > graph_end {
            Some(data[graph_end..].to_vec())
        } else {
            None
        };

        Self {
            entries: HashMap::new(),
            deferred_parse_data: deferred,
            cached_graph,
            stale_file_mtimes: None,
            stale_unresolved: None,
        }
    }

    fn ensure_entries(&mut self) {
        if let Some(bytes) = self.deferred_parse_data.take() {
            self.entries = bitcode::deserialize(&bytes).unwrap_or_default();
        }
    }

    /// Try to load the cached graph (tier 1).
    ///
    /// Returns `Hit` if all files are unchanged, `Stale` if some files changed
    /// (incremental update possible), or `Miss` for entry mismatch/deleted files/
    /// newly-resolved imports.
    pub fn try_load_graph(
        &mut self,
        entry: &Path,
        resolve_fn: &(dyn Fn(&str) -> bool + Sync),
    ) -> GraphCacheResult {
        let cached = match self.cached_graph.as_ref() {
            Some(c) if c.entry == entry => c,
            _ => return GraphCacheResult::Miss,
        };

        // Check all file mtimes in parallel — collect changed files.
        // If any file is missing (deleted), return Miss.
        let any_missing = AtomicBool::new(false);
        let changed_files: Vec<PathBuf> = cached
            .file_mtimes
            .par_iter()
            .filter_map(|(path, saved)| if let Ok(meta) = fs::metadata(path) {
                let mtime = mtime_of(&meta)?;
                if mtime != saved.mtime_nanos || meta.len() != saved.size {
                    Some(path.clone())
                } else {
                    None
                }
            } else {
                any_missing.store(true, Ordering::Relaxed);
                None
            })
            .collect();

        if any_missing.load(Ordering::Relaxed) {
            return GraphCacheResult::Miss;
        }

        // Check if any previously-unresolved specifier now resolves.
        // Optimization: if we have lockfile sentinels and none changed, skip the
        // expensive re-resolution check. A new `npm install` / `pip install` would
        // modify the lockfile, triggering the full check.
        let sentinels_unchanged = !cached.dep_sentinels.is_empty()
            && cached.dep_sentinels.iter().all(|(path, saved_mtime)| {
                fs::metadata(path)
                    .ok()
                    .and_then(|m| mtime_of(&m))
                    .is_some_and(|t| t == *saved_mtime)
            });

        if !sentinels_unchanged {
            let any_resolves = cached
                .unresolved_specifiers
                .par_iter()
                .any(|spec| resolve_fn(spec));
            if any_resolves {
                return GraphCacheResult::Miss;
            }
        }

        if changed_files.is_empty() {
            let cached = self.cached_graph.take().unwrap();
            return GraphCacheResult::Hit {
                graph: cached.graph,
                unresolvable_dynamic: cached.unresolvable_dynamic,
                unresolved_specifiers: cached.unresolved_specifiers,
                needs_resave: !sentinels_unchanged,
            };
        }

        // Files changed — extract graph and preserve mtimes for incremental save
        let cached = self.cached_graph.take().unwrap();
        self.stale_file_mtimes = Some(cached.file_mtimes);
        self.stale_unresolved = Some(cached.unresolved_specifiers);
        GraphCacheResult::Stale {
            graph: cached.graph,
            unresolvable_dynamic: cached.unresolvable_dynamic,
            changed_files,
        }
    }

    /// Get the cached parse result for a file WITHOUT verifying its mtime.
    /// Used by incremental update to compare old imports against new parse results.
    pub fn lookup_unchecked(&mut self, path: &Path) -> Option<&ParseResult> {
        self.ensure_entries();
        self.entries.get(path).map(|e| &e.result)
    }

    /// Save after incremental update. Uses the preserved `file_mtimes` from the
    /// Stale result, updating only the changed files' mtimes instead of
    /// re-statting every file. Serialization and disk write happen on a
    /// background thread.
    pub fn save_incremental(
        &mut self,
        root: &Path,
        entry: &Path,
        graph: &ModuleGraph,
        changed_files: &[PathBuf],
        unresolvable_dynamic: usize,
    ) -> CacheWriteHandle {
        let Some(mut file_mtimes) = self.stale_file_mtimes.take() else {
            return CacheWriteHandle::none();
        };
        let unresolved_specifiers = self.stale_unresolved.take().unwrap_or_default();

        // Update only changed files' mtimes (cheap, typically 1-2 files)
        for path in changed_files {
            if let Ok(meta) = fs::metadata(path)
                && let Some(mtime) = mtime_of(&meta)
                && let Some(saved) = file_mtimes.get_mut(path)
            {
                saved.mtime_nanos = mtime;
                saved.size = meta.len();
            }
        }

        self.ensure_entries();
        let entries = std::mem::take(&mut self.entries);
        let root = root.to_path_buf();
        let entry = entry.to_path_buf();
        let graph = graph.clone();
        let dep_sentinels = find_dep_sentinels(&root);

        CacheWriteHandle(Some(thread::spawn(move || {
            write_cache_to_disk(root, entry, graph, entries, file_mtimes, unresolved_specifiers, unresolvable_dynamic, dep_sentinels);
        })))
    }

    /// Save the full graph + parse cache to disk. File mtime collection,
    /// serialization, and disk write all happen on a background thread.
    pub fn save(
        &mut self,
        root: &Path,
        entry: &Path,
        graph: &ModuleGraph,
        unresolved_specifiers: Vec<String>,
        unresolvable_dynamic: usize,
    ) -> CacheWriteHandle {
        self.ensure_entries();
        let entries = std::mem::take(&mut self.entries);
        let root = root.to_path_buf();
        let entry = entry.to_path_buf();
        let graph = graph.clone();

        let dep_sentinels = find_dep_sentinels(&root);

        CacheWriteHandle(Some(thread::spawn(move || {
            let file_mtimes: HashMap<PathBuf, CachedMtime> = graph
                .modules
                .par_iter()
                .filter_map(|m| {
                    let meta = fs::metadata(&m.path).ok()?;
                    let mtime = mtime_of(&meta)?;
                    Some((
                        m.path.clone(),
                        CachedMtime {
                            mtime_nanos: mtime,
                            size: meta.len(),
                        },
                    ))
                })
                .collect();

            write_cache_to_disk(root, entry, graph, entries, file_mtimes, unresolved_specifiers, unresolvable_dynamic, dep_sentinels);
        })))
    }

    pub fn lookup(&mut self, path: &Path) -> Option<(ParseResult, Vec<Option<PathBuf>>)> {
        self.ensure_entries();
        let entry = self.entries.get(path)?;
        let meta = fs::metadata(path).ok()?;
        let current_mtime = mtime_of(&meta)?;
        if current_mtime == entry.mtime_nanos && meta.len() == entry.size {
            Some((entry.result.clone(), entry.resolved_paths.clone()))
        } else {
            None
        }
    }

    pub fn insert(
        &mut self,
        path: PathBuf,
        result: ParseResult,
        resolved_paths: Vec<Option<PathBuf>>,
    ) {
        self.ensure_entries();
        let Ok(meta) = fs::metadata(&path) else {
            return;
        };
        let Some(mtime) = mtime_of(&meta) else {
            return;
        };
        self.entries.insert(
            path,
            CachedParse {
                mtime_nanos: mtime,
                size: meta.len(),
                result,
                resolved_paths,
            },
        );
    }
}

/// Serialize and write the cache to disk. Runs on a background thread.
#[allow(clippy::too_many_arguments, clippy::needless_pass_by_value)]
fn write_cache_to_disk(
    root: PathBuf,
    entry: PathBuf,
    graph: ModuleGraph,
    entries: HashMap<PathBuf, CachedParse>,
    file_mtimes: HashMap<PathBuf, CachedMtime>,
    unresolved_specifiers: Vec<String>,
    unresolvable_dynamic: usize,
    dep_sentinels: Vec<(PathBuf, u128)>,
) {
    let graph_cache = CachedGraph {
        entry,
        graph,
        file_mtimes,
        unresolved_specifiers,
        unresolvable_dynamic,
        dep_sentinels,
    };

    let graph_data = match bitcode::serialize(&graph_cache) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("warning: failed to serialize graph cache: {e}");
            return;
        }
    };
    let parse_data = match bitcode::serialize(&entries) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("warning: failed to serialize parse cache: {e}");
            return;
        }
    };

    let mut out = Vec::with_capacity(HEADER_SIZE + graph_data.len() + parse_data.len());
    out.extend_from_slice(&CACHE_MAGIC.to_le_bytes());
    out.extend_from_slice(&CACHE_VERSION.to_le_bytes());
    out.extend_from_slice(&(graph_data.len() as u64).to_le_bytes());
    out.extend_from_slice(&graph_data);
    out.extend_from_slice(&parse_data);

    if let Err(e) = fs::write(cache_path(&root), &out) {
        eprintln!("warning: failed to write cache: {e}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::EdgeKind;
    use crate::lang::RawImport;

    #[test]
    fn parse_cache_hit_when_unchanged() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("test.py");
        fs::write(&file, "import os").unwrap();

        let mut cache = ParseCache::new();
        let result = ParseResult {
            imports: vec![RawImport {
                specifier: "os".into(),
                kind: EdgeKind::Static,
            }],
            unresolvable_dynamic: 0,
        };
        let resolved = vec![None];
        cache.insert(file.clone(), result, resolved);

        let cached = cache.lookup(&file);
        assert!(cached.is_some());
        let (parse_result, resolved_paths) = cached.unwrap();
        assert_eq!(parse_result.imports.len(), 1);
        assert_eq!(resolved_paths.len(), 1);
        assert!(resolved_paths[0].is_none());
    }

    #[test]
    fn parse_cache_miss_when_modified() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("test.py");
        fs::write(&file, "import os").unwrap();

        let mut cache = ParseCache::new();
        let result = ParseResult {
            imports: vec![],
            unresolvable_dynamic: 0,
        };
        cache.insert(file.clone(), result, vec![]);

        fs::write(&file, "import os\nimport sys").unwrap();

        assert!(cache.lookup(&file).is_none());
    }

    #[test]
    fn parse_cache_miss_when_not_cached() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("test.py");
        fs::write(&file, "import os").unwrap();

        let mut cache = ParseCache::new();
        assert!(cache.lookup(&file).is_none());
    }

    #[test]
    fn parse_cache_save_and_load_roundtrip() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("test.py");
        let target = root.join("os_impl.py");
        fs::write(&file, "import os").unwrap();
        fs::write(&target, "").unwrap();

        let mut cache = ParseCache::new();
        let result = ParseResult {
            imports: vec![RawImport {
                specifier: "os".into(),
                kind: EdgeKind::Static,
            }],
            unresolvable_dynamic: 1,
        };
        let resolved = vec![Some(target.clone())];
        cache.insert(file.clone(), result, resolved);

        let graph = ModuleGraph::new();
        drop(cache.save(&root, &file, &graph, vec![], 0));

        let mut loaded = ParseCache::load(&root);
        let cached = loaded.lookup(&file);
        assert!(cached.is_some());
        let (parse_result, resolved_paths) = cached.unwrap();
        assert_eq!(parse_result.imports.len(), 1);
        assert_eq!(parse_result.imports[0].specifier, "os");
        assert_eq!(parse_result.unresolvable_dynamic, 1);
        assert_eq!(resolved_paths.len(), 1);
        assert_eq!(resolved_paths[0], Some(target));
    }

    #[test]
    fn graph_cache_valid_when_unchanged() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("entry.py");
        fs::write(&file, "x = 1").unwrap();

        let mut graph = ModuleGraph::new();
        let size = fs::metadata(&file).unwrap().len();
        graph.add_module(file.clone(), size, None);

        let mut cache = ParseCache::new();
        drop(cache.save(&root, &file, &graph, vec!["os".into()], 2));

        let mut loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        let result = loaded.try_load_graph(&file, &resolve_fn);
        assert!(matches!(result, GraphCacheResult::Hit { .. }));
        if let GraphCacheResult::Hit { graph: g, unresolvable_dynamic: unresolvable, .. } = result {
            assert_eq!(g.module_count(), 1);
            assert_eq!(unresolvable, 2);
        }
    }

    #[test]
    fn graph_cache_stale_when_file_modified() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("entry.py");
        fs::write(&file, "x = 1").unwrap();

        let mut graph = ModuleGraph::new();
        let size = fs::metadata(&file).unwrap().len();
        graph.add_module(file.clone(), size, None);

        let mut cache = ParseCache::new();
        drop(cache.save(&root, &file, &graph, vec![], 0));

        fs::write(&file, "x = 2; y = 3").unwrap();

        let mut loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        let result = loaded.try_load_graph(&file, &resolve_fn);
        assert!(matches!(result, GraphCacheResult::Stale { .. }));
        if let GraphCacheResult::Stale { changed_files, .. } = result {
            assert_eq!(changed_files.len(), 1);
            assert_eq!(changed_files[0], file);
        }
    }

    #[test]
    fn graph_cache_invalidates_when_unresolved_import_resolves() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("entry.py");
        fs::write(&file, "import foo").unwrap();

        let mut graph = ModuleGraph::new();
        let size = fs::metadata(&file).unwrap().len();
        graph.add_module(file.clone(), size, None);

        let mut cache = ParseCache::new();
        drop(cache.save(&root, &file, &graph, vec!["foo".into()], 0));

        let mut loaded = ParseCache::load(&root);
        let resolve_fn = |spec: &str| spec == "foo";
        assert!(matches!(
            loaded.try_load_graph(&file, &resolve_fn),
            GraphCacheResult::Miss
        ));
    }

    #[test]
    fn graph_cache_invalidates_for_different_entry() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file_a = root.join("a.py");
        let file_b = root.join("b.py");
        fs::write(&file_a, "x = 1").unwrap();
        fs::write(&file_b, "y = 2").unwrap();

        let mut graph = ModuleGraph::new();
        let size = fs::metadata(&file_a).unwrap().len();
        graph.add_module(file_a.clone(), size, None);

        let mut cache = ParseCache::new();
        drop(cache.save(&root, &file_a, &graph, vec![], 0));

        let mut loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        assert!(matches!(
            loaded.try_load_graph(&file_b, &resolve_fn),
            GraphCacheResult::Miss
        ));
    }

    #[test]
    fn incremental_save_updates_changed_mtimes() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("entry.py");
        fs::write(&file, "x = 1").unwrap();

        let mut graph = ModuleGraph::new();
        let size = fs::metadata(&file).unwrap().len();
        graph.add_module(file.clone(), size, None);

        let mut cache = ParseCache::new();
        drop(cache.save(&root, &file, &graph, vec![], 0));

        // Modify the file
        fs::write(&file, "x = 2").unwrap();

        let mut loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        let result = loaded.try_load_graph(&file, &resolve_fn);
        assert!(matches!(result, GraphCacheResult::Stale { .. }));

        if let GraphCacheResult::Stale { graph, changed_files, .. } = result {
            // Incremental save with updated mtimes
            drop(loaded.save_incremental(&root, &file, &graph, &changed_files, 0));

            // Reload — should now be a Hit
            let mut reloaded = ParseCache::load(&root);
            let result = reloaded.try_load_graph(&file, &resolve_fn);
            assert!(
                matches!(result, GraphCacheResult::Hit { .. }),
                "expected Hit after incremental save"
            );
        }
    }
}
