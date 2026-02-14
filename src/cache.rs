use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::SystemTime;

use crate::graph::ModuleGraph;
use crate::lang::ParseResult;

const CACHE_FILE: &str = ".chainsaw.cache";
const CACHE_VERSION: u32 = 4;

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

#[derive(Clone, Serialize, Deserialize)]
struct CachedParse {
    mtime_nanos: u128,
    size: u64,
    result: ParseResult,
}

// --- Whole-graph cache (tier 1) ---

#[derive(Clone, Serialize, Deserialize)]
struct CachedMtime {
    mtime_nanos: u128,
    size: u64,
}

#[derive(Serialize, Deserialize)]
struct CachedGraph {
    entry: PathBuf,
    graph: ModuleGraph,
    file_mtimes: HashMap<PathBuf, CachedMtime>,
    unresolved_specifiers: Vec<String>,
    unresolvable_dynamic: usize,
}

// --- Envelope ---

#[derive(Serialize, Deserialize)]
struct CacheEnvelope {
    version: u32,
    parse_entries: HashMap<PathBuf, CachedParse>,
    graph_cache: Option<CachedGraph>,
}

pub struct ParseCache {
    entries: HashMap<PathBuf, CachedParse>,
    cached_graph: Option<CachedGraph>,
}

impl ParseCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            cached_graph: None,
        }
    }

    pub fn load(root: &Path) -> Self {
        let path = cache_path(root);
        let data = match fs::read(&path) {
            Ok(d) => d,
            Err(_) => return Self::new(),
        };
        let envelope: CacheEnvelope = match bitcode::deserialize::<CacheEnvelope>(&data) {
            Ok(e) if e.version == CACHE_VERSION => e,
            _ => return Self::new(),
        };
        Self {
            entries: envelope.parse_entries,
            cached_graph: envelope.graph_cache,
        }
    }

    /// Try to load the cached graph (tier 1). Returns the graph and
    /// unresolvable_dynamic count if the cache is valid for this entry point.
    pub fn try_load_graph(
        &self,
        entry: &Path,
        resolve_fn: &dyn Fn(&str) -> bool,
    ) -> Option<(ModuleGraph, usize)> {
        let cached = self.cached_graph.as_ref()?;
        if cached.entry != entry {
            return None;
        }
        // Check all file mtimes in parallel — bail early on first mismatch
        let valid = AtomicBool::new(true);
        cached.file_mtimes.par_iter().for_each(|(path, saved)| {
            if !valid.load(Ordering::Relaxed) {
                return;
            }
            let ok = fs::metadata(path)
                .ok()
                .and_then(|meta| {
                    let mtime = mtime_of(&meta)?;
                    Some(mtime == saved.mtime_nanos && meta.len() == saved.size)
                })
                .unwrap_or(false);
            if !ok {
                valid.store(false, Ordering::Relaxed);
            }
        });
        if !valid.load(Ordering::Relaxed) {
            return None;
        }
        // Check if any previously-unresolved specifier now resolves
        for spec in &cached.unresolved_specifiers {
            if resolve_fn(spec) {
                return None;
            }
        }
        Some((cached.graph.clone(), cached.unresolvable_dynamic))
    }

    pub fn save(
        &self,
        root: &Path,
        entry: &Path,
        graph: &ModuleGraph,
        unresolved_specifiers: Vec<String>,
        unresolvable_dynamic: usize,
    ) {
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

        let graph_cache = CachedGraph {
            entry: entry.to_path_buf(),
            graph: graph.clone(),
            file_mtimes,
            unresolved_specifiers,
            unresolvable_dynamic,
        };

        let envelope = CacheEnvelope {
            version: CACHE_VERSION,
            parse_entries: self.entries.clone(),
            graph_cache: Some(graph_cache),
        };
        let data = match bitcode::serialize(&envelope) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("warning: failed to serialize cache: {e}");
                return;
            }
        };
        if let Err(e) = fs::write(cache_path(root), &data) {
            eprintln!("warning: failed to write cache: {e}");
        }
    }

    pub fn lookup(&self, path: &Path) -> Option<ParseResult> {
        let entry = self.entries.get(path)?;
        let meta = fs::metadata(path).ok()?;
        let current_mtime = mtime_of(&meta)?;
        if current_mtime == entry.mtime_nanos && meta.len() == entry.size {
            Some(entry.result.clone())
        } else {
            None
        }
    }

    pub fn insert(&mut self, path: PathBuf, result: &ParseResult) {
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
                result: result.clone(),
            },
        );
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
        cache.insert(file.clone(), &result);

        let cached = cache.lookup(&file);
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().imports.len(), 1);
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
        cache.insert(file.clone(), &result);

        fs::write(&file, "import os\nimport sys").unwrap();

        assert!(cache.lookup(&file).is_none());
    }

    #[test]
    fn parse_cache_miss_when_not_cached() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("test.py");
        fs::write(&file, "import os").unwrap();

        let cache = ParseCache::new();
        assert!(cache.lookup(&file).is_none());
    }

    #[test]
    fn parse_cache_save_and_load_roundtrip() {
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
            unresolvable_dynamic: 1,
        };
        cache.insert(file.clone(), &result);

        let graph = ModuleGraph::new();
        cache.save(&root, &file, &graph, vec![], 0);

        let loaded = ParseCache::load(&root);
        let cached = loaded.lookup(&file);
        assert!(cached.is_some());
        let cached = cached.unwrap();
        assert_eq!(cached.imports.len(), 1);
        assert_eq!(cached.imports[0].specifier, "os");
        assert_eq!(cached.unresolvable_dynamic, 1);
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

        let cache = ParseCache::new();
        cache.save(&root, &file, &graph, vec!["os".into()], 2);

        let loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        let result = loaded.try_load_graph(&file, &resolve_fn);
        assert!(result.is_some());
        let (g, unresolvable) = result.unwrap();
        assert_eq!(g.module_count(), 1);
        assert_eq!(unresolvable, 2);
    }

    #[test]
    fn graph_cache_invalidates_when_file_modified() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let file = root.join("entry.py");
        fs::write(&file, "x = 1").unwrap();

        let mut graph = ModuleGraph::new();
        let size = fs::metadata(&file).unwrap().len();
        graph.add_module(file.clone(), size, None);

        let cache = ParseCache::new();
        cache.save(&root, &file, &graph, vec![], 0);

        fs::write(&file, "x = 2; y = 3").unwrap();

        let loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        assert!(loaded.try_load_graph(&file, &resolve_fn).is_none());
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

        let cache = ParseCache::new();
        cache.save(&root, &file, &graph, vec!["foo".into()], 0);

        let loaded = ParseCache::load(&root);
        let resolve_fn = |spec: &str| spec == "foo";
        assert!(loaded.try_load_graph(&file, &resolve_fn).is_none());
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

        let cache = ParseCache::new();
        cache.save(&root, &file_a, &graph, vec![], 0);

        let loaded = ParseCache::load(&root);
        let resolve_fn = |_: &str| false;
        assert!(loaded.try_load_graph(&file_b, &resolve_fn).is_none());
    }
}
