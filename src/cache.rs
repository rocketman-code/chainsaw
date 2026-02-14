use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::graph::ModuleGraph;
use crate::lang::ParseResult;

const CACHE_FILE: &str = ".chainsaw.cache";
const CACHE_VERSION: u32 = 2;

#[derive(Clone, Serialize, Deserialize)]
struct CacheEntry {
    mtime_nanos: u128,
    size: u64,
}

#[derive(Serialize, Deserialize)]
struct CacheEnvelope {
    version: u32,
    file_mtimes: HashMap<PathBuf, CacheEntry>,
    /// Mtimes (nanos) of directories visited during source file discovery.
    /// If any directory mtime changes, files were added or removed and the cache is stale.
    dir_mtimes: HashMap<PathBuf, u128>,
    /// Import specifiers that could not be resolved during graph building.
    /// On load, the caller re-resolves these; if any now resolves, the cache is stale.
    unresolved_specifiers: Vec<String>,
    graph: ModuleGraph,
}

fn mtime_nanos(path: &Path) -> Option<u128> {
    fs::metadata(path)
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_nanos())
}

fn build_mtime_map(graph: &ModuleGraph) -> HashMap<PathBuf, CacheEntry> {
    let mut map = HashMap::new();
    for module in &graph.modules {
        if let Some(nanos) = mtime_nanos(&module.path) {
            map.insert(
                module.path.clone(),
                CacheEntry {
                    mtime_nanos: nanos,
                    size: module.size_bytes,
                },
            );
        }
    }
    map
}

fn build_dir_mtime_map(dirs: &[PathBuf]) -> HashMap<PathBuf, u128> {
    let mut map = HashMap::new();
    for dir in dirs {
        if let Some(nanos) = mtime_nanos(dir) {
            map.insert(dir.clone(), nanos);
        }
    }
    map
}

pub fn cache_path(root: &Path) -> PathBuf {
    root.join(CACHE_FILE)
}

/// Try to load a cached graph. Returns None if cache is missing or stale.
///
/// A cache is considered stale if:
/// - Any walked directory's mtime has changed (files added/removed)
/// - Any previously-unresolved import specifier now resolves
/// - Any cached file's mtime or size has changed
///
/// The `resolve_fn` is called for each unresolved specifier to check if it now resolves.
pub fn load_cache(
    root: &Path,
    resolve_fn: &dyn Fn(&str) -> bool,
) -> Option<ModuleGraph> {
    let path = cache_path(root);
    let data = fs::read(&path).ok()?;
    let envelope: CacheEnvelope = bitcode::deserialize(&data).ok()?;

    if envelope.version != CACHE_VERSION {
        return None;
    }

    // Check if any walked directory's mtime changed (new/removed source files)
    for (dir_path, saved_mtime) in &envelope.dir_mtimes {
        let current_mtime = mtime_nanos(dir_path)?;
        if current_mtime != *saved_mtime {
            return None;
        }
    }

    // Check if any previously-unresolved specifier now resolves
    for specifier in &envelope.unresolved_specifiers {
        if resolve_fn(specifier) {
            return None;
        }
    }

    // Validate all file mtimes
    for (file_path, entry) in &envelope.file_mtimes {
        let current_mtime = mtime_nanos(file_path)?;
        let current_size = fs::metadata(file_path).ok()?.len();
        if current_mtime != entry.mtime_nanos || current_size != entry.size {
            return None;
        }
    }

    Some(envelope.graph)
}

/// Save the graph to the cache file.
///
/// Dir mtimes are snapshotted after the cache file is written to disk,
/// because writing the cache file itself changes the root directory's mtime.
pub fn save_cache(
    root: &Path,
    graph: &ModuleGraph,
    walked_dirs: &[PathBuf],
    unresolved_specifiers: Vec<String>,
) {
    let file_mtimes = build_mtime_map(graph);
    let graph = graph.clone();
    let path = cache_path(root);

    // First write: establishes the cache file (changes root dir mtime)
    let envelope = CacheEnvelope {
        version: CACHE_VERSION,
        file_mtimes: file_mtimes.clone(),
        dir_mtimes: HashMap::new(),
        unresolved_specifiers: unresolved_specifiers.clone(),
        graph: graph.clone(),
    };
    let data = match bitcode::serialize(&envelope) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("warning: failed to serialize cache: {e}");
            return;
        }
    };
    if let Err(e) = fs::write(&path, &data) {
        eprintln!("warning: failed to write cache: {e}");
        return;
    }

    // Second write: now snapshot dir mtimes (reflects the cache file's existence)
    let envelope = CacheEnvelope {
        version: CACHE_VERSION,
        file_mtimes,
        dir_mtimes: build_dir_mtime_map(walked_dirs),
        unresolved_specifiers,
        graph,
    };
    let data = match bitcode::serialize(&envelope) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("warning: failed to serialize cache: {e}");
            return;
        }
    };
    if let Err(e) = fs::write(&path, &data) {
        eprintln!("warning: failed to write cache: {e}");
    }
}

// --- File-level parse cache ---

const PARSE_CACHE_VERSION: u32 = 3;

#[derive(Clone, Serialize, Deserialize)]
struct CachedParse {
    mtime_nanos: u128,
    size: u64,
    result: ParseResult,
}

#[derive(Serialize, Deserialize)]
struct ParseCacheEnvelope {
    version: u32,
    entries: HashMap<PathBuf, CachedParse>,
}

pub struct ParseCache {
    entries: HashMap<PathBuf, CachedParse>,
}

impl ParseCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn load(root: &Path) -> Self {
        let path = cache_path(root);
        let data = match fs::read(&path) {
            Ok(d) => d,
            Err(_) => return Self::new(),
        };
        let envelope: ParseCacheEnvelope =
            match bitcode::deserialize::<ParseCacheEnvelope>(&data) {
                Ok(e) if e.version == PARSE_CACHE_VERSION => e,
                _ => return Self::new(),
            };
        Self {
            entries: envelope.entries,
        }
    }

    pub fn save(&self, root: &Path) {
        let envelope = ParseCacheEnvelope {
            version: PARSE_CACHE_VERSION,
            entries: self.entries.clone(),
        };
        let data = match bitcode::serialize(&envelope) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("warning: failed to serialize parse cache: {e}");
                return;
            }
        };
        if let Err(e) = fs::write(cache_path(root), &data) {
            eprintln!("warning: failed to write parse cache: {e}");
        }
    }

    pub fn lookup(&self, path: &Path) -> Option<ParseResult> {
        let entry = self.entries.get(path)?;
        let current_mtime = mtime_nanos(path)?;
        let current_size = fs::metadata(path).ok()?.len();
        if current_mtime == entry.mtime_nanos && current_size == entry.size {
            Some(entry.result.clone())
        } else {
            None
        }
    }

    pub fn insert(&mut self, path: PathBuf, result: &ParseResult) {
        if let (Some(mtime), Ok(meta)) = (mtime_nanos(&path), fs::metadata(&path)) {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::ModuleGraph;

    fn make_graph(root: &Path, files: &[&str]) -> ModuleGraph {
        let mut g = ModuleGraph::new();
        for name in files {
            let path = root.join(name);
            let size = fs::metadata(&path).map(|m| m.len()).unwrap_or(0);
            g.add_module(path, size, None);
        }
        g
    }

    #[test]
    fn cache_valid_when_unchanged() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("entry.py"), "x = 1").unwrap();

        let graph = make_graph(&root, &["entry.py"]);
        let walked_dirs = vec![root.clone()];
        save_cache(&root, &graph, &walked_dirs, vec!["os".into()]);

        let resolve_fn = |_: &str| false;
        let loaded = load_cache(&root, &resolve_fn);
        assert!(loaded.is_some(), "cache should load when nothing changed");
        assert_eq!(loaded.unwrap().module_count(), 1);
    }

    #[test]
    fn cache_invalidates_when_file_added_to_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("entry.py"), "x = 1").unwrap();

        let graph = make_graph(&root, &["entry.py"]);
        let walked_dirs = vec![root.clone()];
        save_cache(&root, &graph, &walked_dirs, vec![]);

        // Add a new file — changes dir mtime
        fs::write(root.join("newfile.py"), "y = 2").unwrap();

        let resolve_fn = |_: &str| false;
        let loaded = load_cache(&root, &resolve_fn);
        assert!(loaded.is_none(), "cache should invalidate when a file is added");
    }

    #[test]
    fn cache_invalidates_when_unresolved_import_resolves() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("entry.py"), "import foo").unwrap();

        let graph = make_graph(&root, &["entry.py"]);
        let walked_dirs = vec![root.clone()];
        save_cache(&root, &graph, &walked_dirs, vec!["foo".into()]);

        // "foo" now resolves (simulating pip install)
        let resolve_fn = |spec: &str| spec == "foo";
        let loaded = load_cache(&root, &resolve_fn);
        assert!(loaded.is_none(), "cache should invalidate when unresolved import becomes resolvable");
    }

    #[test]
    fn cache_invalidates_when_file_modified() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("entry.py"), "x = 1").unwrap();

        let graph = make_graph(&root, &["entry.py"]);
        let walked_dirs = vec![root.clone()];
        save_cache(&root, &graph, &walked_dirs, vec![]);

        // Modify without changing dir mtime (overwrite existing file)
        fs::write(root.join("entry.py"), "x = 2; y = 3").unwrap();

        let resolve_fn = |_: &str| false;
        let loaded = load_cache(&root, &resolve_fn);
        assert!(loaded.is_none(), "cache should invalidate when a file is modified");
    }

    #[test]
    fn cache_invalidates_when_file_deleted() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let sub = root.join("src");
        fs::create_dir(&sub).unwrap();
        fs::write(sub.join("a.py"), "x = 1").unwrap();
        fs::write(sub.join("b.py"), "y = 2").unwrap();

        let graph = make_graph(&root, &["src/a.py", "src/b.py"]);
        let walked_dirs = vec![root.clone(), sub.clone()];
        save_cache(&root, &graph, &walked_dirs, vec![]);

        fs::remove_file(sub.join("b.py")).unwrap();

        let resolve_fn = |_: &str| false;
        let loaded = load_cache(&root, &resolve_fn);
        assert!(loaded.is_none(), "cache should invalidate when a file is deleted");
    }

    // --- ParseCache tests ---

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
        cache.save(&root);

        let loaded = ParseCache::load(&root);
        let cached = loaded.lookup(&file);
        assert!(cached.is_some());
        let cached = cached.unwrap();
        assert_eq!(cached.imports.len(), 1);
        assert_eq!(cached.imports[0].specifier, "os");
        assert_eq!(cached.unresolvable_dynamic, 1);
    }
}
