use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::graph::ModuleGraph;

const CACHE_FILE: &str = ".chainsaw.cache";
const CACHE_VERSION: u32 = 1;

#[derive(Serialize, Deserialize)]
struct CacheEntry {
    mtime_secs: u64,
    size: u64,
}

#[derive(Serialize, Deserialize)]
struct CacheEnvelope {
    version: u32,
    file_mtimes: HashMap<PathBuf, CacheEntry>,
    graph: ModuleGraph,
}

fn file_mtime_secs(path: &Path) -> Option<u64> {
    fs::metadata(path)
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
}

fn build_mtime_map(graph: &ModuleGraph) -> HashMap<PathBuf, CacheEntry> {
    let mut map = HashMap::new();
    for module in &graph.modules {
        if let Some(secs) = file_mtime_secs(&module.path) {
            map.insert(
                module.path.clone(),
                CacheEntry {
                    mtime_secs: secs,
                    size: module.size_bytes,
                },
            );
        }
    }
    map
}

pub fn cache_path(root: &Path) -> PathBuf {
    root.join(CACHE_FILE)
}

/// Try to load a cached graph. Returns None if cache is missing or stale.
/// A cache is considered stale if any file's mtime or size has changed.
pub fn load_cache(root: &Path) -> Option<ModuleGraph> {
    let path = cache_path(root);
    let data = fs::read(&path).ok()?;
    let envelope: CacheEnvelope = bitcode::deserialize(&data).ok()?;

    if envelope.version != CACHE_VERSION {
        return None;
    }

    // Validate all mtimes
    for (file_path, entry) in &envelope.file_mtimes {
        let current_mtime = file_mtime_secs(file_path)?;
        let current_size = fs::metadata(file_path).ok()?.len();
        if current_mtime != entry.mtime_secs || current_size != entry.size {
            return None;
        }
    }

    Some(envelope.graph)
}

/// Save the graph to the cache file.
pub fn save_cache(root: &Path, graph: &ModuleGraph) {
    let envelope = CacheEnvelope {
        version: CACHE_VERSION,
        file_mtimes: build_mtime_map(graph),
        graph: graph.clone(),
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
