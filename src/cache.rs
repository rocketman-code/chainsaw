use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::lang::ParseResult;

const CACHE_FILE: &str = ".chainsaw.cache";
const CACHE_VERSION: u32 = 3;

pub fn cache_path(root: &Path) -> PathBuf {
    root.join(CACHE_FILE)
}

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
                Ok(e) if e.version == CACHE_VERSION => e,
                _ => return Self::new(),
            };
        Self {
            entries: envelope.entries,
        }
    }

    pub fn save(&self, root: &Path) {
        let envelope = ParseCacheEnvelope {
            version: CACHE_VERSION,
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
        let meta = fs::metadata(path).ok()?;
        let current_mtime = meta
            .modified()
            .ok()
            .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
            .map(|d| d.as_nanos())?;
        if current_mtime == entry.mtime_nanos && meta.len() == entry.size {
            Some(entry.result.clone())
        } else {
            None
        }
    }

    pub fn insert(&mut self, path: PathBuf, result: &ParseResult) {
        let Ok(meta) = fs::metadata(&path) else { return };
        let Some(mtime) = meta
            .modified()
            .ok()
            .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
            .map(|d| d.as_nanos())
        else {
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
