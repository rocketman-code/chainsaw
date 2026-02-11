pub(crate) mod parser;
pub(crate) mod resolver;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use crate::lang::{LanguageSupport, RawImport};

use self::resolver::{package_name_from_path, ImportResolver};

const EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mjs", "cjs", "mts", "cts"];
const SKIP_DIRS: &[&str] = &["node_modules", ".git"];

pub struct TypeScriptSupport {
    resolver: ImportResolver,
    workspace_cache: Mutex<HashMap<PathBuf, Option<String>>>,
}

impl TypeScriptSupport {
    pub fn new(root: &Path) -> Self {
        Self {
            resolver: ImportResolver::new(root),
            workspace_cache: Mutex::new(HashMap::new()),
        }
    }
}

impl LanguageSupport for TypeScriptSupport {
    fn extensions(&self) -> &[&str] {
        EXTENSIONS
    }

    fn skip_dirs(&self) -> &[&str] {
        SKIP_DIRS
    }

    fn parse(&self, path: &Path) -> Result<Vec<RawImport>, String> {
        parser::parse_file(path)
    }

    fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        self.resolver.resolve(from_dir, specifier)
    }

    fn package_name(&self, resolved_path: &Path) -> Option<String> {
        package_name_from_path(resolved_path)
    }

    fn workspace_package_name(&self, file_path: &Path, project_root: &Path) -> Option<String> {
        let mut cache = self.workspace_cache.lock().unwrap();
        let mut dir = file_path.parent()?;
        loop {
            if let Some(cached) = cache.get(dir) {
                return cached.clone();
            }

            let pkg_json = dir.join("package.json");
            if pkg_json.exists() {
                let result = if dir == project_root {
                    None
                } else {
                    resolver::read_package_name(&pkg_json)
                };
                cache.insert(dir.to_path_buf(), result.clone());
                return result;
            }

            match dir.parent() {
                Some(parent) if parent != dir => dir = parent,
                _ => break,
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extensions_includes_ts_and_js() {
        let root = Path::new("/tmp");
        let support = TypeScriptSupport::new(root);
        let exts = support.extensions();
        assert!(exts.contains(&"ts"));
        assert!(exts.contains(&"tsx"));
        assert!(exts.contains(&"js"));
        assert!(exts.contains(&"mjs"));
    }

    #[test]
    fn skip_dirs_includes_node_modules() {
        let root = Path::new("/tmp");
        let support = TypeScriptSupport::new(root);
        assert!(support.skip_dirs().contains(&"node_modules"));
        assert!(support.skip_dirs().contains(&".git"));
    }
}
