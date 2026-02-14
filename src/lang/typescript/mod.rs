mod parser;
mod resolver;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use crate::lang::{LanguageSupport, ParseResult};

use self::resolver::{package_name_from_path, ImportResolver};

const EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mjs", "cjs", "mts", "cts"];
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

    fn parse(&self, path: &Path) -> Result<ParseResult, String> {
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
    use std::fs;

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

    fn setup_workspace(tmp: &Path) {
        let app = tmp.join("packages/app");
        let lib = tmp.join("packages/lib/src");
        fs::create_dir_all(&app).unwrap();
        fs::create_dir_all(&lib).unwrap();
        fs::write(app.join("package.json"), r#"{"name": "my-app"}"#).unwrap();
        fs::write(
            tmp.join("packages/lib/package.json"),
            r#"{"name": "@my/lib"}"#,
        )
        .unwrap();
        fs::write(lib.join("index.ts"), "export const x = 1;").unwrap();
    }

    #[test]
    fn read_package_name_valid() {
        let tmp = tempfile::tempdir().unwrap();
        let nested = tmp.path().join("packages/pkg");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("package.json"), r#"{"name": "@scope/pkg"}"#).unwrap();
        let support = TypeScriptSupport::new(tmp.path());
        assert_eq!(
            support.workspace_package_name(&nested.join("index.ts"), tmp.path()),
            Some("@scope/pkg".to_string())
        );
    }

    #[test]
    fn read_package_name_missing_name_field() {
        let tmp = tempfile::tempdir().unwrap();
        let nested = tmp.path().join("packages/pkg");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("package.json"), r#"{"version": "1.0.0"}"#).unwrap();
        let support = TypeScriptSupport::new(tmp.path());
        assert_eq!(
            support.workspace_package_name(&nested.join("index.ts"), tmp.path()),
            None
        );
    }

    #[test]
    fn workspace_detects_sibling_package() {
        let tmp = tempfile::tempdir().unwrap();
        setup_workspace(tmp.path());
        let project_root = tmp.path().join("packages/app");
        let support = TypeScriptSupport::new(&project_root);
        let sibling_file = tmp.path().join("packages/lib/src/index.ts");
        assert_eq!(
            support.workspace_package_name(&sibling_file, &project_root),
            Some("@my/lib".to_string())
        );
    }

    #[test]
    fn workspace_skips_project_root() {
        let tmp = tempfile::tempdir().unwrap();
        setup_workspace(tmp.path());
        let project_root = tmp.path().join("packages/app");
        let support = TypeScriptSupport::new(&project_root);
        let own_file = project_root.join("src/cli.ts");
        assert_eq!(
            support.workspace_package_name(&own_file, &project_root),
            None
        );
    }

    #[test]
    fn workspace_caches_lookups() {
        let tmp = tempfile::tempdir().unwrap();
        setup_workspace(tmp.path());
        let project_root = tmp.path().join("packages/app");
        let support = TypeScriptSupport::new(&project_root);

        let file1 = tmp.path().join("packages/lib/src/index.ts");
        let file2 = tmp.path().join("packages/lib/src/utils.ts");

        assert_eq!(
            support.workspace_package_name(&file1, &project_root),
            Some("@my/lib".to_string())
        );
        assert_eq!(
            support.workspace_package_name(&file2, &project_root),
            Some("@my/lib".to_string())
        );
    }

    #[test]
    fn workspace_no_package_json() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("some/random/dir");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("file.ts"), "").unwrap();
        let support = TypeScriptSupport::new(tmp.path());
        assert_eq!(
            support.workspace_package_name(&dir.join("file.ts"), tmp.path()),
            None
        );
    }
}
