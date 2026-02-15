mod parser;
mod resolver;

use std::path::{Path, PathBuf};

use dashmap::DashMap;

use crate::lang::{LanguageSupport, ParseError, ParseResult};

use self::resolver::{package_name_from_path, ImportResolver};

const EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mjs", "cjs", "mts", "cts"];
#[derive(Debug)]
pub struct TypeScriptSupport {
    resolver: ImportResolver,
    workspace_cache: DashMap<PathBuf, Option<String>>,
}

impl TypeScriptSupport {
    pub fn new(root: &Path) -> Self {
        Self {
            resolver: ImportResolver::new(root),
            workspace_cache: DashMap::new(),
        }
    }
}

impl LanguageSupport for TypeScriptSupport {
    fn extensions(&self) -> &'static [&'static str] {
        EXTENSIONS
    }

    fn parse(&self, path: &Path, source: &str) -> Result<ParseResult, ParseError> {
        parser::parse_file(path, source)
    }

    fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        self.resolver.resolve(from_dir, specifier)
    }

    fn package_name(&self, resolved_path: &Path) -> Option<String> {
        package_name_from_path(resolved_path)
    }

    fn workspace_package_name(&self, file_path: &Path, project_root: &Path) -> Option<String> {
        let mut dir = file_path.parent()?;

        // Fast path: starting directory already cached.
        // Clone and drop the Ref immediately to avoid holding a read lock
        // across insert() calls (DashMap shard deadlock).
        if let Some(result) = self.workspace_cache.get(dir).map(|e| e.value().clone()) {
            return result;
        }

        // Walk up from the file's directory, collecting uncached directories
        let mut uncached: Vec<PathBuf> = Vec::new();

        let result = loop {
            // Check cache (clone + drop Ref before any insert)
            if let Some(result) = self.workspace_cache.get(dir).map(|e| e.value().clone()) {
                break result;
            }

            let pkg_json = dir.join("package.json");
            if pkg_json.exists() {
                let result = if dir == project_root {
                    None
                } else {
                    resolver::read_package_name(&pkg_json)
                };
                uncached.push(dir.to_path_buf());
                break result;
            }

            uncached.push(dir.to_path_buf());

            match dir.parent() {
                Some(parent) if parent != dir => dir = parent,
                _ => break None,
            }
        };

        // Cache all visited directories with the result
        for d in uncached {
            self.workspace_cache.insert(d, result.clone());
        }
        result
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

    #[test]
    fn workspace_deep_tree_caches_negatives() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        // Root package.json exists but should be skipped (project root)
        fs::write(root.join("package.json"), r#"{"name": "root"}"#).unwrap();
        // Deep directories with no intermediate package.json
        let deep = root.join("src/features/auth/components/forms");
        fs::create_dir_all(&deep).unwrap();

        let support = TypeScriptSupport::new(&root);

        // Multiple files at different depths should all return None
        let file1 = deep.join("LoginForm.ts");
        let file2 = deep.join("SignupForm.ts");
        let file3 = root.join("src/features/auth/index.ts");
        let file4 = root.join("src/features/auth/components/Button.ts");

        assert_eq!(support.workspace_package_name(&file1, &root), None);
        assert_eq!(support.workspace_package_name(&file2, &root), None);
        assert_eq!(support.workspace_package_name(&file3, &root), None);
        assert_eq!(support.workspace_package_name(&file4, &root), None);
    }

    #[test]
    fn workspace_mixed_depths_correct_package() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("package.json"), r#"{"name": "root"}"#).unwrap();

        // Two sibling packages at different depths
        let lib_a = root.join("packages/lib-a");
        let lib_b = root.join("packages/lib-b/src/deep/nested");
        fs::create_dir_all(&lib_a).unwrap();
        fs::create_dir_all(&lib_b).unwrap();
        fs::write(
            root.join("packages/lib-a/package.json"),
            r#"{"name": "@org/lib-a"}"#,
        )
        .unwrap();
        fs::write(
            root.join("packages/lib-b/package.json"),
            r#"{"name": "@org/lib-b"}"#,
        )
        .unwrap();

        let support = TypeScriptSupport::new(&root);

        // Files in lib-a
        assert_eq!(
            support.workspace_package_name(&lib_a.join("index.ts"), &root),
            Some("@org/lib-a".to_string())
        );
        // Files deep in lib-b (tests intermediate directory caching)
        assert_eq!(
            support.workspace_package_name(&lib_b.join("file.ts"), &root),
            Some("@org/lib-b".to_string())
        );
        // Another file in same deep dir (should hit cache)
        assert_eq!(
            support.workspace_package_name(&lib_b.join("other.ts"), &root),
            Some("@org/lib-b".to_string())
        );
        // File at intermediate depth in lib-b (should also hit cache)
        assert_eq!(
            support.workspace_package_name(
                &root.join("packages/lib-b/src/shallow.ts"),
                &root
            ),
            Some("@org/lib-b".to_string())
        );
        // First-party file at root level — should be None
        assert_eq!(
            support.workspace_package_name(&root.join("src/app.ts"), &root),
            None
        );
    }
}
