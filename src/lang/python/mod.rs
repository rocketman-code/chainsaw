mod parser;
mod resolver;

use std::path::{Path, PathBuf};

use crate::lang::{LanguageSupport, RawImport};

use self::resolver::{PythonResolver, package_name_from_path};

pub struct PythonSupport {
    resolver: PythonResolver,
}

impl PythonSupport {
    pub fn new(root: &Path) -> Self {
        Self {
            resolver: PythonResolver::new(root),
        }
    }
}

impl LanguageSupport for PythonSupport {
    fn extensions(&self) -> &[&str] {
        &["py"]
    }

    fn skip_dirs(&self) -> &[&str] {
        &[
            "__pycache__",
            ".git",
            ".venv",
            "venv",
            "node_modules",
            ".mypy_cache",
            ".pytest_cache",
            ".tox",
            ".eggs",
        ]
    }

    fn parse(&self, path: &Path) -> Result<Vec<RawImport>, String> {
        parser::parse_file(path)
    }

    fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        self.resolver.resolve(from_dir, specifier)
    }

    fn package_name(&self, resolved_path: &Path) -> Option<String> {
        package_name_from_path(resolved_path, self.resolver.site_packages())
    }

    fn workspace_package_name(&self, _file_path: &Path, _project_root: &Path) -> Option<String> {
        None
    }
}
