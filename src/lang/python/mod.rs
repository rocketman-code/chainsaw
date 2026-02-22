//! Python support: tree-sitter parser with source-root and virtualenv resolution.

mod parser;
mod resolver;

#[cfg(test)]
mod conformance;

use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::lang::{LanguageSupport, ParseError, ParseResult};
use crate::vfs::{OsVfs, Vfs};

use self::resolver::{PythonResolver, package_name_from_path};

pub struct PythonSupport {
    resolver: PythonResolver,
}

impl std::fmt::Debug for PythonSupport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PythonSupport").finish_non_exhaustive()
    }
}

impl PythonSupport {
    pub fn new(root: &Path) -> Self {
        Self::with_vfs(root, Arc::new(OsVfs))
    }

    pub fn with_vfs(root: &Path, vfs: Arc<dyn Vfs>) -> Self {
        Self {
            resolver: PythonResolver::with_vfs(root, vfs),
        }
    }

    pub fn source_roots(&self) -> &[PathBuf] {
        self.resolver.source_roots()
    }
}

impl LanguageSupport for PythonSupport {
    fn extensions(&self) -> &'static [&'static str] {
        &["py"]
    }

    fn parse(&self, path: &Path, source: &str) -> Result<ParseResult, ParseError> {
        parser::parse_file(path, source)
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
