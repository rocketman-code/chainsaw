//! Python support: tree-sitter parser with source-root and virtualenv resolution.

mod parser;
mod resolver;

#[cfg(test)]
mod conformance;

use std::path::{Path, PathBuf};

use crate::lang::{LanguageSupport, ParseError, ParseResult};

use self::resolver::{PythonResolver, package_name_from_path};

#[derive(Debug)]
pub struct PythonSupport {
    resolver: PythonResolver,
}

impl PythonSupport {
    pub fn new(root: &Path) -> Self {
        Self {
            resolver: PythonResolver::new(root),
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
