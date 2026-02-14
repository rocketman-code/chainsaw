mod parser;
mod resolver;

#[cfg(test)]
mod conformance;

use std::path::{Path, PathBuf};

use crate::lang::{LanguageSupport, ParseResult};

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

    fn parse(&self, path: &Path, source: &str) -> Result<ParseResult, String> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extensions_includes_py() {
        let support = PythonSupport::new(Path::new("/tmp"));
        assert_eq!(support.extensions(), &["py"]);
    }

    #[test]
    fn workspace_package_name_returns_none() {
        let support = PythonSupport::new(Path::new("/tmp"));
        assert_eq!(
            support.workspace_package_name(Path::new("/tmp/foo.py"), Path::new("/tmp")),
            None
        );
    }
}
