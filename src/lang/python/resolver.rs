use std::path::{Path, PathBuf};

pub struct PythonResolver {
    site_packages_dirs: Vec<PathBuf>,
}

impl PythonResolver {
    pub fn new(_root: &Path) -> Self {
        Self {
            site_packages_dirs: Vec::new(),
        }
    }

    pub fn resolve(&self, _from_dir: &Path, _specifier: &str) -> Option<PathBuf> {
        None
    }

    pub fn site_packages(&self) -> &[PathBuf] {
        &self.site_packages_dirs
    }
}

pub fn package_name_from_path(_path: &Path, _site_packages: &[PathBuf]) -> Option<String> {
    None
}
