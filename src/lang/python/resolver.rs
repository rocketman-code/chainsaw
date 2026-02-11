use std::path::{Path, PathBuf};
use std::process::Command;

pub struct PythonResolver {
    project_root: PathBuf,
    site_packages_dirs: Vec<PathBuf>,
}

impl PythonResolver {
    pub fn new(root: &Path) -> Self {
        Self {
            project_root: root.to_path_buf(),
            site_packages_dirs: discover_site_packages(),
        }
    }

    pub fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        if specifier.starts_with('.') {
            self.resolve_relative(from_dir, specifier)
        } else {
            self.resolve_absolute(specifier)
        }
    }

    pub fn site_packages(&self) -> &[PathBuf] {
        &self.site_packages_dirs
    }

    fn resolve_relative(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        let dots = specifier.bytes().take_while(|&b| b == b'.').count();
        let module = &specifier[dots..];

        let mut base = from_dir.to_path_buf();
        for _ in 1..dots {
            base = base.parent()?.to_path_buf();
        }

        try_resolve_module(&base, module)
    }

    fn resolve_absolute(&self, specifier: &str) -> Option<PathBuf> {
        if let Some(path) = try_resolve_module(&self.project_root, specifier) {
            return Some(path);
        }
        for sp in &self.site_packages_dirs {
            if let Some(path) = try_resolve_module(sp, specifier) {
                return Some(path);
            }
        }
        None
    }
}

fn try_resolve_module(base: &Path, dotted_name: &str) -> Option<PathBuf> {
    if dotted_name.is_empty() {
        let init = base.join("__init__.py");
        return if init.exists() { Some(init) } else { None };
    }

    let rel_path = dotted_name.replace('.', "/");

    let pkg_init = base.join(&rel_path).join("__init__.py");
    if pkg_init.exists() {
        return Some(pkg_init);
    }

    let module_file = base.join(format!("{rel_path}.py"));
    if module_file.exists() {
        return Some(module_file);
    }

    None
}

pub fn package_name_from_path(path: &Path, site_packages: &[PathBuf]) -> Option<String> {
    for sp in site_packages {
        if let Ok(relative) = path.strip_prefix(sp) {
            let top = relative.components().next()?;
            let name = top.as_os_str().to_str()?;
            if name.ends_with(".dist-info") || name.ends_with(".egg-info") {
                continue;
            }
            return Some(name.to_string());
        }
    }
    None
}

fn discover_site_packages() -> Vec<PathBuf> {
    let output = Command::new("python3")
        .args(["-c", "import site; print('\\n'.join(site.getsitepackages()))"])
        .output();

    match output {
        Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout)
            .lines()
            .map(PathBuf::from)
            .filter(|p| p.exists())
            .collect(),
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn setup_project(tmp: &Path) {
        let pkg = tmp.join("mypackage");
        let utils = pkg.join("utils");
        fs::create_dir_all(&utils).unwrap();
        fs::write(pkg.join("__init__.py"), "").unwrap();
        fs::write(pkg.join("core.py"), "x = 1").unwrap();
        fs::write(utils.join("__init__.py"), "").unwrap();
        fs::write(utils.join("helpers.py"), "y = 2").unwrap();
        fs::write(tmp.join("standalone.py"), "z = 3").unwrap();
    }

    fn make_resolver(root: &Path) -> PythonResolver {
        PythonResolver {
            project_root: root.to_path_buf(),
            site_packages_dirs: vec![],
        }
    }

    #[test]
    fn resolve_module_file() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let result = resolver.resolve(&root, "standalone");
        assert_eq!(result, Some(root.join("standalone.py")));
    }

    #[test]
    fn resolve_package_init() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let result = resolver.resolve(&root, "mypackage");
        assert_eq!(result, Some(root.join("mypackage/__init__.py")));
    }

    #[test]
    fn resolve_dotted_module() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let result = resolver.resolve(&root, "mypackage.core");
        assert_eq!(result, Some(root.join("mypackage/core.py")));
    }

    #[test]
    fn resolve_dotted_package() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let result = resolver.resolve(&root, "mypackage.utils");
        assert_eq!(result, Some(root.join("mypackage/utils/__init__.py")));
    }

    #[test]
    fn resolve_relative_sibling() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let from_dir = root.join("mypackage");
        let result = resolver.resolve(&from_dir, ".utils");
        assert_eq!(result, Some(root.join("mypackage/utils/__init__.py")));
    }

    #[test]
    fn resolve_relative_parent() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let from_dir = root.join("mypackage/utils");
        let result = resolver.resolve(&from_dir, "..core");
        assert_eq!(result, Some(root.join("mypackage/core.py")));
    }

    #[test]
    fn resolve_site_packages() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let sp = root.join("site-packages");
        let requests = sp.join("requests");
        fs::create_dir_all(&requests).unwrap();
        fs::write(requests.join("__init__.py"), "").unwrap();

        let resolver = PythonResolver {
            project_root: root.clone(),
            site_packages_dirs: vec![sp.clone()],
        };

        let result = resolver.resolve(&root, "requests");
        assert_eq!(result, Some(sp.join("requests/__init__.py")));
    }

    #[test]
    fn resolve_not_found() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        setup_project(&root);
        let resolver = make_resolver(&root);

        let result = resolver.resolve(&root, "nonexistent");
        assert_eq!(result, None);
    }

    #[test]
    fn package_name_from_site_packages() {
        let sp = PathBuf::from("/fake/site-packages");
        let path = sp.join("requests/api.py");
        let result = package_name_from_path(&path, &[sp]);
        assert_eq!(result, Some("requests".to_string()));
    }

    #[test]
    fn package_name_not_in_site_packages() {
        let sp = PathBuf::from("/fake/site-packages");
        let path = PathBuf::from("/other/place/module.py");
        let result = package_name_from_path(&path, &[sp]);
        assert_eq!(result, None);
    }

    #[test]
    fn package_name_skips_dist_info() {
        let sp = PathBuf::from("/fake/site-packages");
        let path = sp.join("requests-2.31.0.dist-info/METADATA");
        let result = package_name_from_path(&path, &[sp]);
        assert_eq!(result, None);
    }
}
