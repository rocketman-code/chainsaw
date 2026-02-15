use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug)]
pub struct PythonResolver {
    source_roots: Vec<PathBuf>,
    site_packages_dirs: Vec<PathBuf>,
}

impl PythonResolver {
    pub fn new(root: &Path) -> Self {
        let mut source_roots = vec![root.to_path_buf()];
        for subdir in &["src", "lib"] {
            let candidate = root.join(subdir);
            if candidate.is_dir() {
                source_roots.push(candidate);
            }
        }

        Self {
            source_roots,
            site_packages_dirs: discover_site_packages(root),
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

    #[allow(clippy::unused_self)]
    fn resolve_relative(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        let dots = specifier.bytes().take_while(|&b| b == b'.').count();
        let module = &specifier[dots..];

        let mut base = from_dir.to_path_buf();
        for _ in 1..dots {
            base = base.parent()?.to_path_buf();
        }

        try_resolve_module(&base, module, false)
    }

    fn resolve_absolute(&self, specifier: &str) -> Option<PathBuf> {
        let all_roots = self.source_roots.iter().chain(&self.site_packages_dirs);
        // Pass 1: regular packages and modules only (no namespace)
        for root in all_roots.clone() {
            if let Some(path) = try_resolve_module(root, specifier, false) {
                return Some(path);
            }
        }
        // Pass 2: namespace packages (directories without __init__.py)
        for root in all_roots {
            if let Some(path) = try_resolve_module(root, specifier, true) {
                return Some(path);
            }
        }
        None
    }
}

fn try_resolve_module(base: &Path, dotted_name: &str, allow_namespace: bool) -> Option<PathBuf> {
    if dotted_name.is_empty() {
        let init = base.join("__init__.py");
        if init.exists() {
            return Some(init);
        }
        if allow_namespace && base.is_dir() {
            return Some(base.to_path_buf());
        }
        return None;
    }

    let rel_path = dotted_name.replace('.', "/");

    // Regular package: directory with __init__.py
    let pkg_dir = base.join(&rel_path);
    let pkg_init = pkg_dir.join("__init__.py");
    if pkg_init.exists() {
        return Some(pkg_init);
    }

    // Module file
    let module_file = base.join(format!("{rel_path}.py"));
    if module_file.exists() {
        return Some(module_file);
    }

    // Namespace package: directory exists without __init__.py
    if allow_namespace && pkg_dir.is_dir() {
        return Some(pkg_dir);
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

fn find_python(root: &Path) -> PathBuf {
    let venv_python = root.join(".venv/bin/python");
    if venv_python.exists() {
        return venv_python;
    }
    PathBuf::from("python3")
}

fn discover_site_packages_from_cfg(root: &Path) -> Option<Vec<PathBuf>> {
    const VENV_NAMES: &[&str] = &[".venv", "venv", ".env", "env"];
    for name in VENV_NAMES {
        let venv_dir = root.join(name);
        let cfg_path = venv_dir.join("pyvenv.cfg");
        if let Ok(contents) = std::fs::read_to_string(&cfg_path) {
            if let Some(version) = parse_python_version(&contents) {
                let sp = venv_dir.join(format!("lib/python{version}/site-packages"));
                if sp.is_dir() {
                    return Some(vec![sp]);
                }
            }
        }
    }
    None
}

fn parse_python_version(cfg: &str) -> Option<String> {
    for line in cfg.lines() {
        let Some((key, value)) = line.split_once('=') else {
            continue;
        };
        let key = key.trim();
        if key != "version" && key != "version_info" {
            continue;
        }
        let mut parts = value.trim().splitn(3, '.');
        let major = parts.next()?;
        let minor = parts.next()?;
        return Some(format!("{major}.{minor}"));
    }
    None
}

fn discover_site_packages(root: &Path) -> Vec<PathBuf> {
    // Fast path: parse pyvenv.cfg directly (no subprocess)
    if let Some(dirs) = discover_site_packages_from_cfg(root) {
        return dirs;
    }
    // Fallback: shell out to python
    let python = find_python(root);
    let output = Command::new(&python)
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
            source_roots: vec![root.to_path_buf()],
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
            source_roots: vec![root.clone()],
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
    fn resolve_src_layout() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let src_pkg = root.join("src/mypackage");
        fs::create_dir_all(&src_pkg).unwrap();
        fs::write(src_pkg.join("__init__.py"), "").unwrap();
        fs::write(src_pkg.join("core.py"), "x = 1").unwrap();

        let resolver = PythonResolver::new(&root);
        let result = resolver.resolve(&root, "mypackage.core");
        assert_eq!(result, Some(src_pkg.join("core.py")));
    }

    #[test]
    fn resolve_lib_layout() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let lib_pkg = root.join("lib/mypackage");
        fs::create_dir_all(&lib_pkg).unwrap();
        fs::write(lib_pkg.join("__init__.py"), "").unwrap();
        fs::write(lib_pkg.join("core.py"), "x = 1").unwrap();

        let resolver = PythonResolver::new(&root);
        let result = resolver.resolve(&root, "mypackage.core");
        assert_eq!(result, Some(lib_pkg.join("core.py")));
    }

    #[test]
    fn resolve_root_preferred_over_src() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        // Package in both root and src/ — root wins
        let root_pkg = root.join("mypackage");
        let src_pkg = root.join("src/mypackage");
        fs::create_dir_all(&root_pkg).unwrap();
        fs::create_dir_all(&src_pkg).unwrap();
        fs::write(root_pkg.join("__init__.py"), "").unwrap();
        fs::write(src_pkg.join("__init__.py"), "").unwrap();

        let resolver = PythonResolver::new(&root);
        let result = resolver.resolve(&root, "mypackage");
        assert_eq!(result, Some(root_pkg.join("__init__.py")));
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

    #[test]
    fn resolve_namespace_package() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let ns_pkg = root.join("mynamespace");
        fs::create_dir_all(&ns_pkg).unwrap();
        fs::write(ns_pkg.join("core.py"), "x = 1").unwrap();

        let resolver = make_resolver(&root);
        let result = resolver.resolve(&root, "mynamespace");
        assert_eq!(result, Some(ns_pkg.clone()));
    }

    #[test]
    fn resolve_namespace_package_submodule() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let ns_pkg = root.join("mynamespace");
        fs::create_dir_all(&ns_pkg).unwrap();
        fs::write(ns_pkg.join("core.py"), "x = 1").unwrap();

        let resolver = make_resolver(&root);
        let result = resolver.resolve(&root, "mynamespace.core");
        assert_eq!(result, Some(ns_pkg.join("core.py")));
    }

    #[test]
    fn resolve_regular_package_preferred_over_namespace() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let pkg = root.join("mypkg");
        fs::create_dir_all(&pkg).unwrap();
        fs::write(pkg.join("__init__.py"), "").unwrap();

        let resolver = make_resolver(&root);
        let result = resolver.resolve(&root, "mypkg");
        assert_eq!(result, Some(pkg.join("__init__.py")));
    }

    #[test]
    fn namespace_in_source_root_does_not_shadow_site_packages() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let ns_dir = root.join("packaging");
        fs::create_dir_all(&ns_dir).unwrap();
        let sp = root.join("site-packages");
        let real_pkg = sp.join("packaging");
        fs::create_dir_all(&real_pkg).unwrap();
        fs::write(real_pkg.join("__init__.py"), "").unwrap();

        let resolver = PythonResolver {
            source_roots: vec![root.clone()],
            site_packages_dirs: vec![sp],
        };
        let result = resolver.resolve(&root, "packaging");
        assert_eq!(result, Some(real_pkg.join("__init__.py")));
    }

    #[test]
    fn discover_from_pyvenv_cfg_version() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let venv = root.join(".venv");
        let sp = venv.join("lib/python3.12/site-packages");
        fs::create_dir_all(&sp).unwrap();
        fs::write(
            venv.join("pyvenv.cfg"),
            "home = /usr/bin\nversion = 3.12.8\n",
        )
        .unwrap();

        let result = discover_site_packages_from_cfg(&root);
        assert_eq!(result, Some(vec![sp]));
    }

    #[test]
    fn discover_from_pyvenv_cfg_version_info() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let venv = root.join(".venv");
        let sp = venv.join("lib/python3.11/site-packages");
        fs::create_dir_all(&sp).unwrap();
        fs::write(
            venv.join("pyvenv.cfg"),
            "home = /usr/bin\nversion_info = 3.11.5\n",
        )
        .unwrap();

        let result = discover_site_packages_from_cfg(&root);
        assert_eq!(result, Some(vec![sp]));
    }

    #[test]
    fn discover_from_pyvenv_cfg_venv_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let venv = root.join("venv");
        let sp = venv.join("lib/python3.10/site-packages");
        fs::create_dir_all(&sp).unwrap();
        fs::write(
            venv.join("pyvenv.cfg"),
            "home = /usr/bin\nversion = 3.10.0\n",
        )
        .unwrap();

        let result = discover_site_packages_from_cfg(&root);
        assert_eq!(result, Some(vec![sp]));
    }

    #[test]
    fn discover_from_pyvenv_cfg_no_venv_returns_none() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let result = discover_site_packages_from_cfg(&root);
        assert_eq!(result, None);
    }
}
