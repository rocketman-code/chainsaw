use std::collections::HashMap;
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

        source_roots.extend(scan_conftest_paths(root));

        let mut site_packages_dirs = discover_site_packages(root);
        let pth_dirs: Vec<PathBuf> = site_packages_dirs
            .iter()
            .flat_map(|sp| parse_pth_files(sp))
            .collect();
        site_packages_dirs.extend(pth_dirs);
        let vendor_dirs = scan_site_packages_paths(&site_packages_dirs);
        site_packages_dirs.extend(vendor_dirs);
        Self {
            source_roots,
            site_packages_dirs,
        }
    }

    pub fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf> {
        if specifier.starts_with('.') {
            self.resolve_relative(from_dir, specifier)
        } else {
            self.resolve_absolute(specifier)
        }
    }

    pub fn source_roots(&self) -> &[PathBuf] {
        &self.source_roots
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
        let components: Vec<&str> = specifier.split('.').collect();
        let all_roots: Vec<&Path> = self
            .source_roots
            .iter()
            .chain(&self.site_packages_dirs)
            .map(PathBuf::as_path)
            .collect();

        // For dotted imports, resolve component-by-component matching Python's
        // recursive import algorithm (_find_and_load_unlocked in _bootstrap.py).
        // A regular package (__init__.py) at any level locks __path__ to that
        // single root — subsequent components can only be found there.
        if components.len() > 1 {
            let mut valid_roots = all_roots.clone();
            for (i, _) in components.iter().enumerate().take(components.len() - 1) {
                let prefix: String = components[..=i].join("/");
                let mut locked_root = None;
                for &root in &valid_roots {
                    if root.join(&prefix).join("__init__.py").exists() {
                        locked_root = Some(root);
                        break;
                    }
                }
                if let Some(root) = locked_root {
                    valid_roots = vec![root];
                } else {
                    // Namespace: keep only roots where the directory exists
                    valid_roots.retain(|root| root.join(&prefix).is_dir());
                }
                if valid_roots.is_empty() {
                    return None;
                }
                if valid_roots.len() == 1 {
                    break;
                }
            }

            // Resolve the leaf: mirror non-dotted pass structure.
            // Source roots: __init__.py and .py only (no C extension probing).
            // Site-packages: __init__.py, C extension, then .py.
            let rel_path = components.join("/");
            for &root in &valid_roots {
                let pkg_init = root.join(&rel_path).join("__init__.py");
                if pkg_init.exists() {
                    return Some(pkg_init);
                }
                if self.site_packages_dirs.iter().any(|sp| sp.as_path() == root) {
                    if let Some(ext) = find_c_extension(root, &rel_path) {
                        return Some(ext);
                    }
                }
                let module_file = root.join(format!("{rel_path}.py"));
                if module_file.exists() {
                    return Some(module_file);
                }
            }
            // Namespace package for the full path
            for &root in &valid_roots {
                let pkg_dir = root.join(&rel_path);
                if pkg_dir.is_dir() {
                    return Some(pkg_dir);
                }
            }
            return None;
        }

        // Non-dotted: check all roots in order.
        let rel_path = &components[0];

        // Pass 1: source roots — regular packages and .py modules
        for root in &self.source_roots {
            if let Some(path) = try_resolve_module(root, rel_path, false) {
                return Some(path);
            }
        }

        // Pass 2: site-packages — __init__.py, then C extension, then .py
        for sp in &self.site_packages_dirs {
            if let Some(path) = self.resolve_in_root(sp, rel_path) {
                return Some(path);
            }
        }

        // Pass 3: namespace packages (directories without __init__.py)
        for root in &all_roots {
            let pkg_dir = root.join(rel_path);
            if pkg_dir.is_dir() {
                return Some(pkg_dir);
            }
        }
        None
    }

    /// Resolve a module within a single root, using Python's per-directory
    /// loader order: __init__.py > C extension (.so/.pyd) > source (.py).
    #[allow(clippy::unused_self)]
    fn resolve_in_root(&self, root: &Path, rel_path: &str) -> Option<PathBuf> {
        let pkg_init = root.join(rel_path).join("__init__.py");
        if pkg_init.exists() {
            return Some(pkg_init);
        }
        if let Some(ext) = find_c_extension(root, rel_path) {
            return Some(ext);
        }
        let module_file = root.join(format!("{rel_path}.py"));
        if module_file.exists() {
            return Some(module_file);
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

fn find_c_extension(base: &Path, rel_path: &str) -> Option<PathBuf> {
    let (parent, leaf) = rel_path.rfind('/').map_or_else(
        || (base.to_path_buf(), rel_path),
        |i| (base.join(&rel_path[..i]), &rel_path[i + 1..]),
    );
    let prefix = format!("{leaf}.");
    for entry in std::fs::read_dir(&parent).ok()?.flatten() {
        let file_name = entry.file_name();
        let Some(name) = file_name.to_str() else {
            continue;
        };
        if !name.starts_with(&prefix) {
            continue;
        }
        let path = entry.path();
        let is_c_ext = path
            .extension()
            .is_some_and(|ext| ext == "so" || ext == "pyd");
        if is_c_ext {
            return Some(path);
        }
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
            // Strip C extension platform suffix: ciso8601.cpython-314-darwin.so -> ciso8601
            let ext = std::path::Path::new(name).extension().and_then(|e| e.to_str());
            let stem = if matches!(ext, Some("so" | "pyd")) {
                // split_once always succeeds here since name ends with .so/.pyd
                name.split_once('.').unwrap().0
            } else {
                name
            };
            return Some(stem.to_string());
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
        Ok(out) => {
            eprintln!(
                "warning: failed to discover site-packages: {} exited with {}",
                python.display(),
                out.status,
            );
            Vec::new()
        }
        Err(e) => {
            eprintln!(
                "warning: failed to discover site-packages: {}: {e}",
                python.display(),
            );
            Vec::new()
        }
    }
}

fn parse_pth_files(site_packages: &Path) -> Vec<PathBuf> {
    let Ok(entries) = std::fs::read_dir(site_packages) else {
        return Vec::new();
    };
    let mut paths = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("pth") {
            continue;
        }
        let Ok(contents) = std::fs::read_to_string(&path) else {
            continue;
        };
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with("import ") {
                continue;
            }
            let dir = if Path::new(line).is_absolute() {
                PathBuf::from(line)
            } else {
                site_packages.join(line)
            };
            if dir.is_dir() {
                paths.push(dir);
            }
        }
    }
    paths
}

const CONFTEST_SEARCH_DIRS: &[&str] = &["test", "tests", "testing", "spec", "specs"];

fn scan_conftest_paths(root: &Path) -> Vec<PathBuf> {
    // Check root-level conftest.py and common test directories only.
    // Walking the entire project tree is too expensive for large codebases.
    let mut files = Vec::new();

    let root_conftest = root.join("conftest.py");
    if root_conftest.exists() {
        files.push(root_conftest);
    }

    for dir in CONFTEST_SEARCH_DIRS {
        let test_dir = root.join(dir);
        if test_dir.is_dir() {
            walk_for_conftest(&test_dir, &mut files);
        }
    }

    let mut paths = Vec::new();
    for file in &files {
        let Ok(source) = std::fs::read_to_string(file) else {
            continue;
        };
        paths.extend(extract_sys_path_additions(file, &source));
    }
    paths
}

fn scan_site_packages_paths(site_packages: &[PathBuf]) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    for sp in site_packages {
        // Only scan top-level package __init__.py files. Vendor dirs are
        // set up in the top-level package's __init__.py, not in sub-packages.
        // Recursing all of site-packages is too expensive.
        let Ok(entries) = std::fs::read_dir(sp) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if name == "__pycache__"
                || name.ends_with(".dist-info")
                || name.ends_with(".egg-info")
            {
                continue;
            }
            let init = path.join("__init__.py");
            let Ok(source) = std::fs::read_to_string(&init) else {
                continue;
            };
            paths.extend(extract_sys_path_additions(&init, &source));
        }
    }
    paths
}

const CONFTEST_SKIP_DIRS: &[&str] = &[
    "__pycache__",
    ".git",
    ".venv",
    "venv",
    ".env",
    "env",
    "node_modules",
    ".mypy_cache",
    ".pytest_cache",
    ".tox",
    ".eggs",
];

fn walk_for_conftest(dir: &Path, results: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if path.is_dir() {
            if !CONFTEST_SKIP_DIRS.contains(&name) {
                walk_for_conftest(&path, results);
            }
        } else if name == "conftest.py" {
            results.push(path);
        }
    }
}

fn extract_sys_path_additions(file_path: &Path, source: &str) -> Vec<PathBuf> {
    if !source.contains("sys.path") || !source.contains("__file__") {
        return Vec::new();
    }

    let mut parser = tree_sitter::Parser::new();
    let Ok(()) = parser.set_language(&tree_sitter_python::LANGUAGE.into()) else {
        return Vec::new();
    };
    let Some(tree) = parser.parse(source, None) else {
        return Vec::new();
    };

    let src = source.as_bytes();
    let root = tree.root_node();
    let assignments = collect_assignments(root, src);

    let mut paths = Vec::new();
    find_sys_path_calls(root, src, file_path, &assignments, &mut paths);

    paths
        .into_iter()
        .filter_map(|p| p.canonicalize().ok())
        .filter(|p| p.is_dir())
        .collect()
}

fn collect_assignments<'a>(
    node: tree_sitter::Node<'a>,
    src: &[u8],
) -> HashMap<String, tree_sitter::Node<'a>> {
    let mut map = HashMap::new();
    collect_assignments_recursive(node, src, &mut map);
    map
}

fn collect_assignments_recursive<'a>(
    node: tree_sitter::Node<'a>,
    src: &[u8],
    map: &mut HashMap<String, tree_sitter::Node<'a>>,
) {
    let is_assignment = node.kind() == "expression_statement"
        && node.named_child(0).is_some_and(|n| n.kind() == "assignment");
    if is_assignment {
        let expr = node.named_child(0).unwrap();
        let left = expr.child_by_field_name("left");
        let right = expr.child_by_field_name("right");
        if let (Some(l), Some(r)) = (left, right) {
            if l.kind() == "identifier" {
                map.insert(node_text(l, src).to_string(), r);
            }
        }
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_assignments_recursive(child, src, map);
    }
}

fn find_sys_path_calls(
    node: tree_sitter::Node,
    src: &[u8],
    file_path: &Path,
    assignments: &HashMap<String, tree_sitter::Node>,
    paths: &mut Vec<PathBuf>,
) {
    if node.kind() == "call" {
        if let Some(path) = try_extract_sys_path_arg(node, src, file_path, assignments) {
            paths.push(path);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        find_sys_path_calls(child, src, file_path, assignments, paths);
    }
}

fn try_extract_sys_path_arg(
    node: tree_sitter::Node,
    src: &[u8],
    file_path: &Path,
    assignments: &HashMap<String, tree_sitter::Node>,
) -> Option<PathBuf> {
    let func = node.child_by_field_name("function")?;
    let func_text = node_text(func, src);
    let arg_idx = match func_text {
        "sys.path.insert" => 1,
        "sys.path.append" => 0,
        _ => return None,
    };
    let args = node.child_by_field_name("arguments")?;
    let arg = args.named_child(arg_idx)?;
    eval_path_expr(arg, src, file_path, assignments)
}

fn eval_path_expr(
    node: tree_sitter::Node,
    src: &[u8],
    file_path: &Path,
    assignments: &HashMap<String, tree_sitter::Node>,
) -> Option<PathBuf> {
    match node.kind() {
        "identifier" => {
            let name = node_text(node, src);
            if name == "__file__" {
                Some(file_path.to_path_buf())
            } else {
                let expr = assignments.get(name)?;
                eval_path_expr(*expr, src, file_path, assignments)
            }
        }
        "string" => extract_string_content(node, src).map(PathBuf::from),
        "call" => {
            let func = node.child_by_field_name("function")?;
            let args = node.child_by_field_name("arguments")?;
            let func_text = node_text(func, src);

            match func_text {
                "os.path.dirname" => {
                    let arg = args.named_child(0)?;
                    let inner = eval_path_expr(arg, src, file_path, assignments)?;
                    inner.parent().map(Path::to_path_buf)
                }
                "os.path.join" => {
                    let count = args.named_child_count();
                    let first = args.named_child(0)?;
                    let mut result = eval_path_expr(first, src, file_path, assignments)?;
                    for i in 1..count {
                        let arg = args.named_child(i)?;
                        let segment = eval_path_expr(arg, src, file_path, assignments)?;
                        result = result.join(segment);
                    }
                    Some(result)
                }
                "Path" | "str" => {
                    let arg = args.named_child(0)?;
                    eval_path_expr(arg, src, file_path, assignments)
                }
                _ if func.kind() == "attribute" => {
                    let attr = func.child_by_field_name("attribute")?;
                    match node_text(attr, src) {
                        "as_posix" | "resolve" => {
                            let obj = func.child_by_field_name("object")?;
                            eval_path_expr(obj, src, file_path, assignments)
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        "attribute" => {
            let attr = node.child_by_field_name("attribute")?;
            if node_text(attr, src) == "parent" {
                let obj = node.child_by_field_name("object")?;
                let inner = eval_path_expr(obj, src, file_path, assignments)?;
                inner.parent().map(Path::to_path_buf)
            } else {
                None
            }
        }
        "binary_operator" => {
            let op = node.child_by_field_name("operator")?;
            if node_text(op, src) != "/" {
                return None;
            }
            let left = node.child_by_field_name("left")?;
            let right = node.child_by_field_name("right")?;
            let base = eval_path_expr(left, src, file_path, assignments)?;
            let segment = eval_path_expr(right, src, file_path, assignments)?;
            Some(base.join(segment))
        }
        "parenthesized_expression" => {
            let inner = node.named_child(0)?;
            eval_path_expr(inner, src, file_path, assignments)
        }
        _ => None,
    }
}

fn node_text<'a>(node: tree_sitter::Node, src: &'a [u8]) -> &'a str {
    std::str::from_utf8(&src[node.byte_range()]).unwrap_or("")
}

fn extract_string_content(node: tree_sitter::Node, src: &[u8]) -> Option<String> {
    let mut cursor = node.walk();
    if let Some(child) = node.children(&mut cursor).find(|c| c.kind() == "string_content") {
        return Some(node_text(child, src).to_string());
    }
    let text = node_text(node, src);
    let s = text
        .trim_start_matches("\"\"\"")
        .trim_end_matches("\"\"\"")
        .trim_start_matches("'''")
        .trim_end_matches("'''")
        .trim_start_matches('"')
        .trim_end_matches('"')
        .trim_start_matches('\'')
        .trim_end_matches('\'');
    (!s.is_empty()).then(|| s.to_string())
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
    fn package_name_strips_c_extension_suffix() {
        let sp = PathBuf::from("/fake/site-packages");
        // Top-level C extension like ciso8601
        let path = sp.join("ciso8601.cpython-314-darwin.so");
        let result = package_name_from_path(&path, std::slice::from_ref(&sp));
        assert_eq!(result, Some("ciso8601".to_string()));

        // Top-level C extension with underscores
        let path = sp.join("_cffi_backend.cpython-314-darwin.so");
        let result = package_name_from_path(&path, std::slice::from_ref(&sp));
        assert_eq!(result, Some("_cffi_backend".to_string()));

        // .pyd on Windows
        let path = sp.join("ciso8601.cp314-win_amd64.pyd");
        let result = package_name_from_path(&path, &[sp]);
        assert_eq!(result, Some("ciso8601".to_string()));
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
        assert_eq!(result, Some(ns_pkg));
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
    fn discover_from_pyvenv_cfg_no_venv_returns_none() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let result = discover_site_packages_from_cfg(&root);
        assert_eq!(result, None);
    }

    #[test]
    fn pth_file_absolute_path() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let sp = root.join("site-packages");
        fs::create_dir_all(&sp).unwrap();

        let extra = root.join("extra-src");
        fs::create_dir_all(&extra).unwrap();

        fs::write(sp.join("myproject.pth"), extra.to_string_lossy().as_ref()).unwrap();

        let result = parse_pth_files(&sp);
        assert_eq!(result, vec![extra]);
    }

    #[test]
    fn pth_file_relative_path() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let sp = root.join("site-packages");
        let rel_dir = sp.join("my-subdir");
        fs::create_dir_all(&rel_dir).unwrap();

        fs::write(sp.join("thing.pth"), "my-subdir\n").unwrap();

        let result = parse_pth_files(&sp);
        assert_eq!(result, vec![rel_dir]);
    }

    #[test]
    fn pth_file_skips_import_lines() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let sp = root.join("site-packages");
        fs::create_dir_all(&sp).unwrap();

        let extra = root.join("real-dir");
        fs::create_dir_all(&extra).unwrap();

        fs::write(
            sp.join("mixed.pth"),
            format!("import _virtualenv\n# comment\n{}\n", extra.display()),
        )
        .unwrap();

        let result = parse_pth_files(&sp);
        assert_eq!(result, vec![extra]);
    }

    #[test]
    fn pth_file_skips_nonexistent_dirs() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let sp = root.join("site-packages");
        fs::create_dir_all(&sp).unwrap();

        fs::write(sp.join("gone.pth"), "/nonexistent/path/here\n").unwrap();

        let result = parse_pth_files(&sp);
        assert!(result.is_empty());
    }

    #[test]
    fn resolve_through_pth_path() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let sp = root.join("site-packages");
        fs::create_dir_all(&sp).unwrap();

        let ext_src = root.join("external-src");
        let ext_pkg = ext_src.join("mypkg");
        fs::create_dir_all(&ext_pkg).unwrap();
        fs::write(ext_pkg.join("__init__.py"), "").unwrap();

        // .pth file points to external-src
        fs::write(sp.join("editable.pth"), ext_src.to_string_lossy().as_ref()).unwrap();

        // Construct resolver WITH .pth scanning via helper
        let mut site_packages_dirs = vec![sp.clone()];
        let pth_dirs = parse_pth_files(&sp);
        site_packages_dirs.extend(pth_dirs);

        let resolver = PythonResolver {
            source_roots: vec![root.clone()],
            site_packages_dirs,
        };
        let result = resolver.resolve(&root, "mypkg");
        assert_eq!(result, Some(ext_pkg.join("__init__.py")));
    }

    #[test]
    fn resolve_c_extension_module() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        // Create a package with a C extension submodule
        let yaml_dir = root.join("site-packages/yaml");
        fs::create_dir_all(&yaml_dir).unwrap();
        fs::write(yaml_dir.join("__init__.py"), "").unwrap();
        // Simulate a C extension file
        fs::write(yaml_dir.join("_yaml.cpython-312-darwin.so"), "").unwrap();

        let resolver = PythonResolver {
            source_roots: vec![root.clone()],
            site_packages_dirs: vec![root.join("site-packages")],
        };

        let result = resolver.resolve(&root, "yaml._yaml");
        assert!(result.is_some(), "should resolve C extension module");
        let found = result.unwrap();
        assert!(
            found.to_string_lossy().contains("_yaml.cpython-312-darwin.so"),
            "should resolve to the .so file, got: {}",
            found.display()
        );
    }

    #[test]
    fn resolve_c_extension_preferred_over_py_in_site_packages() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let pkg_dir = root.join("site-packages/pkg");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(pkg_dir.join("__init__.py"), "").unwrap();
        // Both .py and .so exist — .so should win (matches Python's loader order)
        fs::write(pkg_dir.join("mod.py"), "x = 1").unwrap();
        fs::write(pkg_dir.join("mod.cpython-312-darwin.so"), "").unwrap();

        let resolver = PythonResolver {
            source_roots: vec![root.clone()],
            site_packages_dirs: vec![root.join("site-packages")],
        };

        let result = resolver.resolve(&root, "pkg.mod");
        let found = result.unwrap();
        assert!(
            found.to_string_lossy().contains("mod.cpython-312-darwin.so"),
            "expected .so, got: {}",
            found.display()
        );
    }

    #[test]
    fn resolve_top_level_c_extension_over_namespace_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let sp = root.join("site-packages");
        // Top-level .so (like ciso8601)
        fs::create_dir_all(&sp).unwrap();
        fs::write(sp.join("cmod.cpython-312-darwin.so"), "").unwrap();
        // Directory with only type stubs (no __init__.py) — not a real package
        let stub_dir = sp.join("cmod");
        fs::create_dir_all(&stub_dir).unwrap();
        fs::write(stub_dir.join("__init__.pyi"), "").unwrap();

        let resolver = PythonResolver {
            source_roots: vec![root.clone()],
            site_packages_dirs: vec![sp],
        };

        let result = resolver.resolve(&root, "cmod");
        let found = result.unwrap();
        assert!(
            found.to_string_lossy().contains("cmod.cpython-312-darwin.so"),
            "expected .so over namespace dir, got: {}",
            found.display()
        );
    }

    #[test]
    fn dotted_import_source_root_skips_c_extension_probing() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        // Source root has a package with a C extension .so (no .py)
        let pkg = root.join("mypkg");
        fs::create_dir_all(&pkg).unwrap();
        fs::write(pkg.join("__init__.py"), "").unwrap();
        fs::write(pkg.join("ext.cpython-312-darwin.so"), "").unwrap();

        let resolver = make_resolver(&root);

        // Non-dotted "ext" in source root: try_resolve_module skips C extensions
        let non_dotted = resolver.resolve(&root, "ext");
        assert_eq!(non_dotted, None, "non-dotted should not find C ext in source root");

        // Dotted "mypkg.ext" should be consistent: also skip C extensions
        let dotted = resolver.resolve(&root, "mypkg.ext");
        assert_eq!(
            dotted, None,
            "dotted import should not probe C extensions in source roots"
        );
    }

    #[test]
    fn regular_package_in_source_prevents_site_packages_submodule() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        // Source root has foo as a regular package
        let src = root.join("src");
        let foo_src = src.join("foo");
        fs::create_dir_all(&foo_src).unwrap();
        fs::write(foo_src.join("__init__.py"), "").unwrap();

        // Site-packages has foo with a different sub-module
        let sp = root.join("site-packages");
        let foo_sp = sp.join("foo");
        fs::create_dir_all(&foo_sp).unwrap();
        fs::write(foo_sp.join("__init__.py"), "").unwrap();
        fs::write(foo_sp.join("extras.py"), "x = 1").unwrap();

        let resolver = PythonResolver {
            source_roots: vec![src],
            site_packages_dirs: vec![sp],
        };

        // foo.extras only exists in site-packages, but foo is owned by source root
        let result = resolver.resolve(&root, "foo.extras");
        assert_eq!(result, None, "should not leak sub-modules from shadowed site-packages");
    }

    #[test]
    fn intermediate_regular_package_prevents_cross_root_resolution() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        // root_a: ns/ (namespace) -> pkg/ (regular, has __init__.py) -> mod.py
        let root_a = root.join("root_a");
        let ns_pkg_a = root_a.join("ns/pkg");
        fs::create_dir_all(&ns_pkg_a).unwrap();
        fs::write(ns_pkg_a.join("__init__.py"), "").unwrap();
        fs::write(ns_pkg_a.join("mod.py"), "x = 1").unwrap();

        // root_b: ns/ (namespace) -> pkg/ (no __init__.py) -> other.py
        let root_b = root.join("root_b");
        let ns_pkg_b = root_b.join("ns/pkg");
        fs::create_dir_all(&ns_pkg_b).unwrap();
        fs::write(ns_pkg_b.join("other.py"), "x = 1").unwrap();

        let resolver = PythonResolver {
            source_roots: vec![root_a, root_b],
            site_packages_dirs: vec![],
        };

        // ns is namespace (merged across roots)
        // ns.pkg is regular in root_a (locks __path__)
        // ns.pkg.other only in root_b — Python would NOT find it
        let result = resolver.resolve(&root, "ns.pkg.other");
        assert_eq!(result, None, "intermediate regular package should lock resolution");

        // ns.pkg.mod is in root_a — should still be found
        let result = resolver.resolve(&root, "ns.pkg.mod");
        assert_eq!(result, Some(root.join("root_a/ns/pkg/mod.py")));
    }

    #[test]
    fn sys_path_pathlib_vendor() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let pkg_dir = root.join("site-packages/mypkg/core");
        let vendor = pkg_dir.join("_vendor");
        fs::create_dir_all(&vendor).unwrap();
        fs::write(vendor.join("somelib.py"), "x = 1").unwrap();
        fs::write(
            pkg_dir.join("__init__.py"),
            r#"
import sys
from pathlib import Path
__vendor_site__ = (Path(__file__).parent / "_vendor").as_posix()
if __vendor_site__ not in sys.path:
    sys.path.insert(0, __vendor_site__)
"#,
        )
        .unwrap();

        let init_path = pkg_dir.join("__init__.py");
        let source = fs::read_to_string(&init_path).unwrap();
        let result = extract_sys_path_additions(&init_path, &source);
        assert_eq!(result, vec![vendor]);
    }

    #[test]
    fn sys_path_os_dirname_chain() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        // Simulate: conftest.py 3 levels deep, adds root/test/lib
        let conftest_dir = root.join("test/units/mytest");
        fs::create_dir_all(&conftest_dir).unwrap();
        let test_lib = root.join("test/lib");
        let test_pkg = test_lib.join("mytest_pkg");
        fs::create_dir_all(&test_pkg).unwrap();
        fs::write(test_pkg.join("__init__.py"), "").unwrap();

        let conftest = conftest_dir.join("conftest.py");
        fs::write(
            &conftest,
            r"
import os
import sys
test_lib = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), 'lib')
sys.path.insert(0, test_lib)
",
        )
        .unwrap();

        let source = fs::read_to_string(&conftest).unwrap();
        let result = extract_sys_path_additions(&conftest, &source);
        assert_eq!(result, vec![test_lib]);
    }

    #[test]
    fn sys_path_no_modification() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let init = root.join("__init__.py");
        fs::write(&init, "x = 1\n").unwrap();

        let source = fs::read_to_string(&init).unwrap();
        let result = extract_sys_path_additions(&init, &source);
        assert!(result.is_empty());
    }

    #[test]
    fn sys_path_conftest_discovers_source_root() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        // Create test/lib with a package
        let test_lib = root.join("test/lib");
        let test_pkg = test_lib.join("testpkg");
        fs::create_dir_all(&test_pkg).unwrap();
        fs::write(test_pkg.join("__init__.py"), "").unwrap();

        // Create conftest.py that adds test/lib
        let conftest_dir = root.join("test/units");
        fs::create_dir_all(&conftest_dir).unwrap();
        fs::write(
            conftest_dir.join("conftest.py"),
            r"
import os, sys
test_lib = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'lib')
sys.path.insert(0, test_lib)
",
        )
        .unwrap();

        let paths = scan_conftest_paths(&root);
        assert_eq!(paths, vec![test_lib]);
    }

    #[test]
    fn sys_path_init_vendor_discovered() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let sp = root.join("site-packages");
        let pkg = sp.join("mypkg");
        let vendor = pkg.join("_vendor");
        fs::create_dir_all(&vendor).unwrap();
        fs::write(vendor.join("somelib.py"), "x = 1").unwrap();
        fs::write(
            pkg.join("__init__.py"),
            r#"
import sys
from pathlib import Path
__vendor_site__ = (Path(__file__).parent / "_vendor").as_posix()
sys.path.insert(0, __vendor_site__)
"#,
        )
        .unwrap();

        let paths = scan_site_packages_paths(&[sp]);
        assert_eq!(paths, vec![vendor]);
    }

    #[test]
    fn sys_path_vendor_fallback_for_missing_packages() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let sp = root.join("site-packages");
        let pkg = sp.join("mypkg");
        let vendor = pkg.join("_vendor");
        fs::create_dir_all(&vendor).unwrap();

        // A package only in vendor (not in regular site-packages)
        let vendor_only = vendor.join("vendoronly");
        fs::create_dir_all(&vendor_only).unwrap();
        fs::write(vendor_only.join("__init__.py"), "").unwrap();

        // A real package in regular site-packages
        let real_pkg = sp.join("realpkg");
        fs::create_dir_all(&real_pkg).unwrap();
        fs::write(real_pkg.join("__init__.py"), "").unwrap();

        fs::write(
            pkg.join("__init__.py"),
            r#"
import sys
from pathlib import Path
sys.path.insert(0, (Path(__file__).parent / "_vendor").as_posix())
"#,
        )
        .unwrap();

        // Build resolver: vendor dirs are appended after site-packages
        let mut site_packages_dirs = vec![sp];
        let vendor_dirs = scan_site_packages_paths(&site_packages_dirs);
        site_packages_dirs.extend(vendor_dirs);

        let resolver = PythonResolver {
            source_roots: vec![root.clone()],
            site_packages_dirs,
        };

        // Vendor-only package is found via fallback
        let result = resolver.resolve(&root, "vendoronly");
        assert_eq!(result, Some(vendor_only.join("__init__.py")));

        // Real package resolves to regular site-packages, not vendor
        let result = resolver.resolve(&root, "realpkg");
        assert_eq!(result, Some(real_pkg.join("__init__.py")));
    }

    // ========================================================================
    // CPython test_namespace_pkgs conformance
    //
    // These tests use CPython's own test fixtures from
    // Lib/test/test_importlib/namespace_pkgs/ to verify our resolver matches
    // Python's import semantics. Each test corresponds to a test class in
    // CPython's test_namespace_pkgs.py.
    // ========================================================================

    const CPYTHON_NS_FIXTURES: &str = concat!(
        env!("HOME"),
        "/dev/python/cpython/Lib/test/test_importlib/namespace_pkgs"
    );

    fn cpython_resolver(paths: &[&str]) -> (PythonResolver, PathBuf) {
        let root = PathBuf::from(CPYTHON_NS_FIXTURES);
        let source_roots: Vec<PathBuf> = paths.iter().map(|p| root.join(p)).collect();
        let resolver = PythonResolver {
            source_roots,
            site_packages_dirs: vec![],
        };
        (resolver, root)
    }

    // CPython: SingleNamespacePackage (paths = ['portion1'])
    #[test]
    fn cpython_single_namespace_package() {
        let (resolver, root) = cpython_resolver(&["portion1"]);
        // foo.one resolves
        let result = resolver.resolve(&root, "foo.one");
        assert_eq!(result, Some(root.join("portion1/foo/one.py")));
        // foo.two does not exist in portion1
        let result = resolver.resolve(&root, "foo.two");
        assert_eq!(result, None);
    }

    // CPython: CombinedNamespacePackages (paths = ['both_portions'])
    #[test]
    fn cpython_combined_namespace_packages() {
        let (resolver, root) = cpython_resolver(&["both_portions"]);
        let result = resolver.resolve(&root, "foo.one");
        assert_eq!(result, Some(root.join("both_portions/foo/one.py")));
        let result = resolver.resolve(&root, "foo.two");
        assert_eq!(result, Some(root.join("both_portions/foo/two.py")));
    }

    // CPython: SeparatedNamespacePackages (paths = ['portion1', 'portion2'])
    #[test]
    fn cpython_separated_namespace_packages() {
        let (resolver, root) = cpython_resolver(&["portion1", "portion2"]);
        let result = resolver.resolve(&root, "foo.one");
        assert_eq!(result, Some(root.join("portion1/foo/one.py")));
        let result = resolver.resolve(&root, "foo.two");
        assert_eq!(result, Some(root.join("portion2/foo/two.py")));
    }

    // CPython: SeparatedOverlappingNamespacePackages (paths = ['portion1', 'both_portions'])
    // "first path wins" — portion1 has foo/one.py, both_portions also has foo/one.py,
    // but portion1 is searched first.
    #[test]
    fn cpython_separated_overlapping_first_path_wins() {
        let (resolver, root) = cpython_resolver(&["portion1", "both_portions"]);
        let result = resolver.resolve(&root, "foo.one");
        assert_eq!(result, Some(root.join("portion1/foo/one.py")));
        let result = resolver.resolve(&root, "foo.two");
        assert_eq!(result, Some(root.join("both_portions/foo/two.py")));
    }

    // CPython: LegacySupport (paths = ['not_a_namespace_pkg', 'portion1', 'portion2', 'both_portions'])
    // Regular package (with __init__.py) takes precedence over namespace portions.
    // foo.one resolves within not_a_namespace_pkg, foo.two does NOT resolve because
    // __init__.py locks __path__ to not_a_namespace_pkg only.
    #[test]
    fn cpython_legacy_regular_package_takes_precedence() {
        let (resolver, root) =
            cpython_resolver(&["not_a_namespace_pkg", "portion1", "portion2", "both_portions"]);
        let result = resolver.resolve(&root, "foo.one");
        assert_eq!(result, Some(root.join("not_a_namespace_pkg/foo/one.py")));
        // foo.two is NOT importable because foo is a regular package (has __init__.py)
        // in not_a_namespace_pkg, which locks __path__ to that directory.
        let result = resolver.resolve(&root, "foo.two");
        assert_eq!(result, None);
    }

    // CPython: DynamicPathCalculation (paths = ['project1', 'project2'])
    // Nested namespace packages: parent and parent.child are both namespaces
    #[test]
    fn cpython_dynamic_path_nested_namespaces() {
        let (resolver, root) = cpython_resolver(&["project1", "project2"]);
        let result = resolver.resolve(&root, "parent.child.one");
        assert_eq!(result, Some(root.join("project1/parent/child/one.py")));
        let result = resolver.resolve(&root, "parent.child.two");
        assert_eq!(result, Some(root.join("project2/parent/child/two.py")));
        // project3 is NOT on path, so parent.child.three fails
        let result = resolver.resolve(&root, "parent.child.three");
        assert_eq!(result, None);
    }

    // CPython: DynamicPathCalculation with project3 added
    #[test]
    fn cpython_dynamic_path_three_projects() {
        let (resolver, root) = cpython_resolver(&["project1", "project2", "project3"]);
        let result = resolver.resolve(&root, "parent.child.one");
        assert_eq!(result, Some(root.join("project1/parent/child/one.py")));
        let result = resolver.resolve(&root, "parent.child.two");
        assert_eq!(result, Some(root.join("project2/parent/child/two.py")));
        let result = resolver.resolve(&root, "parent.child.three");
        assert_eq!(result, Some(root.join("project3/parent/child/three.py")));
    }

    // CPython: ModuleAndNamespacePackageInSameDir (paths = ['module_and_namespace_package'])
    // Module a_test.py should be found in preference to namespace dir a_test/
    #[test]
    fn cpython_module_preferred_over_namespace_dir() {
        let (resolver, root) = cpython_resolver(&["module_and_namespace_package"]);
        let result = resolver.resolve(&root, "a_test");
        assert_eq!(
            result,
            Some(root.join("module_and_namespace_package/a_test.py"))
        );
    }

    #[test]
    fn sys_path_nonexistent_dir_skipped() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();

        let init = root.join("__init__.py");
        fs::write(
            &init,
            r#"
import sys
from pathlib import Path
sys.path.insert(0, (Path(__file__).parent / "_vendor").as_posix())
"#,
        )
        .unwrap();

        let source = fs::read_to_string(&init).unwrap();
        let result = extract_sys_path_additions(&init, &source);
        assert!(result.is_empty());
    }
}
