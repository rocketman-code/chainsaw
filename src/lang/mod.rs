pub mod python;
pub mod typescript;

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::graph::EdgeKind;

/// Opaque error from a language parser.
///
/// Callers never need to distinguish parse failure causes — they log
/// and skip the file — so this is intentionally opaque.
#[derive(Debug, Clone)]
pub struct ParseError(String);

impl ParseError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self(msg.into())
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RawImport {
    pub specifier: String,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseResult {
    pub imports: Vec<RawImport>,
    pub unresolvable_dynamic: usize,
}

pub trait LanguageSupport: Send + Sync {
    fn extensions(&self) -> &[&str];
    fn parse(&self, path: &Path, source: &str) -> Result<ParseResult, ParseError>;
    fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf>;
    fn package_name(&self, resolved_path: &Path) -> Option<String>;
    fn workspace_package_name(&self, file_path: &Path, project_root: &Path) -> Option<String>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectKind {
    TypeScript,
    Python,
}

/// Detect the project kind from the entry file extension, then walk up
/// to find the matching project root marker. Returns `None` for
/// unsupported file extensions.
pub fn detect_project(entry: &Path) -> Option<(PathBuf, ProjectKind)> {
    let kind = match entry.extension().and_then(|e| e.to_str()) {
        Some("ts" | "tsx" | "js" | "jsx" | "mjs" | "cjs" | "mts" | "cts") => {
            ProjectKind::TypeScript
        }
        Some("py") => ProjectKind::Python,
        _ => return None,
    };

    let markers: &[&str] = match kind {
        ProjectKind::TypeScript => &["package.json"],
        ProjectKind::Python => &["pyproject.toml", "setup.py", "setup.cfg"],
    };

    let root = find_root_with_markers(entry, markers)
        .unwrap_or_else(|| entry.parent().unwrap_or(entry).to_path_buf());

    Some((root, kind))
}

fn find_root_with_markers(entry: &Path, markers: &[&str]) -> Option<PathBuf> {
    let mut dir = entry.parent()?;
    loop {
        for marker in markers {
            if dir.join(marker).exists() {
                return Some(dir.to_path_buf());
            }
        }
        dir = dir.parent()?;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn detect_typescript_from_ts_extension() {
        let tmp = tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("package.json"), "{}").unwrap();
        let entry = root.join("src/index.ts");
        fs::create_dir_all(entry.parent().unwrap()).unwrap();
        fs::write(&entry, "").unwrap();

        let (detected_root, kind) = detect_project(&entry).unwrap();
        assert_eq!(kind, ProjectKind::TypeScript);
        assert_eq!(detected_root, root);
    }

    #[test]
    fn detect_python_from_py_extension() {
        let tmp = tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("pyproject.toml"), "").unwrap();
        let entry = root.join("src/main.py");
        fs::create_dir_all(entry.parent().unwrap()).unwrap();
        fs::write(&entry, "").unwrap();

        let (detected_root, kind) = detect_project(&entry).unwrap();
        assert_eq!(kind, ProjectKind::Python);
        assert_eq!(detected_root, root);
    }

    #[test]
    fn detect_python_setup_py_fallback() {
        let tmp = tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        // No pyproject.toml — only setup.py
        fs::write(root.join("setup.py"), "").unwrap();
        let entry = root.join("app.py");
        fs::write(&entry, "").unwrap();

        let (detected_root, kind) = detect_project(&entry).unwrap();
        assert_eq!(kind, ProjectKind::Python);
        assert_eq!(detected_root, root);
    }

    #[test]
    fn detect_unknown_extension_returns_none() {
        let tmp = tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        fs::write(root.join("package.json"), "{}").unwrap();
        let entry = root.join("src/main.rs");
        fs::create_dir_all(entry.parent().unwrap()).unwrap();
        fs::write(&entry, "").unwrap();

        assert!(detect_project(&entry).is_none());
    }
}
