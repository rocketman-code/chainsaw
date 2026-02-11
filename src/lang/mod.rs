pub mod typescript;

use std::path::{Path, PathBuf};

use crate::graph::EdgeKind;

#[derive(Debug, Clone)]
pub struct RawImport {
    pub specifier: String,
    pub kind: EdgeKind,
}

pub trait LanguageSupport: Send + Sync {
    fn extensions(&self) -> &[&str];
    fn skip_dirs(&self) -> &[&str];
    fn parse(&self, path: &Path) -> Result<Vec<RawImport>, String>;
    fn resolve(&self, from_dir: &Path, specifier: &str) -> Option<PathBuf>;
    fn package_name(&self, resolved_path: &Path) -> Option<String>;
    fn workspace_package_name(&self, file_path: &Path, project_root: &Path) -> Option<String>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectKind {
    TypeScript,
}

/// Detect the project kind from the entry file extension, then walk up
/// to find the matching project root marker.
pub fn detect_project(entry: &Path) -> (PathBuf, ProjectKind) {
    let kind = match entry.extension().and_then(|e| e.to_str()) {
        Some("ts" | "tsx" | "js" | "jsx" | "mjs" | "cjs" | "mts" | "cts") => {
            ProjectKind::TypeScript
        }
        _ => ProjectKind::TypeScript,
    };

    let marker = match kind {
        ProjectKind::TypeScript => "package.json",
    };

    let root = find_root_with_marker(entry, marker)
        .unwrap_or_else(|| entry.parent().unwrap_or(entry).to_path_buf());

    (root, kind)
}

fn find_root_with_marker(entry: &Path, marker: &str) -> Option<PathBuf> {
    let mut dir = entry.parent()?;
    loop {
        if dir.join(marker).exists() {
            return Some(dir.to_path_buf());
        }
        dir = dir.parent()?;
    }
}
