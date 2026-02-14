use std::path::PathBuf;

/// Errors that can occur when loading a dependency graph.
///
/// The four variants represent the distinct failure modes of entry
/// validation and graph loading.
#[derive(Debug)]
pub enum Error {
    /// Entry point file not found on disk.
    EntryNotFound(PathBuf, std::io::Error),
    /// Entry point path refers to a directory, not a file.
    EntryIsDirectory(PathBuf),
    /// File has an unsupported extension.
    UnsupportedFileType(String),
    /// Entry point exists but was not found in the dependency graph.
    EntryNotInGraph(PathBuf),
}

impl Error {
    /// User-facing hint to accompany the error message.
    pub fn hint(&self) -> Option<&str> {
        match self {
            Self::UnsupportedFileType(_) => {
                Some("chainsaw supports TypeScript/JavaScript and Python files")
            }
            Self::EntryNotInGraph(_) => Some("is it reachable from the project root?"),
            _ => None,
        }
    }
}

// Display: lowercase, no trailing punctuation, so it composes into
// larger error messages.
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EntryNotFound(path, source) => {
                write!(f, "cannot find entry file '{}': {source}", path.display())
            }
            Self::EntryIsDirectory(path) => {
                write!(f, "'{}' is a directory, not a source file", path.display())
            }
            Self::UnsupportedFileType(ext) => {
                write!(f, "unsupported file type '.{ext}'")
            }
            Self::EntryNotInGraph(path) => {
                write!(f, "entry file '{}' not found in graph", path.display())
            }
        }
    }
}

// Implement source() for error chain introspection.
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::EntryNotFound(_, e) => Some(e),
            _ => None,
        }
    }
}
