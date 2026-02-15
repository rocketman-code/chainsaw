use std::path::PathBuf;

/// Errors from entry validation, graph loading, and snapshot I/O.
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// Entry point file not found on disk.
    EntryNotFound(PathBuf, std::io::Error),
    /// Entry point path refers to a directory, not a file.
    EntryIsDirectory(PathBuf),
    /// File has an unsupported extension.
    UnsupportedFileType(String),
    /// Entry point exists but was not found in the dependency graph.
    EntryNotInGraph(PathBuf),
    /// Cannot read a snapshot file from disk.
    SnapshotRead(PathBuf, std::io::Error),
    /// Snapshot file contains invalid JSON.
    SnapshotParse(PathBuf, String),
    /// Cannot write a snapshot file to disk.
    SnapshotWrite(PathBuf, std::io::Error),
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
            Self::SnapshotRead(path, source) => {
                write!(f, "cannot read snapshot '{}': {source}", path.display())
            }
            Self::SnapshotParse(path, msg) => {
                write!(f, "invalid snapshot '{}': {msg}", path.display())
            }
            Self::SnapshotWrite(path, source) => {
                write!(f, "cannot write snapshot '{}': {source}", path.display())
            }
        }
    }
}

// Implement source() for error chain introspection.
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::EntryNotFound(_, e)
            | Self::SnapshotRead(_, e)
            | Self::SnapshotWrite(_, e) => Some(e),
            _ => None,
        }
    }
}
