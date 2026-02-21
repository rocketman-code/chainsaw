//! Error types for the chainsaw CLI.

use std::path::PathBuf;

/// Errors from entry validation, graph loading, and snapshot I/O.
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// Entry point file not found on disk.
    EntryNotFound(PathBuf, std::io::Error),
    /// Entry point path refers to a directory, not a file.
    EntryIsDirectory(PathBuf),
    /// File has an unsupported or missing extension.
    UnsupportedFileType(Option<String>),
    /// Entry point exists but was not found in the dependency graph.
    EntryNotInGraph(PathBuf),
    /// Cannot read a snapshot file from disk.
    SnapshotRead(PathBuf, std::io::Error),
    /// Snapshot file contains invalid JSON.
    SnapshotParse(PathBuf, serde_json::Error),
    /// Cannot write a snapshot file to disk.
    SnapshotWrite(PathBuf, std::io::Error),
    /// Mutually exclusive CLI flags were used together.
    MutuallyExclusiveFlags(String),
    /// --chain/--cut target is the entry point itself.
    TargetIsEntryPoint(String),
    /// --entry is required when comparing git refs.
    EntryRequired,
    /// Not inside a git repository.
    NotAGitRepo,
    /// Argument is not a snapshot file or valid git ref.
    NotSnapshotOrRef(String),
    /// A path-like argument that doesn't exist on disk.
    DiffFileNotFound(String),
    /// Git command failed.
    GitError(String),
}

impl Error {
    /// User-facing hint to accompany the error message.
    pub fn hint(&self) -> Option<&str> {
        match self {
            Self::UnsupportedFileType(_) => Some(
                "chainsaw supports TypeScript/JavaScript (.ts, .tsx, .js, .jsx, .mjs, .cjs) and Python (.py) files",
            ),
            Self::EntryNotInGraph(_) => Some("is it reachable from the project root?"),
            Self::TargetIsEntryPoint(flag) => Some(if flag == "--chain" {
                "--chain finds import chains from the entry to a dependency"
            } else {
                "--cut finds where to sever import chains to a dependency"
            }),
            Self::EntryRequired => Some("use --entry to specify the entry point to trace"),
            Self::EntryIsDirectory(_) => {
                Some("provide a source file (e.g. src/index.ts or main.py)")
            }
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
            Self::UnsupportedFileType(Some(ext)) => {
                write!(f, "unsupported file type '.{ext}'")
            }
            Self::UnsupportedFileType(None) => {
                write!(f, "file has no extension")
            }
            Self::EntryNotInGraph(path) => {
                write!(f, "entry file '{}' not found in graph", path.display())
            }
            Self::SnapshotRead(path, source) => {
                write!(f, "cannot read snapshot '{}': {source}", path.display())
            }
            Self::SnapshotParse(path, source) => {
                write!(f, "invalid snapshot '{}': {source}", path.display())
            }
            Self::SnapshotWrite(path, source) => {
                write!(f, "cannot write snapshot '{}': {source}", path.display())
            }
            Self::MutuallyExclusiveFlags(flags) => {
                write!(f, "{flags} cannot be used together")
            }
            Self::TargetIsEntryPoint(flag) => {
                write!(f, "{flag} target is the entry point itself")
            }
            Self::EntryRequired => {
                write!(
                    f,
                    "--entry is required when diffing against a git ref or the working tree"
                )
            }
            Self::NotAGitRepo => write!(f, "not inside a git repository"),
            Self::NotSnapshotOrRef(arg) => {
                write!(f, "'{arg}' is not a snapshot file or a valid git ref")
            }
            Self::DiffFileNotFound(arg) => write!(f, "file not found: {arg}"),
            Self::GitError(msg) => write!(f, "git: {msg}"),
        }
    }
}

// Implement source() for error chain introspection.
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::EntryNotFound(_, e) | Self::SnapshotRead(_, e) | Self::SnapshotWrite(_, e) => {
                Some(e)
            }
            Self::SnapshotParse(_, e) => Some(e),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unsupported_file_type_no_extension() {
        let err = Error::UnsupportedFileType(None);
        assert!(err.to_string().contains("no extension"));
        assert!(err.hint().unwrap().contains(".ts"));
    }

    #[test]
    fn unsupported_file_type_with_extension() {
        let err = Error::UnsupportedFileType(Some("rs".to_string()));
        assert!(err.to_string().contains(".rs"));
    }

    #[test]
    fn entry_is_directory_has_hint() {
        let err = Error::EntryIsDirectory(PathBuf::from("/tmp/src"));
        assert!(err.hint().unwrap().contains("source file"));
    }
}
