//! Virtual filesystem abstraction for dependency graph construction.
//!
//! The [`Vfs`] trait abstracts filesystem access so the graph-building pipeline
//! can operate transparently on the real filesystem ([`OsVfs`]) or on git tree
//! objects ([`GitTreeVfs`]). This enables in-process git ref diffs without
//! spawning worktrees.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

/// Metadata returned by [`Vfs::metadata`].
pub struct VfsMetadata {
    pub len: u64,
    pub is_file: bool,
    pub is_dir: bool,
    /// File modification time (nanos since epoch). `None` for non-OS sources
    /// like git tree objects that have no mtime.
    pub mtime_nanos: Option<u128>,
}

/// Filesystem abstraction for the graph-building pipeline.
///
/// All methods mirror their `std::fs` counterparts. Implementations must be
/// safe to call from multiple threads concurrently.
pub trait Vfs: Send + Sync {
    fn read_to_string(&self, path: &Path) -> io::Result<String>;
    fn read(&self, path: &Path) -> io::Result<Vec<u8>>;
    fn metadata(&self, path: &Path) -> io::Result<VfsMetadata>;
    fn exists(&self, path: &Path) -> bool;
    fn is_dir(&self, path: &Path) -> bool;
    fn is_file(&self, path: &Path) -> bool;
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;

    /// Read file content and metadata in one operation. The default calls
    /// `read_to_string` + `metadata` separately; `OsVfs` overrides this to
    /// reuse the file descriptor (open + fstat + read = 3 syscalls, not 4).
    fn read_with_metadata(&self, path: &Path) -> io::Result<(String, VfsMetadata)> {
        let content = self.read_to_string(path)?;
        let meta = self.metadata(path)?;
        Ok((content, meta))
    }
}

/// Pass-through to `std::fs`. Zero overhead for normal (non-git) operation.
pub struct OsVfs;

fn fs_meta_to_vfs(meta: &std::fs::Metadata) -> VfsMetadata {
    let mtime_nanos = meta
        .modified()
        .ok()
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_nanos());
    VfsMetadata {
        len: meta.len(),
        is_file: meta.is_file(),
        is_dir: meta.is_dir(),
        mtime_nanos,
    }
}

impl Vfs for OsVfs {
    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        std::fs::read(path)
    }

    fn metadata(&self, path: &Path) -> io::Result<VfsMetadata> {
        std::fs::metadata(path).map(|m| fs_meta_to_vfs(&m))
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn is_file(&self, path: &Path) -> bool {
        path.is_file()
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(path)? {
            entries.push(entry?.path());
        }
        Ok(entries)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }

    /// Open + fstat + read in one pass (3 syscalls, not 4).
    fn read_with_metadata(&self, path: &Path) -> io::Result<(String, VfsMetadata)> {
        use std::io::Read;

        let mut file = std::fs::File::open(path)?;
        let meta = fs_meta_to_vfs(&file.metadata()?);
        #[allow(clippy::cast_possible_truncation)]
        let mut content = String::with_capacity(meta.len as usize + 1);
        file.read_to_string(&mut content)?;
        Ok((content, meta))
    }
}

/// Adapter that wraps our [`Vfs`] trait to satisfy [`oxc_resolver::FileSystem`].
///
/// Used to inject a VFS into `ResolverGeneric::new_with_file_system` so the
/// TypeScript resolver can transparently operate on git tree objects.
pub struct OxcVfsAdapter(pub Arc<dyn Vfs>);

impl oxc_resolver::FileSystem for OxcVfsAdapter {
    fn new() -> Self {
        // Never called — we always construct via new_with_file_system.
        unreachable!("OxcVfsAdapter must be constructed with a Vfs instance")
    }

    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.0.read(path)
    }

    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        self.0.read_to_string(path)
    }

    fn metadata(&self, path: &Path) -> io::Result<oxc_resolver::FileMetadata> {
        let m = self.0.metadata(path)?;
        Ok(oxc_resolver::FileMetadata::new(m.is_file, m.is_dir, false))
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<oxc_resolver::FileMetadata> {
        // No lstat equivalent in Vfs; follows symlinks via metadata().
        // For GitTreeVfs: correct (git trees have no symlinks).
        // For OsVfs: matches the previous Resolver::new behavior.
        self.metadata(path)
    }

    fn read_link(&self, path: &Path) -> Result<PathBuf, oxc_resolver::ResolveError> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            format!("read_link not supported: {}", path.display()),
        )
        .into())
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.0.canonicalize(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn os_vfs_reads_file() {
        let vfs = OsVfs;
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml");
        let content = vfs.read_to_string(&path).unwrap();
        assert!(content.contains("chainsaw-cli"));
    }

    #[test]
    fn os_vfs_metadata() {
        let vfs = OsVfs;
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml");
        let meta = vfs.metadata(&path).unwrap();
        assert!(meta.is_file);
        assert!(!meta.is_dir);
        assert!(meta.len > 0);
        assert!(meta.mtime_nanos.is_some());
    }

    #[test]
    fn os_vfs_read_with_metadata() {
        let vfs = OsVfs;
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml");
        let (content, meta) = vfs.read_with_metadata(&path).unwrap();
        assert!(content.contains("chainsaw-cli"));
        assert!(meta.is_file);
        assert!(meta.len > 0);
        assert!(meta.mtime_nanos.is_some());
    }

    #[test]
    fn os_vfs_dir_operations() {
        let vfs = OsVfs;
        let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        assert!(vfs.is_dir(&src));
        assert!(!vfs.is_file(&src));
        let entries = vfs.read_dir(&src).unwrap();
        assert!(entries.iter().any(|p| p.ends_with("main.rs")));
    }

    #[test]
    fn os_vfs_nonexistent() {
        let vfs = OsVfs;
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("nonexistent.xyz");
        assert!(!vfs.exists(&path));
        assert!(vfs.read_to_string(&path).is_err());
        assert!(vfs.metadata(&path).is_err());
    }
}
