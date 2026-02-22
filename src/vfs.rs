//! Virtual filesystem abstraction for dependency graph construction.
//!
//! The [`Vfs`] trait abstracts filesystem access so the graph-building pipeline
//! can operate transparently on the real filesystem ([`OsVfs`]) or on git tree
//! objects ([`GitTreeVfs`]). This enables in-process git ref diffs without
//! spawning worktrees.

use std::collections::{HashMap, HashSet};
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
        // Single stat syscall via Vfs::metadata, then map to oxc_resolver's type.
        // oxc_resolver has its own per-path OnceLock cache, so this is called at
        // most once per unique CachedPath.
        let meta = self.0.metadata(path)?;
        Ok(oxc_resolver::FileMetadata::new(
            meta.is_file,
            meta.is_dir,
            false,
        ))
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<oxc_resolver::FileMetadata> {
        // We always follow symlinks (stat, not lstat) and never report
        // is_symlink=true. Combined with symlinks=false in ResolveOptions,
        // this means the resolver skips canonicalize entirely.
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

/// Reads files from a git tree object, enabling in-process git ref diffs
/// without spawning worktrees. Construction walks the tree once to build
/// an in-memory index; subsequent reads decompress blobs from the pack.
pub struct GitTreeVfs {
    repo: gix::ThreadSafeRepository,
    /// Relative path -> (blob `ObjectId`, uncompressed size).
    blobs: HashMap<PathBuf, (gix::ObjectId, u64)>,
    /// Set of directories present in the tree (relative paths).
    dirs: HashSet<PathBuf>,
    /// Directory (relative) -> direct children as absolute paths.
    children: HashMap<PathBuf, Vec<PathBuf>>,
    /// Absolute prefix joined to relative paths for external consumers.
    root: PathBuf,
}

impl GitTreeVfs {
    /// Open the repository, resolve `git_ref` to a commit, walk its tree,
    /// and build an in-memory index of all blobs and directories.
    pub fn new(repo_path: &Path, git_ref: &str, root: &Path) -> io::Result<Self> {
        use gix::prelude::FindExt;

        // Resolve ref to SHA via git CLI (avoids pulling in gix revision feature).
        let sha = resolve_ref_to_sha(repo_path, git_ref)?;
        let commit_id = gix::ObjectId::from_hex(sha.as_bytes())
            .map_err(|e| io::Error::other(format!("parse oid: {e}")))?;

        let ts_repo = gix::ThreadSafeRepository::open(repo_path)
            .map_err(|e| io::Error::other(format!("open repo: {e}")))?;
        let repo = ts_repo.to_thread_local();

        // commit -> tree
        let commit = repo
            .find_object(commit_id)
            .map_err(|e| io::Error::other(format!("find commit: {e}")))?
            .try_into_commit()
            .map_err(|e| io::Error::other(format!("not a commit: {e}")))?;
        let tree_id = commit
            .tree_id()
            .map_err(|e| io::Error::other(format!("tree id: {e}")))?;

        // Walk tree with Recorder
        let mut buf = Vec::new();
        let tree_iter = repo
            .objects
            .find_tree_iter(&tree_id, &mut buf)
            .map_err(|e| io::Error::other(format!("find tree: {e}")))?;

        let mut recorder = gix::traverse::tree::Recorder::default()
            .track_location(Some(gix::traverse::tree::recorder::Location::Path));
        gix::traverse::tree::breadthfirst(
            tree_iter,
            gix::traverse::tree::breadthfirst::State::default(),
            &repo.objects,
            &mut recorder,
        )
        .map_err(|e| io::Error::other(format!("traverse: {e}")))?;

        let mut blobs = HashMap::new();
        let mut dirs = HashSet::new();
        let mut children: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();

        dirs.insert(PathBuf::new()); // root dir

        for entry in &recorder.records {
            let rel = PathBuf::from(
                std::str::from_utf8(entry.filepath.as_ref())
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
            );
            let abs = root.join(&rel);
            let parent_rel = rel.parent().unwrap_or(Path::new("")).to_path_buf();

            if entry.mode.is_tree() {
                dirs.insert(rel);
                children.entry(parent_rel).or_default().push(abs);
            } else if entry.mode.is_blob() {
                let size = repo
                    .find_header(entry.oid)
                    .map_err(|e| io::Error::other(format!("header: {e}")))?
                    .size();
                blobs.insert(rel, (entry.oid, size));
                children.entry(parent_rel).or_default().push(abs);
            }
            // Skip symlinks, submodules
        }

        Ok(Self {
            repo: ts_repo,
            blobs,
            dirs,
            children,
            root: root.to_path_buf(),
        })
    }

    /// Strip the root prefix to get the relative tree path.
    fn relative(&self, path: &Path) -> Option<PathBuf> {
        path.strip_prefix(&self.root).ok().map(Path::to_path_buf)
    }

    fn not_found(path: &Path) -> io::Error {
        io::Error::new(
            io::ErrorKind::NotFound,
            format!("{} not in git tree", path.display()),
        )
    }
}

fn resolve_ref_to_sha(repo_path: &Path, git_ref: &str) -> io::Result<String> {
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--verify", git_ref])
        .current_dir(repo_path)
        .output()
        .map_err(|e| io::Error::other(format!("git rev-parse: {e}")))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(format!(
            "git rev-parse failed for '{git_ref}': {}",
            stderr.trim()
        )));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

impl Vfs for GitTreeVfs {
    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        let rel = self.relative(path).ok_or_else(|| Self::not_found(path))?;
        let &(oid, _) = self.blobs.get(&rel).ok_or_else(|| Self::not_found(path))?;
        let repo = self.repo.to_thread_local();
        let obj = repo
            .find_object(oid)
            .map_err(|e| io::Error::other(format!("read object: {e}")))?;
        String::from_utf8(obj.data.clone())
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let rel = self.relative(path).ok_or_else(|| Self::not_found(path))?;
        let &(oid, _) = self.blobs.get(&rel).ok_or_else(|| Self::not_found(path))?;
        let repo = self.repo.to_thread_local();
        let obj = repo
            .find_object(oid)
            .map_err(|e| io::Error::other(format!("read object: {e}")))?;
        Ok(obj.data.clone())
    }

    fn metadata(&self, path: &Path) -> io::Result<VfsMetadata> {
        let rel = self.relative(path).ok_or_else(|| Self::not_found(path))?;
        if let Some(&(_, size)) = self.blobs.get(&rel) {
            Ok(VfsMetadata {
                len: size,
                is_file: true,
                is_dir: false,
                mtime_nanos: None,
            })
        } else if self.dirs.contains(&rel) {
            Ok(VfsMetadata {
                len: 0,
                is_file: false,
                is_dir: true,
                mtime_nanos: None,
            })
        } else {
            Err(Self::not_found(path))
        }
    }

    fn exists(&self, path: &Path) -> bool {
        let Some(rel) = self.relative(path) else {
            return false;
        };
        self.blobs.contains_key(&rel) || self.dirs.contains(&rel)
    }

    fn is_dir(&self, path: &Path) -> bool {
        let Some(rel) = self.relative(path) else {
            return false;
        };
        self.dirs.contains(&rel)
    }

    fn is_file(&self, path: &Path) -> bool {
        let Some(rel) = self.relative(path) else {
            return false;
        };
        self.blobs.contains_key(&rel)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let rel = self.relative(path).ok_or_else(|| Self::not_found(path))?;
        self.children
            .get(&rel)
            .cloned()
            .ok_or_else(|| Self::not_found(path))
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        // Git trees have no symlinks; verify existence and return normalized path.
        let rel = self.relative(path).ok_or_else(|| Self::not_found(path))?;
        if self.blobs.contains_key(&rel) || self.dirs.contains(&rel) {
            Ok(self.root.join(&rel))
        } else {
            Err(Self::not_found(path))
        }
    }

    /// Single lookup + blob decompress for both content and metadata.
    fn read_with_metadata(&self, path: &Path) -> io::Result<(String, VfsMetadata)> {
        let rel = self.relative(path).ok_or_else(|| Self::not_found(path))?;
        let &(oid, size) = self.blobs.get(&rel).ok_or_else(|| Self::not_found(path))?;
        let repo = self.repo.to_thread_local();
        let obj = repo
            .find_object(oid)
            .map_err(|e| io::Error::other(format!("read object: {e}")))?;
        let content = String::from_utf8(obj.data.clone())
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let meta = VfsMetadata {
            len: size,
            is_file: true,
            is_dir: false,
            mtime_nanos: None,
        };
        Ok((content, meta))
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

    #[test]
    fn git_tree_vfs_reads_head() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let vfs = GitTreeVfs::new(root, "HEAD", root).unwrap();

        // Known file exists and is readable
        assert!(vfs.is_file(&root.join("Cargo.toml")));
        assert!(!vfs.is_dir(&root.join("Cargo.toml")));
        let content = vfs.read_to_string(&root.join("Cargo.toml")).unwrap();
        assert!(content.contains("chainsaw-cli"));

        // Known directory
        assert!(vfs.is_dir(&root.join("src")));
        assert!(!vfs.is_file(&root.join("src")));

        // Non-existent path
        assert!(!vfs.exists(&root.join("nonexistent.rs")));
        assert!(vfs.read_to_string(&root.join("nonexistent.rs")).is_err());
    }

    #[test]
    fn git_tree_vfs_metadata() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let vfs = GitTreeVfs::new(root, "HEAD", root).unwrap();

        let meta = vfs.metadata(&root.join("Cargo.toml")).unwrap();
        assert!(meta.is_file);
        assert!(!meta.is_dir);
        assert!(meta.len > 0);
        assert!(meta.mtime_nanos.is_none(), "git blobs have no mtime");
    }

    #[test]
    fn git_tree_vfs_read_dir() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let vfs = GitTreeVfs::new(root, "HEAD", root).unwrap();

        let entries = vfs.read_dir(&root.join("src")).unwrap();
        assert!(entries.iter().any(|p| p.ends_with("main.rs")));
        assert!(entries.iter().any(|p| p.ends_with("lib.rs")));
    }

    #[test]
    fn git_tree_vfs_canonicalize() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let vfs = GitTreeVfs::new(root, "HEAD", root).unwrap();

        let canonical = vfs.canonicalize(&root.join("Cargo.toml")).unwrap();
        assert_eq!(canonical, root.join("Cargo.toml"));

        assert!(vfs.canonicalize(&root.join("nonexistent")).is_err());
    }

    #[test]
    fn oxc_adapter_metadata_single_stat() {
        use oxc_resolver::FileSystem;

        let adapter = OxcVfsAdapter(Arc::new(OsVfs));
        let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");

        // File should be found
        let meta = adapter.metadata(&src.join("main.rs")).unwrap();
        assert!(meta.is_file());
        assert!(!meta.is_dir());

        // Directory should be found
        let meta = adapter.metadata(&src.join("lang")).unwrap();
        assert!(!meta.is_file());
        assert!(meta.is_dir());

        // Non-existent should be NotFound
        assert!(adapter.metadata(&src.join("nonexistent.xyz")).is_err());
    }
}
