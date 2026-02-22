//! Session: owns a loaded dependency graph and exposes query methods.
//!
//! A [`Session`] is the primary interface for library consumers (CLI, REPL,
//! language server). It wraps graph loading, entry resolution, and keeps the
//! background cache-write handle alive for the duration of the session.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::cache::CacheWriteHandle;
use crate::error::Error;
use crate::graph::{EdgeId, EdgeKind, ModuleGraph, ModuleId, PackageInfo};
use crate::loader;
use crate::query::{self, ChainTarget, CutModule, DiffResult, TraceOptions, TraceResult};

/// The result of resolving a `--chain`/`--cut` argument against the graph.
///
/// The argument might be a file path (resolved to a [`ChainTarget::Module`])
/// or a package name (resolved to a [`ChainTarget::Package`]).
pub struct ResolvedTarget {
    pub target: ChainTarget,
    pub label: String,
    pub exists: bool,
}

/// An open dependency-graph session.
///
/// Created via [`Session::open`], which loads (or builds) the graph and
/// resolves the entry module. The background cache writer is joined on drop.
pub struct Session {
    graph: ModuleGraph,
    reverse_adj: Vec<Vec<EdgeId>>,
    root: PathBuf,
    entry: PathBuf,
    entry_id: ModuleId,
    valid_extensions: &'static [&'static str],
    from_cache: bool,
    unresolvable_dynamic_count: usize,
    unresolvable_dynamic_files: Vec<(PathBuf, usize)>,
    file_warnings: Vec<String>,
    _cache_handle: CacheWriteHandle,
}

fn build_reverse_adj(graph: &ModuleGraph) -> Vec<Vec<EdgeId>> {
    let mut rev = vec![Vec::new(); graph.module_count()];
    for edge in &graph.edges {
        rev[edge.to.0 as usize].push(edge.id);
    }
    rev
}

impl Session {
    /// Load a dependency graph from `entry` and resolve the entry module.
    ///
    /// When `no_cache` is true the on-disk cache is bypassed entirely.
    pub fn open(entry: &Path, no_cache: bool) -> Result<Self, Error> {
        let (loaded, cache_handle) = loader::load_graph(entry, no_cache)?;

        let entry_id = *loaded
            .graph
            .path_to_id
            .get(&loaded.entry)
            .ok_or_else(|| Error::EntryNotInGraph(loaded.entry.clone()))?;

        let reverse_adj = build_reverse_adj(&loaded.graph);

        Ok(Self {
            graph: loaded.graph,
            reverse_adj,
            root: loaded.root,
            entry: loaded.entry,
            entry_id,
            valid_extensions: loaded.valid_extensions,
            from_cache: loaded.from_cache,
            unresolvable_dynamic_count: loaded.unresolvable_dynamic_count,
            unresolvable_dynamic_files: loaded.unresolvable_dynamic_files,
            file_warnings: loaded.file_warnings,
            _cache_handle: cache_handle,
        })
    }

    /// Trace transitive import weight from the entry module.
    pub fn trace(&self, opts: &TraceOptions) -> TraceResult {
        query::trace(&self.graph, self.entry_id, opts)
    }

    /// Trace transitive import weight from an arbitrary file in the graph.
    pub fn trace_from(
        &self,
        file: &Path,
        opts: &TraceOptions,
    ) -> Result<(TraceResult, PathBuf), Error> {
        let canon = file
            .canonicalize()
            .or_else(|_| self.root.join(file).canonicalize())
            .map_err(|e| Error::EntryNotFound(file.to_path_buf(), e))?;
        let Some(&id) = self.graph.path_to_id.get(&canon) else {
            return Err(Error::EntryNotInGraph(canon));
        };
        Ok((query::trace(&self.graph, id, opts), canon))
    }

    /// Resolve a chain/cut argument to a [`ChainTarget`].
    ///
    /// If the argument looks like a file path and resolves to a module in
    /// the graph, returns `ChainTarget::Module`. Otherwise falls through
    /// to a package name lookup.
    pub fn resolve_target(&self, arg: &str) -> ResolvedTarget {
        if looks_like_path(arg, self.valid_extensions)
            && let Ok(target_path) = self.root.join(arg).canonicalize()
            && let Some(&id) = self.graph.path_to_id.get(&target_path)
        {
            let p = &self.graph.module(id).path;
            let label = p
                .strip_prefix(&self.root)
                .unwrap_or(p)
                .to_string_lossy()
                .into_owned();
            return ResolvedTarget {
                target: ChainTarget::Module(id),
                label,
                exists: true,
            };
        }
        // File doesn't exist or isn't in the graph -- fall through to
        // package name lookup. Handles packages like "six.py" or
        // "highlight.js" whose names match file extensions.
        let name = arg.to_string();
        let exists = self.graph.package_map.contains_key(arg);
        let label = name.clone();
        ResolvedTarget {
            target: ChainTarget::Package(name),
            label,
            exists,
        }
    }

    /// Find all shortest import chains from the entry to a target.
    pub fn chain(
        &self,
        target_arg: &str,
        include_dynamic: bool,
    ) -> (ResolvedTarget, Vec<Vec<ModuleId>>) {
        let resolved = self.resolve_target(target_arg);
        let chains = query::find_all_chains(
            &self.graph,
            self.entry_id,
            &resolved.target,
            include_dynamic,
        );
        (resolved, chains)
    }

    /// Find import chains and optimal cut points to sever them.
    pub fn cut(
        &self,
        target_arg: &str,
        top: i32,
        include_dynamic: bool,
    ) -> (ResolvedTarget, Vec<Vec<ModuleId>>, Vec<CutModule>) {
        let resolved = self.resolve_target(target_arg);
        let chains = query::find_all_chains(
            &self.graph,
            self.entry_id,
            &resolved.target,
            include_dynamic,
        );
        let cuts = query::find_cut_modules(
            &self.graph,
            &chains,
            self.entry_id,
            &resolved.target,
            top,
            include_dynamic,
        );
        (resolved, chains, cuts)
    }

    /// Trace from a different entry point in the same graph and diff
    /// against the current entry. Returns the diff and the canonical
    /// path of the other entry (avoids redundant canonicalization by
    /// the caller).
    pub fn diff_entry(
        &self,
        other: &Path,
        opts: &TraceOptions,
    ) -> Result<(DiffResult, PathBuf), Error> {
        let other_canon = other
            .canonicalize()
            .or_else(|_| self.root.join(other).canonicalize())
            .map_err(|e| Error::EntryNotFound(other.to_path_buf(), e))?;
        let Some(&other_id) = self.graph.path_to_id.get(&other_canon) else {
            return Err(Error::EntryNotInGraph(other_canon.clone()));
        };
        let snap_a = self.trace(opts).to_snapshot(&self.entry_label());
        let snap_b = query::trace(&self.graph, other_id, opts)
            .to_snapshot(&self.entry_label_for(&other_canon));
        Ok((query::diff_snapshots(&snap_a, &snap_b), other_canon))
    }

    /// All third-party packages in the dependency graph.
    pub fn packages(&self) -> &HashMap<String, PackageInfo> {
        &self.graph.package_map
    }

    /// List direct imports of a file (outgoing edges).
    pub fn imports(&self, file: &Path) -> Result<Vec<(PathBuf, EdgeKind)>, Error> {
        let canon = file
            .canonicalize()
            .or_else(|_| self.root.join(file).canonicalize())
            .map_err(|e| Error::EntryNotFound(file.to_path_buf(), e))?;
        let Some(&id) = self.graph.path_to_id.get(&canon) else {
            return Err(Error::EntryNotInGraph(canon));
        };
        let result = self
            .graph
            .outgoing_edges(id)
            .iter()
            .map(|&eid| {
                let edge = self.graph.edge(eid);
                (self.graph.module(edge.to).path.clone(), edge.kind)
            })
            .collect();
        Ok(result)
    }

    /// List files that import a given file (reverse edge lookup).
    pub fn importers(&self, file: &Path) -> Result<Vec<(PathBuf, EdgeKind)>, Error> {
        let canon = file
            .canonicalize()
            .or_else(|_| self.root.join(file).canonicalize())
            .map_err(|e| Error::EntryNotFound(file.to_path_buf(), e))?;
        let Some(&id) = self.graph.path_to_id.get(&canon) else {
            return Err(Error::EntryNotInGraph(canon));
        };
        let result = self.reverse_adj[id.0 as usize]
            .iter()
            .map(|&eid| {
                let edge = self.graph.edge(eid);
                (self.graph.module(edge.from).path.clone(), edge.kind)
            })
            .collect();
        Ok(result)
    }

    /// Look up package info by name.
    pub fn info(&self, package_name: &str) -> Option<&PackageInfo> {
        self.graph.package_map.get(package_name)
    }

    /// Display label for the current entry point, including the project
    /// directory name for disambiguation (e.g. `wrangler/src/index.ts`).
    pub fn entry_label(&self) -> String {
        self.entry_label_for(&self.entry)
    }

    /// Display label for an arbitrary path, relative to the project root.
    pub fn entry_label_for(&self, path: &Path) -> String {
        entry_label(path, &self.root)
    }

    /// Switch the default entry point to a different file in the graph.
    ///
    /// The file must already be in the graph (no rebuild). Accepts both
    /// absolute paths and paths relative to the project root.
    pub fn set_entry(&mut self, path: &Path) -> Result<(), Error> {
        let canon = path
            .canonicalize()
            .or_else(|_| self.root.join(path).canonicalize())
            .map_err(|e| Error::EntryNotFound(path.to_path_buf(), e))?;
        let Some(&id) = self.graph.path_to_id.get(&canon) else {
            return Err(Error::EntryNotInGraph(canon));
        };
        self.entry = canon;
        self.entry_id = id;
        Ok(())
    }

    /// Check for file changes and rebuild the graph if needed.
    ///
    /// Returns `true` if the graph was updated (cold build or module count
    /// changed since the last load).
    #[allow(clippy::used_underscore_binding)] // _cache_handle held for drop
    pub fn refresh(&mut self) -> Result<bool, Error> {
        let (loaded, handle) = loader::load_graph(&self.entry, false)?;
        let Some(&entry_id) = loaded.graph.path_to_id.get(&loaded.entry) else {
            return Err(Error::EntryNotInGraph(loaded.entry));
        };
        // Detect structural change: cold build (not from cache) or module count
        // changed. When from_cache is true and module count matches, edges are
        // guaranteed identical (tier 1.5 only returns from_cache when imports
        // are unchanged), so we can reuse the existing reverse adjacency index.
        let changed =
            !loaded.from_cache || loaded.graph.module_count() != self.graph.module_count();
        if changed {
            self.reverse_adj = build_reverse_adj(&loaded.graph);
        } else {
            debug_assert_eq!(
                self.reverse_adj,
                build_reverse_adj(&loaded.graph),
                "reverse_adj out of sync: cache reported unchanged but edges differ"
            );
        }
        self.graph = loaded.graph;
        self.root = loaded.root;
        self.entry = loaded.entry;
        self.entry_id = entry_id;
        self.valid_extensions = loaded.valid_extensions;
        self.from_cache = loaded.from_cache;
        self.unresolvable_dynamic_count = loaded.unresolvable_dynamic_count;
        self.unresolvable_dynamic_files = loaded.unresolvable_dynamic_files;
        self.file_warnings = loaded.file_warnings;
        self._cache_handle = handle;
        Ok(changed)
    }

    // -- accessors --

    pub fn graph(&self) -> &ModuleGraph {
        &self.graph
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn entry(&self) -> &Path {
        &self.entry
    }

    pub fn entry_id(&self) -> ModuleId {
        self.entry_id
    }

    pub fn valid_extensions(&self) -> &'static [&'static str] {
        self.valid_extensions
    }

    pub fn from_cache(&self) -> bool {
        self.from_cache
    }

    pub fn unresolvable_dynamic_count(&self) -> usize {
        self.unresolvable_dynamic_count
    }

    pub fn unresolvable_dynamic_files(&self) -> &[(PathBuf, usize)] {
        &self.unresolvable_dynamic_files
    }

    pub fn file_warnings(&self) -> &[String] {
        &self.file_warnings
    }
}

/// Build a display label for an entry point that includes the project
/// directory name for disambiguation (e.g. `wrangler/src/index.ts`
/// instead of just `src/index.ts`).
pub fn entry_label(path: &Path, root: &Path) -> String {
    let rel = path.strip_prefix(root).unwrap_or(path);
    root.file_name().map_or_else(
        || rel.to_string_lossy().into_owned(),
        |name| Path::new(name).join(rel).to_string_lossy().into_owned(),
    )
}

/// Determine whether a chain/cut argument looks like a file path
/// (as opposed to a package name).
pub fn looks_like_path(arg: &str, extensions: &[&str]) -> bool {
    !arg.starts_with('@')
        && (arg.contains('/')
            || arg.contains(std::path::MAIN_SEPARATOR)
            || arg
                .rsplit_once('.')
                .is_some_and(|(_, suffix)| extensions.contains(&suffix)))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_project() -> (tempfile::TempDir, PathBuf) {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let entry = root.join("index.ts");
        std::fs::write(&entry, r#"import { x } from "./a";"#).unwrap();
        std::fs::write(root.join("a.ts"), "export const x = 1;").unwrap();
        (tmp, entry)
    }

    #[test]
    fn open_and_trace() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        assert_eq!(session.graph().module_count(), 2);
        let opts = TraceOptions::default();
        let result = session.trace(&opts);
        assert!(result.static_weight > 0);
    }

    #[test]
    fn chain_finds_dependency() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let (resolved, chains) = session.chain("a.ts", false);
        assert!(resolved.exists);
        assert!(!chains.is_empty());
    }

    #[test]
    fn cut_finds_no_intermediate_on_direct_import() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        // index.ts -> a.ts is a 1-hop chain, no intermediate to cut
        let (resolved, chains, cuts) = session.cut("a.ts", 10, false);
        assert!(resolved.exists);
        assert!(!chains.is_empty());
        assert!(cuts.is_empty());
    }

    #[test]
    fn diff_two_entries() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        // a.ts imports b.ts (so both are in the graph when built from a.ts)
        // b.ts imports extra.ts (so tracing from b.ts has more weight)
        let a = root.join("a.ts");
        std::fs::write(&a, r#"import { foo } from "./b";"#).unwrap();
        let b = root.join("b.ts");
        std::fs::write(&b, r#"import { bar } from "./extra";"#).unwrap();
        std::fs::write(root.join("extra.ts"), "export const y = 2;").unwrap();

        let session = Session::open(&a, true).unwrap();
        let (diff, _) = session.diff_entry(&b, &TraceOptions::default()).unwrap();
        // b.ts trace (b + extra) should have less weight than a.ts trace (a + b + extra)
        assert!(diff.entry_a_weight >= diff.entry_b_weight);
    }

    #[test]
    fn packages_returns_package_map() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        // Test project has no third-party packages
        assert!(session.packages().is_empty());
    }

    #[test]
    fn resolve_target_file_path() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let resolved = session.resolve_target("a.ts");
        assert!(resolved.exists);
        assert!(matches!(resolved.target, ChainTarget::Module(_)));
    }

    #[test]
    fn resolve_target_missing_package() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let resolved = session.resolve_target("nonexistent-pkg");
        assert!(!resolved.exists);
        assert!(matches!(resolved.target, ChainTarget::Package(_)));
    }

    #[test]
    fn scoped_npm_package_is_not_path() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!looks_like_path("@slack/web-api", exts));
        assert!(!looks_like_path("@aws-sdk/client-s3", exts));
        assert!(!looks_like_path("@anthropic-ai/sdk", exts));
    }

    #[test]
    fn relative_file_path_is_path() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(looks_like_path("src/index.ts", exts));
        assert!(looks_like_path("lib/utils.js", exts));
    }

    #[test]
    fn bare_package_name_is_not_path() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!looks_like_path("zod", exts));
        assert!(!looks_like_path("express", exts));
        // highlight.js is ambiguous — .js extension triggers path heuristic.
        // resolve_target tries as file path first, falls back to package lookup.
        assert!(looks_like_path("highlight.js", exts));
    }

    #[test]
    fn file_with_extension_is_path() {
        let exts = &["ts", "tsx", "js", "jsx", "py"];
        assert!(looks_like_path("utils.ts", exts));
        assert!(looks_like_path("main.py", exts));
        assert!(!looks_like_path("utils.txt", exts));
    }

    #[test]
    fn resolve_target_falls_back_to_package_for_extension_name() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        // "six.py" looks like a file (.py extension) but no such file exists,
        // so it falls back to package name lookup.
        let resolved = session.resolve_target("six.py");
        assert!(!resolved.exists);
        assert!(matches!(resolved.target, ChainTarget::Package(ref name) if name == "six.py"));
    }

    #[test]
    fn imports_lists_direct_dependencies() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let imports = session.imports(session.entry()).unwrap();
        assert_eq!(imports.len(), 1);
        assert!(imports[0].0.ends_with("a.ts"));
        assert!(matches!(imports[0].1, EdgeKind::Static));
    }

    #[test]
    fn importers_lists_reverse_dependencies() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let a_path = session.root().join("a.ts");
        let importers = session.importers(&a_path).unwrap();
        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("index.ts"));
    }

    #[test]
    fn set_entry_switches_entry_point() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let a = root.join("a.ts");
        std::fs::write(&a, r#"import { x } from "./b";"#).unwrap();
        let b = root.join("b.ts");
        std::fs::write(&b, "export const x = 1;").unwrap();

        let mut session = Session::open(&a, true).unwrap();
        assert!(session.entry().ends_with("a.ts"));
        session.set_entry(&b).unwrap();
        assert!(session.entry().ends_with("b.ts"));
        // Tracing from b: only b itself (no imports).
        let result = session.trace(&crate::query::TraceOptions::default());
        assert_eq!(result.static_module_count, 1);
    }

    #[test]
    fn refresh_detects_file_change() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let entry = root.join("index.ts");
        std::fs::write(&entry, r#"import { x } from "./a";"#).unwrap();
        std::fs::write(root.join("a.ts"), "export const x = 1;").unwrap();

        let mut session = Session::open(&entry, true).unwrap();
        assert_eq!(session.graph().module_count(), 2);

        // Modify entry to add a new import; sleep for mtime granularity.
        std::thread::sleep(std::time::Duration::from_millis(50));
        std::fs::write(
            &entry,
            r#"import { x } from "./a"; import { y } from "./b";"#,
        )
        .unwrap();
        std::fs::write(root.join("b.ts"), "export const y = 2;").unwrap();

        let changed = session.refresh().unwrap();
        assert!(changed);
        assert_eq!(session.graph().module_count(), 3);
    }

    #[test]
    fn entry_label_includes_project_dir() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let label = session.entry_label();
        // The temp dir has a name, so label should be "dirname/index.ts"
        assert!(label.ends_with("index.ts"));
        assert!(label.contains('/'));
    }
}
