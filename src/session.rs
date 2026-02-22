//! Session: owns a loaded dependency graph and exposes query methods.
//!
//! A [`Session`] is the primary interface for library consumers (CLI, REPL,
//! language server). It wraps graph loading, entry resolution, and keeps the
//! background cache-write handle alive for the duration of the session.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use notify::{RecommendedWatcher, RecursiveMode, Watcher};

use crate::cache::{CacheWriteHandle, LOCKFILES};
use crate::error::Error;
use crate::graph::{EdgeId, EdgeKind, ModuleGraph, ModuleId, PackageInfo};
use crate::loader;
use crate::query::{self, ChainTarget, CutModule, DiffResult, TraceOptions, TraceResult};
use crate::report::{
    self, ChainReport, CutEntry, CutReport, DiffReport, ModuleEntry, PackageEntry,
    PackageListEntry, PackagesReport, TraceReport,
};

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
    dirty: Arc<AtomicBool>,
    watcher: Option<RecommendedWatcher>,
    cached_trace: Option<CachedTrace>,
    cached_weights: Option<CachedWeights>,
}

/// Cached entry trace result, keyed on `(entry_id, include_dynamic)`.
///
/// The cache intentionally ignores `TraceOptions::top_n` and `ignore` because
/// those fields only affect `heavy_packages` filtering — they don't change the
/// underlying traversal (`static_weight`, `modules_by_cost`, `all_packages`).
/// This is safe as long as callers use consistent options across cached calls
/// (the REPL always uses `TraceOptions::default()`).
struct CachedTrace {
    entry_id: ModuleId,
    include_dynamic: bool,
    result: TraceResult,
}

struct CachedWeights {
    entry_id: ModuleId,
    include_dynamic: bool,
    weights: Vec<u64>,
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
            dirty: Arc::new(AtomicBool::new(false)),
            watcher: None,
            cached_trace: None,
            cached_weights: None,
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
        &mut self,
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
        self.ensure_weights(include_dynamic);
        let weights = &self.cached_weights.as_ref().unwrap().weights;
        let cuts = query::find_cut_modules(
            &self.graph,
            &chains,
            self.entry_id,
            &resolved.target,
            top,
            weights,
        );
        (resolved, chains, cuts)
    }

    /// Trace from a different entry point in the same graph and diff
    /// against the current entry. Returns the diff and the canonical
    /// path of the other entry (avoids redundant canonicalization by
    /// the caller).
    pub fn diff_entry(
        &mut self,
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
        self.ensure_trace(opts);
        let snap_a = self
            .cached_trace
            .as_ref()
            .unwrap()
            .result
            .to_snapshot(&self.entry_label());
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
        self.invalidate_cache();
        Ok(())
    }

    /// Start watching the project root for file changes.
    ///
    /// After calling this, `refresh()` will short-circuit when no relevant
    /// files have changed since the last refresh. Idempotent: calling
    /// `watch()` again replaces the existing watcher.
    pub fn watch(&mut self) {
        let dirty = Arc::clone(&self.dirty);
        let extensions: Vec<String> = self
            .valid_extensions
            .iter()
            .map(|&e| e.to_string())
            .collect();

        let handler = move |event: notify::Result<notify::Event>| {
            if dirty.load(Ordering::Relaxed) {
                return;
            }
            let Ok(event) = event else { return };
            match event.kind {
                notify::EventKind::Create(_)
                | notify::EventKind::Modify(_)
                | notify::EventKind::Remove(_) => {}
                _ => return,
            }
            if event.paths.iter().any(|p| is_relevant_path(p, &extensions)) {
                dirty.store(true, Ordering::Release);
            }
        };

        if let Ok(mut watcher) = RecommendedWatcher::new(handler, notify::Config::default())
            && watcher.watch(&self.root, RecursiveMode::Recursive).is_ok()
        {
            self.watcher = Some(watcher);
        }
    }

    /// Whether the watcher has detected changes since the last refresh.
    pub fn is_dirty(&self) -> bool {
        self.dirty.load(Ordering::Acquire)
    }

    /// Check for file changes and rebuild the graph if needed.
    ///
    /// Returns `true` if the graph was updated (cold build or module count
    /// changed since the last load).
    #[allow(clippy::used_underscore_binding)] // _cache_handle held for drop
    pub fn refresh(&mut self) -> Result<bool, Error> {
        // Fast path: if a watcher is active and no relevant files changed,
        // skip the full cache-hit path entirely.
        if self.watcher.is_some() && !self.dirty.swap(false, Ordering::AcqRel) {
            return Ok(false);
        }

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
            self.invalidate_cache();
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

    // -- query cache --

    fn invalidate_cache(&mut self) {
        self.cached_trace = None;
        self.cached_weights = None;
    }

    fn ensure_trace(&mut self, opts: &TraceOptions) {
        let valid = self.cached_trace.as_ref().is_some_and(|c| {
            c.entry_id == self.entry_id && c.include_dynamic == opts.include_dynamic
        });
        if !valid {
            let result = query::trace(&self.graph, self.entry_id, opts);
            self.cached_trace = Some(CachedTrace {
                entry_id: self.entry_id,
                include_dynamic: opts.include_dynamic,
                result,
            });
        }
    }

    fn ensure_weights(&mut self, include_dynamic: bool) {
        let valid = self
            .cached_weights
            .as_ref()
            .is_some_and(|c| c.entry_id == self.entry_id && c.include_dynamic == include_dynamic);
        if !valid {
            let weights =
                query::compute_exclusive_weights(&self.graph, self.entry_id, include_dynamic);
            self.cached_weights = Some(CachedWeights {
                entry_id: self.entry_id,
                include_dynamic,
                weights,
            });
        }
    }

    // -- report builders --

    /// Trace and produce a display-ready report.
    pub fn trace_report(&mut self, opts: &TraceOptions, top_modules: i32) -> TraceReport {
        self.ensure_trace(opts);
        let result = &self.cached_trace.as_ref().unwrap().result;
        build_trace_report(
            result,
            &self.entry,
            &self.graph,
            &self.root,
            opts,
            top_modules,
        )
    }

    /// Trace from a different file and produce a display-ready report.
    pub fn trace_from_report(
        &self,
        file: &Path,
        opts: &TraceOptions,
        top_modules: i32,
    ) -> Result<(TraceReport, PathBuf), Error> {
        let (result, canon) = self.trace_from(file, opts)?;
        Ok((
            build_trace_report(&result, &canon, &self.graph, &self.root, opts, top_modules),
            canon,
        ))
    }

    /// Find import chains and produce a display-ready report.
    pub fn chain_report(&self, target_arg: &str, include_dynamic: bool) -> ChainReport {
        let (resolved, chains) = self.chain(target_arg, include_dynamic);
        ChainReport {
            target: resolved.label,
            found_in_graph: resolved.exists,
            chain_count: chains.len(),
            hop_count: chains.first().map_or(0, |c| c.len().saturating_sub(1)),
            chains: chains
                .iter()
                .map(|chain| report::chain_display_names(&self.graph, chain, &self.root))
                .collect(),
        }
    }

    /// Find cut points and produce a display-ready report.
    pub fn cut_report(&mut self, target_arg: &str, top: i32, include_dynamic: bool) -> CutReport {
        let (resolved, chains, cuts) = self.cut(target_arg, top, include_dynamic);
        CutReport {
            target: resolved.label,
            found_in_graph: resolved.exists,
            chain_count: chains.len(),
            direct_import: cuts.is_empty() && chains.iter().all(|c| c.len() == 2),
            cut_points: cuts
                .iter()
                .map(|c| CutEntry {
                    module: report::display_name(&self.graph, c.module_id, &self.root),
                    exclusive_size_bytes: c.exclusive_size,
                    chains_broken: c.chains_broken,
                })
                .collect(),
        }
    }

    /// Diff two entry points and produce a display-ready report.
    pub fn diff_report(
        &mut self,
        other: &Path,
        opts: &TraceOptions,
        limit: i32,
    ) -> Result<DiffReport, Error> {
        let (diff, other_canon) = self.diff_entry(other, opts)?;
        let entry_a = self.entry_label();
        let entry_b = self.entry_label_for(&other_canon);
        Ok(DiffReport::from_diff(&diff, &entry_a, &entry_b, limit))
    }

    /// List packages and produce a display-ready report.
    #[allow(clippy::cast_sign_loss)]
    pub fn packages_report(&self, top: i32) -> PackagesReport {
        let mut packages: Vec<_> = self.graph.package_map.values().collect();
        packages.sort_by(|a, b| b.total_reachable_size.cmp(&a.total_reachable_size));
        let total = packages.len();
        let display_count = if top < 0 {
            total
        } else {
            total.min(top as usize)
        };

        PackagesReport {
            package_count: total,
            packages: packages[..display_count]
                .iter()
                .map(|pkg| PackageListEntry {
                    name: pkg.name.clone(),
                    size: pkg.total_reachable_size,
                    files: pkg.total_reachable_files,
                })
                .collect(),
        }
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

#[allow(clippy::cast_sign_loss)]
fn build_trace_report(
    result: &TraceResult,
    entry_path: &Path,
    graph: &ModuleGraph,
    root: &Path,
    opts: &TraceOptions,
    top_modules: i32,
) -> TraceReport {
    let heavy_packages = result
        .heavy_packages
        .iter()
        .map(|pkg| PackageEntry {
            name: pkg.name.clone(),
            total_size_bytes: pkg.total_size,
            file_count: pkg.file_count,
            chain: report::chain_display_names(graph, &pkg.chain, root),
        })
        .collect();

    let display_count = if top_modules < 0 {
        result.modules_by_cost.len()
    } else {
        result.modules_by_cost.len().min(top_modules as usize)
    };
    let modules_by_cost = result.modules_by_cost[..display_count]
        .iter()
        .map(|mc| ModuleEntry {
            path: report::relative_path(&graph.module(mc.module_id).path, root),
            exclusive_size_bytes: mc.exclusive_size,
        })
        .collect();

    TraceReport {
        entry: report::relative_path(entry_path, root),
        static_weight_bytes: result.static_weight,
        static_module_count: result.static_module_count,
        dynamic_only_weight_bytes: result.dynamic_only_weight,
        dynamic_only_module_count: result.dynamic_only_module_count,
        heavy_packages,
        modules_by_cost,
        total_modules_with_cost: result.modules_by_cost.len(),
        include_dynamic: opts.include_dynamic,
        top: opts.top_n,
    }
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

/// Directories whose contents are never relevant to the dependency graph.
const EXCLUDED_DIRS: &[&str] = &["node_modules", ".git", "__pycache__", ".chainsaw", "target"];

/// Check whether a filesystem event path is relevant to the dependency graph.
///
/// Returns true for source files with matching extensions and lockfiles.
/// Returns false for files inside excluded directories or with unrelated extensions.
fn is_relevant_path<S: AsRef<str>>(path: &Path, valid_extensions: &[S]) -> bool {
    // Reject paths inside excluded directories.
    for component in path.components() {
        if let std::path::Component::Normal(s) = component
            && let Some(s) = s.to_str()
            && EXCLUDED_DIRS.contains(&s)
        {
            return false;
        }
    }

    // Accept lockfiles by filename.
    if let Some(name) = path.file_name().and_then(|n| n.to_str())
        && LOCKFILES.contains(&name)
    {
        return true;
    }

    // Accept source files by extension.
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| valid_extensions.iter().any(|e| e.as_ref() == ext))
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
        let mut session = Session::open(&entry, true).unwrap();
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

        let mut session = Session::open(&a, true).unwrap();
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
    fn event_filter_accepts_ts_source() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(is_relevant_path(Path::new("/project/src/index.ts"), exts));
        assert!(is_relevant_path(Path::new("/project/lib/utils.jsx"), exts));
    }

    #[test]
    fn event_filter_accepts_py_source() {
        let exts = &["py"];
        assert!(is_relevant_path(Path::new("/project/app/main.py"), exts));
    }

    #[test]
    fn event_filter_rejects_wrong_extension() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!is_relevant_path(Path::new("/project/README.md"), exts));
        assert!(!is_relevant_path(Path::new("/project/image.png"), exts));
        assert!(!is_relevant_path(Path::new("/project/Makefile"), exts));
    }

    #[test]
    fn event_filter_rejects_excluded_dirs() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!is_relevant_path(
            Path::new("/project/node_modules/zod/index.ts"),
            exts
        ));
        assert!(!is_relevant_path(
            Path::new("/project/.git/objects/abc"),
            exts
        ));
        assert!(!is_relevant_path(
            Path::new("/project/__pycache__/mod.py"),
            exts
        ));
        assert!(!is_relevant_path(
            Path::new("/project/.chainsaw/cache"),
            exts
        ));
        assert!(!is_relevant_path(
            Path::new("/project/target/debug/build.rs"),
            exts
        ));
    }

    #[test]
    fn event_filter_accepts_lockfiles() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(is_relevant_path(
            Path::new("/project/package-lock.json"),
            exts
        ));
        assert!(is_relevant_path(Path::new("/project/pnpm-lock.yaml"), exts));
        assert!(is_relevant_path(Path::new("/project/yarn.lock"), exts));
        assert!(is_relevant_path(Path::new("/project/bun.lockb"), exts));
        assert!(is_relevant_path(Path::new("/project/poetry.lock"), exts));
        assert!(is_relevant_path(Path::new("/project/Pipfile.lock"), exts));
        assert!(is_relevant_path(Path::new("/project/uv.lock"), exts));
        assert!(is_relevant_path(
            Path::new("/project/requirements.txt"),
            exts
        ));
    }

    #[test]
    fn event_filter_rejects_no_extension_non_lockfile() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!is_relevant_path(Path::new("/project/Dockerfile"), exts));
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

    #[test]
    fn trace_report_has_display_ready_fields() {
        let (_tmp, entry) = test_project();
        let mut session = Session::open(&entry, true).unwrap();
        let opts = TraceOptions::default();
        let report = session.trace_report(&opts, report::DEFAULT_TOP_MODULES);
        assert!(report.entry.contains("index.ts"));
        assert!(report.static_weight_bytes > 0);
        assert_eq!(report.static_module_count, 2);
        // No ModuleIds -- paths are strings
        assert!(
            report
                .modules_by_cost
                .iter()
                .all(|m| m.path.contains(".ts"))
        );
    }

    #[test]
    fn chain_report_resolves_to_strings() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let report = session.chain_report("a.ts", false);
        assert!(report.found_in_graph);
        assert_eq!(report.chain_count, 1);
        assert!(report.chains[0].iter().any(|s| s.contains("a.ts")));
    }

    #[test]
    fn cut_report_direct_import() {
        let (_tmp, entry) = test_project();
        let mut session = Session::open(&entry, true).unwrap();
        let report = session.cut_report("a.ts", 10, false);
        assert!(report.found_in_graph);
        assert_eq!(report.chain_count, 1);
        assert!(report.direct_import);
        assert!(report.cut_points.is_empty());
    }

    #[test]
    fn packages_report_empty_for_first_party() {
        let (_tmp, entry) = test_project();
        let session = Session::open(&entry, true).unwrap();
        let report = session.packages_report(report::DEFAULT_TOP);
        assert_eq!(report.package_count, 0);
        assert!(report.packages.is_empty());
    }

    #[test]
    fn watch_then_refresh_returns_false_when_clean() {
        let (_tmp, entry) = test_project();
        let mut session = Session::open(&entry, true).unwrap();
        session.watch();
        // No files changed — refresh should be instant and return false.
        let changed = session.refresh().unwrap();
        assert!(!changed);
    }

    #[test]
    fn refresh_without_watch_still_works() {
        // Backward compat: no watch() call, refresh runs the full path.
        let (_tmp, entry) = test_project();
        let mut session = Session::open(&entry, false).unwrap();
        // Wait for cache write to complete, then refresh hits the cache.
        std::thread::sleep(std::time::Duration::from_millis(50));
        let changed = session.refresh().unwrap();
        assert!(!changed); // cache hit, nothing changed
    }

    #[test]
    fn watch_detects_file_modification() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let entry = root.join("index.ts");
        std::fs::write(&entry, r#"import { x } from "./a";"#).unwrap();
        std::fs::write(root.join("a.ts"), "export const x = 1;").unwrap();

        let mut session = Session::open(&entry, true).unwrap();
        session.watch();

        // Modify a source file.
        std::thread::sleep(std::time::Duration::from_millis(100));
        std::fs::write(root.join("a.ts"), "export const x = 2;").unwrap();

        // Give the watcher time to deliver the event.
        std::thread::sleep(std::time::Duration::from_millis(200));

        assert!(session.is_dirty());
        let _changed = session.refresh().unwrap();
        // After refresh, the flag is cleared regardless of changed return value.
        assert!(!session.is_dirty());
    }

    #[test]
    fn cached_trace_invalidated_on_set_entry() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let a = root.join("a.ts");
        std::fs::write(&a, r#"import { x } from "./b";"#).unwrap();
        let b = root.join("b.ts");
        std::fs::write(&b, "export const x = 1;").unwrap();

        let mut session = Session::open(&a, true).unwrap();
        let opts = crate::query::TraceOptions::default();

        let r1 = session.trace_report(&opts, 10);
        assert_eq!(r1.static_module_count, 2);

        session.set_entry(&b).unwrap();

        let r2 = session.trace_report(&opts, 10);
        assert_eq!(r2.static_module_count, 1);
    }

    #[test]
    fn cached_trace_invalidated_on_refresh() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let entry = root.join("index.ts");
        std::fs::write(&entry, r#"import { x } from "./a";"#).unwrap();
        std::fs::write(root.join("a.ts"), "export const x = 1;").unwrap();

        let mut session = Session::open(&entry, true).unwrap();
        let opts = crate::query::TraceOptions::default();

        let r1 = session.trace_report(&opts, 10);
        assert_eq!(r1.static_module_count, 2);

        std::thread::sleep(std::time::Duration::from_millis(50));
        std::fs::write(
            &entry,
            r#"import { x } from "./a"; import { y } from "./b";"#,
        )
        .unwrap();
        std::fs::write(root.join("b.ts"), "export const y = 2;").unwrap();

        let changed = session.refresh().unwrap();
        assert!(changed);

        let r2 = session.trace_report(&opts, 10);
        assert_eq!(r2.static_module_count, 3);
    }

    #[test]
    fn cut_uses_cached_exclusive_weights() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        std::fs::write(root.join("package.json"), r#"{"name":"test"}"#).unwrap();
        let entry = root.join("entry.ts");
        std::fs::write(
            &entry,
            r#"import { a } from "./a"; import { b } from "./b";"#,
        )
        .unwrap();
        std::fs::write(
            root.join("a.ts"),
            r#"import { c } from "./c"; export const a = 1;"#,
        )
        .unwrap();
        std::fs::write(
            root.join("b.ts"),
            r#"import { c } from "./c"; export const b = 1;"#,
        )
        .unwrap();
        std::fs::write(
            root.join("c.ts"),
            r#"import { z } from "./node_modules/zod/index.js"; export const c = 1;"#,
        )
        .unwrap();
        std::fs::create_dir_all(root.join("node_modules/zod")).unwrap();
        std::fs::write(
            root.join("node_modules/zod/index.js"),
            "export const z = 1;",
        )
        .unwrap();
        std::fs::write(
            root.join("node_modules/zod/package.json"),
            r#"{"name":"zod"}"#,
        )
        .unwrap();

        let mut session = Session::open(&entry, true).unwrap();

        let opts = crate::query::TraceOptions::default();
        session.trace_report(&opts, 10);

        let (_, chains, cuts) = session.cut("zod", 10, false);
        assert!(!chains.is_empty());
        assert!(
            cuts.iter()
                .any(|c| session.graph().module(c.module_id).path.ends_with("c.ts"))
        );
    }

    /// Verify query cache produces measurable speedup.
    /// Run: `cargo test --lib session::tests::verify_cache_speedup -- --ignored --nocapture`
    #[test]
    #[ignore = "requires local wrangler checkout"]
    fn verify_cache_speedup() {
        use std::time::Instant;

        let wrangler =
            Path::new("/Users/hlal/dev/cloudflare/workers-sdk/packages/wrangler/src/index.ts");
        if !wrangler.exists() {
            eprintln!("SKIP: wrangler not found");
            return;
        }
        let mut session = Session::open(wrangler, true).unwrap();
        let opts = crate::query::TraceOptions::default();

        let t1 = Instant::now();
        let r1 = session.trace_report(&opts, 10);
        let first = t1.elapsed();

        let t2 = Instant::now();
        let r2 = session.trace_report(&opts, 10);
        let second = t2.elapsed();

        assert_eq!(r1.static_weight_bytes, r2.static_weight_bytes);
        assert_eq!(r1.static_module_count, r2.static_module_count);

        eprintln!(
            "  first trace_report:  {:.0}us",
            first.as_secs_f64() * 1_000_000.0
        );
        eprintln!(
            "  second trace_report: {:.0}us",
            second.as_secs_f64() * 1_000_000.0
        );
        eprintln!(
            "  speedup: {:.1}x",
            first.as_secs_f64() / second.as_secs_f64()
        );

        assert!(
            second < first / 3,
            "expected cache hit to be at least 3x faster: first={first:?}, second={second:?}"
        );
    }
}
