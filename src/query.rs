//! Graph queries: trace weight, import chains, cut points, and diffs.

use std::collections::{HashMap, HashSet, VecDeque};

use serde::{Deserialize, Serialize};

use crate::graph::{EdgeKind, ModuleGraph, ModuleId};

/// Results of tracing transitive import weight from an entry module.
#[derive(Debug)]
#[non_exhaustive]
pub struct TraceResult {
    /// Total file size reachable via static imports
    pub static_weight: u64,
    /// Number of modules reachable via static imports
    pub static_module_count: usize,
    /// Total file size reachable only via dynamic imports (not already counted in static)
    pub dynamic_only_weight: u64,
    /// Number of modules reachable only via dynamic imports
    pub dynamic_only_module_count: usize,
    /// Heavy packages found via static imports, sorted by total reachable size descending
    pub heavy_packages: Vec<HeavyPackage>,
    /// All reachable modules with their exclusive weight, sorted descending
    pub modules_by_cost: Vec<ModuleCost>,
    /// All statically reachable packages with their total size
    pub all_packages: HashMap<String, u64>,
    /// Packages reachable only via dynamic imports (not in static set)
    pub dynamic_packages: HashMap<String, u64>,
}

/// A third-party package with its reachable size and shortest import chain.
#[derive(Debug)]
#[non_exhaustive]
pub struct HeavyPackage {
    pub name: String,
    pub total_size: u64,
    pub file_count: u32,
    /// Shortest chain from entry point to the first module in this package
    pub chain: Vec<ModuleId>,
}

/// A module with its exclusive weight (bytes only it contributes to the graph).
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub struct ModuleCost {
    pub module_id: ModuleId,
    pub exclusive_size: u64,
}

/// Options controlling which edges to follow and how many results to return.
#[derive(Debug)]
pub struct TraceOptions {
    pub include_dynamic: bool,
    pub top_n: i32,
    pub ignore: Vec<String>,
}

impl Default for TraceOptions {
    fn default() -> Self {
        Self {
            include_dynamic: false,
            top_n: 10,
            ignore: Vec::new(),
        }
    }
}

/// Target for `--chain`/`--cut` queries: a package name or a specific module.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum ChainTarget {
    /// Match any module belonging to this third-party package.
    Package(String),
    /// Match a specific module by its graph id.
    Module(ModuleId),
}

impl ChainTarget {
    fn matches(&self, graph: &ModuleGraph, mid: ModuleId) -> bool {
        match self {
            Self::Package(name) => graph.module(mid).package.as_deref() == Some(name),
            Self::Module(target) => mid == *target,
        }
    }
}

/// Whether to follow an edge based on its kind and the `include_dynamic` flag.
const fn should_follow(kind: EdgeKind, include_dynamic: bool) -> bool {
    match kind {
        EdgeKind::Static => true,
        EdgeKind::Dynamic if include_dynamic => true,
        _ => false,
    }
}

/// Iterative DFS to compute reverse postorder and predecessor lists in one pass.
fn reverse_postorder_with_preds(
    graph: &ModuleGraph,
    entry: ModuleId,
    include_dynamic: bool,
) -> (Vec<ModuleId>, Vec<Vec<u32>>) {
    let n = graph.modules.len();
    let mut visited = vec![false; n];
    let mut postorder = Vec::new();
    let mut preds: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut stack: Vec<(ModuleId, bool)> = vec![(entry, false)];

    while let Some((mid, post_visit)) = stack.pop() {
        let idx = mid.0 as usize;
        if post_visit {
            postorder.push(mid);
            continue;
        }
        if visited[idx] {
            continue;
        }
        visited[idx] = true;
        stack.push((mid, true));
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            if should_follow(edge.kind, include_dynamic) {
                let to_idx = edge.to.0 as usize;
                if visited[to_idx] {
                    // Back/cross edge to already-visited (reachable) node
                    preds[to_idx].push(mid.0);
                } else {
                    // Tree edge — record predecessor and visit
                    preds[to_idx].push(mid.0);
                    stack.push((edge.to, false));
                }
            }
        }
    }

    postorder.reverse();
    (postorder, preds)
}

/// Walk up dominator tree to find common dominator of a and b.
fn intersect_idom(idom: &[u32], rpo_num: &[u32], mut a: u32, mut b: u32) -> u32 {
    while a != b {
        while rpo_num[a as usize] > rpo_num[b as usize] {
            a = idom[a as usize];
        }
        while rpo_num[b as usize] > rpo_num[a as usize] {
            b = idom[b as usize];
        }
    }
    a
}

/// Compute exclusive weight for every reachable module using a dominator tree.
///
/// Exclusive weight of module M = total size of all modules in M's dominator
/// subtree (modules that become unreachable if M is removed from the graph).
/// Uses the Cooper-Harvey-Kennedy iterative dominator algorithm: O(N).
#[allow(clippy::cast_possible_truncation)]
fn compute_exclusive_weights(
    graph: &ModuleGraph,
    entry: ModuleId,
    include_dynamic: bool,
) -> Vec<u64> {
    let n = graph.modules.len();

    // Step 1+3: DFS for reverse postorder and predecessor lists in one pass
    let (rpo, preds) = reverse_postorder_with_preds(graph, entry, include_dynamic);
    if rpo.is_empty() {
        return vec![0; n];
    }

    // Step 2: RPO numbering (lower = earlier)
    let mut rpo_num = vec![u32::MAX; n];
    for (i, &mid) in rpo.iter().enumerate() {
        rpo_num[mid.0 as usize] = i as u32;
    }

    // Step 4: Cooper-Harvey-Kennedy iterative idom computation
    let entry_idx = entry.0;
    let mut idom = vec![u32::MAX; n];
    idom[entry_idx as usize] = entry_idx;

    let mut changed = true;
    while changed {
        changed = false;
        for &mid in rpo.iter().skip(1) {
            let idx = mid.0 as usize;
            let mut new_idom: Option<u32> = None;
            for &p in &preds[idx] {
                if idom[p as usize] != u32::MAX {
                    new_idom = Some(
                        new_idom.map_or(p, |current| intersect_idom(&idom, &rpo_num, current, p)),
                    );
                }
            }
            if let Some(ni) = new_idom
                && idom[idx] != ni
            {
                idom[idx] = ni;
                changed = true;
            }
        }
    }

    // Step 5: Build dominator tree children
    let mut children: Vec<Vec<u32>> = vec![Vec::new(); n];
    for &mid in &rpo {
        let idx = mid.0 as usize;
        let dom = idom[idx];
        if dom != u32::MAX && dom != idx as u32 {
            children[dom as usize].push(idx as u32);
        }
    }

    // Step 6: Post-order DFS of dominator tree to accumulate subtree sums
    let mut weights = vec![0u64; n];
    let mut stack: Vec<(u32, bool)> = vec![(entry_idx, false)];
    while let Some((node, post_visit)) = stack.pop() {
        if post_visit {
            weights[node as usize] = graph.modules[node as usize].size_bytes;
            for &child in &children[node as usize] {
                weights[node as usize] += weights[child as usize];
            }
            continue;
        }
        stack.push((node, true));
        for &child in &children[node as usize] {
            stack.push((child, false));
        }
    }

    weights
}

struct BfsResult {
    static_set: Vec<ModuleId>,
    dynamic_set: Vec<ModuleId>,
    /// BFS parent pointers from the static traversal. parent[i] is the
    /// predecessor of module i on the shortest static path from entry.
    /// Entry and unreachable modules have `u32::MAX`.
    static_parent: Vec<u32>,
}

/// BFS from entry point, collecting all reachable modules.
/// Also records parent pointers during the static phase for chain reconstruction.
fn bfs_reachable(graph: &ModuleGraph, entry: ModuleId) -> BfsResult {
    let n = graph.modules.len();
    let mut visited = vec![false; n];
    let mut parent = vec![u32::MAX; n];
    let mut static_set: Vec<ModuleId> = Vec::new();
    let mut queue: VecDeque<ModuleId> = VecDeque::new();

    visited[entry.0 as usize] = true;
    static_set.push(entry);
    queue.push_back(entry);

    // BFS following static edges
    while let Some(mid) = queue.pop_front() {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            let idx = edge.to.0 as usize;
            if edge.kind == EdgeKind::Static && !visited[idx] {
                visited[idx] = true;
                parent[idx] = mid.0;
                static_set.push(edge.to);
                queue.push_back(edge.to);
            }
        }
    }

    // BFS following dynamic edges from all statically reachable modules
    let mut dynamic_set: Vec<ModuleId> = Vec::new();
    let mut dyn_queue: VecDeque<ModuleId> = VecDeque::new();

    for &mid in &static_set {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            let idx = edge.to.0 as usize;
            if edge.kind == EdgeKind::Dynamic && !visited[idx] {
                visited[idx] = true;
                dynamic_set.push(edge.to);
                dyn_queue.push_back(edge.to);
            }
        }
    }

    // Continue BFS from dynamic-only modules (they may have static imports of their own)
    while let Some(mid) = dyn_queue.pop_front() {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            let idx = edge.to.0 as usize;
            if (edge.kind == EdgeKind::Static || edge.kind == EdgeKind::Dynamic) && !visited[idx] {
                visited[idx] = true;
                dynamic_set.push(edge.to);
                dyn_queue.push_back(edge.to);
            }
        }
    }

    BfsResult {
        static_set,
        dynamic_set,
        static_parent: parent,
    }
}

/// Reconstruct the shortest chain from entry to target using pre-computed
/// BFS parent pointers. Returns empty vec if target is unreachable.
fn reconstruct_chain(parent: &[u32], entry: ModuleId, target: ModuleId) -> Vec<ModuleId> {
    let mut chain = vec![target];
    let mut current = target.0;
    while current != entry.0 {
        let p = parent[current as usize];
        if p == u32::MAX {
            return Vec::new();
        }
        chain.push(ModuleId(p));
        current = p;
    }
    chain.reverse();
    chain
}

#[must_use]
#[allow(clippy::cast_sign_loss)]
pub fn trace(graph: &ModuleGraph, entry: ModuleId, opts: &TraceOptions) -> TraceResult {
    let bfs = bfs_reachable(graph, entry);
    let mut reachable = bfs.static_set;
    let dynamic_only = bfs.dynamic_set;

    // When --include-dynamic is set, fold dynamic modules into the reachable
    // set. There's nothing "only dynamic" when the user asked to include them.
    // Compute dynamic-only packages before potentially merging sets
    let mut dynamic_pkg_sizes: HashMap<String, u64> = HashMap::new();
    for &mid in &dynamic_only {
        let module = graph.module(mid);
        if let Some(ref pkg) = module.package {
            *dynamic_pkg_sizes.entry(pkg.clone()).or_default() += module.size_bytes;
        }
    }

    let (dynamic_only_weight, dynamic_only_module_count) = if opts.include_dynamic {
        reachable.extend_from_slice(&dynamic_only);
        (0, 0)
    } else {
        let w: u64 = dynamic_only
            .iter()
            .map(|&mid| graph.module(mid).size_bytes)
            .sum();
        (w, dynamic_only.len())
    };

    let static_weight: u64 = reachable
        .iter()
        .map(|&mid| graph.module(mid).size_bytes)
        .sum();

    // Find heavy packages in the reachable set, tracking the first module
    // encountered per package (BFS order = shortest distance from entry).
    let mut package_sizes: HashMap<String, (u64, u32)> = HashMap::new();
    let mut package_nearest: HashMap<String, ModuleId> = HashMap::new();
    for &mid in &reachable {
        let module = graph.module(mid);
        if let Some(ref pkg) = module.package {
            let e = package_sizes.entry(pkg.clone()).or_default();
            e.0 += module.size_bytes;
            e.1 += 1;
            package_nearest.entry(pkg.clone()).or_insert(mid);
        }
    }

    let all_packages: HashMap<String, u64> = package_sizes
        .iter()
        .map(|(k, (size, _))| (k.clone(), *size))
        .collect();

    // Sort and truncate BEFORE computing chains (each chain is a full BFS)
    let mut sorted_packages: Vec<(String, u64, u32)> = package_sizes
        .into_iter()
        .map(|(name, (total_size, file_count))| (name, total_size, file_count))
        .collect();
    sorted_packages.sort_by(|a, b| b.1.cmp(&a.1));
    if !opts.ignore.is_empty() {
        sorted_packages.retain(|(name, _, _)| !opts.ignore.iter().any(|i| i == name));
    }
    if opts.top_n >= 0 {
        sorted_packages.truncate(opts.top_n as usize);
    }
    let heavy_packages: Vec<HeavyPackage> = sorted_packages
        .into_iter()
        .map(|(name, total_size, file_count)| {
            let chain = match package_nearest.get(&name) {
                Some(&nearest) => reconstruct_chain(&bfs.static_parent, entry, nearest),
                None => Vec::new(),
            };
            HeavyPackage {
                name,
                total_size,
                file_count,
                chain,
            }
        })
        .collect();

    // Compute exclusive weight for all reachable modules via dominator tree
    let exclusive = compute_exclusive_weights(graph, entry, opts.include_dynamic);

    // Prefer first-party (no package) modules for the per-file breakdown.
    // Fall back to all modules when no first-party modules exist (e.g. Python
    // projects where every reachable module is a third-party package).
    let mut modules_by_cost: Vec<ModuleCost> = reachable
        .iter()
        .filter(|&&mid| mid != entry && graph.module(mid).package.is_none())
        .map(|&mid| ModuleCost {
            module_id: mid,
            exclusive_size: exclusive[mid.0 as usize],
        })
        .collect();
    if modules_by_cost.is_empty() {
        modules_by_cost = reachable
            .iter()
            .filter(|&&mid| mid != entry)
            .map(|&mid| ModuleCost {
                module_id: mid,
                exclusive_size: exclusive[mid.0 as usize],
            })
            .collect();
    }

    modules_by_cost.sort_by(|a, b| b.exclusive_size.cmp(&a.exclusive_size));

    TraceResult {
        static_weight,
        static_module_count: reachable.len(),
        dynamic_only_weight,
        dynamic_only_module_count,
        heavy_packages,
        modules_by_cost,
        all_packages,
        dynamic_packages: dynamic_pkg_sizes,
    }
}

/// Find ALL shortest chains from entry to a specific target (package or module).
///
/// Returns up to `max_chains` distinct shortest paths (all same hop count),
/// deduplicated at the package-name level so chains that differ only by
/// internal file paths are collapsed into one.
#[must_use]
pub fn find_all_chains(
    graph: &ModuleGraph,
    entry: ModuleId,
    target: &ChainTarget,
    include_dynamic: bool,
) -> Vec<Vec<ModuleId>> {
    let raw = all_shortest_chains(graph, entry, target, 10, include_dynamic);
    dedup_chains_by_package(graph, raw)
}

/// Deduplicate chains that look identical at the package-name level.
/// Two chains that differ only by which internal file within a package
/// they pass through will have the same package-level key and only the
/// first is kept.
fn dedup_chains_by_package(graph: &ModuleGraph, chains: Vec<Vec<ModuleId>>) -> Vec<Vec<ModuleId>> {
    let mut seen: HashSet<Vec<String>> = HashSet::new();
    let mut result = Vec::new();

    for chain in chains {
        let key: Vec<String> = chain
            .iter()
            .map(|&mid| {
                let m = graph.module(mid);
                m.package
                    .clone()
                    .unwrap_or_else(|| m.path.to_string_lossy().into_owned())
            })
            .collect();

        if seen.insert(key) {
            result.push(chain);
        }
    }

    result
}

/// BFS with multi-parent tracking to find all shortest paths to a target.
fn all_shortest_chains(
    graph: &ModuleGraph,
    entry: ModuleId,
    target: &ChainTarget,
    max_chains: usize,
    include_dynamic: bool,
) -> Vec<Vec<ModuleId>> {
    let n = graph.modules.len();
    let mut parents: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut depth: Vec<u32> = vec![u32::MAX; n];
    let mut queue: VecDeque<ModuleId> = VecDeque::new();

    depth[entry.0 as usize] = 0;
    queue.push_back(entry);

    let mut target_depth: Option<u32> = None;
    let mut targets: Vec<ModuleId> = Vec::new();

    while let Some(mid) = queue.pop_front() {
        let d = depth[mid.0 as usize];

        // If we've found targets and moved past their depth, stop
        if let Some(td) = target_depth
            && d > td
        {
            break;
        }

        // Check if this module matches the target
        if target.matches(graph, mid) {
            if target_depth.is_none() {
                target_depth = Some(d);
            }
            targets.push(mid);
            continue; // Don't expand past target package modules
        }

        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            match edge.kind {
                EdgeKind::Static => {}
                EdgeKind::Dynamic if include_dynamic => {}
                _ => continue,
            }

            let next_depth = d + 1;
            let idx = edge.to.0 as usize;
            match depth[idx] {
                d if d == next_depth => {
                    // Same depth -- add as alternate parent
                    parents[idx].push(mid.0);
                }
                u32::MAX => {
                    // First visit
                    depth[idx] = next_depth;
                    parents[idx].push(mid.0);
                    queue.push_back(edge.to);
                }
                _ => {} // Already visited at shorter depth, skip
            }
        }
    }

    if targets.is_empty() {
        return Vec::new();
    }

    // Backtrack from each target to reconstruct all paths
    let mut all_chains: Vec<Vec<ModuleId>> = Vec::new();
    for &target_mid in &targets {
        let mut partial_paths: Vec<Vec<ModuleId>> = vec![vec![target_mid]];

        loop {
            let mut next_partial: Vec<Vec<ModuleId>> = Vec::new();
            let mut any_extended = false;

            for path in &partial_paths {
                let &head = path.last().unwrap();
                if head == entry {
                    next_partial.push(path.clone());
                    continue;
                }
                let pars = &parents[head.0 as usize];
                if !pars.is_empty() {
                    any_extended = true;
                    for &p in pars {
                        let mut new_path = path.clone();
                        new_path.push(ModuleId(p));
                        next_partial.push(new_path);
                        if next_partial.len() > max_chains * 2 {
                            break; // Prevent combinatorial explosion
                        }
                    }
                }
            }

            partial_paths = next_partial;
            if !any_extended || partial_paths.len() > max_chains * 2 {
                break;
            }
        }

        for mut path in partial_paths {
            path.reverse();
            if path.first() == Some(&entry) {
                all_chains.push(path);
                if all_chains.len() >= max_chains {
                    return all_chains;
                }
            }
        }
    }

    all_chains
}

/// A module whose dynamic conversion would sever one or more import chains.
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub struct CutModule {
    pub module_id: ModuleId,
    pub chains_broken: usize,
    pub exclusive_size: u64,
}

/// Find modules that appear in all chains from entry to a package.
///
/// Removing any one of these severs every import path to the target.
/// Sorted by exclusive weight descending (highest-impact first),
/// truncated to `top_n`.
#[must_use]
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub fn find_cut_modules(
    graph: &ModuleGraph,
    chains: &[Vec<ModuleId>],
    entry: ModuleId,
    target: &ChainTarget,
    top_n: i32,
    include_dynamic: bool,
) -> Vec<CutModule> {
    if chains.is_empty() {
        return Vec::new();
    }

    let exclusive = compute_exclusive_weights(graph, entry, include_dynamic);

    let total = chains.len();
    let mut frequency = vec![0usize; graph.modules.len()];
    for chain in chains {
        for &mid in chain {
            frequency[mid.0 as usize] += 1;
        }
    }

    let mut cuts: Vec<CutModule> = frequency
        .iter()
        .enumerate()
        .filter(|&(idx, &count)| {
            let mid = ModuleId(idx as u32);
            count == total && mid != entry && !target.matches(graph, mid)
        })
        .map(|(idx, &count)| CutModule {
            module_id: ModuleId(idx as u32),
            chains_broken: count,
            exclusive_size: exclusive[idx],
        })
        .collect();

    // Deduplicate at package level -- multiple files within the same
    // node_modules package are the same cut point from the user's perspective.
    // Sort descending first so we keep the highest-weight entry per package.
    cuts.sort_by(|a, b| b.exclusive_size.cmp(&a.exclusive_size));
    let mut seen_packages: HashSet<String> = HashSet::new();
    cuts.retain(|c| {
        let m = graph.module(c.module_id);
        m.package
            .as_ref()
            .is_none_or(|pkg| seen_packages.insert(pkg.clone()))
    });

    // Single chain: sort ascending (most surgical cut first).
    // Multiple chains: sort descending (highest-impact convergence point first).
    if chains.len() == 1 {
        cuts.sort_by(|a, b| a.exclusive_size.cmp(&b.exclusive_size));
    }

    if top_n >= 0 {
        cuts.truncate(top_n as usize);
    }
    cuts
}

/// Minimal snapshot of a trace result for before/after comparison.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[non_exhaustive]
pub struct TraceSnapshot {
    pub entry: String,
    pub static_weight: u64,
    pub packages: HashMap<String, u64>,
    #[serde(default)]
    pub dynamic_weight: u64,
    #[serde(default)]
    pub dynamic_packages: HashMap<String, u64>,
}

impl TraceResult {
    pub fn to_snapshot(&self, entry: &str) -> TraceSnapshot {
        TraceSnapshot {
            entry: entry.to_string(),
            static_weight: self.static_weight,
            packages: self.all_packages.clone(),
            dynamic_weight: self.dynamic_only_weight,
            dynamic_packages: self.dynamic_packages.clone(),
        }
    }
}

/// A package that appears in only one side of a diff, with its size.
#[derive(Debug)]
#[non_exhaustive]
pub struct DiffPackage {
    pub name: String,
    pub size: u64,
}

/// Compute a diff between two trace snapshots.
#[derive(Debug)]
#[non_exhaustive]
pub struct DiffResult {
    pub entry_a_weight: u64,
    pub entry_b_weight: u64,
    pub weight_delta: i64,
    pub dynamic_a_weight: u64,
    pub dynamic_b_weight: u64,
    pub dynamic_weight_delta: i64,
    pub shared_count: usize,
    pub only_in_a: Vec<DiffPackage>,
    pub only_in_b: Vec<DiffPackage>,
    pub dynamic_only_in_a: Vec<DiffPackage>,
    pub dynamic_only_in_b: Vec<DiffPackage>,
}

#[must_use]
#[allow(clippy::cast_possible_wrap)]
pub fn diff_snapshots(a: &TraceSnapshot, b: &TraceSnapshot) -> DiffResult {
    let keys_a: HashSet<&str> = a.packages.keys().map(String::as_str).collect();
    let keys_b: HashSet<&str> = b.packages.keys().map(String::as_str).collect();

    let mut only_in_a: Vec<DiffPackage> = keys_a
        .difference(&keys_b)
        .map(|&name| DiffPackage {
            name: name.to_string(),
            size: a.packages[name],
        })
        .collect();
    only_in_a.sort_by(|x, y| y.size.cmp(&x.size));

    let mut only_in_b: Vec<DiffPackage> = keys_b
        .difference(&keys_a)
        .map(|&name| DiffPackage {
            name: name.to_string(),
            size: b.packages[name],
        })
        .collect();
    only_in_b.sort_by(|x, y| y.size.cmp(&x.size));

    let dyn_keys_a: HashSet<&str> = a.dynamic_packages.keys().map(String::as_str).collect();
    let dyn_keys_b: HashSet<&str> = b.dynamic_packages.keys().map(String::as_str).collect();

    let mut dynamic_only_in_a: Vec<DiffPackage> = dyn_keys_a
        .difference(&dyn_keys_b)
        .map(|&name| DiffPackage {
            name: name.to_string(),
            size: a.dynamic_packages[name],
        })
        .collect();
    dynamic_only_in_a.sort_by(|x, y| y.size.cmp(&x.size));

    let mut dynamic_only_in_b: Vec<DiffPackage> = dyn_keys_b
        .difference(&dyn_keys_a)
        .map(|&name| DiffPackage {
            name: name.to_string(),
            size: b.dynamic_packages[name],
        })
        .collect();
    dynamic_only_in_b.sort_by(|x, y| y.size.cmp(&x.size));

    DiffResult {
        entry_a_weight: a.static_weight,
        entry_b_weight: b.static_weight,
        weight_delta: b.static_weight as i64 - a.static_weight as i64,
        dynamic_a_weight: a.dynamic_weight,
        dynamic_b_weight: b.dynamic_weight,
        dynamic_weight_delta: b.dynamic_weight as i64 - a.dynamic_weight as i64,
        shared_count: keys_a.intersection(&keys_b).count(),
        only_in_a,
        only_in_b,
        dynamic_only_in_a,
        dynamic_only_in_b,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::ModuleGraph;
    use std::path::PathBuf;

    /// Build a small graph from declarative specs.
    /// `nodes`: `(path, size_bytes, package_name)`
    /// `edges`: `(from_index, to_index, kind)`
    fn make_graph(
        nodes: &[(&str, u64, Option<&str>)],
        edges: &[(usize, usize, EdgeKind)],
    ) -> ModuleGraph {
        let mut graph = ModuleGraph::new();
        for &(path, size, pkg) in nodes {
            graph.add_module(PathBuf::from(path), size, pkg.map(str::to_string));
        }
        for &(from, to, kind) in edges {
            #[allow(clippy::cast_possible_truncation)]
            graph.add_edge(ModuleId(from as u32), ModuleId(to as u32), kind, "");
        }
        graph
    }

    // --- BFS / trace ---

    #[test]
    fn trace_static_weight() {
        // A(100) -> B(200) -> C(300)
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 200, None),
                ("c.ts", 300, None),
            ],
            &[(0, 1, EdgeKind::Static), (1, 2, EdgeKind::Static)],
        );
        let result = trace(&graph, ModuleId(0), &TraceOptions::default());
        assert_eq!(result.static_weight, 600);
        assert_eq!(result.static_module_count, 3);
    }

    #[test]
    fn trace_dynamic_excluded_by_default() {
        // A(100) -static-> B(200), A -dynamic-> C(300)
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 200, None),
                ("c.ts", 300, None),
            ],
            &[(0, 1, EdgeKind::Static), (0, 2, EdgeKind::Dynamic)],
        );
        let result = trace(&graph, ModuleId(0), &TraceOptions::default());
        assert_eq!(result.static_weight, 300); // A + B only
        assert_eq!(result.dynamic_only_weight, 300); // C
        assert_eq!(result.dynamic_only_module_count, 1);
    }

    #[test]
    fn trace_include_dynamic() {
        // A(100) -dynamic-> B(200)
        let graph = make_graph(
            &[("a.ts", 100, None), ("b.ts", 200, None)],
            &[(0, 1, EdgeKind::Dynamic)],
        );
        let opts = TraceOptions {
            include_dynamic: true,
            top_n: 10,
            ignore: Vec::new(),
        };
        let result = trace(&graph, ModuleId(0), &opts);
        // B should appear in modules_by_cost when include_dynamic is set
        assert!(
            result
                .modules_by_cost
                .iter()
                .any(|m| m.module_id == ModuleId(1))
        );
    }

    // --- Chain finding ---

    #[test]
    fn chain_linear_path() {
        // A -> B -> C -> zod
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 100, None),
                ("c.ts", 100, None),
                ("node_modules/zod/index.js", 500, Some("zod")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (1, 2, EdgeKind::Static),
                (2, 3, EdgeKind::Static),
            ],
        );
        let chains = find_all_chains(
            &graph,
            ModuleId(0),
            &ChainTarget::Package("zod".to_string()),
            false,
        );
        assert_eq!(chains.len(), 1);
        assert_eq!(
            chains[0],
            vec![ModuleId(0), ModuleId(1), ModuleId(2), ModuleId(3)]
        );
    }

    #[test]
    fn chain_dedup_same_package_path() {
        // Two paths to zod that differ only by internal zod file:
        // A -> B -> zod/index.js
        // A -> B -> zod/lib.js
        // These should dedup to one chain at the package level.
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 100, None),
                ("node_modules/zod/index.js", 250, Some("zod")),
                ("node_modules/zod/lib.js", 250, Some("zod")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (1, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static),
            ],
        );
        let chains = find_all_chains(
            &graph,
            ModuleId(0),
            &ChainTarget::Package("zod".to_string()),
            false,
        );
        assert_eq!(chains.len(), 1);
    }

    #[test]
    fn chain_not_reachable() {
        // A -> B, no path to zod
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 100, None),
                ("node_modules/zod/index.js", 500, Some("zod")),
            ],
            &[(0, 1, EdgeKind::Static)],
        );
        let chains = find_all_chains(
            &graph,
            ModuleId(0),
            &ChainTarget::Package("zod".to_string()),
            false,
        );
        assert!(chains.is_empty());
    }

    #[test]
    fn chain_through_dynamic_edge() {
        // A -dynamic-> B -static-> zod
        // Without include_dynamic: no chain. With: chain found.
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 100, None),
                ("node_modules/zod/index.js", 500, Some("zod")),
            ],
            &[(0, 1, EdgeKind::Dynamic), (1, 2, EdgeKind::Static)],
        );
        let chains_static = find_all_chains(
            &graph,
            ModuleId(0),
            &ChainTarget::Package("zod".to_string()),
            false,
        );
        assert!(chains_static.is_empty());

        let chains_dynamic = find_all_chains(
            &graph,
            ModuleId(0),
            &ChainTarget::Package("zod".to_string()),
            true,
        );
        assert_eq!(chains_dynamic.len(), 1);
        assert_eq!(
            chains_dynamic[0],
            vec![ModuleId(0), ModuleId(1), ModuleId(2)]
        );
    }

    // --- Cut points ---

    #[test]
    fn cut_single_convergence_point() {
        // Diamond: A -> B -> D -> zod
        //          A -> C -> D -> zod
        // D is the cut point (appears in all chains)
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 100, None),
                ("c.ts", 100, None),
                ("d.ts", 100, None),
                ("node_modules/zod/index.js", 500, Some("zod")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static),
                (2, 3, EdgeKind::Static),
                (3, 4, EdgeKind::Static),
            ],
        );
        let target = ChainTarget::Package("zod".to_string());
        let chains = find_all_chains(&graph, ModuleId(0), &target, false);
        assert_eq!(chains.len(), 2);

        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), &target, 10, false);
        assert!(!cuts.is_empty());
        assert!(cuts.iter().any(|c| c.module_id == ModuleId(3)));
    }

    #[test]
    fn cut_no_convergence_point() {
        // Two independent paths:
        // A -> B -> zod1
        // A -> C -> zod2
        // No module (other than A and zod) appears in all chains.
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 100, None),
                ("c.ts", 100, None),
                ("node_modules/zod/index.js", 250, Some("zod")),
                ("node_modules/zod/lib.js", 250, Some("zod")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static),
                (2, 4, EdgeKind::Static),
            ],
        );
        let target = ChainTarget::Package("zod".to_string());
        let chains = find_all_chains(&graph, ModuleId(0), &target, false);
        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), &target, 10, false);
        assert!(cuts.is_empty());
    }

    #[test]
    fn cut_direct_import_no_intermediate() {
        // Entry directly imports target: A -> zod (1 hop)
        // No intermediate module exists to cut.
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("node_modules/zod/index.js", 500, Some("zod")),
            ],
            &[(0, 1, EdgeKind::Static)],
        );
        let target = ChainTarget::Package("zod".to_string());
        let chains = find_all_chains(&graph, ModuleId(0), &target, false);
        assert_eq!(chains.len(), 1);
        assert_eq!(chains[0].len(), 2); // 1 hop = 2 nodes

        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), &target, 10, false);
        assert!(cuts.is_empty());
    }

    #[test]
    fn cut_single_chain_ascending_sort() {
        // Single chain: A -> B(big) -> C(small) -> zod
        // With single chain, cuts should sort ascending (surgical first).
        let graph = make_graph(
            &[
                ("a.ts", 100, None),
                ("b.ts", 5000, None),
                ("c.ts", 100, None),
                ("node_modules/zod/index.js", 500, Some("zod")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (1, 2, EdgeKind::Static),
                (2, 3, EdgeKind::Static),
            ],
        );
        let target = ChainTarget::Package("zod".to_string());
        let chains = find_all_chains(&graph, ModuleId(0), &target, false);
        assert_eq!(chains.len(), 1);

        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), &target, 10, false);
        assert!(cuts.len() >= 2);
        // First cut should have smaller exclusive_size (more surgical)
        assert!(cuts[0].exclusive_size <= cuts[1].exclusive_size);
    }

    // --- Diff ---

    fn snap(entry: &str, static_weight: u64, packages: &[(&str, u64)]) -> TraceSnapshot {
        TraceSnapshot {
            entry: entry.to_string(),
            static_weight,
            packages: packages.iter().map(|(k, v)| (k.to_string(), *v)).collect(),
            dynamic_weight: 0,
            dynamic_packages: HashMap::new(),
        }
    }

    fn snap_with_dynamic(
        entry: &str,
        static_weight: u64,
        packages: &[(&str, u64)],
        dynamic_weight: u64,
        dynamic_packages: &[(&str, u64)],
    ) -> TraceSnapshot {
        TraceSnapshot {
            entry: entry.to_string(),
            static_weight,
            packages: packages.iter().map(|(k, v)| (k.to_string(), *v)).collect(),
            dynamic_weight,
            dynamic_packages: dynamic_packages
                .iter()
                .map(|(k, v)| (k.to_string(), *v))
                .collect(),
        }
    }

    #[test]
    fn diff_snapshots_computes_sets() {
        let a = snap(
            "a.ts",
            1000,
            &[("zod", 500), ("chalk", 200), ("tslog", 300)],
        );
        let b = snap("b.ts", 800, &[("chalk", 200), ("tslog", 300), ("ajv", 100)]);
        let diff = diff_snapshots(&a, &b);

        assert_eq!(diff.entry_a_weight, 1000);
        assert_eq!(diff.entry_b_weight, 800);
        assert_eq!(diff.weight_delta, -200);
        assert_eq!(diff.only_in_a.len(), 1);
        assert_eq!(diff.only_in_a[0].name, "zod");
        assert_eq!(diff.only_in_a[0].size, 500);
        assert_eq!(diff.only_in_b.len(), 1);
        assert_eq!(diff.only_in_b[0].name, "ajv");
        assert_eq!(diff.only_in_b[0].size, 100);
        assert_eq!(diff.shared_count, 2);
    }

    #[test]
    fn diff_snapshots_sorted_by_size_descending() {
        let a = snap(
            "a.ts",
            1000,
            &[("small", 10), ("big", 500), ("medium", 100)],
        );
        let b = snap("b.ts", 0, &[]);
        let diff = diff_snapshots(&a, &b);

        assert_eq!(diff.only_in_a.len(), 3);
        assert_eq!(diff.only_in_a[0].name, "big");
        assert_eq!(diff.only_in_a[1].name, "medium");
        assert_eq!(diff.only_in_a[2].name, "small");
    }

    #[test]
    fn diff_snapshots_dynamic_packages() {
        let a = snap_with_dynamic("a.ts", 1000, &[("zod", 500)], 200, &[("lodash", 200)]);
        let b = snap_with_dynamic("b.ts", 800, &[("zod", 500)], 350, &[("moment", 350)]);
        let diff = diff_snapshots(&a, &b);

        assert_eq!(diff.dynamic_a_weight, 200);
        assert_eq!(diff.dynamic_b_weight, 350);
        assert_eq!(diff.dynamic_weight_delta, 150);
        assert_eq!(diff.dynamic_only_in_a.len(), 1);
        assert_eq!(diff.dynamic_only_in_a[0].name, "lodash");
        assert_eq!(diff.dynamic_only_in_b.len(), 1);
        assert_eq!(diff.dynamic_only_in_b[0].name, "moment");
    }

    // --- Exclusive weight ---

    #[test]
    fn exclusive_weight_linear_chain() {
        // Entry(100) -> A(200) -> B(300)
        // No sharing: exclusive == full subtree
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("a.ts", 200, None),
                ("b.ts", 300, None),
            ],
            &[(0, 1, EdgeKind::Static), (1, 2, EdgeKind::Static)],
        );
        let weights = compute_exclusive_weights(&graph, ModuleId(0), false);
        assert_eq!(weights[0], 600); // entry: entire graph
        assert_eq!(weights[1], 500); // a: a + b
        assert_eq!(weights[2], 300); // b: just b
    }

    #[test]
    fn exclusive_weight_diamond_shared() {
        // Entry(100) -> A(200) -> D(500)
        // Entry(100) -> B(300) -> D(500)
        // D is shared: not in A's or B's exclusive subtree
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("a.ts", 200, None),
                ("b.ts", 300, None),
                ("d.ts", 500, None),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static),
                (2, 3, EdgeKind::Static),
            ],
        );
        let weights = compute_exclusive_weights(&graph, ModuleId(0), false);
        assert_eq!(weights[0], 1100); // entry: everything
        assert_eq!(weights[1], 200); // a: only itself (D shared)
        assert_eq!(weights[2], 300); // b: only itself (D shared)
        assert_eq!(weights[3], 500); // d: only itself
    }

    #[test]
    fn exclusive_weight_mixed_exclusive_and_shared() {
        // Entry(100) -> A(200) -> D(500)
        // Entry(100) -> B(300) -> D(500)
        // A(200) -> E(600)  -- E only reachable through A
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("a.ts", 200, None),
                ("b.ts", 300, None),
                ("d.ts", 500, None),
                ("e.ts", 600, None),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static),
                (2, 3, EdgeKind::Static),
                (1, 4, EdgeKind::Static),
            ],
        );
        let weights = compute_exclusive_weights(&graph, ModuleId(0), false);
        assert_eq!(weights[0], 1700); // entry: everything
        assert_eq!(weights[1], 800); // a: a(200) + e(600), not d
        assert_eq!(weights[2], 300); // b: only itself
        assert_eq!(weights[3], 500); // d: only itself (shared)
        assert_eq!(weights[4], 600); // e: only itself
    }

    #[test]
    fn exclusive_weight_with_dynamic_edges() {
        // Entry(100) -static-> A(200) -static-> C(400)
        // Entry(100) -dynamic-> B(300) -static-> C(400)
        // Static only: C exclusively through A
        // With dynamic: C shared between A and B
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("a.ts", 200, None),
                ("b.ts", 300, None),
                ("c.ts", 400, None),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Dynamic),
                (1, 3, EdgeKind::Static),
                (2, 3, EdgeKind::Static),
            ],
        );
        // Static only: B unreachable, C exclusively through A
        let static_weights = compute_exclusive_weights(&graph, ModuleId(0), false);
        assert_eq!(static_weights[1], 600); // a: a(200) + c(400)

        // With dynamic: C shared between A and B
        let all_weights = compute_exclusive_weights(&graph, ModuleId(0), true);
        assert_eq!(all_weights[1], 200); // a: only itself (c shared with b)
        assert_eq!(all_weights[2], 300); // b: only itself (c shared with a)
    }

    // --- Ignore filter ---

    #[test]
    fn trace_ignore_filters_heavy_packages() {
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("a.ts", 50, Some("pkg-a")),
                ("b.ts", 200, Some("pkg-b")),
                ("c.ts", 300, Some("pkg-c")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Static),
                (0, 3, EdgeKind::Static),
            ],
        );
        let opts = TraceOptions {
            include_dynamic: false,
            top_n: 10,
            ignore: vec!["pkg-c".to_string()],
        };
        let result = trace(&graph, ModuleId(0), &opts);
        let names: Vec<&str> = result
            .heavy_packages
            .iter()
            .map(|p| p.name.as_str())
            .collect();
        assert!(names.contains(&"pkg-a"));
        assert!(names.contains(&"pkg-b"));
        assert!(!names.contains(&"pkg-c"));
    }

    #[test]
    fn trace_ignore_does_not_affect_total_weight() {
        let graph = make_graph(
            &[("entry.ts", 100, None), ("a.ts", 500, Some("big-pkg"))],
            &[(0, 1, EdgeKind::Static)],
        );
        let opts = TraceOptions {
            include_dynamic: false,
            top_n: 10,
            ignore: vec!["big-pkg".to_string()],
        };
        let result = trace(&graph, ModuleId(0), &opts);
        assert!(result.heavy_packages.is_empty());
        assert_eq!(result.static_weight, 600);
    }

    // --- ChainTarget::Module ---

    #[test]
    fn chain_to_module_by_id() {
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("a.ts", 50, None),
                ("b.ts", 200, None),
            ],
            &[(0, 1, EdgeKind::Static), (1, 2, EdgeKind::Static)],
        );
        let target_id = graph.path_to_id[&PathBuf::from("b.ts")];
        let chains = find_all_chains(&graph, ModuleId(0), &ChainTarget::Module(target_id), false);
        assert_eq!(chains.len(), 1);
        assert_eq!(chains[0].len(), 3);
        assert_eq!(*chains[0].last().unwrap(), target_id);
    }

    #[test]
    fn cut_to_module_by_id() {
        let graph = make_graph(
            &[
                ("entry.ts", 100, None),
                ("bridge.ts", 50, None),
                ("target.ts", 200, None),
            ],
            &[(0, 1, EdgeKind::Static), (1, 2, EdgeKind::Static)],
        );
        let target_id = graph.path_to_id[&PathBuf::from("target.ts")];
        let target = ChainTarget::Module(target_id);
        let chains = find_all_chains(&graph, ModuleId(0), &target, false);
        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), &target, 10, false);
        assert_eq!(cuts.len(), 1);
        assert_eq!(
            cuts[0].module_id,
            graph.path_to_id[&PathBuf::from("bridge.ts")]
        );
    }

    // --- top_n negative/zero ---

    #[test]
    fn trace_top_n_negative_shows_all() {
        let graph = make_graph(
            &[
                ("entry.ts", 10, None),
                ("a.ts", 10, Some("pkg-a")),
                ("b.ts", 10, Some("pkg-b")),
                ("c.ts", 10, Some("pkg-c")),
            ],
            &[
                (0, 1, EdgeKind::Static),
                (0, 2, EdgeKind::Static),
                (0, 3, EdgeKind::Static),
            ],
        );
        let opts = TraceOptions {
            top_n: -1,
            ..Default::default()
        };
        let result = trace(&graph, ModuleId(0), &opts);
        assert_eq!(result.heavy_packages.len(), 3);
    }

    #[test]
    fn trace_top_n_zero_shows_none() {
        let graph = make_graph(
            &[("entry.ts", 10, None), ("a.ts", 10, Some("pkg-a"))],
            &[(0, 1, EdgeKind::Static)],
        );
        let opts = TraceOptions {
            top_n: 0,
            ..Default::default()
        };
        let result = trace(&graph, ModuleId(0), &opts);
        assert_eq!(result.heavy_packages.len(), 0);
    }
}
