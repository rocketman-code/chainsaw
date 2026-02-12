use std::collections::{HashMap, HashSet, VecDeque};

use serde::{Deserialize, Serialize};

use crate::graph::{EdgeKind, ModuleGraph, ModuleId};

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
    /// All statically reachable package names (for accurate diff)
    pub all_packages: HashSet<String>,
}

pub struct HeavyPackage {
    pub name: String,
    pub total_size: u64,
    pub file_count: u32,
    /// Shortest chain from entry point to the first module in this package
    pub chain: Vec<ModuleId>,
}

pub struct ModuleCost {
    pub module_id: ModuleId,
    pub exclusive_size: u64,
}

pub struct TraceOptions {
    pub include_dynamic: bool,
    pub top_n: usize,
}

impl Default for TraceOptions {
    fn default() -> Self {
        Self {
            include_dynamic: false,
            top_n: 10,
        }
    }
}

/// Whether to follow an edge based on its kind and the include_dynamic flag.
fn should_follow(kind: EdgeKind, include_dynamic: bool) -> bool {
    match kind {
        EdgeKind::Static => true,
        EdgeKind::Dynamic if include_dynamic => true,
        _ => false,
    }
}

/// Iterative DFS to compute reverse postorder of reachable modules.
fn reverse_postorder(
    graph: &ModuleGraph,
    entry: ModuleId,
    include_dynamic: bool,
) -> Vec<ModuleId> {
    let n = graph.modules.len();
    let mut visited = vec![false; n];
    let mut postorder = Vec::new();
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
            if should_follow(edge.kind, include_dynamic) && !visited[edge.to.0 as usize] {
                stack.push((edge.to, false));
            }
        }
    }

    postorder.reverse();
    postorder
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
fn compute_exclusive_weights(
    graph: &ModuleGraph,
    entry: ModuleId,
    include_dynamic: bool,
) -> Vec<u64> {
    let n = graph.modules.len();

    // Step 1: Reverse postorder DFS
    let rpo = reverse_postorder(graph, entry, include_dynamic);
    if rpo.is_empty() {
        return vec![0; n];
    }

    // Step 2: RPO numbering (lower = earlier)
    let mut rpo_num = vec![u32::MAX; n];
    for (i, &mid) in rpo.iter().enumerate() {
        rpo_num[mid.0 as usize] = i as u32;
    }

    // Step 3: Build predecessor lists
    let mut preds: Vec<Vec<u32>> = vec![Vec::new(); n];
    for &mid in &rpo {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            if should_follow(edge.kind, include_dynamic)
                && rpo_num[edge.to.0 as usize] != u32::MAX
            {
                preds[edge.to.0 as usize].push(mid.0);
            }
        }
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
                    new_idom = Some(match new_idom {
                        None => p,
                        Some(current) => intersect_idom(&idom, &rpo_num, current, p),
                    });
                }
            }
            if let Some(ni) = new_idom {
                if idom[idx] != ni {
                    idom[idx] = ni;
                    changed = true;
                }
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

/// BFS from entry point, collecting all reachable modules.
/// Returns (static_reachable, dynamic_only_reachable) as sets of ModuleIds.
fn bfs_reachable(
    graph: &ModuleGraph,
    entry: ModuleId,
) -> (HashSet<ModuleId>, HashSet<ModuleId>) {
    let mut static_visited: HashSet<ModuleId> = HashSet::new();
    let mut static_queue: VecDeque<ModuleId> = VecDeque::new();

    static_visited.insert(entry);
    static_queue.push_back(entry);

    // BFS following static edges
    while let Some(mid) = static_queue.pop_front() {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            if edge.kind == EdgeKind::Static && static_visited.insert(edge.to) {
                static_queue.push_back(edge.to);
            }
        }
    }

    // BFS following dynamic edges from all statically reachable modules
    let mut dynamic_only: HashSet<ModuleId> = HashSet::new();
    let mut dyn_queue: VecDeque<ModuleId> = VecDeque::new();

    for &mid in &static_visited {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            if edge.kind == EdgeKind::Dynamic
                && !static_visited.contains(&edge.to)
                && dynamic_only.insert(edge.to)
            {
                dyn_queue.push_back(edge.to);
            }
        }
    }

    // Continue BFS from dynamic-only modules (they may have static imports of their own)
    while let Some(mid) = dyn_queue.pop_front() {
        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            if (edge.kind == EdgeKind::Static || edge.kind == EdgeKind::Dynamic)
                && !static_visited.contains(&edge.to)
                && dynamic_only.insert(edge.to)
            {
                dyn_queue.push_back(edge.to);
            }
        }
    }

    (static_visited, dynamic_only)
}

/// BFS shortest path from entry to any module in the target package.
fn shortest_chain_to_package(
    graph: &ModuleGraph,
    entry: ModuleId,
    package_name: &str,
) -> Vec<ModuleId> {
    let mut visited: HashSet<ModuleId> = HashSet::new();
    let mut parent: HashMap<ModuleId, ModuleId> = HashMap::new();
    let mut queue: VecDeque<ModuleId> = VecDeque::new();

    visited.insert(entry);
    queue.push_back(entry);

    while let Some(mid) = queue.pop_front() {
        let module = graph.module(mid);
        if module.package.as_deref() == Some(package_name) {
            // Reconstruct path
            let mut chain = vec![mid];
            let mut current = mid;
            while let Some(&p) = parent.get(&current) {
                chain.push(p);
                current = p;
            }
            chain.reverse();
            return chain;
        }

        for &edge_id in graph.outgoing_edges(mid) {
            let edge = graph.edge(edge_id);
            if edge.kind == EdgeKind::Static && visited.insert(edge.to) {
                parent.insert(edge.to, mid);
                queue.push_back(edge.to);
            }
        }
    }

    Vec::new()
}

pub fn trace(graph: &ModuleGraph, entry: ModuleId, opts: &TraceOptions) -> TraceResult {
    let (static_reachable, dynamic_only) = bfs_reachable(graph, entry);

    let static_weight: u64 = static_reachable
        .iter()
        .map(|&mid| graph.module(mid).size_bytes)
        .sum();
    let dynamic_only_weight: u64 = dynamic_only
        .iter()
        .map(|&mid| graph.module(mid).size_bytes)
        .sum();

    // Find heavy packages in the static reachable set
    let mut package_sizes: HashMap<String, (u64, u32)> = HashMap::new();
    for &mid in &static_reachable {
        let module = graph.module(mid);
        if let Some(ref pkg) = module.package {
            let entry = package_sizes.entry(pkg.clone()).or_default();
            entry.0 += module.size_bytes;
            entry.1 += 1;
        }
    }

    let all_packages: HashSet<String> = package_sizes.keys().cloned().collect();

    let mut heavy_packages: Vec<HeavyPackage> = package_sizes
        .into_iter()
        .map(|(name, (total_size, file_count))| {
            let chain = shortest_chain_to_package(graph, entry, &name);
            HeavyPackage {
                name,
                total_size,
                file_count,
                chain,
            }
        })
        .collect();
    heavy_packages.sort_by(|a, b| b.total_size.cmp(&a.total_size));
    heavy_packages.truncate(opts.top_n);

    // Compute exclusive weight for all reachable modules via dominator tree
    let exclusive = compute_exclusive_weights(graph, entry, opts.include_dynamic);

    let mut modules_by_cost: Vec<ModuleCost> = static_reachable
        .iter()
        .filter(|&&mid| mid != entry && graph.module(mid).package.is_none())
        .map(|&mid| ModuleCost {
            module_id: mid,
            exclusive_size: exclusive[mid.0 as usize],
        })
        .collect();

    if opts.include_dynamic {
        let dynamic_costs = dynamic_only
            .iter()
            .filter(|&&mid| graph.module(mid).package.is_none())
            .map(|&mid| ModuleCost {
                module_id: mid,
                exclusive_size: exclusive[mid.0 as usize],
            });
        modules_by_cost.extend(dynamic_costs);
    }

    modules_by_cost.sort_by(|a, b| b.exclusive_size.cmp(&a.exclusive_size));

    TraceResult {
        static_weight,
        static_module_count: static_reachable.len(),
        dynamic_only_weight,
        dynamic_only_module_count: dynamic_only.len(),
        heavy_packages,
        modules_by_cost,
        all_packages,
    }
}

/// Find ALL shortest chains from entry to a specific package.
/// Returns up to `max_chains` distinct shortest paths (all same hop count),
/// deduplicated at the package-name level so chains that differ only by
/// internal node_modules file paths are collapsed into one.
pub fn find_all_chains(
    graph: &ModuleGraph,
    entry: ModuleId,
    package_name: &str,
    include_dynamic: bool,
) -> Vec<Vec<ModuleId>> {
    let raw = all_shortest_chains_to_package(graph, entry, package_name, 10, include_dynamic);
    dedup_chains_by_package(graph, raw)
}

/// Deduplicate chains that look identical at the package-name level.
/// Two chains that differ only by which internal file within a package
/// they pass through will have the same package-level key and only the
/// first is kept.
fn dedup_chains_by_package(
    graph: &ModuleGraph,
    chains: Vec<Vec<ModuleId>>,
) -> Vec<Vec<ModuleId>> {
    let mut seen: HashSet<Vec<String>> = HashSet::new();
    let mut result = Vec::new();

    for chain in chains {
        let key: Vec<String> = chain
            .iter()
            .map(|&mid| {
                let m = graph.module(mid);
                if let Some(ref pkg) = m.package {
                    pkg.clone()
                } else {
                    m.path.to_string_lossy().into_owned()
                }
            })
            .collect();

        if seen.insert(key) {
            result.push(chain);
        }
    }

    result
}

/// BFS with multi-parent tracking to find all shortest paths to a package.
fn all_shortest_chains_to_package(
    graph: &ModuleGraph,
    entry: ModuleId,
    package_name: &str,
    max_chains: usize,
    include_dynamic: bool,
) -> Vec<Vec<ModuleId>> {
    let mut parents: HashMap<ModuleId, Vec<ModuleId>> = HashMap::new();
    let mut depth: HashMap<ModuleId, u32> = HashMap::new();
    let mut queue: VecDeque<ModuleId> = VecDeque::new();

    depth.insert(entry, 0);
    queue.push_back(entry);

    let mut target_depth: Option<u32> = None;
    let mut targets: Vec<ModuleId> = Vec::new();

    while let Some(mid) = queue.pop_front() {
        let d = depth[&mid];

        // If we've found targets and moved past their depth, stop
        if let Some(td) = target_depth
            && d > td
        {
            break;
        }

        // Check if this module is in the target package
        let module = graph.module(mid);
        if module.package.as_deref() == Some(package_name) {
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
            match depth.get(&edge.to) {
                Some(&existing) if existing == next_depth => {
                    // Same depth -- add as alternate parent
                    parents.entry(edge.to).or_default().push(mid);
                }
                None => {
                    // First visit
                    depth.insert(edge.to, next_depth);
                    parents.entry(edge.to).or_default().push(mid);
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
    for &target in &targets {
        let mut partial_paths: Vec<Vec<ModuleId>> = vec![vec![target]];

        loop {
            let mut next_partial: Vec<Vec<ModuleId>> = Vec::new();
            let mut any_extended = false;

            for path in &partial_paths {
                let &head = path.last().unwrap();
                if head == entry {
                    next_partial.push(path.clone());
                    continue;
                }
                if let Some(pars) = parents.get(&head) {
                    any_extended = true;
                    for &p in pars {
                        let mut new_path = path.clone();
                        new_path.push(p);
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

pub struct CutModule {
    pub module_id: ModuleId,
    pub chains_broken: usize,
    pub exclusive_size: u64,
}

/// Find modules that appear in all chains from entry to a package.
/// Removing any one of these severs every import path to the target.
/// Sorted by exclusive weight descending (highest-impact first),
/// truncated to `top_n`.
pub fn find_cut_modules(
    graph: &ModuleGraph,
    chains: &[Vec<ModuleId>],
    entry: ModuleId,
    target_package: &str,
    top_n: usize,
    include_dynamic: bool,
) -> Vec<CutModule> {
    if chains.is_empty() {
        return Vec::new();
    }

    let exclusive = compute_exclusive_weights(graph, entry, include_dynamic);

    let total = chains.len();
    let mut frequency: HashMap<ModuleId, usize> = HashMap::new();
    for chain in chains {
        for &mid in chain {
            *frequency.entry(mid).or_insert(0) += 1;
        }
    }

    let mut cuts: Vec<CutModule> = frequency
        .into_iter()
        .filter(|&(mid, count)| {
            count == total
                && mid != entry
                && graph.module(mid).package.as_deref() != Some(target_package)
        })
        .map(|(mid, count)| CutModule {
            module_id: mid,
            chains_broken: count,
            exclusive_size: exclusive[mid.0 as usize],
        })
        .collect();

    // Deduplicate at package level -- multiple files within the same
    // node_modules package are the same cut point from the user's perspective.
    // Sort descending first so we keep the highest-weight entry per package.
    cuts.sort_by(|a, b| b.exclusive_size.cmp(&a.exclusive_size));
    let mut seen_packages: HashSet<String> = HashSet::new();
    cuts.retain(|c| {
        let m = graph.module(c.module_id);
        match m.package {
            Some(ref pkg) => seen_packages.insert(pkg.clone()),
            None => true,
        }
    });

    // Single chain: sort ascending (most surgical/targeted cut first).
    // Multiple chains: sort descending (highest-impact convergence point first).
    if chains.len() == 1 {
        cuts.sort_by(|a, b| a.exclusive_size.cmp(&b.exclusive_size));
    }

    cuts.truncate(top_n);
    cuts
}

/// Minimal snapshot of a trace result for before/after comparison.
#[derive(Serialize, Deserialize)]
pub struct TraceSnapshot {
    pub static_weight: u64,
    pub all_packages: HashSet<String>,
}

impl TraceResult {
    pub fn to_snapshot(&self) -> TraceSnapshot {
        TraceSnapshot {
            static_weight: self.static_weight,
            all_packages: self.all_packages.clone(),
        }
    }
}

/// Compute a diff between two trace snapshots.
pub struct DiffResult {
    pub entry_a_weight: u64,
    pub entry_b_weight: u64,
    pub weight_delta: i64,
    pub shared_packages: Vec<String>,
    pub only_in_a: Vec<String>,
    pub only_in_b: Vec<String>,
}

pub fn diff_snapshots(a: &TraceSnapshot, b: &TraceSnapshot) -> DiffResult {
    let pkgs_a: HashSet<&str> = a.all_packages.iter().map(|s| s.as_str()).collect();
    let pkgs_b: HashSet<&str> = b.all_packages.iter().map(|s| s.as_str()).collect();

    DiffResult {
        entry_a_weight: a.static_weight,
        entry_b_weight: b.static_weight,
        weight_delta: b.static_weight as i64 - a.static_weight as i64,
        shared_packages: pkgs_a
            .intersection(&pkgs_b)
            .map(|s| s.to_string())
            .collect(),
        only_in_a: pkgs_a
            .difference(&pkgs_b)
            .map(|s| s.to_string())
            .collect(),
        only_in_b: pkgs_b
            .difference(&pkgs_a)
            .map(|s| s.to_string())
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::ModuleGraph;
    use std::path::PathBuf;

    /// Build a small graph from declarative specs.
    /// `nodes`: (path, size_bytes, package_name)
    /// `edges`: (from_index, to_index, kind)
    fn make_graph(
        nodes: &[(&str, u64, Option<&str>)],
        edges: &[(usize, usize, EdgeKind)],
    ) -> ModuleGraph {
        let mut graph = ModuleGraph::new();
        for &(path, size, pkg) in nodes {
            graph.add_module(
                PathBuf::from(path),
                size,
                pkg.map(|s| s.to_string()),
            );
        }
        for &(from, to, kind) in edges {
            graph.add_edge(
                ModuleId(from as u32),
                ModuleId(to as u32),
                kind,
                String::new(),
            );
        }
        graph
    }

    // --- BFS / trace ---

    #[test]
    fn trace_static_weight() {
        // A(100) -> B(200) -> C(300)
        let graph = make_graph(
            &[("a.ts", 100, None), ("b.ts", 200, None), ("c.ts", 300, None)],
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
            &[("a.ts", 100, None), ("b.ts", 200, None), ("c.ts", 300, None)],
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
        };
        let result = trace(&graph, ModuleId(0), &opts);
        // B should appear in modules_by_cost when include_dynamic is set
        assert!(result.modules_by_cost.iter().any(|m| m.module_id == ModuleId(1)));
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
        assert_eq!(chains.len(), 1);
        assert_eq!(chains[0], vec![ModuleId(0), ModuleId(1), ModuleId(2), ModuleId(3)]);
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
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
            &[
                (0, 1, EdgeKind::Dynamic),
                (1, 2, EdgeKind::Static),
            ],
        );
        let chains_static = find_all_chains(&graph, ModuleId(0), "zod", false);
        assert!(chains_static.is_empty());

        let chains_dynamic = find_all_chains(&graph, ModuleId(0), "zod", true);
        assert_eq!(chains_dynamic.len(), 1);
        assert_eq!(chains_dynamic[0], vec![ModuleId(0), ModuleId(1), ModuleId(2)]);
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
        assert_eq!(chains.len(), 2);

        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), "zod", 10, false);
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), "zod", 10, false);
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
        assert_eq!(chains.len(), 1);
        assert_eq!(chains[0].len(), 2); // 1 hop = 2 nodes

        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), "zod", 10, false);
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
        let chains = find_all_chains(&graph, ModuleId(0), "zod", false);
        assert_eq!(chains.len(), 1);

        let cuts = find_cut_modules(&graph, &chains, ModuleId(0), "zod", 10, false);
        assert!(cuts.len() >= 2);
        // First cut should have smaller exclusive_size (more surgical)
        assert!(cuts[0].exclusive_size <= cuts[1].exclusive_size);
    }

    // --- Diff ---

    #[test]
    fn diff_snapshots_computes_sets() {
        let a = TraceSnapshot {
            static_weight: 1000,
            all_packages: ["zod", "chalk", "tslog"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
        };
        let b = TraceSnapshot {
            static_weight: 800,
            all_packages: ["chalk", "tslog", "ajv"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
        };
        let diff = diff_snapshots(&a, &b);

        assert_eq!(diff.entry_a_weight, 1000);
        assert_eq!(diff.entry_b_weight, 800);
        assert_eq!(diff.weight_delta, -200);
        assert!(diff.only_in_a.contains(&"zod".to_string()));
        assert!(diff.only_in_b.contains(&"ajv".to_string()));
        assert!(diff.shared_packages.contains(&"chalk".to_string()));
        assert!(diff.shared_packages.contains(&"tslog".to_string()));
        assert!(!diff.shared_packages.contains(&"zod".to_string()));
        assert!(!diff.shared_packages.contains(&"ajv".to_string()));
    }

    // --- Exclusive weight ---

    #[test]
    fn exclusive_weight_linear_chain() {
        // Entry(100) -> A(200) -> B(300)
        // No sharing: exclusive == full subtree
        let graph = make_graph(
            &[("entry.ts", 100, None), ("a.ts", 200, None), ("b.ts", 300, None)],
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
                ("entry.ts", 100, None), ("a.ts", 200, None),
                ("b.ts", 300, None), ("d.ts", 500, None),
            ],
            &[
                (0, 1, EdgeKind::Static), (0, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static), (2, 3, EdgeKind::Static),
            ],
        );
        let weights = compute_exclusive_weights(&graph, ModuleId(0), false);
        assert_eq!(weights[0], 1100); // entry: everything
        assert_eq!(weights[1], 200);  // a: only itself (D shared)
        assert_eq!(weights[2], 300);  // b: only itself (D shared)
        assert_eq!(weights[3], 500);  // d: only itself
    }

    #[test]
    fn exclusive_weight_mixed_exclusive_and_shared() {
        // Entry(100) -> A(200) -> D(500)
        // Entry(100) -> B(300) -> D(500)
        // A(200) -> E(600)  -- E only reachable through A
        let graph = make_graph(
            &[
                ("entry.ts", 100, None), ("a.ts", 200, None),
                ("b.ts", 300, None), ("d.ts", 500, None),
                ("e.ts", 600, None),
            ],
            &[
                (0, 1, EdgeKind::Static), (0, 2, EdgeKind::Static),
                (1, 3, EdgeKind::Static), (2, 3, EdgeKind::Static),
                (1, 4, EdgeKind::Static),
            ],
        );
        let weights = compute_exclusive_weights(&graph, ModuleId(0), false);
        assert_eq!(weights[0], 1700); // entry: everything
        assert_eq!(weights[1], 800);  // a: a(200) + e(600), not d
        assert_eq!(weights[2], 300);  // b: only itself
        assert_eq!(weights[3], 500);  // d: only itself (shared)
        assert_eq!(weights[4], 600);  // e: only itself
    }

    #[test]
    fn exclusive_weight_with_dynamic_edges() {
        // Entry(100) -static-> A(200) -static-> C(400)
        // Entry(100) -dynamic-> B(300) -static-> C(400)
        // Static only: C exclusively through A
        // With dynamic: C shared between A and B
        let graph = make_graph(
            &[
                ("entry.ts", 100, None), ("a.ts", 200, None),
                ("b.ts", 300, None), ("c.ts", 400, None),
            ],
            &[
                (0, 1, EdgeKind::Static), (0, 2, EdgeKind::Dynamic),
                (1, 3, EdgeKind::Static), (2, 3, EdgeKind::Static),
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
}
