use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EdgeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeKind {
    Static,
    Dynamic,
    TypeOnly,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub id: ModuleId,
    pub path: PathBuf,
    pub size_bytes: u64,
    /// None for source files, Some("package-name") for node_modules
    pub package: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Edge {
    pub id: EdgeId,
    pub from: ModuleId,
    pub to: ModuleId,
    pub kind: EdgeKind,
    /// The raw import specifier (e.g. "./foo", "@aws-sdk/client-bedrock")
    pub specifier: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub entry_module: ModuleId,
    pub total_reachable_size: u64,
    pub total_reachable_files: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleGraph {
    pub modules: Vec<Module>,
    pub edges: Vec<Edge>,
    /// Outgoing edges per module (indexed by ModuleId)
    pub forward_adj: Vec<Vec<EdgeId>>,
    pub path_to_id: HashMap<PathBuf, ModuleId>,
    pub package_map: HashMap<String, PackageInfo>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            edges: Vec::new(),
            forward_adj: Vec::new(),
            path_to_id: HashMap::new(),
            package_map: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, path: PathBuf, size_bytes: u64, package: Option<String>) -> ModuleId {
        if let Some(&id) = self.path_to_id.get(&path) {
            return id;
        }
        let id = ModuleId(self.modules.len() as u32);
        self.modules.push(Module {
            id,
            path: path.clone(),
            size_bytes,
            package,
        });
        self.forward_adj.push(Vec::new());
        self.path_to_id.insert(path, id);
        id
    }

    pub fn add_edge(&mut self, from: ModuleId, to: ModuleId, kind: EdgeKind, specifier: String) -> EdgeId {
        // Deduplicate by (from, to, kind) — scan outgoing edges (typically <30)
        if let Some(&existing) = self.forward_adj[from.0 as usize]
            .iter()
            .find(|&&eid| {
                let e = &self.edges[eid.0 as usize];
                e.to == to && e.kind == kind
            })
        {
            return existing;
        }
        let id = EdgeId(self.edges.len() as u32);
        self.edges.push(Edge {
            id,
            from,
            to,
            kind,
            specifier,
        });
        self.forward_adj[from.0 as usize].push(id);
        id
    }

    pub fn module(&self, id: ModuleId) -> &Module {
        &self.modules[id.0 as usize]
    }

    pub fn edge(&self, id: EdgeId) -> &Edge {
        &self.edges[id.0 as usize]
    }

    pub fn outgoing_edges(&self, id: ModuleId) -> &[EdgeId] {
        &self.forward_adj[id.0 as usize]
    }

    pub fn module_count(&self) -> usize {
        self.modules.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_edge_deduplicates_same_from_to_kind() {
        let mut g = ModuleGraph::new();
        let a = g.add_module("a.ts".into(), 100, None);
        let b = g.add_module("b.ts".into(), 200, None);

        // Add same edge three times (simulating symlink-resolved duplicates)
        g.add_edge(a, b, EdgeKind::Static, "./b".to_string());
        g.add_edge(a, b, EdgeKind::Static, "./link-to-b".to_string());
        g.add_edge(a, b, EdgeKind::Static, "./double-link".to_string());

        // Should have only one edge (deduped by from+to+kind)
        assert_eq!(g.edges.len(), 1, "duplicate edges should be deduped");
        assert_eq!(g.forward_adj[a.0 as usize].len(), 1);
    }

    #[test]
    fn add_edge_allows_different_kinds() {
        let mut g = ModuleGraph::new();
        let a = g.add_module("a.ts".into(), 100, None);
        let b = g.add_module("b.ts".into(), 200, None);

        // Static and Dynamic edges between same nodes should both exist
        g.add_edge(a, b, EdgeKind::Static, "./b".to_string());
        g.add_edge(a, b, EdgeKind::Dynamic, "./b".to_string());

        assert_eq!(g.edges.len(), 2, "different edge kinds should not be deduped");
    }
}
