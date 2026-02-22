//! Build and query dependency graphs for TypeScript/JavaScript and Python codebases.
//!
//! Chainsaw parses import statements, resolves them against the filesystem, and
//! constructs a full transitive dependency graph from any entry file. The graph
//! can then be queried for total import weight, heaviest packages, shortest
//! import chains, optimal cut points, and before/after diffs.
//!
//! This library backs the `chainsaw` CLI. The public API is internal and
//! unstable -- it exists so benchmarks and tests can access internals.

#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_name_repetitions)]
// compact_str 0.8 (oxc_span) + 0.9 (oxc_resolver) — transitive, out of our control.
#![allow(clippy::multiple_crate_versions)]

pub mod cache;
pub mod error;
pub mod git;
pub mod graph;
pub mod lang;
pub mod loader;
pub mod query;
pub mod repl;
pub mod report;
pub mod session;
pub mod walker;

/// Compile-time guard: all public types must be Send + Sync + Unpin.
/// If an internal change (e.g. adding Rc or Cell) breaks these, this
/// test will fail to compile rather than silently degrading the API.
#[cfg(test)]
mod auto_trait_tests {
    fn is_normal<T: Sized + Send + Sync + Unpin>() {}

    #[test]
    fn public_types_are_send_sync() {
        is_normal::<crate::graph::ModuleId>();
        is_normal::<crate::graph::EdgeId>();
        is_normal::<crate::graph::EdgeKind>();
        is_normal::<crate::graph::Module>();
        is_normal::<crate::graph::Edge>();
        is_normal::<crate::graph::ModuleGraph>();
        is_normal::<crate::query::TraceResult>();
        is_normal::<crate::query::TraceSnapshot>();
        is_normal::<crate::query::DiffResult>();
        is_normal::<crate::walker::BuildResult>();
        is_normal::<crate::error::Error>();
        is_normal::<crate::loader::LoadedGraph>();
        is_normal::<crate::session::Session>();
        is_normal::<crate::session::ResolvedTarget>();
        is_normal::<crate::lang::ParseError>();
        is_normal::<crate::lang::RawImport>();
        is_normal::<crate::lang::ParseResult>();
        is_normal::<crate::lang::ProjectKind>();
        is_normal::<crate::git::DiffArg>();
        is_normal::<crate::git::TempWorktree>();
        is_normal::<crate::repl::Command>();
    }
}
