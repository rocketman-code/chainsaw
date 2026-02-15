#![warn(clippy::pedantic)]
// Binary crate with internal library — all callers are us.
// These doc lints are for public API documentation, not applicable here.
#![allow(clippy::must_use_candidate)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_name_repetitions)]

pub mod cache;
pub mod error;
pub mod graph;
pub mod lang;
pub mod loader;
pub mod query;
pub mod report;
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
        is_normal::<crate::lang::ParseError>();
        is_normal::<crate::lang::RawImport>();
        is_normal::<crate::lang::ParseResult>();
        is_normal::<crate::lang::ProjectKind>();
    }
}
