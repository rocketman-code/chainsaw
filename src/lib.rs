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
