# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
cargo build                    # debug build
cargo build --release          # optimized build (~3.9 MB binary, LTO enabled)
cargo test                     # run all 45 tests (~0.01s)
cargo test lang::typescript::parser  # run parser tests only (15 tests)
cargo test query               # run query tests only (11 tests)
cargo test graph               # run graph tests only (2 tests)
cargo test lang::typescript::tests   # run TypeScript support tests (8 tests)
cargo test walker              # run walker tests only (1 test)
cargo test test_name           # run a single test by name
```

Requires rustc 1.85+ (edition = 2024).

## Architecture

Chainsaw is a Rust CLI that traces transitive import weight in TypeScript/JavaScript codebases. Given an entry file, it builds a dependency graph and reports which npm packages contribute the most code loaded at initialization.

### Data flow

```
detect_project (lang/mod.rs)
  → discover_source_files (walker.rs)
    → parallel parsing (lang trait, rayon)
      → import resolution (lang trait)
        → iterative graph building (walker.rs, BFS)
          → bitcode cache write (cache.rs)
            → BFS query (query.rs)
              → formatted output (report.rs)
```

### Module responsibilities

- **lang/mod.rs** - `LanguageSupport` trait (parse, resolve, package_name, workspace_package_name, extensions, skip_dirs), `RawImport`, `ProjectKind` enum, `detect_project()` for language detection via entry file extension + project root marker
- **lang/typescript/mod.rs** - `TypeScriptSupport` struct implementing `LanguageSupport`. Owns the oxc_resolver and workspace package cache (Mutex\<HashMap\>)
- **lang/typescript/parser.rs** - SWC-based import extraction. Handles static imports, dynamic `import()`, `require()`, re-exports, type-only imports. Tests use `parse_ts()` helper (parses source strings, no filesystem)
- **lang/typescript/resolver.rs** - `ImportResolver` wrapping oxc_resolver with 3-tier pnpm fallback: oxc_resolver -> `.pnpm/node_modules/` -> virtual store glob. Reads `.modules.yaml` for `virtualStoreDir`
- **graph.rs** - Core data structures: `ModuleGraph` (arena-allocated), `Module`, `Edge` (Static/Dynamic/TypeOnly), `PackageInfo`. Edge dedup by `(from, to, kind)` in `add_edge`. Language-agnostic
- **walker.rs** - `build_graph(root, &dyn LanguageSupport)` orchestrates discovery + iterative resolution via trait methods. Tracks parse failures to avoid retries. Language-agnostic
- **cache.rs** - Bitcode serialization with per-file mtime validation
- **query.rs** - BFS traversal, weight aggregation, shortest chain (`--chain`), cut point detection (`--cut`), diff between two entries or against saved snapshots. Tests use `make_graph()` helper (in-memory graphs). Language-agnostic
- **report.rs** - Human-readable and `--json` output formatting. Uses `display_name()` helper for module display. Language-agnostic
- **main.rs** - Clap CLI definition, `chainsaw trace <ENTRY> [OPTIONS]`. Calls `detect_project()` to determine language, constructs the appropriate `LanguageSupport` impl, passes it through to `load_or_build_graph()`

### SWC v33 API quirks

- `Str.value` is `Wtf8Atom` — use `.to_string_lossy()` to get `String`
- `Ident.sym` is `Atom` — use `.as_str()` to get `&str`

### Test patterns

All tests are `#[cfg(test)] mod tests` inline within their source files. No separate test files or integration test directory.

- **Parser tests** (15): Call `parse_ts(source_code)` -> assert on returned `Vec<RawImport>`
- **Query tests** (11): Call `make_graph(nodes, edges)` -> run query functions -> assert results
- **Graph tests** (2): Test edge dedup (same kind deduped, different kinds kept)
- **TypeScript support tests** (8): Test trait impl -- extensions, skip_dirs, workspace package detection via `TypeScriptSupport`
- **Walker tests** (1): Parse failure tracking with `tempfile`

## CLI flags

`chainsaw trace <ENTRY>` with: `--chain`, `--cut`, `--diff`, `--diff-from`, `--save`, `--include-dynamic`, `--top`, `--top-modules`, `--json`, `--no-cache`

Mutually exclusive pairs: `--chain`/`--cut`, `--diff`/`--diff-from`.

## pnpm support

Three-tier resolution strategy when oxc_resolver fails for pnpm virtual stores:
1. Try `<project_root>/.pnpm/node_modules/<pkg>`
2. Glob the pnpm virtual store directory for the package

Supports all pnpm layouts (isolated, hoisted, shamefully-hoisted) and workspaces.
