# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
cargo build                    # debug build
cargo build --release          # optimized build (~3.9 MB binary, LTO enabled)
cargo test                     # run all 97 tests (~0.03s)
cargo test lang::typescript::parser  # run TS parser tests only (15 tests)
cargo test lang::python::parser      # run Python parser tests only (17 tests)
cargo test lang::python::resolver    # run Python resolver tests only (14 tests)
cargo test query               # run query tests only (16 tests)
cargo test graph               # run graph tests only (2 tests)
cargo test lang::typescript::tests   # run TypeScript support tests (8 tests)
cargo test lang::python::tests       # run Python support tests (3 tests)
cargo test lang::tests               # run language detection tests (3 tests)
cargo test walker              # run walker tests only (1 test)
cargo test test_name           # run a single test by name
```

Requires rustc 1.85+ (edition = 2024).

## Architecture

Chainsaw is a Rust CLI that traces transitive import weight in TypeScript/JavaScript and Python codebases. Given an entry file, it builds a dependency graph and reports which packages contribute the most code loaded at initialization.

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

- **lang/mod.rs** - `LanguageSupport` trait (parse, resolve, package_name, workspace_package_name, extensions, skip_dirs), `RawImport`, `ProjectKind` enum (TypeScript, Python), `detect_project()` for language detection via entry file extension + project root marker
- **lang/typescript/mod.rs** - `TypeScriptSupport` struct implementing `LanguageSupport`. Owns the oxc_resolver and workspace package cache (Mutex\<HashMap\>)
- **lang/typescript/parser.rs** - SWC-based import extraction. Handles static imports, dynamic `import()`, `require()`, re-exports, type-only imports. Tests use `parse_ts()` helper
- **lang/typescript/resolver.rs** - `ImportResolver` wrapping oxc_resolver with 3-tier pnpm fallback
- **lang/python/mod.rs** - `PythonSupport` struct implementing `LanguageSupport`. Delegates to tree-sitter parser and Python resolver
- **lang/python/parser.rs** - tree-sitter-python import extraction. Handles `import`, `from...import`, relative imports, `TYPE_CHECKING` blocks (TypeOnly), `importlib.import_module()`/`__import__()` (Dynamic), skips `__future__`. Tests use `parse_py()` helper
- **lang/python/resolver.rs** - `PythonResolver` resolving dotted module names to `.py`/`__init__.py` files. Searches source roots (project root, `src/`, `lib/`) then site-packages. Uses project `.venv/bin/python` for site-packages discovery (falls back to system `python3`). Extracts package names from site-packages paths
- **graph.rs** - Core data structures: `ModuleGraph` (arena-allocated), `Module`, `Edge` (Static/Dynamic/TypeOnly), `PackageInfo`. Edge dedup by `(from, to, kind)` in `add_edge`. Language-agnostic
- **walker.rs** - `build_graph(root, &dyn LanguageSupport)` orchestrates discovery + iterative resolution via trait methods. Returns `BuildResult` with graph, unresolved specifiers, and walked directories for cache invalidation. Tracks parse failures to avoid retries. Language-agnostic
- **cache.rs** - Bitcode serialization with per-file mtime validation, directory mtime tracking for new file detection, and unresolved specifier re-resolution for dependency environment changes
- **query.rs** - BFS traversal, exclusive weight via dominator tree (Cooper-Harvey-Kennedy), shortest chain (`--chain`), cut point detection (`--cut`), diff between two entries or against saved snapshots. Tests use `make_graph()` helper. Language-agnostic
- **report.rs** - Human-readable and `--json` output formatting. Chains disambiguate duplicate package names by expanding to package-relative file paths. Language-agnostic
- **main.rs** - Clap CLI definition, `chainsaw trace <ENTRY> [OPTIONS]`. Calls `detect_project()` to determine language, constructs the appropriate `LanguageSupport` impl, passes it through to `load_or_build_graph()`

### Language-specific notes

**TypeScript/JavaScript (SWC v33)**
- `Str.value` is `Wtf8Atom` — use `.to_string_lossy()` to get `String`
- `Ident.sym` is `Atom` — use `.as_str()` to get `&str`
- pnpm 3-tier resolution: oxc_resolver -> `.pnpm/node_modules/` -> virtual store glob

**Python (tree-sitter-python)**
- Parser creates a new `tree_sitter::Parser` per `parse_file()` call (Parser is not Send)
- Relative imports encoded as dot-prefixed specifiers: `.foo`, `..bar.baz`
- `from . import foo, bar` (bare dots) emits separate specifiers `.foo`, `.bar`
- Site-packages discovered at construction time via `.venv/bin/python` (or system `python3`)
- Source roots: project root, then `src/`, `lib/` if they exist (handles PyPA src-layout and lib-layout projects)
- Module resolution: tries `__init__.py` (package) then `.py` (module file)
- `workspace_package_name` returns None (no Python monorepo support in MVP)

### Test patterns

All tests are `#[cfg(test)] mod tests` inline within their source files. No separate test files or integration test directory.

- **TS parser tests** (15): Call `parse_ts(source_code)` -> assert on returned `Vec<RawImport>`
- **Python parser tests** (17): Call `parse_py(source_code)` -> assert on returned `Vec<RawImport>`
- **Python resolver tests** (14): Create `PythonResolver` with struct literal (bypasses site-packages discovery), assert resolution against tempdir layouts including src/lib layouts
- **Query tests** (16): Call `make_graph(nodes, edges)` -> run query functions -> assert results. Includes exclusive weight tests (linear, diamond, mixed, dynamic edges)
- **Cache tests** (5): Test cache validity, invalidation on file add/modify/delete, and unresolved import re-resolution
- **Graph tests** (2): Test edge dedup (same kind deduped, different kinds kept)
- **TypeScript support tests** (8): Test trait impl -- extensions, skip_dirs, workspace package detection
- **Python support tests** (3): Test trait impl -- extensions, skip_dirs, workspace_package_name
- **Language detection tests** (3): Test detect_project for .ts/.py extensions and marker fallback
- **Walker tests** (1): Parse failure tracking with `tempfile`

## CLI flags

`chainsaw trace <ENTRY>` with: `--chain`, `--cut`, `--diff`, `--diff-from`, `--save`, `--include-dynamic`, `--top`, `--top-modules`, `--json`, `--no-cache`

Mutually exclusive pairs: `--chain`/`--cut`, `--diff`/`--diff-from`.

Entry file extension determines language: `.ts`/`.tsx`/`.js`/`.jsx`/`.mjs`/`.cjs`/`.mts`/`.cts` for TypeScript, `.py` for Python.
