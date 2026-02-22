# Code

## Rust Style

- Edition 2024
- `#![warn(clippy::pedantic)]` with targeted `#[allow(...)]` where justified
- Idiomatic Rust: prefer stdlib patterns (iterators, windows, if-let chains, Result propagation with `?`) over manual indexing, `process::exit` (except at the CLI boundary in `main.rs`), or hand-rolled logic
- No bash scripts in the project: use `cargo xtask` for Rust logic, `Justfile` for external tool orchestration

## API Hygiene

- All public types enforced Send + Sync + Unpin via compile-time `auto_trait_tests` in lib.rs
- `#[non_exhaustive]` on public structs and enums (prevents external construction, allows future fields)
- `publish = false` on internal workspace crates (`stats/`, `xtask/`)

## Testing

Tests co-located in source files via `#[cfg(test)]` modules, not in a separate `tests/` directory. Integration tests in `tests/` are the exception (e.g., `tests/perf_registry.rs`).

Test helpers: `parse_ts()` / `parse_py()` for parser tests, `make_graph()` for query tests.

### When to Test

- ALWAYS test: silent/delayed failures (parsers, resolvers, query algorithms, cache invalidation, graph construction)
- Test at boundaries: where a dependency could silently return something different
- Test data transformations: where external data enters and gets transformed

### When NOT to Test

- Subjective design choices (help text wording, formatting preferences)
- External library internals
- Loud failures (code that crashes immediately and obviously)
- Trivial delegation (just passing args to a well-tested dependency)

### Special Categories

- Conformance tests: `#[ignore]`, run manually with `PYTHON_PROJECT` env var
- Auto-trait guards: compile-time, kept always
- Infrastructure tests: perf registry, benchmark harness, xtask -- kept (broken tooling silently produces wrong measurements)

## New Files

Every new `.rs` file must be registered in `perf.toml` (with `benchmarks = []` if not perf-sensitive). An integration test enforces completeness.

## Cache Version

Bump `CACHE_VERSION` whenever the serialization format changes. This triggers automatic cache rebuild for all users.

## Performance Infrastructure

`perf.toml` maps source files to benchmarks. Pre-push hook runs `cargo xtask perf-validate` when perf-sensitive files change. The adaptive benchmark harness uses Welch t-test with early stopping.

Manual benchmarks:

```
cargo bench --bench benchmarks         # raw benchmark run
cargo xtask bench --scenario cold      # hyperfine cold-start
cargo xtask bench                      # all scenarios on all targets
just bench                             # shorthand for cargo xtask bench
just bench-matrix                      # all scenarios x all targets
```

Performance workflow:
1. Make your change
2. `git commit` -- pre-commit hook runs `cargo xtask check`
3. If perf-sensitive files changed: `cargo xtask perf-validate` to generate attestation (must run after commit -- the attestation includes the commit SHA)
4. `git push` -- pre-push hook verifies attestation

## CI

Smart change detection via `dorny/paths-filter`: test, clippy, fmt, perf only run when code files change (.rs, Cargo.toml, Cargo.lock, rust-toolchain.toml, .cargo/config.toml, perf.toml, ci.yml). Docs-only PRs skip all compilation.

Jobs: test (ubuntu + macos), clippy, fmt, perf (synthetic corpus micros), build (gate job, always runs). On tags: publish-check (`cargo publish --dry-run`).

NOT in CI: cold-build I/O benchmarks (kernel-bound, noise on shared runners), conformance tests, `cargo publish` itself, full perf-validate.
