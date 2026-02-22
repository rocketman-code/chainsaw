# Contributing to Chainsaw

## Prerequisites

- Rust toolchain -- `rust-toolchain.toml` pins the exact version, so `rustup` will install it automatically on first build
- [just](https://github.com/casey/just) (optional, for benchmark recipes)
- [hyperfine](https://github.com/sharkdp/hyperfine) (optional, for A/B benchmarking)

## Getting started

```
git clone https://github.com/rocketman-code/chainsaw.git
cd chainsaw
cargo xtask install-hooks
cargo xtask check
```

`install-hooks` sets up pre-commit and pre-push git hooks:

- On feature branches: pre-commit runs `cargo xtask check` (fmt, clippy, tests) before every commit
- On main: pre-commit checks attestation file exists for perf-sensitive files
- pre-push (all branches): fully verifies attestation (commit SHA, benchmarks) when perf-sensitive files changed since main

## Building and testing

```
cargo xtask check                 # run all local CI checks (fmt + clippy + test)
cargo build                       # debug build
cargo build --release             # release build
cargo test --workspace            # run all tests (~0.3s)
cargo clippy --workspace --all-targets -- -D warnings
cargo fmt --check
```

Some tests are `#[ignore]` because they require local fixtures (CPython source tree, real Python projects). Run them with:

```
cargo test -- --ignored
```

## Testing

Tests are co-located in source files via `#[cfg(test)]` modules, not in a separate `tests/` directory. Integration tests in `tests/` are the exception (e.g. `tests/perf_registry.rs`).

## Project structure

```
src/
  lang/           # language-specific parsing and resolution
    typescript/   # OXC parser + oxc_resolver
    python/       # tree-sitter parser + custom resolver
  walker.rs       # concurrent file discovery
  cache.rs        # three-tier caching (graph + per-file)
  git.rs          # git ref diff support (worktrees, auto-detection)
  graph.rs        # module graph data structure
  query.rs        # trace, chain, cut, diff algorithms
  report.rs       # terminal output formatting
  loader.rs       # cached graph loading pipeline
  session.rs      # stateful query API
  repl.rs         # interactive exploration mode
  vfs.rs          # virtual filesystem abstraction
  lib.rs          # module root, re-exports, auto-trait guards
  error.rs        # error types
  main.rs         # CLI entry point
stats/            # zero-dep statistics library (workspace crate)
xtask/            # dev tooling: hooks, perf gate, registry
benches/          # adaptive benchmark harness
Justfile          # task runner recipes (thin aliases to xtask)
```

## Standards

These documents define our project conventions:

- [Commits](docs/standards/commits.md) -- message format, types, scoping, granularity
- [Pull requests](docs/standards/pull-requests.md) -- branching, merge strategy, PR iteration, pre-merge cleanup
- [Issues](docs/standards/issues.md) -- labeling, triage, priorities, milestones
- [Releases](docs/standards/releases.md) -- versioning, compatibility surface, release checklist
- [Code](docs/standards/code.md) -- Rust style, testing philosophy, performance infrastructure, CI
