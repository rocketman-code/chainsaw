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

## Commit conventions

We use [Conventional Commits](https://www.conventionalcommits.org/):

```
type(scope): lowercase imperative description
```

Types: `feat`, `fix`, `refactor`, `test`, `docs`, `chore`, `style`, `perf`

Examples:
- `feat(resolver): add namespace package support`
- `fix(cache): handle stale lockfile sentinel`
- `test(parser): add edge case for re-exports`

## Performance

Chainsaw has a performance regression gate. If your change touches a file listed in `perf.toml`, the hooks will require a passing perf attestation before the push goes through.

`perf.toml` maps every `.rs` file in the workspace to its relevant benchmarks. When adding new source files, you must add them to `perf.toml` (with `benchmarks = []` if not perf-sensitive) -- the integration test enforces completeness.

The workflow:

1. Make your change
2. `git commit` -- pre-commit hook runs `cargo xtask check` (fmt, clippy, tests)
3. If perf-sensitive files changed: `cargo xtask perf-validate` to generate attestation (must run after commit — the attestation includes the commit SHA)
4. `git push` -- pre-push hook verifies the attestation; blocks push if missing or stale

To run benchmarks manually:

```
cargo xtask perf-validate              # compare against baseline
cargo bench --bench benchmarks         # raw benchmark run
just bench-cold                        # hyperfine cold-start comparison
```

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
  lib.rs          # module root, re-exports, auto-trait guards
  error.rs        # error types
  main.rs         # CLI entry point
stats/            # zero-dep statistics library (workspace crate)
xtask/            # dev tooling: hooks, perf gate, registry
benches/          # adaptive benchmark harness
scripts/          # benchmark scripts
Justfile          # task runner recipes
```

## Branching and merging

All changes go through pull requests -- no direct pushes to main. Main is branch-protected: PRs required, CI must pass, no force pushes.

Branch naming follows conventional commit types: `type/slug` (e.g. `feat/namespace-packages`, `fix/cache-invalidation`, `refactor/walker-pipeline`, `docs/contributing`).

Merge strategy is rebase only (linear history). Individual commits are preserved on main -- no squash, no merge commits. Use GitHub's "Rebase and merge" button.

During PR iteration: add new commits on top, never amend or force-push. Each fix is a separate commit describing what it addresses.

### Push early

Always push feature branches to origin after the first commit. An unpushed branch is an unrecoverable branch -- if it gets deleted locally or garbage collected, the code is gone. A draft PR costs nothing and creates a paper trail.

```
git push -u origin feat/my-feature
gh pr create --draft
```

The pre-commit hook warns when it detects commits accumulating on a branch with no remote tracking ref. Don't ignore this warning.

## Pull requests

One `type(scope)` per PR. If you can't describe the PR with a single conventional commit prefix, split it. A PR can contain multiple commits, but they should all serve the same `type(scope)`.

Examples:
- `fix(cli): improve error messages` -- one PR even if it's 6 commits fixing different error messages
- `feat(xtask): add check subcommand` -- one PR for the new subcommand
- A branch with both a new feature and unrelated doc updates -- split into two PRs

Checklist:
- `cargo xtask check` passes (fmt + clippy + test)
- If your change affects performance-sensitive code, include benchmark results
