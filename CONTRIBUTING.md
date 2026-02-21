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
cargo test --workspace
```

`install-hooks` sets up pre-commit and pre-push git hooks that enforce formatting, clippy, and performance attestation.

## Building and testing

```
cargo build                    # debug build
cargo build --release          # release build
cargo test --workspace         # run all tests (~0.3s)
cargo clippy --workspace --all-targets -- -D warnings
cargo fmt --check
```

Some tests are `#[ignore]` because they require local fixtures (CPython source tree, real Python projects). Run them with:

```
cargo test -- --ignored
```

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

Chainsaw has a performance regression gate. If your change touches a file listed in `perf.toml`, the pre-push hook will run benchmarks and block the push if there's a statistically significant regression.

The workflow:

1. Make your change
2. `git commit` -- pre-commit hook runs fmt + clippy
3. `git push` -- pre-push hook checks if perf-sensitive files changed; if so, runs `cargo xtask perf-validate`
4. If benchmarks pass, the push goes through. If not, you'll see which benchmark regressed.

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
  cache.rs        # two-tier caching (graph + per-file)
  git.rs          # git ref diff support (worktrees, auto-detection)
  graph.rs        # module graph data structure
  query.rs        # trace, chain, cut, diff algorithms
  report.rs       # terminal output formatting
  main.rs         # CLI entry point
stats/            # zero-dep statistics library (workspace crate)
xtask/            # dev tooling: hooks, perf gate, registry
benches/          # adaptive benchmark harness
```

## Pull requests

- Keep PRs focused on a single concern
- Tests should pass (`cargo test --workspace`)
- Clippy should be clean (`cargo clippy --workspace --all-targets -- -D warnings`)
- Format should match (`cargo fmt --check`)
- If your change affects performance-sensitive code, include benchmark results
