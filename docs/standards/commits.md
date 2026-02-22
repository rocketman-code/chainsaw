# Commits

## Format

[Conventional Commits](https://www.conventionalcommits.org/):

```
type(scope): lowercase imperative description
```

## Types

- `feat` -- new feature
- `fix` -- bug fix
- `refactor` -- code restructuring (no behavior change)
- `test` -- tests only (no production code)
- `docs` -- documentation
- `chore` -- maintenance (deps, tooling, config)
- `style` -- formatting (no logic change)
- `perf` -- performance improvement (see [perf commit standard](#the-perf-commit-standard))

## Scope

The affected area in lowercase. Examples: `resolver`, `cache`, `cli`, `walker`, `parser`, `repl`, `xtask`, `cargo`, `ci`.

## Granularity

Many tightly scoped commits, one logical concern per commit. Never batch multiple concerns into a monolith.

Example: adding a new feature with tests, updating the registry, and adding scaffolding = 3 separate commits, not 1 (feature + tests go together per the rule below).

## The perf Commit Standard

A `perf(scope):` commit on main is a claim of measured improvement. It must meet this bar:

1. PR includes benchmark evidence (before/after numbers, which benchmarks, which targets)
2. At least one benchmark shows statistically significant improvement (Welch t-test via the adaptive harness)
3. No benchmark regresses beyond noise

If the improvement is within noise (not statistically significant), label it `refactor`, not `perf`. A real structural improvement that happens to not move the needle is still valuable -- it's just not a performance commit.

If a change improves one target but regresses another, don't commit it. The optimization isn't understood well enough. Profile further or revert.

### Adding benchmarks alongside optimizations

When optimizing a code path with no existing benchmark, add the benchmark in the same PR. This is TDD for performance: the new benchmark both proves the improvement and guards against future regressions on that code path. The PR must still show before/after evidence -- run the new benchmark against both the base branch and the optimized branch to produce the comparison.

## Fix + Tests in Same Commit

Implementation and its tests go in the same commit. Never split a fix from the test that validates it. Standalone test additions (new edge cases, coverage improvements unrelated to a specific change) can be separate `test(scope)` commits.

## Examples

```
feat(resolver): add namespace package support
fix(cache): handle stale lockfile sentinel
test(parser): add edge case for re-exports
refactor(walker): extract work queue into separate function
perf(vfs): reuse file handles to reduce syscalls
chore(cargo): bump version to 0.3.0
docs(contributing): add branching and merge policy
style(error): apply consistent quote style
```
