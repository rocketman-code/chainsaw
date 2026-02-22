# Releases

## Versioning

Pre-1.0 semver (0.x.y):
- Minor bump (0.x.0) for new features
- Patch bump (0.x.1) for bug fixes
- Breaking changes allowed in minor bumps (pre-1.0 semver) but treated as a last resort
- 1.0 = "the CLI interface is stable, your scripts won't break"

## Compatibility Surface (breaking = version bump)

- CLI flags and subcommands (renaming/removing breaks scripts)
- Exit codes (`--max-weight` returns 1 on failure, CI depends on this)
- JSON output schema (people pipe `--json` to jq in CI)
- Snapshot file format (people save these and compare days/weeks later)

## NOT Compatibility Concerns

- Cache format -- transparent, auto-rebuilds on mismatch, CACHE_VERSION bumps are free
- Internal lib API -- documented as unstable in crate-level docs
- Human-readable output wording -- if someone parses non-JSON text output, that's on them
- Performance -- faster is never breaking, slower is a regression but not a semver event

## Release Cadence

Batch related changes, release when there's a meaningful set of user-facing changes. No version bump per PR.

## Release Checklist

1. All milestone issues closed
2. `cargo test --workspace` passes
3. `cargo clippy --workspace --all-targets -- -D warnings` clean
4. `cargo publish --dry-run` clean
5. Perf validation passes locally (`cargo xtask perf-validate`)
6. Bump version in `Cargo.toml` + run `cargo generate-lockfile`
7. Commit: `chore(cargo): bump version to 0.x.y`
8. Tag: `git tag v0.x.y`
9. Push: `git push && git push --tags`
10. Publish: `cargo publish`
11. Close the milestone on GitHub

## Git Tags

Format: `v0.x.y` (e.g., `v0.1.0`, `v0.2.0`, `v0.3.0`). Tag the version bump commit.

## Changelog

No CHANGELOG.md file. Commit messages use conventional commits. GitHub releases with auto-generated notes from tags are sufficient.
