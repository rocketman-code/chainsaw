# Changelog

All notable changes to chainsaw are documented here. Each release corresponds to a
[milestone](https://github.com/rocketman-code/chainsaw/milestones?state=closed) on GitHub.

## [0.4.1] - 2026-02-27

Packaging fixes, Linux performance, cross-platform file watching.

### Added

- jemalloc as global allocator on Linux, reducing cold-build syscalls ~100x vs musl ([#178])

### Fixed

- REPL file watching silently degraded on Linux -- only macOS FSEvents backend was compiled ([#176])
- `tempfile` crate shipped in production binary despite being test-only ([#176])
- Bare `.unwrap()` calls in production code now document their invariants via `expect()` ([#176])

## [0.4.0] - 2026-02-22

Structured output layer, REPL polish, file watching.

### Added

- Structured report types (`TraceReport`, `ChainReport`, `CutReport`, `DiffReport`, `PackagesReport`) with `--json` output for all commands ([#103])
- VFS abstraction layer: `Vfs` trait with `OsVfs` (zero-overhead pass-through), `GitTreeVfs` (gix-backed in-process git reading), and `OxcVfsAdapter` (oxc_resolver bridge) ([#113])
- In-process git tree reading for `diff`, replacing temporary worktree checkouts ([#113])
- REPL inline flags: `--json`, `--include-dynamic`, `--top`, `--top-modules`, `--ignore` on any command ([#99], [#124])
- REPL session settings via `set`/`unset`/`show` commands for persistent flag defaults ([#124])
- notify-based file watching with dirty flag and auto-refresh on REPL commands ([#98], [#117])
- Query result caching for instant REPL repeat commands ([#105], [#120])
- Pre-sorted tab completion candidates for binary search lookup ([#100], [#118])
- Project standards directory at `docs/standards/` ([#119])
- `cargo xtask bench` and `cargo xtask find-entry` subcommands ([#128])
- 154 integration tests closing dispatch-layer blind spot ([#169])

### Fixed

- `--chain`/`--cut` returned false "not reachable" on high-fanout modules due to backtracking discarding all paths ([#172], [#174])
- Side-effect imports (`import "./foo"`) silently dropped by parser ([#171], [#173])
- Diff cache corruption when working tree reads cache written by git-ref side ([#142])
- `PackageListEntry` JSON field names inconsistent with `PackageEntry` ([#149])
- False `direct_import: true` for nonexistent `--cut` targets ([#151])
- Annotated tags cause "not a commit" error in diff ([#145])
- Branch names containing slashes misclassified as file paths ([#146])
- `--max-weight` threshold error printed twice ([#148])
- `--diff-from` silently ignored `--json` flag ([#150])
- REPL: unknown inline flags silently became positional arguments ([#144])
- `cargo xtask perf-validate` failed in git worktrees ([#170])

### Changed

- Replaced bash scripts with thin xtask aliases in Justfile ([#127], [#128])
- Synthetic benchmark corpus augmented with pnpm, workspaces, and barrel files ([#125], [#129])

## [0.3.0] - 2026-02-21

Session abstraction, interactive REPL, contributor onboarding, CI.

### Added

- Interactive REPL mode (`chainsaw repl`) with commands: trace, chain, cut, diff, packages, imports, importers, entry, info, help ([#39], [#97])
- Tab completion via rustyline for module paths and package names ([#97])
- `Session` struct with stateful query API wrapping graph, cache, and entry resolution ([#97])
- GitHub Actions CI: fmt, clippy, test on Linux/macOS/Windows ([#81], [#89])
- `cargo xtask check` for local CI (fmt + clippy + test) ([#86])
- Pre-commit hook running `cargo xtask check` on feature branches ([#87])
- Contributor guide, issue templates, and PR template ([#84])

### Fixed

- Cache hit suppressed per-file unresolvable dynamic import details ([#80], [#94])
- Numeric flag validation: reject non-numeric values for `--top`, `--top-modules`, `--limit` ([#88])
- `--chain`/`--cut` not-found messages written to stdout instead of stderr ([#88])
- Misleading error messages for unsupported file types and directory entries ([#88])

## [0.2.0] - 2026-02-20

Git ref diff: compare dependency weight across commits, branches, and snapshots.

### Added

- `diff` subcommand: compare dependency weight across git refs (`chainsaw diff main feature src/index.ts`) ([#106], [#82])
- Auto-detection of diff arguments: file snapshot path vs git ref ([#82])
- Git worktree lifecycle helpers for clean branch comparison ([#82])
- Crate-level, module-level, and public API documentation ([#82])
- `#[non_exhaustive]` on all public structs for forward compatibility ([#82])

### Changed

- Crate renamed from `chainsaw` to `chainsaw-cli` for crates.io publishing ([#82])

## [0.1.0] - 2026-02-20

First publish. Dependency graph analysis for TypeScript/JavaScript and Python.

### Added

#### Language Support

- TypeScript/JavaScript: OXC parser with full ES module support, dynamic `import()`, `require()` detection, and re-export traversal
- TypeScript resolver: `oxc_resolver` with pnpm strict mode, 3-tier virtual store fallback, and workspace package name resolution via DashMap with negative caching
- Monorepo support: workspace package detection by walking up to `package.json`, cross-package `--diff` comparison ([#14], [#15])
- Python: tree-sitter parser with `import`, `from ... import`, relative imports, `TYPE_CHECKING` (TypeOnly), `importlib.import_module` (Dynamic), function-scoped imports (Dynamic)
- Python resolver: source roots (project root, `src/`, `lib/`, conftest.py-discovered), venv site-packages (`.pth` files, `pyvenv.cfg` fast path), namespace packages (PEP 420), C extension probing (`.so`/`.pyd`)
- Python: `sys.path` expression evaluator via tree-sitter for `conftest.py` and `sitecustomize.py` modifications
- Conformance test suite validated against CPython's `importlib.find_spec`
- `LanguageSupport` trait with `ProjectKind` auto-detection from file extension

#### CLI

- `trace` command: transitive import weight with exclusive weight via dominator tree, heavy dependency ranking ([#23], [#24])
- `--chain <target>`: show all import chains from entry to a package or file ([#1], [#2], [#3], [#4], [#34])
- `--cut <target>`: find optimal cut points to sever import chains ([#5], [#6], [#19])
- `diff` subcommand: compare two saved snapshots side-by-side with added/removed/changed sections ([#26])
- `packages` subcommand: list all third-party dependencies with module counts ([#38])
- `--save <path>` and `--diff-from <path>` for before/after snapshot comparison ([#9])
- `--ignore <pkg>`: exclude packages from trace output ([#25])
- `--top N` and `--top-modules N`: limit heavy dependency and module lists ([#11], [#12], [#65])
- `--max-weight <threshold>`: exit non-zero when weight exceeds limit for CI gating ([#58])
- `--include-dynamic`: fold dynamic imports into weight calculations and chain traversal ([#13], [#29])
- `--quiet` / `-q`: suppress informational output for scripting ([#50])
- `--no-color` flag with `NO_COLOR` and `TERM=dumb` environment variable support ([#57])
- `--json` output for trace command
- `--version` flag ([#37])
- `completions` subcommand for bash/zsh/fish/powershell ([#59])
- Color output with auto-detection via `IsTerminal` ([#57])
- Unresolvable dynamic import warnings with file locations ([#22], [#68])
- Dynamic import data captured in trace snapshots ([#67])

#### Caching

- Three-tier cache: tier 1 (whole-graph with file mtime stat-only fast path), tier 1.5 (incremental: re-parse changed files, reuse graph if imports unchanged), tier 2 (per-file parse+resolve, survives single-file edits)
- Lockfile sentinels tracking package manager lockfiles for cache invalidation ([#27])
- Background cache serialization and disk write via `std::thread::spawn`
- `CacheWriteHandle::join` for deterministic shutdown

#### Performance

- OXC parser replacing SWC for TypeScript/JavaScript parsing
- Concurrent discovery pipeline with lock-free `SegQueue` work queue + `DashSet` dedup + `rayon::scope` workers ([#76])
- Fused parse+resolve in single parallel pass
- `Vec<bool>` / `Vec<u32>` BFS indexing replacing `HashMap`/`HashSet` for dense `ModuleId` ranges ([#72], [#74])
- Single-pass parent recording replacing per-package BFS for shortest chains ([#70], [#71])
- Background thread cache serialization
- Thread pool capped at 8 to reduce VFS contention on macOS
- Python: `pyvenv.cfg` fast path for site-packages discovery, deferred C extension probing, bounded conftest scanning

#### Performance Infrastructure

- Adaptive benchmark harness with Welch t-test and early stopping (replacing criterion)
- `stats/` workspace crate with noise-aware statistical primitives (trimmed mean, confidence intervals)
- `perf.toml` registry mapping source files to benchmarks with complete coverage enforcement
- Pre-push hook running `cargo xtask perf-validate` on performance-sensitive changes
- Synthetic benchmark corpus as default target
- Justfile for external tool orchestration (`just bench-cold`, `just bench-all`)

### Fixed

- Scoped npm packages (`@scope/name`) in `--chain` and `--cut` ([#45])
- Cache false invalidation on pnpm workspaces ([#41], [#27])
- Dynamic `import()` in expression position not detected ([#8])
- `--diff` misclassifying shared packages as "only in" one entry point ([#7])
- Stale caches from new files and newly-resolvable imports ([#18])
- Workspace packages showing as absolute filesystem paths instead of package names ([#15])
- Duplicate edges from symlinks resolving to same target ([#17])
- `--chain`/`--cut` not traversing dynamic edges with `--include-dynamic` ([#13])
- `--cut` showing duplicate entries and every intermediate as a cut point ([#5], [#6], [#64])
- `--diff` comparing only top N packages instead of all reachable ([#7])
- `--save` silently doing nothing when combined with query flags ([#9])
- `--chain`/`--cut` exit 0 when target not found ([#46])
- `--top 0` showing misleading message instead of hiding section ([#47])
- Negative deltas missing minus sign in diff output ([#61])
- Entry point names showing as filenames in diff subcommand ([#63])
- Chain display showing bare filenames for pnpm store paths ([#53])
- Singular/plural forms for all count labels ([#56])
- Flag conflict validation: `--chain`/`--cut` with `--diff`, same entry point in `--diff` ([#10], [#51], [#54])
- Python: function-scoped imports classified as Static instead of Dynamic
- Python: cross-root phantom resolution for regular packages
- Python: dotted import component-by-component resolution
- Python: C extension loader precedence matching CPython
- `--chain`/`--cut` when target is the entry point itself ([#69])

[0.4.1]: https://github.com/rocketman-code/chainsaw/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/rocketman-code/chainsaw/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/rocketman-code/chainsaw/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/rocketman-code/chainsaw/compare/819d7f1...v0.2.0
[0.1.0]: https://github.com/rocketman-code/chainsaw/commits/819d7f1

[#1]: https://github.com/rocketman-code/chainsaw/issues/1
[#2]: https://github.com/rocketman-code/chainsaw/issues/2
[#3]: https://github.com/rocketman-code/chainsaw/issues/3
[#4]: https://github.com/rocketman-code/chainsaw/issues/4
[#5]: https://github.com/rocketman-code/chainsaw/issues/5
[#6]: https://github.com/rocketman-code/chainsaw/issues/6
[#7]: https://github.com/rocketman-code/chainsaw/issues/7
[#8]: https://github.com/rocketman-code/chainsaw/issues/8
[#9]: https://github.com/rocketman-code/chainsaw/issues/9
[#10]: https://github.com/rocketman-code/chainsaw/issues/10
[#11]: https://github.com/rocketman-code/chainsaw/issues/11
[#12]: https://github.com/rocketman-code/chainsaw/issues/12
[#13]: https://github.com/rocketman-code/chainsaw/issues/13
[#14]: https://github.com/rocketman-code/chainsaw/issues/14
[#15]: https://github.com/rocketman-code/chainsaw/issues/15
[#17]: https://github.com/rocketman-code/chainsaw/issues/17
[#18]: https://github.com/rocketman-code/chainsaw/issues/18
[#19]: https://github.com/rocketman-code/chainsaw/issues/19
[#22]: https://github.com/rocketman-code/chainsaw/issues/22
[#23]: https://github.com/rocketman-code/chainsaw/issues/23
[#24]: https://github.com/rocketman-code/chainsaw/issues/24
[#25]: https://github.com/rocketman-code/chainsaw/issues/25
[#26]: https://github.com/rocketman-code/chainsaw/issues/26
[#27]: https://github.com/rocketman-code/chainsaw/issues/27
[#29]: https://github.com/rocketman-code/chainsaw/issues/29
[#34]: https://github.com/rocketman-code/chainsaw/issues/34
[#37]: https://github.com/rocketman-code/chainsaw/issues/37
[#38]: https://github.com/rocketman-code/chainsaw/issues/38
[#39]: https://github.com/rocketman-code/chainsaw/issues/39
[#41]: https://github.com/rocketman-code/chainsaw/issues/41
[#45]: https://github.com/rocketman-code/chainsaw/issues/45
[#46]: https://github.com/rocketman-code/chainsaw/issues/46
[#47]: https://github.com/rocketman-code/chainsaw/issues/47
[#50]: https://github.com/rocketman-code/chainsaw/issues/50
[#51]: https://github.com/rocketman-code/chainsaw/issues/51
[#53]: https://github.com/rocketman-code/chainsaw/issues/53
[#54]: https://github.com/rocketman-code/chainsaw/issues/54
[#56]: https://github.com/rocketman-code/chainsaw/issues/56
[#57]: https://github.com/rocketman-code/chainsaw/issues/57
[#58]: https://github.com/rocketman-code/chainsaw/issues/58
[#59]: https://github.com/rocketman-code/chainsaw/issues/59
[#61]: https://github.com/rocketman-code/chainsaw/issues/61
[#63]: https://github.com/rocketman-code/chainsaw/issues/63
[#64]: https://github.com/rocketman-code/chainsaw/issues/64
[#65]: https://github.com/rocketman-code/chainsaw/issues/65
[#67]: https://github.com/rocketman-code/chainsaw/issues/67
[#68]: https://github.com/rocketman-code/chainsaw/issues/68
[#69]: https://github.com/rocketman-code/chainsaw/issues/69
[#70]: https://github.com/rocketman-code/chainsaw/issues/70
[#71]: https://github.com/rocketman-code/chainsaw/issues/71
[#72]: https://github.com/rocketman-code/chainsaw/issues/72
[#74]: https://github.com/rocketman-code/chainsaw/issues/74
[#76]: https://github.com/rocketman-code/chainsaw/issues/76
[#80]: https://github.com/rocketman-code/chainsaw/issues/80
[#81]: https://github.com/rocketman-code/chainsaw/issues/81
[#82]: https://github.com/rocketman-code/chainsaw/pull/82
[#84]: https://github.com/rocketman-code/chainsaw/pull/84
[#86]: https://github.com/rocketman-code/chainsaw/pull/86
[#87]: https://github.com/rocketman-code/chainsaw/pull/87
[#88]: https://github.com/rocketman-code/chainsaw/pull/88
[#89]: https://github.com/rocketman-code/chainsaw/pull/89
[#94]: https://github.com/rocketman-code/chainsaw/pull/94
[#97]: https://github.com/rocketman-code/chainsaw/pull/97
[#98]: https://github.com/rocketman-code/chainsaw/issues/98
[#99]: https://github.com/rocketman-code/chainsaw/issues/99
[#100]: https://github.com/rocketman-code/chainsaw/issues/100
[#103]: https://github.com/rocketman-code/chainsaw/issues/103
[#105]: https://github.com/rocketman-code/chainsaw/issues/105
[#106]: https://github.com/rocketman-code/chainsaw/issues/106
[#113]: https://github.com/rocketman-code/chainsaw/pull/113
[#117]: https://github.com/rocketman-code/chainsaw/pull/117
[#118]: https://github.com/rocketman-code/chainsaw/pull/118
[#119]: https://github.com/rocketman-code/chainsaw/pull/119
[#120]: https://github.com/rocketman-code/chainsaw/pull/120
[#124]: https://github.com/rocketman-code/chainsaw/pull/124
[#125]: https://github.com/rocketman-code/chainsaw/issues/125
[#127]: https://github.com/rocketman-code/chainsaw/issues/127
[#128]: https://github.com/rocketman-code/chainsaw/pull/128
[#129]: https://github.com/rocketman-code/chainsaw/pull/129
[#142]: https://github.com/rocketman-code/chainsaw/issues/142
[#144]: https://github.com/rocketman-code/chainsaw/issues/144
[#145]: https://github.com/rocketman-code/chainsaw/issues/145
[#146]: https://github.com/rocketman-code/chainsaw/issues/146
[#148]: https://github.com/rocketman-code/chainsaw/issues/148
[#149]: https://github.com/rocketman-code/chainsaw/issues/149
[#150]: https://github.com/rocketman-code/chainsaw/issues/150
[#151]: https://github.com/rocketman-code/chainsaw/issues/151
[#169]: https://github.com/rocketman-code/chainsaw/pull/169
[#170]: https://github.com/rocketman-code/chainsaw/issues/170
[#171]: https://github.com/rocketman-code/chainsaw/issues/171
[#172]: https://github.com/rocketman-code/chainsaw/issues/172
[#173]: https://github.com/rocketman-code/chainsaw/pull/173
[#174]: https://github.com/rocketman-code/chainsaw/pull/174
[#176]: https://github.com/rocketman-code/chainsaw/issues/176
[#178]: https://github.com/rocketman-code/chainsaw/issues/178
