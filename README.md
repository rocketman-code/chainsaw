# chainsaw

Rust CLI that scans TypeScript/JavaScript codebases, builds a full dependency graph (including node_modules), and reports transitive import weight from any entry point -- showing exactly which heavy dependencies load at startup and the shortest chain to each.

## Why

In large TypeScript projects, a single static import of a lightweight function can transitively pull in megabytes of heavy dependencies (AWS SDK, AJV, OpenAI SDK, etc.) at module load time. These chains are invisible during development and cause severe cold-start latency.

chainsaw automates discovery in milliseconds.

## Install

```bash
cargo install --path .
```

Or build from source:

```bash
cargo build --release
# Binary at target/release/chainsaw
```

## Usage

### Trace an entry point

```bash
chainsaw trace src/cli/program/config-guard.ts
```

```
src/cli/program/config-guard.ts
Static transitive weight: 17.7 MB (1870 modules)
Dynamic-only weight: 16 KB (16 modules, not loaded at startup)

Heavy dependencies (static):
  @mariozechner/clipboard-darwin-universal 4.3 MB  1 files
    -> src/cli/program/config-guard.ts -> ... -> @mariozechner/clipboard -> @mariozechner/clipboard-darwin-universal
  highlight.js                        1.4 MB  193 files
    -> src/cli/program/config-guard.ts -> ... -> cli-highlight -> highlight.js
  zod                                 537 KB  76 files
    -> src/cli/program/config-guard.ts -> src/config/config.ts -> src/config/zod-schema.ts -> zod

Modules (sorted by transitive cost):
  src/cli/program/config-guard.ts                         17.7 MB
  src/commands/doctor-config-flow.ts                      17.7 MB
  src/config/sessions/transcript.ts                       17.5 MB
  ...
```

### Find the chain to a specific package

```bash
chainsaw trace src/index.ts --chain zod
```

```
Chain (3 hops):
  src/index.ts -> src/config/config.ts -> src/config/zod-schema.ts -> zod
```

### Compare two entry points

```bash
chainsaw trace src/cli/program/config-guard.ts --diff src/cli/program/preaction.ts
```

```
Diff: src/cli/program/config-guard.ts vs src/cli/program/preaction.ts

  src/cli/program/config-guard.ts          17.7 MB
  src/cli/program/preaction.ts             213 KB
  Delta                                    17.5 MB

Only in src/cli/program/config-guard.ts:
  - highlight.js
  - zod
  - openai
Only in src/cli/program/preaction.ts:
  + chalk
  + tslog
```

### JSON output

```bash
chainsaw trace src/index.ts --json
```

### All flags

```
chainsaw trace [OPTIONS] <ENTRY>

Arguments:
  <ENTRY>  Entry point file to trace from

Options:
      --diff <DIFF>      Compare against another entry point
      --include-dynamic  Also traverse dynamic imports
      --top <TOP>        Show top N heaviest dependencies [default: 10]
      --chain <CHAIN>    Show the full shortest chain to a specific package
      --json             Output machine-readable JSON
      --no-cache         Force full re-parse
  -h, --help             Print help
```

## How it works

1. Walk source files with the [ignore](https://crates.io/crates/ignore) crate (respects .gitignore, skips node_modules)
2. Parse all files in parallel with [SWC](https://crates.io/crates/swc_ecma_parser) via [rayon](https://crates.io/crates/rayon)
3. Classify every import as static, dynamic, or type-only
4. Resolve import specifiers with [oxc_resolver](https://crates.io/crates/oxc_resolver) (full Node module resolution)
5. Follow resolved paths into node_modules recursively, building an in-memory adjacency graph
6. Cache the graph to disk with [bitcode](https://crates.io/crates/bitcode) for instant reloads
7. BFS from the entry point, following only static edges (startup-relevant imports)
8. Report transitive weight, heavy packages, and shortest import chains

## Import classification

| Syntax | Kind | Counts toward startup |
|--------|------|-----------------------|
| `import { x } from "y"` | static | yes |
| `export { x } from "y"` | static | yes |
| `require("y")` | static | yes |
| `await import("y")` | dynamic | no |
| `import type { x } from "y"` | type-only | no |
| `export type { x } from "y"` | type-only | no |

## Package manager support

- npm
- yarn
- pnpm (all layouts: isolated/strict, hoisted, shamefully-hoist, workspaces)

## Performance

Tested against a ~480K line TypeScript codebase:

| | Time |
|---|---|
| Cold parse (full graph build) | ~1.5s |
| Cached load + query | ~55ms |
| Cache file size | ~4.4 MB |
