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

### Show all import chains to a package

```bash
chainsaw trace src/cli/program/config-guard.ts --chain zod
```

```
3 chains to "zod" (3 hops):

  1. src/cli/program/config-guard.ts -> src/config/config.ts -> src/config/zod-schema.ts -> zod
  2. src/cli/program/config-guard.ts -> src/config/legacy-migrate.ts -> src/config/zod-schema.ts -> zod
  3. src/cli/program/config-guard.ts -> src/config/validation.ts -> src/config/zod-schema.ts -> zod
```

### Find where to cut

```bash
chainsaw trace src/cli/program/config-guard.ts --cut zod
```

```
1 cut point to sever all 3 chains to "zod":

  src/config/zod-schema.ts                                (breaks 3/3 chains)
```

When no single module can break all chains, chainsaw tells you:

```
No single cut point can sever all 3 chains to "chalk".
Each chain takes a different path — multiple fixes needed.
```

### Before/after comparison

```bash
chainsaw trace src/cli/program/config-guard.ts --save before.json
# ... make changes ...
chainsaw trace src/cli/program/config-guard.ts --diff-from before.json
```

```
Diff: before.json vs src/cli/program/config-guard.ts

  before.json                              1.3 MB
  src/cli/program/config-guard.ts          1.1 MB
  Delta                                    -200 KB

Removed since before.json:
  - zod
Shared: 2 packages
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
Shared: 139 packages
```

### Monorepo support

chainsaw works across packages in a monorepo. Workspace dependencies display as their package name (not filesystem paths), and `--diff` works across package boundaries:

```bash
# Compare entry points in different packages
chainsaw trace packages/create-cloudflare/src/cli.ts --diff packages/miniflare/src/index.ts
```

```
Diff: src/cli.ts vs src/index.ts

  src/cli.ts                               793 KB
  src/index.ts                             3.8 MB
  Delta                                    +3.1 MB

Only in src/cli.ts:
  - degit
  - smol-toml
Only in src/index.ts:
  + undici
  + workerd
  + zod
Shared: 21 packages
```

When the diff target is in a different package, chainsaw automatically builds a second graph from that package's root. Each package retains its own resolution context.

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
      --diff <DIFF>          Compare against another entry point (works across monorepo packages)
      --save <PATH>          Save a trace snapshot for later comparison
      --diff-from <PATH>     Compare against a previously saved snapshot
      --include-dynamic      Also traverse dynamic imports
      --top <TOP>            Show top N heaviest dependencies [default: 10]
      --top-modules <N>      Show top N modules by transitive cost (0 to hide, -1 for all) [default: 20]
      --chain <CHAIN>        Show all shortest import chains to a specific package
      --cut <CUT>            Show where to cut to sever all chains to a package
      --json                 Output machine-readable JSON
      --no-cache             Force full re-parse
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
| `import("y").then(...)` | dynamic | no |
| `import type { x } from "y"` | type-only | no |
| `export type { x } from "y"` | type-only | no |

## Package manager support

- npm (with workspaces)
- yarn (with workspaces)
- pnpm (all layouts: isolated/strict, hoisted, shamefully-hoist, workspaces)

Workspace packages are detected automatically and display as their package name in output (e.g., `@cloudflare/cli` instead of an absolute path).

## Performance

Tested against a ~480K line TypeScript codebase:

| | Time |
|---|---|
| Cold parse (full graph build) | ~1.5s |
| Cached load + query | ~55ms |
| Cache file size | ~4.4 MB |
