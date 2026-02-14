# chainsaw

Trace transitive import weight in TypeScript/JavaScript and Python codebases. Given an entry file, chainsaw builds a dependency graph and shows exactly which packages get pulled in at startup and how much code they bring.

## Features

- Traces static imports through source code and `node_modules` / `site-packages`
- Classifies imports as static, dynamic, or type-only — only static imports count toward startup weight
- Shows the shortest import chain to any dependency (`--chain`) and where to cut it (`--cut`)
- Compares two entry points or before/after snapshots (`--diff`)
- Works across monorepo package boundaries (pnpm, yarn, npm workspaces)
- Two-tier disk cache — returns instantly on cache hit, re-parses only changed files on miss

## Usage

### Trace an entry point

Point chainsaw at any `.ts`, `.tsx`, `.js`, `.jsx`, or `.py` file:

```
$ chainsaw trace src/cli/main.ts

src/cli/main.ts
Static transitive weight: 17.7 MB (1870 modules)
Dynamic-only weight: 16 KB (16 modules, not loaded at startup)

Heavy dependencies (static):
  highlight.js                        1.4 MB  193 files
    -> src/cli/main.ts -> ... -> cli-highlight -> highlight.js
  zod                                 537 KB  76 files
    -> src/cli/main.ts -> src/config/config.ts -> src/config/schema.ts -> zod

Modules (sorted by exclusive weight):
  src/cli/main.ts                                            17.7 MB
  src/commands/doctor.ts                                     17.7 MB
  src/config/sessions/transcript.ts                          17.5 MB
  ...
```

The "static transitive weight" is the total size of every file that gets loaded when this entry point is imported. The "shortest chain" under each heavy dependency shows exactly how it gets pulled in.

### Find all import chains to a package

If you want to understand *why* a package is in your dependency tree:

```
$ chainsaw trace src/cli/main.ts --chain zod

3 chains to "zod" (3 hops):

  1. src/cli/main.ts -> src/config/config.ts -> src/config/schema.ts -> zod
  2. src/cli/main.ts -> src/config/migrate.ts -> src/config/schema.ts -> zod
  3. src/cli/main.ts -> src/config/validation.ts -> src/config/schema.ts -> zod
```

### Find where to cut

All three chains above pass through `src/config/schema.ts`. Chainsaw can find that for you:

```
$ chainsaw trace src/cli/main.ts --cut zod

1 cut point to sever all 3 chains to "zod":

  src/config/schema.ts                                    (breaks 3/3 chains)
```

Convert that file's import to a dynamic `import()` and zod drops out of your startup path.

When no single file can break all chains, chainsaw tells you:

```
No single cut point can sever all 3 chains to "chalk".
Each chain takes a different path — multiple fixes needed.
```

### Compare two entry points

See what's different between two parts of your codebase:

```
$ chainsaw trace src/cli.ts --diff src/server.ts

Diff: src/cli.ts vs src/server.ts

  src/cli.ts                               793 KB
  src/server.ts                            3.8 MB
  Delta                                    +3.1 MB

Only in src/cli.ts:
  - degit
  - smol-toml
Only in src/server.ts:
  + undici
  + workerd
  + zod
Shared: 21 packages
```

In a monorepo, the diff target can be in a different package — chainsaw builds a separate graph from that package's root automatically.

### Before/after comparison

Save a snapshot, make changes, then compare:

```
$ chainsaw trace src/cli/main.ts --save before.json
$ # ... make changes ...
$ chainsaw trace src/cli/main.ts --diff-from before.json

Diff: before.json vs src/cli/main.ts

  before.json                              1.3 MB
  src/cli/main.ts                          1.1 MB
  Delta                                    -200 KB

Removed since before.json:
  - zod
Shared: 2 packages
```

### JSON output

```
$ chainsaw trace src/index.ts --json
```

Pipe to `jq`, feed to a dashboard, or use in CI.

## Install

Build from source (requires Rust 1.85+):

```
$ cargo install --path .
```

Or:

```
$ cargo build --release
$ # binary at target/release/chainsaw
```

### List packages

```
$ chainsaw packages src/index.ts

12 packages:

  commander                                 195 KB  9 files
  zod                                       537 KB  76 files
  ...
```

## Flags

```
chainsaw <COMMAND>

Commands:
  trace     Trace the transitive import weight from an entry point
  diff      Compare two saved trace snapshots
  packages  List all third-party packages in the dependency graph
```

```
chainsaw trace [OPTIONS] <ENTRY>

Options:
      --chain <PKG|FILE>   Show all shortest import chains to a package or file
      --cut <PKG|FILE>     Find where to cut to sever all chains to a package or file
      --diff <ENTRY>       Compare against another entry point
      --diff-from <PATH>   Compare against a previously saved snapshot
      --save <PATH>        Save a trace snapshot for later comparison
      --include-dynamic    Also traverse dynamic imports
      --ignore <PKG>...    Exclude packages from the heavy dependencies list
      --top <N>            Show top N heaviest dependencies (0 to hide, -1 for all) [default: 10]
      --top-modules <N>    Show top N modules by exclusive weight (0 to hide, -1 for all) [default: 20]
      --json               Output machine-readable JSON
      --no-cache           Force full re-parse, ignoring cache
  -V, --version            Print version
```

## License

MIT
