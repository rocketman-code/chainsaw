# chainsaw

Trace the transitive import weight of any TypeScript or Python entry point. Point it at a file, get back what gets pulled in at startup and how much code it costs.

```
$ chainsaw trace src/index.ts

src/index.ts
Static transitive weight: 8.2 MB (2140 modules)
Dynamic-only weight: 120 KB (34 modules, not loaded at startup)

Heavy dependencies (static):
  undici                              1.1 MB  108 files
    -> src/index.ts -> src/http/client.ts -> undici
  date-fns                            619 KB  304 files
    -> src/index.ts -> src/jobs/scheduler.ts -> date-fns
  zod                                 537 KB  76 files
    -> src/index.ts -> src/api/routes.ts -> src/api/validation.ts -> zod

Modules (sorted by exclusive weight):
  src/generated/api-types.ts                                412 KB
  src/i18n/locales.ts                                       238 KB
  src/api/routes.ts                                         127 KB
  ...
```

Only static imports count toward the weight. Dynamic `import()` and type-only imports are tracked separately.

## Install

Requires Rust 1.91+:

```
cargo install chainsaw-cli
```

## Usage

### Python

Works the same way. Resolution matches `importlib` behavior -- source roots, virtualenv, `.pth` files, `sys.path` modifications, C extensions.

```
$ chainsaw trace app/main.py

app/main.py
Static transitive weight: 2.1 MB (608 modules)

Heavy dependencies (static):
  botocore                            1.2 MB  340 files
    -> app/main.py -> ... -> boto3 -> botocore
```

### Why is this package in my tree?

```
$ chainsaw trace src/index.ts --chain zod

3 chains to "zod" (3 hops):

  1. src/index.ts -> src/api/routes.ts -> src/api/validation.ts -> zod
  2. src/index.ts -> src/api/middleware.ts -> src/api/validation.ts -> zod
  3. src/index.ts -> src/config/env.ts -> src/api/validation.ts -> zod
```

### Where to cut

All three chains pass through `src/api/validation.ts`. Chainsaw finds that:

```
$ chainsaw trace src/index.ts --cut zod

1 cut point to sever all 3 chains to "zod":

  src/api/validation.ts                                   (breaks 3/3 chains)
```

Make that import dynamic and zod drops out of your startup path.

When no single file can break all chains:

```
No single cut point can sever all 3 chains to "chalk".
Each chain takes a different path — multiple fixes needed.
```

### Diff

Compare two entry points:

```
$ chainsaw trace src/index.ts --diff src/worker.ts

Diff: src/index.ts vs src/worker.ts

  src/index.ts                               8.2 MB
  src/worker.ts                              2.4 MB
  Delta                                      -5.8 MB

Only in src/index.ts:
  - undici                              1.1 MB
  - date-fns                            619 KB
  - zod                                 537 KB
Only in src/worker.ts:
  + html-rewriter                        84 KB
Shared: 12 packages
```

Or save a snapshot and compare before/after:

```
$ chainsaw trace src/index.ts --save before.json
# ... make changes ...
$ chainsaw trace src/index.ts --diff-from before.json
```

The `diff` subcommand compares two saved snapshots directly: `chainsaw diff before.json after.json`

Compare against a git ref to see how weight changed over time:

```
$ chainsaw diff HEAD~10 --entry src/index.ts
```

In a monorepo, the diff target can be in a different package -- chainsaw builds a separate graph from that package's root automatically.

### Interactive mode

Run multiple queries against a cached graph without rebuilding:

```
$ chainsaw repl src/index.ts

chainsaw> trace
chainsaw> chain zod
chainsaw> cut zod
chainsaw> packages
```

### CI gating

```
$ chainsaw trace src/index.ts --max-weight 5MB --quiet --top 0 --top-modules 0

error: static transitive weight 8.2 MB (2140 modules) exceeds --max-weight threshold 5.0 MB
```

Exits non-zero when static weight exceeds the threshold. Accepts `5MB`, `500KB`, `100B`.

### JSON

```
$ chainsaw trace src/index.ts --json --quiet | jq .static_weight_bytes
```

### Shell completions

```
chainsaw completions zsh > ~/.zfunc/_chainsaw
chainsaw completions bash > /etc/bash_completion.d/chainsaw
chainsaw completions fish > ~/.config/fish/completions/chainsaw.fish
```

Run `chainsaw --help` for the full flag reference.

## Development

```
git clone https://github.com/rocketman-code/chainsaw.git
cd chainsaw
cargo xtask install-hooks
cargo test --workspace
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for the full development guide.

## License

MIT
