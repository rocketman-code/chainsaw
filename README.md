# chainsaw

Trace the transitive import weight of any TypeScript or Python entry point. Point it at a file, get back what gets pulled in at startup and how much code it costs.

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

Only static imports count toward the weight. Dynamic `import()` and type-only imports are tracked separately.

## Install

Requires Rust 1.85+:

```
cargo install chainsaw-cli
```

## Usage

### Python

Works the same way. Resolution matches `importlib` behavior -- source roots, virtualenv, `.pth` files, `sys.path` modifications, C extensions.

```
$ chainsaw trace manage.py

manage.py
Static transitive weight: 2.1 MB (608 modules)

Heavy dependencies (static):
  botocore                            1.2 MB  340 files
    -> manage.py -> ... -> boto3 -> botocore
```

### Why is this package in my tree?

```
$ chainsaw trace src/cli/main.ts --chain zod

3 chains to "zod" (3 hops):

  1. src/cli/main.ts -> src/config/config.ts -> src/config/schema.ts -> zod
  2. src/cli/main.ts -> src/config/migrate.ts -> src/config/schema.ts -> zod
  3. src/cli/main.ts -> src/config/validation.ts -> src/config/schema.ts -> zod
```

### Where to cut

All three chains pass through `src/config/schema.ts`. Chainsaw finds that:

```
$ chainsaw trace src/cli/main.ts --cut zod

1 cut point to sever all 3 chains to "zod":

  src/config/schema.ts                                    (breaks 3/3 chains)
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
$ chainsaw trace src/cli.ts --diff src/server.ts

Diff: src/cli.ts vs src/server.ts

  src/cli.ts                               793 KB
  src/server.ts                            3.8 MB
  Delta                                    +3.1 MB

Only in src/cli.ts:
  - degit                                42 KB
  - smol-toml                            28 KB
Only in src/server.ts:
  + undici                              1.1 MB
  + workerd                             800 KB
  + zod                                 537 KB
Shared: 21 packages
```

Or save a snapshot and compare before/after:

```
$ chainsaw trace src/cli/main.ts --save before.json
# ... make changes ...
$ chainsaw trace src/cli/main.ts --diff-from before.json
```

The `diff` subcommand compares two saved snapshots directly: `chainsaw diff before.json after.json`

In a monorepo, the diff target can be in a different package -- chainsaw builds a separate graph from that package's root automatically.

### CI gating

```
$ chainsaw trace src/index.ts --max-weight 5MB --quiet --top 0 --top-modules 0

error: static weight 36.3 MB exceeds threshold 5.0 MB
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

## License

MIT
