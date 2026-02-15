# Chainsaw development tasks
#
# Benchmark recipes orchestrate hyperfine for A/B comparison.
# Override targets: TS_BENCH_ENTRY, TS_BENCH_ROOT, PY_BENCH_ENTRY, PY_BENCH_ROOT

set dotenv-load := false

ts_entry := env("TS_BENCH_ENTRY", "/tmp/openclaw-fresh/packages/web-app/src/index.tsx")
ts_root := env("TS_BENCH_ROOT", "/tmp/openclaw-fresh")
py_entry := env("PY_BENCH_ENTRY", env("HOME", "") + "/dev/aws/aws-cli/awscli/__init__.py")
py_root := env("PY_BENCH_ROOT", env("HOME", "") + "/dev/aws/aws-cli")

# List available recipes
default:
    @just --list

# Cold-run benchmark: no cache, full parse + resolve + build
bench-cold binary_a="target/release/chainsaw" binary_b="":
    #!/usr/bin/env bash
    set -euo pipefail
    run() {
        local label="$1" entry="$2" root="$3"
        echo "=== Cold: $label ==="
        if [ -n "{{binary_b}}" ]; then
            hyperfine --warmup 1 --min-runs 10 \
                --prepare "rm -f ${root}/.chainsaw.cache" \
                "{{binary_a}} trace $entry --quiet" \
                "{{binary_b}} trace $entry --quiet" \
                --export-json "/tmp/bench-cold-${label}.json"
        else
            hyperfine --warmup 1 --min-runs 10 \
                --prepare "rm -f ${root}/.chainsaw.cache" \
                "{{binary_a}} trace $entry --quiet"
        fi
    }
    [ -f "{{ts_entry}}" ] && run "ts" "{{ts_entry}}" "{{ts_root}}" || true
    [ -f "{{py_entry}}" ] && run "py" "{{py_entry}}" "{{py_root}}" || true

# Warm-run benchmark: cache exists, nothing changed
bench-warm binary_a="target/release/chainsaw" binary_b="":
    #!/usr/bin/env bash
    set -euo pipefail
    prime() {
        local binary="$1" entry="$2"
        "$binary" trace "$entry" --quiet > /dev/null 2>&1
    }
    run() {
        local label="$1" entry="$2" root="$3"
        echo "=== Warm: $label ==="
        prime "{{binary_a}}" "$entry"
        [ -n "{{binary_b}}" ] && prime "{{binary_b}}" "$entry"
        if [ -n "{{binary_b}}" ]; then
            hyperfine --warmup 3 --min-runs 30 \
                "{{binary_a}} trace $entry --quiet" \
                "{{binary_b}} trace $entry --quiet" \
                --export-json "/tmp/bench-warm-${label}.json"
        else
            hyperfine --warmup 3 --min-runs 30 \
                "{{binary_a}} trace $entry --quiet"
        fi
    }
    [ -f "{{ts_entry}}" ] && run "ts" "{{ts_entry}}" "{{ts_root}}" || true
    [ -f "{{py_entry}}" ] && run "py" "{{py_entry}}" "{{py_root}}" || true

# Edit-run benchmark: touch entry file, re-trace (tier 2 cache)
bench-edit binary_a="target/release/chainsaw" binary_b="":
    #!/usr/bin/env bash
    set -euo pipefail
    prime() {
        local binary="$1" entry="$2"
        "$binary" trace "$entry" --quiet > /dev/null 2>&1
    }
    run() {
        local label="$1" entry="$2" root="$3"
        echo "=== Edit: $label ==="
        prime "{{binary_a}}" "$entry"
        [ -n "{{binary_b}}" ] && prime "{{binary_b}}" "$entry"
        if [ -n "{{binary_b}}" ]; then
            hyperfine --warmup 1 --min-runs 15 \
                --prepare "touch $entry" \
                "{{binary_a}} trace $entry --quiet" \
                "{{binary_b}} trace $entry --quiet" \
                --export-json "/tmp/bench-edit-${label}.json"
        else
            hyperfine --warmup 1 --min-runs 15 \
                --prepare "touch $entry" \
                "{{binary_a}} trace $entry --quiet"
        fi
    }
    [ -f "{{ts_entry}}" ] && run "ts" "{{ts_entry}}" "{{ts_root}}" || true
    [ -f "{{py_entry}}" ] && run "py" "{{py_entry}}" "{{py_root}}" || true

# Run all benchmark scenarios (builds release first)
bench-all binary_a="target/release/chainsaw" binary_b="":
    cargo build --release
    just bench-cold {{binary_a}} {{binary_b}}
    just bench-warm {{binary_a}} {{binary_b}}
    just bench-edit {{binary_a}} {{binary_b}}
