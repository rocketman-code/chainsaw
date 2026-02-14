#!/usr/bin/env bash
set -euo pipefail

# Cold-run benchmark: no cache, full parse + resolve + build + cache save
# Measures the real first-run experience by removing the cache before each run.
# Usage: ./scripts/bench-cold.sh [path-to-binary-A] [path-to-binary-B]
# If no args: benchmarks the current release build
#
# Override test targets via environment variables:
#   TS_BENCH_ENTRY, TS_BENCH_ROOT, PY_BENCH_ENTRY, PY_BENCH_ROOT

BINARY_A="${1:-target/release/chainsaw}"
BINARY_B="${2:-}"

TS_ENTRY="${TS_BENCH_ENTRY:-/tmp/openclaw-fresh/packages/web-app/src/index.tsx}"
TS_ROOT="${TS_BENCH_ROOT:-/tmp/openclaw-fresh}"
PY_ENTRY="${PY_BENCH_ENTRY:-/Users/hlal/dev/aws/aws-cli/awscli/__init__.py}"
PY_ROOT="${PY_BENCH_ROOT:-/Users/hlal/dev/aws/aws-cli}"

run_bench() {
    local label="$1" entry="$2" root="$3"

    echo "=== Cold run: $label ==="

    if [ -n "$BINARY_B" ]; then
        hyperfine \
            --warmup 1 \
            --min-runs 10 \
            --prepare "rm -f ${root}/.chainsaw.cache" \
            "$BINARY_A trace $entry --quiet" \
            "$BINARY_B trace $entry --quiet" \
            --export-json "/tmp/bench-cold-${label}.json"
    else
        hyperfine \
            --warmup 1 \
            --min-runs 10 \
            --prepare "rm -f ${root}/.chainsaw.cache" \
            "$BINARY_A trace $entry --quiet"
    fi
}

if [ -f "$TS_ENTRY" ]; then
    run_bench "ts" "$TS_ENTRY" "$TS_ROOT"
fi

if [ -f "$PY_ENTRY" ]; then
    run_bench "py" "$PY_ENTRY" "$PY_ROOT"
fi
