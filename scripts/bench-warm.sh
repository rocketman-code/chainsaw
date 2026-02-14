#!/usr/bin/env bash
set -euo pipefail

# Warm-run benchmark: cache exists, nothing changed (tier 1 cache hit)
# Usage: ./scripts/bench-warm.sh [path-to-binary-A] [path-to-binary-B]
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
    local label="$1" entry="$2" root="$3" binary="$BINARY_A"

    echo "=== Warm run: $label ==="

    # Prime the cache
    $binary trace "$entry" --quiet > /dev/null 2>&1

    if [ -n "$BINARY_B" ]; then
        # Prime cache for B too
        $BINARY_B trace "$entry" --quiet > /dev/null 2>&1
        hyperfine \
            --warmup 3 \
            --min-runs 30 \
            "$BINARY_A trace $entry --quiet" \
            "$BINARY_B trace $entry --quiet" \
            --export-json "/tmp/bench-warm-${label}.json"
    else
        hyperfine \
            --warmup 3 \
            --min-runs 30 \
            "$BINARY_A trace $entry --quiet"
    fi
}

if [ -f "$TS_ENTRY" ]; then
    run_bench "ts" "$TS_ENTRY" "$TS_ROOT"
fi

if [ -f "$PY_ENTRY" ]; then
    run_bench "py" "$PY_ENTRY" "$PY_ROOT"
fi
