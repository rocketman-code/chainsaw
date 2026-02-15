#!/usr/bin/env bash
set -euo pipefail

# Full benchmark matrix: 5 codebases x 3 scenarios, A/B comparison.
# Usage: ./scripts/bench-matrix.sh <binary-A> [binary-B]
# Results exported to /tmp/bench-matrix-<timestamp>/

BINARY_A="${1:?Usage: bench-matrix.sh <binary-A> [binary-B]}"
BINARY_B="${2:-}"
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
OUTDIR="/tmp/bench-matrix-${TIMESTAMP}"
mkdir -p "$OUTDIR"

# --- Benchmark targets ---
# Format: name|entry|root
# Only codebases with 500+ reachable modules (statistically meaningful)
TARGETS=(
  "typeorm|$HOME/dev/typeorm/typeorm/src/index.ts|$HOME/dev/typeorm/typeorm"
  "excalidraw|$HOME/dev/excalidraw/excalidraw/packages/excalidraw/index.tsx|$HOME/dev/excalidraw/excalidraw"
  "wrangler|$HOME/dev/cloudflare/workers-sdk/packages/wrangler/src/index.ts|$HOME/dev/cloudflare/workers-sdk"
  "openclaw|$HOME/dev/openclaw/openclaw/src/index.ts|$HOME/dev/openclaw/openclaw"
  "aws-cli|$HOME/dev/aws/aws-cli/awscli/__init__.py|$HOME/dev/aws/aws-cli"
)

# --- Regression sentinels (small projects, just check for crashes/regressions) ---
SENTINELS=(
  "sentry-python|$HOME/dev/getsentry/sentry-python/sentry_sdk/__init__.py|$HOME/dev/getsentry/sentry-python"
  "ansible|$HOME/dev/ansible/ansible/lib/ansible/__init__.py|$HOME/dev/ansible/ansible"
)

echo "Benchmark matrix: ${#TARGETS[@]} targets x 3 scenarios"
echo "Regression sentinels: ${#SENTINELS[@]} targets"
echo "Output: $OUTDIR"
echo ""

run_cold() {
  local name="$1" entry="$2" root="$3"
  echo "=== COLD: $name ==="
  local args=( --warmup 1 --min-runs 30 --prepare "rm -f ${root}/.chainsaw.cache" )
  if [ -n "$BINARY_B" ]; then
    hyperfine "${args[@]}" \
      -n "baseline" "$BINARY_A trace $entry --quiet" \
      -n "candidate" "$BINARY_B trace $entry --quiet" \
      --export-json "$OUTDIR/cold-${name}.json"
  else
    hyperfine "${args[@]}" \
      -n "baseline" "$BINARY_A trace $entry --quiet" \
      --export-json "$OUTDIR/cold-${name}.json"
  fi
}

run_warm() {
  local name="$1" entry="$2" root="$3"
  echo "=== WARM: $name ==="
  # Prime caches
  "$BINARY_A" trace "$entry" --quiet > /dev/null 2>&1
  [ -n "$BINARY_B" ] && "$BINARY_B" trace "$entry" --quiet > /dev/null 2>&1
  local args=( --warmup 5 --min-runs 50 )
  if [ -n "$BINARY_B" ]; then
    hyperfine "${args[@]}" \
      -n "baseline" "$BINARY_A trace $entry --quiet" \
      -n "candidate" "$BINARY_B trace $entry --quiet" \
      --export-json "$OUTDIR/warm-${name}.json"
  else
    hyperfine "${args[@]}" \
      -n "baseline" "$BINARY_A trace $entry --quiet" \
      --export-json "$OUTDIR/warm-${name}.json"
  fi
}

run_edit() {
  local name="$1" entry="$2" root="$3"
  echo "=== EDIT: $name ==="
  "$BINARY_A" trace "$entry" --quiet > /dev/null 2>&1
  [ -n "$BINARY_B" ] && "$BINARY_B" trace "$entry" --quiet > /dev/null 2>&1
  local args=( --warmup 3 --min-runs 30 --prepare "touch $entry" )
  if [ -n "$BINARY_B" ]; then
    hyperfine "${args[@]}" \
      -n "baseline" "$BINARY_A trace $entry --quiet" \
      -n "candidate" "$BINARY_B trace $entry --quiet" \
      --export-json "$OUTDIR/edit-${name}.json"
  else
    hyperfine "${args[@]}" \
      -n "baseline" "$BINARY_A trace $entry --quiet" \
      --export-json "$OUTDIR/edit-${name}.json"
  fi
}

# Verify all targets exist
for target in "${TARGETS[@]}" "${SENTINELS[@]}"; do
  IFS='|' read -r name entry root <<< "$target"
  if [ ! -f "$entry" ]; then
    echo "FATAL: $name entry not found: $entry"
    exit 1
  fi
done

# Run full matrix on primary targets
for target in "${TARGETS[@]}"; do
  IFS='|' read -r name entry root <<< "$target"
  run_cold "$name" "$entry" "$root"
  run_warm "$name" "$entry" "$root"
  run_edit "$name" "$entry" "$root"
  echo ""
done

# Run regression sentinels (warm only -- too small for meaningful cold/edit)
for target in "${SENTINELS[@]}"; do
  IFS='|' read -r name entry root <<< "$target"
  run_warm "$name" "$entry" "$root"
  echo ""
done

echo ""
echo "All results saved to: $OUTDIR"
echo "Analyze with: cat $OUTDIR/*.json | python3 -m json.tool"
