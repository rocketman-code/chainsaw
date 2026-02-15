#!/usr/bin/env bash
set -euo pipefail

# Find the entry point that produces the largest module graph in a codebase.
# Usage: ./scripts/find-largest-entry.sh <project-root> [chainsaw-binary]
#
# Scans all index.ts, index.tsx, __init__.py files (outside node_modules/.venv)
# and runs chainsaw on each. Prints the entry with the most reachable modules.

ROOT="${1:?Usage: find-largest-entry.sh <project-root> [chainsaw-binary]}"
BINARY="${2:-target/release/chainsaw}"

ROOT="$(cd "$ROOT" && pwd)"

# Find candidate entry points
candidates=()
while IFS= read -r -d '' f; do
  candidates+=("$f")
done < <(find "$ROOT" \
  -not -path "*/node_modules/*" \
  -not -path "*/.venv/*" \
  -not -path "*/venv/*" \
  -not -path "*/__pycache__/*" \
  -not -path "*/.git/*" \
  \( -name "index.ts" -o -name "index.tsx" -o -name "__init__.py" \) \
  -print0)

if [ ${#candidates[@]} -eq 0 ]; then
  echo "No candidate entry points found in $ROOT" >&2
  exit 1
fi

echo "Scanning ${#candidates[@]} candidates in $ROOT..." >&2

best_count=0
best_entry=""

for f in "${candidates[@]}"; do
  line=$("$BINARY" trace "$f" --quiet --no-cache 2>/dev/null | grep -oE '[0-9]+ modules' | head -1 || true)
  count=$(echo "$line" | grep -oE '[0-9]+' | head -1 || echo "0")
  count="${count:-0}"

  if [ "$count" -gt "$best_count" ]; then
    best_count="$count"
    best_entry="$f"
  fi
done

if [ -z "$best_entry" ]; then
  echo "No valid entry points found" >&2
  exit 1
fi

echo "$best_count $best_entry"
