#!/usr/bin/env bash
set -euo pipefail

# Run all benchmarks. Pass two binaries for A/B comparison.
# Usage: ./scripts/bench-all.sh [binary-A] [binary-B]

DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Building release binary..."
cargo build --release

"$DIR/bench-cold.sh" "$@"
"$DIR/bench-warm.sh" "$@"
"$DIR/bench-edit.sh" "$@"
