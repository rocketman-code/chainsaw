# Chainsaw development tasks

set dotenv-load := false

# List available recipes
default:
    @just --list

# Run all benchmark scenarios on available targets
bench *args:
    cargo xtask bench {{args}}

# Run full benchmark matrix across all configured targets
bench-matrix *args:
    cargo xtask bench --matrix {{args}}

# Find the entry point with the largest module graph
find-entry root *args:
    cargo xtask find-entry {{root}} {{args}}
