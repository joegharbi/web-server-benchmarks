# Unified Benchmarks Design

## Design goals

- One benchmark root (`benchmarks/`) for all test types
- Auto-discovery by directory + `Dockerfile`
- Shared script/tool pipeline for build, health, run, and graph
- Stable, reproducible outputs for cross-run comparison

## Current type model

- `static`: HTTP static-serving benchmarks
- `dynamic`: HTTP dynamic/application benchmarks
- `websocket`: WebSocket interaction benchmarks

## Execution flow

1. Discover benchmark image directories
2. Build images
3. Health-check built images
4. Run benchmark loops by type and mode
5. Persist CSV + logs + optional graphs

## Why this layout

- Easier to add/remove benchmarks without editing hardcoded lists
- Easier to run one type or one server only
- Consistent results and tooling across benchmark families
