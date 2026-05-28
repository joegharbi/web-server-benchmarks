# Web Server Benchmarks

A benchmark framework for measuring HTTP and WebSocket server performance and energy in Docker containers.

This repository benchmarks multiple stacks, including:

- Static HTTP servers (Apache, Nginx, Cowboy, Yaws, Erlang variants)
- Dynamic HTTP servers (Apache, Nginx+Python, Cowboy, Yaws, Erlang variants)
- WebSocket servers (Apache, Nginx+Java, Nginx+Python websockets, Nginx+Tornado, Cowboy, Yaws)

## Overview

- Auto-discovers benchmarks from `benchmarks/static`, `benchmarks/dynamic`, `benchmarks/websocket`
- Builds and health-checks containers before running load
- Captures runtime metrics and energy (via Scaphandre when available)
- Stores CSV results by run timestamp
- Includes a GUI graph generator for analysis/export

Typical flow:

```bash
make setup
make build
make check-health
make run-quick
make graph
```

## Prerequisites

- Linux (Debian/Ubuntu recommended)
- Python 3 + venv support (`python3-venv`)
- Docker
- Make
- Optional: Scaphandre for energy metrics

Verify:

```bash
make check-tools
```

## Directory Structure

```text
benchmarks/
  static/      # static HTTP benchmark containers
  dynamic/     # dynamic HTTP benchmark containers
  websocket/   # websocket benchmark containers
scripts/
  install_benchmarks.sh
  run_benchmarks.sh
  check_health.sh
  test_containers.sh
tools/
  measure_docker.py
  measure_websocket.py
  gui_graph_generator.py
results/
logs/
docs/
```

## Main Commands

- `make setup` - create/repair venv and install dependencies
- `make build` - build all benchmark images
- `make check-health` - validate built images are runnable/healthy
- `make test` - build + health check all images
- `make run` - run full benchmark suite
- `make run-quick` - quick run with reduced parameters
- `make run-super-quick` - smoke run
- `make run-single SERVER=<image>` - run only one image
- `make graph` - launch graph GUI

You can also run specific suites with auto pattern targets:

- `make run-static`
- `make run-dynamic`
- `make run-websocket`

## Configuration

Common environment variables:

- `BENCH_DIR` (default `benchmarks`) - benchmark root
- `HOST_PORT` (default `8001`) - host port used for tests
- `HTTP_MAX_WORKERS` - HTTP worker count override
- `BENCH_MEASURE_QUIET` - compact vs verbose measure logs
- `MEASURE_HEARTBEAT_SEC` - quiet-mode heartbeat interval

Examples:

```bash
make run BENCH_DIR=benchmarks
HOST_PORT=9001 make check-health
HTTP_MAX_WORKERS=70 make run-static
```

## Results

Each run writes timestamped outputs under `results/<timestamp>/` with CSV files grouped by benchmark type.

See:

- `docs/RESULTS.md`
- `docs/CONFIGURATION_AUDIT.md`
- `docs/BENCHMARKS_AUDIT.md`

## Notes

- Benchmark container names currently follow legacy prefixes (`st-`, `dy-`, `ws-`).
- The framework is generic and can support additional benchmark types if new runners/tools are added.
