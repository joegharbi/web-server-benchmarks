# Configuration Audit

This document summarizes important runtime and benchmark configuration knobs.

## Core paths

- Benchmark root: `BENCH_DIR` (default: `benchmarks`)
- Scripts: `scripts/`
- Tools: `tools/`

## Ports

- Default host port: `HOST_PORT=8001`
- Container port inferred from Dockerfile `EXPOSE` (fallback `80`)

## Run controls

- `HTTP_MAX_WORKERS` - max worker threads for HTTP benchmark client
- `BENCH_MEASURE_QUIET` - compact/verbose measure output
- `MEASURE_HEARTBEAT_SEC` - heartbeat interval in quiet mode

## Health controls

- `MEASURE_STARTUP_WAIT` - additional startup wait in measure scripts
- `MEASURE_HEALTH_RETRIES` - retries for health readiness
- `MEASURE_HEALTH_DELAY` - delay between health retries

## Resource limits

- Scripts enforce `ulimit -n 100000`
- Containers are started with `--ulimit nofile=100000:100000` for high concurrency tests

## Make targets

- Setup: `make setup`
- Build: `make build`
- Health: `make check-health`
- Full test gate: `make test`
- Runs: `make run`, `make run-quick`, `make run-super-quick`, `make run-<type>`
- Graphing: `make graph`

## Cleanup

- `make clean-results`
- `make clean-build`
- `make clean-env`
- `make clean-all`
- `make clean-benchmarks CONFIRM=1`
- `make clean-nuclear CONFIRM=1`
