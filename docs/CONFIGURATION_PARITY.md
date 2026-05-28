# Configuration Parity

This repo compares heterogeneous servers. To keep comparisons fair, aim for parity in:

## Network/endpoint behavior

- Same host-side port mapping strategy
- Stable primary endpoint for HTTP (`/`) and websocket (`/ws`)

## Runtime constraints

- Same ulimit policy (`nofile=100000`)
- Similar startup timeout assumptions before health/measurement

## Workload shapes

- Same request-count sets for HTTP comparisons
- Same websocket parameter sets (clients, size, rate/burst, duration/interval) for websocket comparisons

## Measurement pipeline

- Same metric collector scripts (`tools/measure_docker.py`, `tools/measure_websocket.py`)
- Same energy capture source (Scaphandre) when enabled

## Known non-parity areas

- Different server implementations and defaults by stack (expected)
- Language/runtime overhead differences (part of what is measured)
