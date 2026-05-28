# Results Format

## Output layout

Each run creates:

```text
results/<timestamp>/
  static/
  dynamic/
  websocket/
```

Plus run logs in `logs/`.

## CSV grouping

- HTTP benchmarks: one CSV per container image with multiple rows by request-count level
- WebSocket benchmarks: CSVs per test family (burst/stream/concurrency/payload), depending on run mode

## Typical HTTP columns

- Container Name
- Type
- Num CPUs
- Total/Successful/Failed Requests
- Execution Time, Requests/s
- Total Energy, Avg Power, Samples
- CPU and Memory metrics

## Typical WebSocket columns

- Container Name
- Test Type
- Total/Successful/Failed Messages
- Messages/s, Throughput
- Latency stats
- Energy, CPU, Memory metrics
- Pattern parameters (clients, size, rate/bursts/duration)

## Analysis

Use:

```bash
make graph
```

The GUI supports selecting CSV files/folders and plotting selected metrics.
