# Minimal Bases And Unification

This repository now uses a unified top-level structure (`benchmarks/`, `scripts/`, `tools/`, `docs/`).

## Practical unification goals

- Keep benchmark images self-contained and reproducible
- Use minimal runtime images where possible
- Keep startup behavior explicit and failure-aware
- Keep exposed ports and entrypoints clear for auto-discovery

## Container authoring checklist

- `Dockerfile` exists in benchmark directory
- `EXPOSE <port>` present
- Service starts reliably in container
- Container exits when service exits unexpectedly
- Optional startup script uses robust process handling (avoid unconditional `sleep infinity`)

## Repository-level unification

- Build via `scripts/install_benchmarks.sh`
- Run via `scripts/run_benchmarks.sh`
- Measure via `tools/measure_*.py`
- Graph via `tools/gui_graph_generator.py`
