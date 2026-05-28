# Extending The Framework

## Add a new benchmark container

1. Create a directory under one of:
   - `benchmarks/static/...`
   - `benchmarks/dynamic/...`
   - `benchmarks/websocket/...`
2. Add a `Dockerfile`.
3. Ensure the container listens on an exposed port (`EXPOSE`).
4. Ensure service startup is foreground/supervised (container should exit on service failure).

Then run:

```bash
make build
make check-health
make run-super-quick
```

## Add a new benchmark type

1. Create `benchmarks/<new-type>/...` directories with Dockerfiles.
2. Add/extend measurement logic in scripts/tools for the new type.
3. Ensure `scripts/run_benchmarks.sh` knows how to run it.
4. Add docs and graph handling if metric schema differs.

## Naming guidance

Existing images use legacy names (`st-*`, `dy-*`, `ws-*`). Keep names stable because they are used in result filenames and graph legends.
