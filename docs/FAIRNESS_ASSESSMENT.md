# Fairness Assessment

This benchmark suite compares servers across different stacks. Fairness depends on workload and environment consistency.

## What is controlled

- Common host environment and Docker runtime
- Common orchestration scripts
- Consistent result schema and collection path
- Shared health-check gate before long runs

## What varies by design

- Language/runtime implementation details
- Server defaults and ecosystem libraries
- Internal eventing/concurrency models

## Recommendations

- Use the same machine and idle baseline for compared runs
- Run multiple iterations and compare medians, not single points
- Validate containers with `make test` before full runs
- Keep benchmark parameters identical when comparing specific servers
