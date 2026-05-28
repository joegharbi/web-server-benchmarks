# Changelog

## Current

- Migrated repo layout to unified benchmark structure:
  - `benchmarks/`
  - `scripts/`
  - `tools/`
  - `docs/`
- Updated Makefile to use script/tool paths and pattern-based run targets.
- Fixed dynamic nginx startup supervision to avoid false-positive healthy containers when services crash.
- Fixed Erlang Dockerfile response binary literal generation (`<<...>>` syntax).
- Fixed graph GUI crashes around stale title/legend artists during rapid redraw.

## Notes

This changelog tracks repository-level framework changes, not benchmark result changes.
