"""
Filter model selector for Scaphandre power data.

Loads raw Scaphandre JSON output files, applies all filter models with
multiple parameter variants, scores each by how much it reduces
measurement noise, and recommends the best model for this machine's data.

Two scoring methods
-------------------
cv (default)
    Requires multiple JSON files from the same container (≥3 runs).
    For each model, apply it to each run's samples, compute the mean
    energy per run, then measure the coefficient of variation (CV = std/mean)
    across runs. Lower CV = more consistent = better filter.

injection
    Works with any number of JSON files (even 1).
    Injects synthetic spikes (5× and 10× baseline) into each run's samples,
    applies each filter, and measures how close the recovered mean is to the
    pre-injection mean. Lower recovery error = better filter.

Usage
-----
    # After collecting raw data (BENCH_RUNS=10 or more):
    python tools/model_selector.py --input "output/*.json" --container st-cowboy-27-self

    # Use injection scoring (works with fewer files):
    python tools/model_selector.py --input "output/*.json" --metric injection

    # Apply the winner to bench.config automatically:
    python tools/model_selector.py --input "output/*.json" --apply

    # Try specific parameter variants:
    python tools/model_selector.py --input "output/*.json" \\
        --contamination 0.05 0.1 0.2 \\
        --iqr-factor 1.5 2.0
"""

import argparse
import configparser
import glob
import json
import os
import statistics
import sys
from dataclasses import dataclass, field
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from tools.stats import (
    apply_filter,
    coefficient_of_variation,
    FILTER_MODELS,
)
from tools.bench_profile import load_config, DEFAULT_CONFIG_PATH

# ANSI colours
_G  = "\033[0;32m"
_Y  = "\033[1;33m"
_R  = "\033[0;31m"
_B  = "\033[1;34m"
_NC = "\033[0m"
_BOLD = "\033[1m"


# ---------------------------------------------------------------------------
# JSON data extraction
# ---------------------------------------------------------------------------

def extract_samples(json_path: str, container_name: Optional[str] = None
                    ) -> dict[str, list[float]]:
    """
    Load a Scaphandre JSON file and extract power samples per container.

    Returns
    -------
    dict mapping container_name → list of power values (microwatts, >0 only)
    If container_name is given, only that container's samples are returned.
    """
    try:
        with open(json_path) as f:
            data = json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        print(f"{_Y}[WARN]{_NC} Could not load {json_path}: {e}")
        return {}

    samples: dict[str, list[float]] = {}

    for entry in data:
        for consumer in entry.get("consumers", []):
            power = consumer.get("consumption", 0.0)
            if power <= 0:
                continue
            container = consumer.get("container")
            name = container.get("name", "") if container else ""

            if not name:
                continue
            if container_name and name != container_name:
                continue

            samples.setdefault(name, []).append(float(power))

    return samples


def extract_samples_by_process(json_path: str, exe_filter: Optional[str] = None
                               ) -> dict[str, list[float]]:
    """
    Like extract_samples but groups by exe basename (process name) instead of
    container name. Used as a fallback when Scaphandre was not run with
    --containers and container metadata is absent.

    Returns
    -------
    dict mapping exe_basename → list of power values (microwatts, >0 only)
    """
    try:
        with open(json_path) as f:
            data = json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        print(f"{_Y}[WARN]{_NC} Could not load {json_path}: {e}")
        return {}

    samples: dict[str, list[float]] = {}

    for entry in data:
        for consumer in entry.get("consumers", []):
            power = consumer.get("consumption", 0.0)
            if power <= 0:
                continue
            exe = consumer.get("exe", "") or consumer.get("cmdline", "") or ""
            name = os.path.basename(exe.split()[0]) if exe else ""
            if not name:
                continue
            if exe_filter and name != exe_filter:
                continue
            samples.setdefault(name, []).append(float(power))

    return samples


def discover_processes(json_paths: list[str]) -> set[str]:
    """Return all process exe basenames found across a list of JSON files."""
    names: set[str] = set()
    for path in json_paths:
        names.update(extract_samples_by_process(path).keys())
    return names


def group_by_process(json_paths: list[str],
                     exe_filter: Optional[str] = None
                     ) -> dict[str, list[list[float]]]:
    """
    Group power samples by process exe basename across multiple JSON files.
    Fallback when no container metadata is present.
    """
    grouped: dict[str, list[list[float]]] = {}
    for path in json_paths:
        per_proc = extract_samples_by_process(path, exe_filter)
        for name, samples in per_proc.items():
            grouped.setdefault(name, []).append(samples)
    return grouped


def discover_containers(json_paths: list[str]) -> set[str]:
    """Return all container names found across a list of JSON files."""
    names: set[str] = set()
    for path in json_paths:
        names.update(extract_samples(path).keys())
    return names


def group_by_container(json_paths: list[str],
                       container_name: Optional[str] = None
                       ) -> dict[str, list[list[float]]]:
    """
    Group power samples by container across multiple JSON files.

    Returns
    -------
    dict: container_name → list of per-file sample lists
    Each inner list is the raw samples from one JSON (one benchmark run).
    """
    grouped: dict[str, list[list[float]]] = {}
    for path in json_paths:
        per_container = extract_samples(path, container_name)
        for name, samples in per_container.items():
            grouped.setdefault(name, []).append(samples)
    return grouped


# ---------------------------------------------------------------------------
# Model variant definitions
# ---------------------------------------------------------------------------

@dataclass
class ModelVariant:
    name: str         # display name, e.g. "isolation_forest(0.10)"
    model: str        # model key for apply_filter()
    kwargs: dict      # kwargs forwarded to apply_filter()


def build_variants(
    contamination_values: list[float],
    iqr_factors: list[float],
    hampel_windows: list[int],
    hampel_thresholds: list[float],
    dbscan_eps_values: list[float],
    dbscan_minpts_values: list[int],
    lof_neighbors_values: list[int],
) -> list[ModelVariant]:
    """Build the full set of model variants to compare."""
    variants: list[ModelVariant] = []

    # Baseline
    variants.append(ModelVariant("none", "none", {}))

    # IQR
    for f in iqr_factors:
        variants.append(ModelVariant(f"iqr(f={f})", "iqr", {"factor": f}))

    # Hampel
    for w in hampel_windows:
        for t in hampel_thresholds:
            variants.append(ModelVariant(
                f"hampel(w={w},t={t})", "hampel",
                {"window": w, "threshold": t}
            ))

    # ML models — all share contamination
    for c in contamination_values:
        variants.append(ModelVariant(
            f"isolation_forest({c:.2f})", "isolation_forest",
            {"contamination": c}
        ))
        variants.append(ModelVariant(
            f"elliptic({c:.2f})", "elliptic",
            {"contamination": c}
        ))
        for n in lof_neighbors_values:
            variants.append(ModelVariant(
                f"lof({c:.2f},n={n})", "lof",
                {"contamination": c, "n_neighbors": n}
            ))

    # DBSCAN
    for eps in dbscan_eps_values:
        for mp in dbscan_minpts_values:
            variants.append(ModelVariant(
                f"dbscan(eps={eps},mp={mp})", "dbscan",
                {"eps": eps, "min_samples": mp}
            ))

    return variants


# ---------------------------------------------------------------------------
# Scoring: CV method
# ---------------------------------------------------------------------------

@dataclass
class Score:
    variant: ModelVariant
    cv: float               # coefficient of variation across runs
    mean_w: float           # mean power across runs (watts)
    std_w: float            # std of per-run means
    samples_kept_pct: float # avg % of samples kept after filtering
    runs_used: int          # number of runs with enough data


def score_cv(
    variant: ModelVariant,
    run_samples: list[list[float]],  # list of per-run sample lists
) -> Optional[Score]:
    """
    Apply variant to each run's samples, compute per-run mean power,
    then score by CV across runs. Returns None if too few runs have data.
    """
    per_run_means = []
    kept_pcts = []

    for samples in run_samples:
        if not samples:
            continue
        try:
            clean = apply_filter(samples, model=variant.model, **variant.kwargs)
        except Exception:
            clean = samples  # fallback: no filter

        if not clean:
            continue

        mean_uw = statistics.mean(clean)
        per_run_means.append(mean_uw / 1e6)  # µW → W
        kept_pcts.append(len(clean) / len(samples) * 100)

    if len(per_run_means) < 2:
        return None

    cv = coefficient_of_variation(per_run_means)
    mean_w = statistics.mean(per_run_means)
    std_w = statistics.stdev(per_run_means)
    kept_pct = statistics.mean(kept_pcts)

    return Score(
        variant=variant,
        cv=cv,
        mean_w=mean_w,
        std_w=std_w,
        samples_kept_pct=kept_pct,
        runs_used=len(per_run_means),
    )


# ---------------------------------------------------------------------------
# Scoring: injection method
# ---------------------------------------------------------------------------

@dataclass
class InjectionScore:
    variant: ModelVariant
    recovery_error_pct: float   # abs(recovered_mean - true_mean) / true_mean * 100
    true_mean_w: float
    recovered_mean_w: float
    runs_used: int


def _inject_spikes(samples: list[float], spike_pct: float = 0.15) -> list[float]:
    """Inject spikes at ~spike_pct positions. Spike value = 10× local median."""
    if not samples:
        return samples
    med = statistics.median(samples)
    n_spikes = max(1, int(len(samples) * spike_pct))
    # Place spikes evenly
    result = list(samples)
    step = max(1, len(result) // n_spikes)
    for i in range(0, len(result), step)[:n_spikes]:
        result[i] = med * 10.0
    return result


def score_injection(
    variant: ModelVariant,
    run_samples: list[list[float]],
) -> Optional[InjectionScore]:
    """
    For each run, record true mean, inject spikes, apply filter, measure recovery.
    """
    errors = []
    true_means = []
    recovered_means = []

    for samples in run_samples:
        if len(samples) < 5:
            continue
        true_mean = statistics.mean(samples)
        spiked = _inject_spikes(samples)
        try:
            clean = apply_filter(spiked, model=variant.model, **variant.kwargs)
        except Exception:
            clean = spiked
        if not clean:
            continue
        recovered = statistics.mean(clean)
        error_pct = abs(recovered - true_mean) / true_mean * 100
        errors.append(error_pct)
        true_means.append(true_mean / 1e6)
        recovered_means.append(recovered / 1e6)

    if not errors:
        return None

    return InjectionScore(
        variant=variant,
        recovery_error_pct=statistics.mean(errors),
        true_mean_w=statistics.mean(true_means),
        recovered_mean_w=statistics.mean(recovered_means),
        runs_used=len(errors),
    )


# ---------------------------------------------------------------------------
# Report printing
# ---------------------------------------------------------------------------

def _bar(cv: float, max_cv: float, width: int = 20) -> str:
    if max_cv == 0:
        return " " * width
    frac = min(cv / max_cv, 1.0)
    filled = int(frac * width)
    return "█" * filled + "░" * (width - filled)


def print_cv_report(scores: list[Score], winner: Score) -> None:
    max_cv = max((s.cv for s in scores), default=1.0)
    print(f"\n{_BOLD}{'Model':<35} {'CV':>7}  {'Mean(W)':>9}  {'Std(W)':>8}  "
          f"{'Kept%':>6}  {'Runs':>4}  {'Noise chart'}{_NC}")
    print("─" * 100)
    for s in scores:
        is_winner = s.variant.name == winner.variant.name
        bar = _bar(s.cv, max_cv)
        marker = f" {_G}← best{_NC}" if is_winner else ""
        colour = _G if is_winner else (_Y if s.cv < 0.2 else "")
        print(
            f"{colour}{s.variant.name:<35}{_NC} "
            f"{s.cv:>7.4f}  "
            f"{s.mean_w:>9.4f}  "
            f"{s.std_w:>8.4f}  "
            f"{s.samples_kept_pct:>6.1f}  "
            f"{s.runs_used:>4}  "
            f"{bar}{marker}"
        )
    print()


def print_injection_report(scores: list[InjectionScore],
                           winner: InjectionScore) -> None:
    max_err = max((s.recovery_error_pct for s in scores), default=100.0)
    print(f"\n{_BOLD}{'Model':<35} {'Err%':>7}  {'True(W)':>9}  "
          f"{'Recovered(W)':>13}  {'Runs':>4}  {'Error chart'}{_NC}")
    print("─" * 100)
    for s in scores:
        is_winner = s.variant.name == winner.variant.name
        bar = _bar(s.recovery_error_pct, max_err)
        marker = f" {_G}← best{_NC}" if is_winner else ""
        colour = _G if is_winner else (_Y if s.recovery_error_pct < 5.0 else "")
        print(
            f"{colour}{s.variant.name:<35}{_NC} "
            f"{s.recovery_error_pct:>7.2f}  "
            f"{s.true_mean_w:>9.4f}  "
            f"{s.recovered_mean_w:>13.4f}  "
            f"{s.runs_used:>4}  "
            f"{bar}{marker}"
        )
    print()


# ---------------------------------------------------------------------------
# bench.config updater
# ---------------------------------------------------------------------------

def apply_to_config(variant: ModelVariant,
                    config_path: str = DEFAULT_CONFIG_PATH) -> None:
    """Write the winning model and its parameters to bench.config."""
    cfg = load_config(config_path)
    if not cfg.has_section("filter"):
        cfg.add_section("filter")

    cfg.set("filter", "model", variant.model)

    kw = variant.kwargs
    if "contamination" in kw:
        cfg.set("filter", "contamination", str(kw["contamination"]))
    if "factor" in kw:
        cfg.set("filter", "iqr_factor", str(kw["factor"]))
    if "window" in kw:
        cfg.set("filter", "hampel_window", str(kw["window"]))
    if "threshold" in kw:
        cfg.set("filter", "hampel_threshold", str(kw["threshold"]))
    if "eps" in kw:
        cfg.set("filter", "dbscan_eps", str(kw["eps"]))
    if "min_samples" in kw:
        cfg.set("filter", "dbscan_minpts", str(kw["min_samples"]))
    if "n_neighbors" in kw:
        cfg.set("filter", "lof_neighbors", str(kw["n_neighbors"]))

    with open(config_path, "w") as f:
        cfg.write(f)

    print(f"{_G}bench.config updated:{_NC} [filter] model = {variant.model} "
          f"with params {variant.kwargs}")


# ---------------------------------------------------------------------------
# Main comparison runner
# ---------------------------------------------------------------------------

def run_comparison(
    json_paths: list[str],
    container_name: Optional[str],
    metric: str,
    variants: list[ModelVariant],
    verbose: bool = True,
    process_mode: bool = False,
    process_name: Optional[str] = None,
) -> tuple[Optional[ModelVariant], list]:
    """
    Run model comparison and return (winning_variant, all_scores).
    Returns (None, []) if not enough data.

    process_mode=True  — group by exe basename instead of container name.
    Used as a fallback when Scaphandre was run without --containers.
    """
    if not json_paths:
        print(f"{_R}[ERROR]{_NC} No JSON files found.")
        return None, []

    if process_mode:
        grouped = group_by_process(json_paths, process_name)
        mode_label = "process"
    else:
        grouped = group_by_container(json_paths, container_name)
        mode_label = "container"

    if not grouped:
        if process_mode:
            print(f"{_R}[ERROR]{_NC} No process power samples found in the provided JSON files.")
        else:
            print(f"{_R}[ERROR]{_NC} No container power samples found in the provided JSON files.")
            # Auto-detect whether process-level data exists and suggest fallback
            proc_names = discover_processes(json_paths)
            if proc_names:
                print(f"{_Y}[HINT]{_NC} Scaphandre was not run with --containers flag.")
                print(f"  No Docker container metadata is present in these JSON files.")
                print(f"  Use --process to analyse process-level data instead.")
                print(f"  Available processes: {', '.join(sorted(proc_names)[:10])}"
                      + (" …" if len(proc_names) > 10 else ""))
            if container_name:
                print(f"  Tried container: '{container_name}'")
                all_names = discover_containers(json_paths)
                if all_names:
                    print(f"  Available containers: {', '.join(sorted(all_names))}")
        return None, []

    # If multiple found and none specified, use the one with most data
    if len(grouped) > 1 and not (container_name or process_name):
        chosen = max(grouped, key=lambda k: len(grouped[k]))
        label = "process" if process_mode else "container"
        print(f"{_Y}[INFO]{_NC} Multiple {label}s found. "
              f"Using '{chosen}' ({len(grouped[chosen])} run(s)). "
              f"Use --{'process' if process_mode else 'container'} to specify.")
        if process_mode:
            process_name = chosen
        else:
            container_name = chosen

    target = (process_name if process_mode else container_name) or list(grouped.keys())[0]
    run_samples = grouped.get(target, [])

    if verbose:
        label = "Process" if process_mode else "Container"
        print(f"\n{_B}{label}:{_NC} {target}")
        print(f"{_B}Mode:{_NC} {'process (no Docker container metadata)' if process_mode else 'container'}")
        print(f"{_B}JSON files:{_NC} {len(json_paths)}")
        print(f"{_B}Runs with data:{_NC} {len(run_samples)}")
        print(f"{_B}Scoring method:{_NC} {metric}")

    if metric == "cv":
        if len(run_samples) < 3:
            print(f"\n{_Y}[WARN]{_NC} CV scoring needs ≥3 runs with data "
                  f"(found {len(run_samples)}). "
                  f"Switching to injection scoring.")
            metric = "injection"

    all_scores = []

    if metric == "cv":
        if verbose:
            print(f"\nScoring {len(variants)} model variants...")
        for v in variants:
            s = score_cv(v, run_samples)
            if s is not None:
                all_scores.append(s)
        if not all_scores:
            print(f"{_R}[ERROR]{_NC} No models produced valid scores.")
            return None, []
        all_scores.sort(key=lambda s: s.cv)
        winner = all_scores[0]
        if verbose:
            print_cv_report(all_scores, winner)
            print(f"{_G}Winner:{_NC} {winner.variant.name}  "
                  f"CV={winner.cv:.4f}  Mean={winner.mean_w:.4f}W  "
                  f"Kept={winner.samples_kept_pct:.1f}%")

    else:  # injection
        if verbose:
            print(f"\nScoring {len(variants)} model variants (injection method)...")
        for v in variants:
            s = score_injection(v, run_samples)
            if s is not None:
                all_scores.append(s)
        if not all_scores:
            print(f"{_R}[ERROR]{_NC} No models produced valid scores.")
            return None, []
        all_scores.sort(key=lambda s: s.recovery_error_pct)
        winner = all_scores[0]
        if verbose:
            print_injection_report(all_scores, winner)
            print(f"{_G}Winner:{_NC} {winner.variant.name}  "
                  f"Error={winner.recovery_error_pct:.2f}%  "
                  f"True={winner.true_mean_w:.4f}W  "
                  f"Recovered={winner.recovered_mean_w:.4f}W")

    return winner.variant, all_scores


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Compare outlier filter models on Scaphandre JSON data",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--input", required=True, metavar="GLOB",
        help='Glob pattern for Scaphandre JSON files, e.g. "output/*.json"'
    )
    parser.add_argument(
        "--container", default=None, metavar="NAME",
        help="Container name to analyse (auto-detected if omitted)"
    )
    parser.add_argument(
        "--metric", choices=["cv", "injection"], default="cv",
        help="Scoring method: cv (cross-run variance) or injection (spike recovery)"
    )
    parser.add_argument(
        "--apply", action="store_true",
        help="Write the winning model to bench.config"
    )
    parser.add_argument(
        "--config", default=DEFAULT_CONFIG_PATH, metavar="PATH",
        help="Path to bench.config (default: bench.config)"
    )

    # Parameter sweep options
    parser.add_argument(
        "--contamination", type=float, nargs="+",
        default=[0.05, 0.1, 0.15, 0.2],
        metavar="F",
        help="Contamination values to try for IF/LOF/Elliptic (default: 0.05 0.1 0.15 0.2)"
    )
    parser.add_argument(
        "--iqr-factor", type=float, nargs="+",
        default=[1.5, 2.0, 2.5],
        metavar="F",
        help="IQR factor values to try (default: 1.5 2.0 2.5)"
    )
    parser.add_argument(
        "--hampel-window", type=int, nargs="+",
        default=[5, 7, 11],
        metavar="W",
        help="Hampel window sizes to try (default: 5 7 11)"
    )
    parser.add_argument(
        "--hampel-threshold", type=float, nargs="+",
        default=[1.5, 2.0, 3.0],
        metavar="T",
        help="Hampel threshold values to try (default: 1.5 2.0 3.0)"
    )
    parser.add_argument(
        "--dbscan-eps", type=float, nargs="+",
        default=[100.0, 200.0, 500.0],
        metavar="E",
        help="DBSCAN eps values to try in µW (default: 100 200 500)"
    )
    parser.add_argument(
        "--dbscan-minpts", type=int, nargs="+",
        default=[3, 5, 10],
        metavar="M",
        help="DBSCAN min_samples values to try (default: 3 5 10)"
    )
    parser.add_argument(
        "--lof-neighbors", type=int, nargs="+",
        default=[5, 10, 20],
        metavar="N",
        help="LOF n_neighbors values to try (default: 5 10 20)"
    )
    parser.add_argument(
        "--list-containers", action="store_true",
        help="List all container names found in the JSON files and exit"
    )
    parser.add_argument(
        "--process", default=None, metavar="NAME",
        help="Use process-level mode: group by exe basename instead of container name. "
             "Specify a process name or omit to auto-select. "
             "Use this when Scaphandre was not run with --containers."
    )
    parser.add_argument(
        "--process-mode", action="store_true",
        help="Enable process-level mode without specifying a process name (auto-selects)"
    )
    parser.add_argument(
        "--list-processes", action="store_true",
        help="List all process exe names found in the JSON files and exit"
    )
    parser.add_argument(
        "--json-output", default=None, metavar="FILE",
        help="Write results as JSON to FILE (used by the GUI to display results in-page)"
    )

    args = parser.parse_args()

    # Expand glob
    json_paths = sorted(glob.glob(args.input))
    if not json_paths:
        print(f"{_R}[ERROR]{_NC} No files matched: {args.input}")
        sys.exit(1)

    if args.list_containers:
        names = discover_containers(json_paths)
        print("Containers found:")
        for n in sorted(names):
            print(f"  {n}")
        return

    if args.list_processes:
        names = discover_processes(json_paths)
        print("Processes found:")
        for n in sorted(names):
            print(f"  {n}")
        return

    process_mode = bool(args.process or args.process_mode)

    variants = build_variants(
        contamination_values=args.contamination,
        iqr_factors=args.iqr_factor,
        hampel_windows=args.hampel_window,
        hampel_thresholds=args.hampel_threshold,
        dbscan_eps_values=args.dbscan_eps,
        dbscan_minpts_values=args.dbscan_minpts,
        lof_neighbors_values=args.lof_neighbors,
    )

    winner_variant, all_scores = run_comparison(
        json_paths=json_paths,
        container_name=args.container,
        metric=args.metric,
        variants=variants,
        verbose=True,
        process_mode=process_mode,
        process_name=args.process,
    )

    if winner_variant is None:
        sys.exit(1)

    if args.apply:
        apply_to_config(winner_variant, args.config)
    else:
        print(f"\nTo apply to bench.config, re-run with {_B}--apply{_NC}")

    if args.json_output:
        _write_json_results(all_scores, winner_variant, args.metric, args.json_output)


def _write_json_results(all_scores, winner_variant, metric: str, output_path: str) -> None:
    """Write comparison results as JSON so the GUI can display them in-page."""
    rows = []
    for s in all_scores:
        if metric == "cv":
            rows.append({
                "model":       s.variant.name,
                "score":       round(s.cv, 6),
                "score_label": "CV",
                "mean_w":      round(s.mean_w, 6),
                "kept_pct":    round(s.samples_kept_pct, 1),
                "runs":        s.runs_used,
                "winner":      s.variant.name == winner_variant.name,
            })
        else:
            rows.append({
                "model":       s.variant.name,
                "score":       round(s.recovery_error_pct, 4),
                "score_label": "Err%",
                "mean_w":      round(s.true_mean_w, 6),
                "kept_pct":    None,
                "runs":        s.runs_used,
                "winner":      s.variant.name == winner_variant.name,
            })
    payload = {
        "metric": metric,
        "winner": winner_variant.name,
        "rows":   rows,
    }
    try:
        with open(output_path, "w") as f:
            json.dump(payload, f)
        print(f"\n[JSON] Results written to {output_path}")
    except OSError as e:
        print(f"[WARN] Could not write JSON results: {e}")


if __name__ == "__main__":
    main()
