"""
Statistical filter models for Scaphandre power sample cleaning.

All functions are pure — no side effects, no I/O, no dependencies on Docker
or Scaphandre. Each filter takes a list of float power samples and returns a
cleaned list. summarise_runs() aggregates N repeated run results into final stats.

Filter models
-------------
none              : passthrough, no filtering
iqr               : Interquartile Range — simple, no distribution assumption
hampel            : Hampel filter — robust time-series, median + MAD based
isolation_forest  : Isolation Forest — tree-based, good for varied noise shapes
lof               : Local Outlier Factor — density-based, good for local clusters
elliptic          : Elliptic Envelope — assumes Gaussian, fast
dbscan            : DBSCAN clustering — no prior contamination estimate needed

Usage
-----
from tools.stats import apply_filter, summarise_runs

clean = apply_filter(samples, model="isolation_forest", contamination=0.2)
stats = summarise_runs(run_energies, model="iqr", factor=1.5, confidence=0.95)
"""

import math
import statistics
from typing import Optional

# ---------------------------------------------------------------------------
# Univariate filters
# ---------------------------------------------------------------------------

def filter_none(samples: list[float]) -> list[float]:
    return [s for s in samples if s > 0]


def filter_iqr(samples: list[float], factor: float = 1.5) -> list[float]:
    """Remove samples outside Q1 - factor*IQR and Q3 + factor*IQR."""
    clean = [s for s in samples if s > 0]
    if len(clean) < 4:
        return clean
    clean_sorted = sorted(clean)
    n = len(clean_sorted)
    q1 = clean_sorted[n // 4]
    q3 = clean_sorted[(3 * n) // 4]
    iqr = q3 - q1
    if iqr == 0:
        return clean
    lo = q1 - factor * iqr
    hi = q3 + factor * iqr
    return [s for s in clean if lo <= s <= hi]


def filter_hampel(
    samples: list[float],
    window: int = 7,
    threshold: float = 1.5,
    k: float = 1.4826,
) -> list[float]:
    """
    Hampel filter: for each sample, compute median and MAD over a local window.
    Samples deviating more than threshold * k * MAD from the window median are
    removed. Robust for time-series power data with local structure.

    Parameters
    ----------
    window    : window size (must be odd; w = 2*half + 1)
    threshold : number of scaled MADs beyond which a point is an outlier
    k         : scale factor for consistency with normal distribution (1.4826)
    """
    clean = [s for s in samples if s > 0]
    if len(clean) < window:
        return clean
    half = window // 2
    result = []
    for i, val in enumerate(clean):
        lo = max(0, i - half)
        hi = min(len(clean), i + half + 1)
        win = clean[lo:hi]
        med = statistics.median(win)
        mad = statistics.median([abs(x - med) for x in win])
        scaled_mad = k * mad
        if scaled_mad == 0 or abs(val - med) <= threshold * scaled_mad:
            result.append(val)
    return result


# ---------------------------------------------------------------------------
# Multivariate / ML-based filters (require scikit-learn)
# ---------------------------------------------------------------------------

def _to_2d(samples: list[float]):
    """Reshape 1-D list to (n,1) array for sklearn."""
    import numpy as np
    return np.array(samples).reshape(-1, 1)


def filter_isolation_forest(
    samples: list[float],
    contamination: float = 0.1,
    random_state: int = 42,
) -> list[float]:
    """
    Isolation Forest: tree-based anomaly detection. Works well on varied noise
    shapes. contamination is the estimated fraction of outliers in the data.
    Does not assume a specific distribution.
    """
    from sklearn.ensemble import IsolationForest
    clean = [s for s in samples if s > 0]
    if len(clean) < 10:
        return clean
    X = _to_2d(clean)
    model = IsolationForest(contamination=contamination, random_state=random_state)
    labels = model.fit_predict(X)  # 1 = inlier, -1 = outlier
    return [s for s, label in zip(clean, labels) if label == 1]


def filter_lof(
    samples: list[float],
    contamination: float = 0.1,
    n_neighbors: int = 20,
) -> list[float]:
    """
    Local Outlier Factor: density-based. Identifies outliers by comparing local
    density to that of neighbours. Good for data with varying density regions.
    """
    from sklearn.neighbors import LocalOutlierFactor
    clean = [s for s in samples if s > 0]
    if len(clean) < n_neighbors + 1:
        return clean
    X = _to_2d(clean)
    model = LocalOutlierFactor(n_neighbors=n_neighbors, contamination=contamination)
    labels = model.fit_predict(X)
    return [s for s, label in zip(clean, labels) if label == 1]


def filter_elliptic(
    samples: list[float],
    contamination: float = 0.1,
) -> list[float]:
    """
    Elliptic Envelope: fits a Gaussian to the data and rejects points with high
    Mahalanobis distance. Works best when data is approximately normally distributed.
    """
    from sklearn.covariance import EllipticEnvelope
    clean = [s for s in samples if s > 0]
    if len(clean) < 10:
        return clean
    X = _to_2d(clean)
    model = EllipticEnvelope(contamination=contamination)
    try:
        labels = model.fit_predict(X)
    except Exception:
        return clean
    return [s for s, label in zip(clean, labels) if label == 1]


def filter_dbscan(
    samples: list[float],
    eps: float = 150.0,
    min_samples: int = 10,
) -> list[float]:
    """
    DBSCAN: density-based clustering. Points not assigned to any cluster (label=-1)
    are treated as outliers. Does not require a prior contamination estimate.
    eps and min_samples must be tuned to the data scale (power values in microwatts).
    """
    from sklearn.cluster import DBSCAN
    clean = [s for s in samples if s > 0]
    if len(clean) < min_samples:
        return clean
    X = _to_2d(clean)
    labels = DBSCAN(eps=eps, min_samples=min_samples).fit_predict(X)
    return [s for s, label in zip(clean, labels) if label != -1]


# ---------------------------------------------------------------------------
# Unified dispatch
# ---------------------------------------------------------------------------

FILTER_MODELS = (
    "none",
    "iqr",
    "hampel",
    "isolation_forest",
    "lof",
    "elliptic",
    "dbscan",
)


def apply_filter(samples: list[float], model: str = "none", **kwargs) -> list[float]:
    """
    Apply a named filter model to a list of power samples.

    Parameters
    ----------
    samples : raw power values (microwatts) from Scaphandre
    model   : one of FILTER_MODELS
    **kwargs: model-specific parameters (factor, contamination, window, etc.)

    Returns
    -------
    Cleaned list of samples with outliers removed.
    """
    if model == "none":
        return filter_none(samples)
    if model == "iqr":
        return filter_iqr(samples, factor=kwargs.get("factor", 1.5))
    if model == "hampel":
        return filter_hampel(
            samples,
            window=kwargs.get("window", 7),
            threshold=kwargs.get("threshold", 1.5),
            k=kwargs.get("k", 1.4826),
        )
    if model == "isolation_forest":
        return filter_isolation_forest(
            samples,
            contamination=kwargs.get("contamination", 0.1),
            random_state=kwargs.get("random_state", 42),
        )
    if model == "lof":
        return filter_lof(
            samples,
            contamination=kwargs.get("contamination", 0.1),
            n_neighbors=kwargs.get("n_neighbors", 20),
        )
    if model == "elliptic":
        return filter_elliptic(samples, contamination=kwargs.get("contamination", 0.1))
    if model == "dbscan":
        return filter_dbscan(
            samples,
            eps=kwargs.get("eps", 150.0),
            min_samples=kwargs.get("min_samples", 10),
        )
    raise ValueError(f"Unknown filter model '{model}'. Choose from: {FILTER_MODELS}")


# ---------------------------------------------------------------------------
# Cross-run outlier rejection
# ---------------------------------------------------------------------------

def reject_outlier_runs(
    run_values: list[float],
    factor: float = 1.5,
) -> tuple[list[float], list[int]]:
    """
    After N repeated runs, reject runs whose total energy is an outlier
    across the run set using IQR. Returns (kept_values, rejected_indices).

    A run is rejected if its energy falls outside Q1 - factor*IQR or
    Q3 + factor*IQR across all runs.
    """
    if len(run_values) < 4:
        return run_values, []
    sorted_vals = sorted(run_values)
    n = len(sorted_vals)
    q1 = sorted_vals[n // 4]
    q3 = sorted_vals[(3 * n) // 4]
    iqr = q3 - q1
    if iqr == 0:
        return run_values, []
    lo = q1 - factor * iqr
    hi = q3 + factor * iqr
    kept, rejected = [], []
    for i, v in enumerate(run_values):
        if lo <= v <= hi:
            kept.append(v)
        else:
            rejected.append(i)
    return kept, rejected


# ---------------------------------------------------------------------------
# Confidence interval
# ---------------------------------------------------------------------------

def confidence_interval(
    values: list[float],
    confidence: float = 0.95,
) -> tuple[float, float, float]:
    """
    Compute mean and symmetric confidence interval.
    Uses t-distribution for small samples (n < 30), z for large.

    Returns (mean, ci_lower, ci_upper).
    """
    n = len(values)
    if n == 0:
        return 0.0, 0.0, 0.0
    if n == 1:
        return values[0], values[0], values[0]
    mean = statistics.mean(values)
    std = statistics.stdev(values)
    se = std / math.sqrt(n)
    # t critical values for common confidence levels (two-tailed)
    # using lookup table for small n, z-approximation for large n
    alpha = 1 - confidence
    if n < 30:
        # t critical values for df = n-1 at common confidence levels
        t_table = {
            (0.90, 1): 6.314, (0.90, 2): 2.920, (0.90, 3): 2.353,
            (0.90, 4): 2.132, (0.90, 5): 2.015, (0.90, 9): 1.833,
            (0.90, 19): 1.729, (0.90, 29): 1.699,
            (0.95, 1): 12.706, (0.95, 2): 4.303, (0.95, 3): 3.182,
            (0.95, 4): 2.776, (0.95, 5): 2.571, (0.95, 6): 2.447,
            (0.95, 7): 2.365, (0.95, 8): 2.306, (0.95, 9): 2.262,
            (0.95, 19): 2.093, (0.95, 29): 2.045,
            (0.99, 1): 63.657, (0.99, 2): 9.925, (0.99, 3): 5.841,
            (0.99, 4): 4.604, (0.99, 5): 4.032, (0.99, 9): 3.250,
            (0.99, 19): 2.861, (0.99, 29): 2.756,
        }
        df = n - 1
        # find closest df in table for this confidence level
        candidates = {k[1]: v for k, v in t_table.items() if k[0] == confidence}
        if candidates:
            closest_df = min(candidates.keys(), key=lambda d: abs(d - df))
            t_crit = candidates[closest_df]
        else:
            t_crit = 2.0  # safe fallback
    else:
        # z critical values
        z_table = {0.90: 1.645, 0.95: 1.960, 0.99: 2.576}
        t_crit = z_table.get(confidence, 1.960)

    margin = t_crit * se
    return mean, mean - margin, mean + margin


# ---------------------------------------------------------------------------
# Run summariser
# ---------------------------------------------------------------------------

def summarise_runs(
    run_energies: list[float],
    run_powers: list[float],
    run_runtimes: list[float],
    run_requests: list[int],
    run_successes: list[int],
    cross_run_factor: float = 1.5,
    confidence: float = 0.95,
) -> dict:
    """
    Aggregate results from N repeated benchmark runs into final statistics.

    Steps:
      1. Reject outlier runs by energy (IQR across runs)
      2. Compute mean, std, CI on surviving runs
      3. Return enriched stats dict ready for CSV writing

    Parameters
    ----------
    run_energies  : total energy (J) per run
    run_powers    : average power (W) per run
    run_runtimes  : wall-clock time (s) per run
    run_requests  : total HTTP requests per run
    run_successes : successful HTTP requests per run
    cross_run_factor : IQR factor for run-level outlier rejection
    confidence    : CI confidence level (0.95 = 95%)

    Returns
    -------
    dict with keys: energy_mean, energy_std, energy_ci_lo, energy_ci_hi,
                    power_mean, runtime_mean, requests_mean, success_mean,
                    runs_total, runs_used, runs_rejected
    """
    runs_total = len(run_energies)

    # Cross-run outlier rejection on energy
    kept_energies, rejected_indices = reject_outlier_runs(run_energies, factor=cross_run_factor)
    rejected_set = set(rejected_indices)
    kept_mask = [i for i in range(runs_total) if i not in rejected_set]

    kept_powers   = [run_powers[i]    for i in kept_mask]
    kept_runtimes = [run_runtimes[i]  for i in kept_mask]
    kept_requests = [run_requests[i]  for i in kept_mask]
    kept_successes= [run_successes[i] for i in kept_mask]

    runs_used = len(kept_energies)
    runs_rejected = runs_total - runs_used

    if runs_used == 0:
        kept_energies  = run_energies
        kept_powers    = run_powers
        kept_runtimes  = run_runtimes
        kept_requests  = run_requests
        kept_successes = run_successes
        runs_used = runs_total
        runs_rejected = 0

    mean_e, ci_lo, ci_hi = confidence_interval(kept_energies, confidence)

    return {
        "energy_mean":    round(mean_e, 4),
        "energy_std":     round(statistics.stdev(kept_energies) if runs_used > 1 else 0.0, 4),
        "energy_ci_lo":   round(ci_lo, 4),
        "energy_ci_hi":   round(ci_hi, 4),
        "power_mean":     round(statistics.mean(kept_powers), 4),
        "runtime_mean":   round(statistics.mean(kept_runtimes), 4),
        "requests_mean":  round(statistics.mean(kept_requests), 1),
        "success_mean":   round(statistics.mean(kept_successes), 1),
        "runs_total":     runs_total,
        "runs_used":      runs_used,
        "runs_rejected":  runs_rejected,
    }


# ---------------------------------------------------------------------------
# Coefficient of variation (used by model_selector)
# ---------------------------------------------------------------------------

def coefficient_of_variation(values: list[float]) -> float:
    """CV = std / mean. Lower = more consistent across runs."""
    if len(values) < 2:
        return 0.0
    mean = statistics.mean(values)
    if mean == 0:
        return 0.0
    return statistics.stdev(values) / mean
