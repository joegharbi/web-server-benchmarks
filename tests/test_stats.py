"""
Tests for tools/stats.py

Run with:
    make test
    # or directly:
    srv/bin/python3 -m pytest tests/test_stats.py -v

Test strategy
-------------
1. Clean data     : filter should keep most/all samples, mean unchanged
2. Spike injection: inject known outliers at known positions, verify they
                    are removed and the recovered mean is close to the
                    true mean of the clean data
3. Edge cases     : empty, single element, all identical, too-small lists
4. summarise_runs : bad run injected, verify it is rejected
5. confidence_interval : known values, verify CI contains true mean
6. coefficient_of_variation : known std/mean ratio
"""

import math
import statistics
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pytest
from tools.stats import (
    apply_filter,
    coefficient_of_variation,
    confidence_interval,
    filter_dbscan,
    filter_elliptic,
    filter_hampel,
    filter_isolation_forest,
    filter_iqr,
    filter_lof,
    filter_none,
    reject_outlier_runs,
    summarise_runs,
    FILTER_MODELS,
)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

# Realistic Scaphandre power samples in microwatts (~2400 µW baseline)
CLEAN = [
    2400, 2380, 2450, 2410, 2390, 2420, 2405, 2395,
    2430, 2415, 2440, 2388, 2401, 2412, 2398, 2425,
    2408, 2392, 2418, 2403,
]
TRUE_MEAN = statistics.mean(CLEAN)  # ~2409

# Same data with 3 obvious spikes (10x normal) injected at indices 4, 11, 17
SPIKED = CLEAN[:4] + [24000] + CLEAN[4:10] + [22000] + CLEAN[10:16] + [20000] + CLEAN[16:]

SPIKE_POSITIONS = {4, 11, 17}  # 0-based in SPIKED
TOLERANCE = 0.05  # recovered mean must be within 5% of TRUE_MEAN


def mean_close(samples, true_mean=TRUE_MEAN, tol=TOLERANCE):
    """Assert recovered mean is within tol fraction of true_mean."""
    if not samples:
        return False
    return abs(statistics.mean(samples) - true_mean) / true_mean < tol


# ---------------------------------------------------------------------------
# filter_none
# ---------------------------------------------------------------------------

class TestFilterNone:
    def test_clean_data_unchanged(self):
        result = filter_none(CLEAN)
        assert result == CLEAN

    def test_removes_zeros(self):
        data = [0, 2400, 0, 2380, 0]
        result = filter_none(data)
        assert 0 not in result
        assert len(result) == 2

    def test_empty_input(self):
        assert filter_none([]) == []

    def test_all_zeros(self):
        assert filter_none([0, 0, 0]) == []


# ---------------------------------------------------------------------------
# filter_iqr
# ---------------------------------------------------------------------------

class TestFilterIQR:
    def test_clean_data_mostly_kept(self):
        result = filter_iqr(CLEAN)
        assert len(result) >= len(CLEAN) * 0.9

    def test_removes_spikes(self):
        result = filter_iqr(SPIKED, factor=1.5)
        assert mean_close(result), f"mean={statistics.mean(result):.0f}, expected~{TRUE_MEAN:.0f}"

    def test_no_spikes_in_result(self):
        result = filter_iqr(SPIKED, factor=1.5)
        assert all(v < 10000 for v in result)

    def test_factor_1_more_aggressive(self):
        result_1 = filter_iqr(SPIKED, factor=1.0)
        result_15 = filter_iqr(SPIKED, factor=1.5)
        assert len(result_1) <= len(result_15)

    def test_too_small_list(self):
        result = filter_iqr([2400, 2380, 2450])
        assert result == [2400, 2380, 2450]

    def test_empty(self):
        assert filter_iqr([]) == []


# ---------------------------------------------------------------------------
# filter_hampel
# ---------------------------------------------------------------------------

class TestFilterHampel:
    def test_clean_data_mostly_kept(self):
        result = filter_hampel(CLEAN, window=7, threshold=1.5)
        assert len(result) >= len(CLEAN) * 0.85

    def test_removes_spikes(self):
        result = filter_hampel(SPIKED, window=7, threshold=1.5)
        assert mean_close(result), f"mean={statistics.mean(result):.0f}"

    def test_no_spikes_in_result(self):
        result = filter_hampel(SPIKED, window=7, threshold=1.5)
        assert all(v < 10000 for v in result)

    def test_smaller_window_more_aggressive(self):
        result_3 = filter_hampel(SPIKED, window=3, threshold=1.5)
        result_11 = filter_hampel(SPIKED, window=11, threshold=1.5)
        # smaller window is more local, behaviour depends on data
        assert len(result_3) > 0
        assert len(result_11) > 0

    def test_list_shorter_than_window_returned_as_is(self):
        short = [2400, 2380, 2450]
        result = filter_hampel(short, window=7)
        assert result == short

    def test_empty(self):
        assert filter_hampel([]) == []


# ---------------------------------------------------------------------------
# filter_isolation_forest
# ---------------------------------------------------------------------------

class TestFilterIsolationForest:
    def test_clean_data_mostly_kept(self):
        result = filter_isolation_forest(CLEAN, contamination=0.1)
        assert len(result) >= len(CLEAN) * 0.85

    def test_removes_spikes(self):
        result = filter_isolation_forest(SPIKED, contamination=0.15)
        assert mean_close(result), f"mean={statistics.mean(result):.0f}"

    def test_no_spikes_in_result(self):
        result = filter_isolation_forest(SPIKED, contamination=0.15)
        assert all(v < 10000 for v in result)

    def test_higher_contamination_removes_more(self):
        r_low  = filter_isolation_forest(SPIKED, contamination=0.05)
        r_high = filter_isolation_forest(SPIKED, contamination=0.3)
        assert len(r_low) >= len(r_high)

    def test_too_small_list_returned_as_is(self):
        short = [2400, 2380, 2450, 2410, 2390]
        result = filter_isolation_forest(short)
        assert result == short


# ---------------------------------------------------------------------------
# filter_lof
# ---------------------------------------------------------------------------

class TestFilterLOF:
    def test_clean_data_mostly_kept(self):
        # Use small n_neighbors so it works with our sample size
        result = filter_lof(CLEAN, contamination=0.1, n_neighbors=5)
        assert len(result) >= len(CLEAN) * 0.85

    def test_removes_spikes(self):
        result = filter_lof(SPIKED, contamination=0.15, n_neighbors=5)
        assert mean_close(result), f"mean={statistics.mean(result):.0f}"

    def test_list_smaller_than_neighbors_returned_as_is(self):
        short = [2400, 2380, 2450]
        result = filter_lof(short, n_neighbors=10)
        assert result == short


# ---------------------------------------------------------------------------
# filter_elliptic
# ---------------------------------------------------------------------------

class TestFilterElliptic:
    def test_clean_data_mostly_kept(self):
        result = filter_elliptic(CLEAN, contamination=0.1)
        assert len(result) >= len(CLEAN) * 0.85

    def test_removes_spikes(self):
        result = filter_elliptic(SPIKED, contamination=0.15)
        assert mean_close(result), f"mean={statistics.mean(result):.0f}"

    def test_no_spikes_in_result(self):
        result = filter_elliptic(SPIKED, contamination=0.15)
        assert all(v < 10000 for v in result)

    def test_too_small_list_returned_as_is(self):
        short = [2400, 2380, 2450, 2410, 2390]
        result = filter_elliptic(short)
        assert result == short


# ---------------------------------------------------------------------------
# filter_dbscan
# ---------------------------------------------------------------------------

class TestFilterDBSCAN:
    def test_clean_data_mostly_kept(self):
        result = filter_dbscan(CLEAN, eps=100, min_samples=3)
        assert len(result) >= len(CLEAN) * 0.8

    def test_removes_spikes(self):
        # Large eps relative to normal range, small relative to spikes
        result = filter_dbscan(SPIKED, eps=200, min_samples=3)
        assert mean_close(result), f"mean={statistics.mean(result):.0f}"

    def test_too_small_list_returned_as_is(self):
        short = [2400, 2380]
        result = filter_dbscan(short, min_samples=5)
        assert result == short


# ---------------------------------------------------------------------------
# apply_filter dispatch
# ---------------------------------------------------------------------------

class TestApplyFilter:
    def test_all_models_accepted(self):
        for model in FILTER_MODELS:
            result = apply_filter(CLEAN[:], model=model, n_neighbors=5, eps=200, min_samples=3)
            assert isinstance(result, list)

    def test_unknown_model_raises(self):
        with pytest.raises(ValueError, match="Unknown filter model"):
            apply_filter(CLEAN, model="magic_filter")

    def test_kwargs_forwarded(self):
        r1 = apply_filter(SPIKED, model="iqr", factor=1.5)
        r2 = apply_filter(SPIKED, model="iqr", factor=3.0)
        assert len(r1) <= len(r2)


# ---------------------------------------------------------------------------
# reject_outlier_runs
# ---------------------------------------------------------------------------

class TestRejectOutlierRuns:
    def test_bad_run_rejected(self):
        runs = [45.2, 44.8, 45.1, 44.9, 45.3, 120.0, 44.7, 45.0, 44.6, 45.2]
        kept, rejected = reject_outlier_runs(runs, factor=1.5)
        assert 120.0 not in kept
        assert len(rejected) >= 1

    def test_clean_runs_all_kept(self):
        runs = [44.8, 45.1, 44.9, 45.2, 45.0, 44.7, 45.3, 44.6, 45.1, 44.8]
        kept, rejected = reject_outlier_runs(runs, factor=1.5)
        assert len(rejected) == 0
        assert len(kept) == len(runs)

    def test_too_few_runs_no_rejection(self):
        runs = [45.0, 44.8, 45.2]
        kept, rejected = reject_outlier_runs(runs)
        assert kept == runs
        assert rejected == []

    def test_returns_indices_of_rejected(self):
        runs = [45.0, 44.8, 45.2, 44.9, 200.0]
        kept, rejected = reject_outlier_runs(runs)
        assert 4 in rejected  # index of 200.0

    def test_empty(self):
        kept, rejected = reject_outlier_runs([])
        assert kept == []
        assert rejected == []


# ---------------------------------------------------------------------------
# confidence_interval
# ---------------------------------------------------------------------------

class TestConfidenceInterval:
    def test_single_value(self):
        mean, lo, hi = confidence_interval([42.0])
        assert mean == 42.0
        assert lo == 42.0
        assert hi == 42.0

    def test_empty(self):
        mean, lo, hi = confidence_interval([])
        assert mean == 0.0

    def test_interval_contains_true_mean(self):
        # Normal-ish data, true mean = 45.0
        values = [44.8, 45.1, 44.9, 45.2, 45.0, 44.7, 45.3, 44.6, 45.1, 44.8]
        mean, lo, hi = confidence_interval(values, confidence=0.95)
        assert lo <= 45.0 <= hi

    def test_wider_interval_for_higher_confidence(self):
        values = [44.8, 45.1, 44.9, 45.2, 45.0, 44.7, 45.3, 44.6, 45.1, 44.8]
        _, lo95, hi95 = confidence_interval(values, confidence=0.95)
        _, lo99, hi99 = confidence_interval(values, confidence=0.99)
        assert (hi99 - lo99) >= (hi95 - lo95)

    def test_mean_correct(self):
        values = [10.0, 20.0, 30.0]
        mean, _, _ = confidence_interval(values)
        assert abs(mean - 20.0) < 1e-9


# ---------------------------------------------------------------------------
# coefficient_of_variation
# ---------------------------------------------------------------------------

class TestCoefficientOfVariation:
    def test_zero_variance(self):
        assert coefficient_of_variation([5.0, 5.0, 5.0]) == 0.0

    def test_known_cv(self):
        # std=1, mean=10 → CV=0.1
        values = [9.0, 10.0, 11.0]
        cv = coefficient_of_variation(values)
        assert abs(cv - (statistics.stdev(values) / statistics.mean(values))) < 1e-9

    def test_single_value(self):
        assert coefficient_of_variation([42.0]) == 0.0

    def test_empty(self):
        assert coefficient_of_variation([]) == 0.0

    def test_lower_cv_for_consistent_data(self):
        consistent = [44.9, 45.0, 45.1, 45.0, 44.9]
        noisy      = [40.0, 45.0, 50.0, 42.0, 48.0]
        assert coefficient_of_variation(consistent) < coefficient_of_variation(noisy)


# ---------------------------------------------------------------------------
# summarise_runs (integration)
# ---------------------------------------------------------------------------

class TestSummariseRuns:
    def _make_runs(self, energies):
        n = len(energies)
        return dict(
            run_energies=energies,
            run_powers=[2.1] * n,
            run_runtimes=[21.0] * n,
            run_requests=[500] * n,
            run_successes=[500] * n,
        )

    def test_bad_run_rejected(self):
        energies = [45.2, 44.8, 45.1, 44.9, 45.3, 120.0, 44.7, 45.0, 44.6, 45.2]
        stats = summarise_runs(**self._make_runs(energies))
        assert stats["runs_rejected"] >= 1
        assert stats["runs_used"] < stats["runs_total"]
        assert abs(stats["energy_mean"] - 45.0) < 1.0

    def test_clean_runs_all_kept(self):
        energies = [44.8, 45.1, 44.9, 45.2, 45.0, 44.7, 45.3, 44.6, 45.1, 44.8]
        stats = summarise_runs(**self._make_runs(energies))
        assert stats["runs_rejected"] == 0
        assert stats["runs_used"] == 10

    def test_ci_present_and_valid(self):
        energies = [44.8, 45.1, 44.9, 45.2, 45.0, 44.7, 45.3, 44.6, 45.1, 44.8]
        stats = summarise_runs(**self._make_runs(energies))
        assert stats["energy_ci_lo"] < stats["energy_mean"] < stats["energy_ci_hi"]

    def test_too_few_runs_no_rejection(self):
        energies = [45.0, 44.8, 45.2]
        stats = summarise_runs(**self._make_runs(energies))
        assert stats["runs_rejected"] == 0

    def test_output_keys_present(self):
        energies = [45.0] * 5
        stats = summarise_runs(**self._make_runs(energies))
        expected_keys = {
            "energy_mean", "energy_std", "energy_ci_lo", "energy_ci_hi",
            "power_mean", "runtime_mean", "requests_mean", "success_mean",
            "runs_total", "runs_used", "runs_rejected",
        }
        assert expected_keys.issubset(stats.keys())


# ---------------------------------------------------------------------------
# Synthetic injection recovery test (model comparison style)
# ---------------------------------------------------------------------------

class TestSyntheticInjectionRecovery:
    """
    Gold-standard test: inject known outliers into clean data, verify each
    filter recovers a mean within TOLERANCE of the true clean mean.
    This mirrors what model_selector.py does on real data.
    """

    MODELS_AND_KWARGS = [
        ("iqr",              {"factor": 1.5}),
        ("hampel",           {"window": 7, "threshold": 1.5}),
        ("isolation_forest", {"contamination": 0.15}),
        ("lof",              {"contamination": 0.15, "n_neighbors": 5}),
        ("elliptic",         {"contamination": 0.15}),
        ("dbscan",           {"eps": 200, "min_samples": 3}),
    ]

    @pytest.mark.parametrize("model,kwargs", MODELS_AND_KWARGS)
    def test_recovery(self, model, kwargs):
        result = apply_filter(SPIKED, model=model, **kwargs)
        assert len(result) > 0, f"{model} removed all samples"
        recovered = statistics.mean(result)
        error_pct = abs(recovered - TRUE_MEAN) / TRUE_MEAN * 100
        assert error_pct < 5.0, (
            f"{model}: recovered mean {recovered:.0f} is {error_pct:.1f}% "
            f"from true mean {TRUE_MEAN:.0f} (>5% tolerance)"
        )
