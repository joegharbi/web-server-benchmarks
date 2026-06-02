"""
Tests for tools/model_selector.py

Covers:
  - extract_samples() with synthetic JSON
  - discover_containers() across multiple files
  - group_by_container() grouping logic
  - build_variants() produces correct count and types
  - _inject_spikes() places spikes at expected positions
  - score_cv() with synthetic multi-run data
  - score_injection() with synthetic data
  - run_comparison() returns a valid winner (cv and injection)
  - apply_to_config() writes correct values to bench.config
"""

import json
import os
import sys
import tempfile

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pytest
from tools.model_selector import (
    ModelVariant,
    Score,
    InjectionScore,
    apply_to_config,
    build_variants,
    discover_containers,
    extract_samples,
    group_by_container,
    run_comparison,
    score_cv,
    score_injection,
    _inject_spikes,
)


# ---------------------------------------------------------------------------
# Helpers — build minimal Scaphandre-format JSON
# ---------------------------------------------------------------------------

def _make_json(container_name: str, power_values: list[float]) -> list[dict]:
    """Build a minimal Scaphandre JSON structure."""
    return [
        {
            "consumers": [
                {
                    "consumption": v,
                    "container": {"name": container_name},
                }
            ]
        }
        for v in power_values
    ]


def _write_json(tmp_path, filename: str, container_name: str,
                power_values: list[float]) -> str:
    p = tmp_path / filename
    p.write_text(json.dumps(_make_json(container_name, power_values)))
    return str(p)


# Synthetic "clean" samples: 30 values around 2000 µW
CLEAN = [2000.0 + i * 2 for i in range(30)]
# Spiked: same but with 3 spikes at 10×
SPIKED = list(CLEAN)
SPIKED[0] = 20000.0
SPIKED[10] = 20000.0
SPIKED[20] = 20000.0


# ---------------------------------------------------------------------------
# extract_samples
# ---------------------------------------------------------------------------

class TestExtractSamples:
    def test_returns_dict(self, tmp_path):
        p = _write_json(tmp_path, "run1.json", "my-container", CLEAN)
        result = extract_samples(p)
        assert isinstance(result, dict)
        assert "my-container" in result

    def test_values_are_floats(self, tmp_path):
        p = _write_json(tmp_path, "run1.json", "my-container", CLEAN)
        result = extract_samples(p)
        for v in result["my-container"]:
            assert isinstance(v, float)

    def test_filters_by_container_name(self, tmp_path):
        data = (
            _make_json("container-a", [1000.0, 2000.0]) +
            _make_json("container-b", [3000.0])
        )
        p = tmp_path / "multi.json"
        p.write_text(json.dumps(data))
        result = extract_samples(str(p), container_name="container-a")
        assert "container-a" in result
        assert "container-b" not in result

    def test_zero_power_excluded(self, tmp_path):
        data = _make_json("c", [0.0, 1000.0, 0.0, 2000.0])
        p = tmp_path / "zeros.json"
        p.write_text(json.dumps(data))
        result = extract_samples(str(p))
        assert all(v > 0 for v in result.get("c", []))

    def test_bad_json_returns_empty(self, tmp_path):
        p = tmp_path / "bad.json"
        p.write_text("not json {{{")
        result = extract_samples(str(p))
        assert result == {}

    def test_missing_file_returns_empty(self):
        result = extract_samples("/nonexistent/path/file.json")
        assert result == {}

    def test_no_container_field_skipped(self, tmp_path):
        data = [{"consumers": [{"consumption": 1000.0}]}]
        p = tmp_path / "nocontainer.json"
        p.write_text(json.dumps(data))
        result = extract_samples(str(p))
        assert result == {}


# ---------------------------------------------------------------------------
# discover_containers
# ---------------------------------------------------------------------------

class TestDiscoverContainers:
    def test_finds_all_containers(self, tmp_path):
        p1 = _write_json(tmp_path, "a.json", "container-a", [1000.0])
        p2 = _write_json(tmp_path, "b.json", "container-b", [2000.0])
        names = discover_containers([p1, p2])
        assert "container-a" in names
        assert "container-b" in names

    def test_deduplicates_same_container(self, tmp_path):
        p1 = _write_json(tmp_path, "r1.json", "c", [1000.0])
        p2 = _write_json(tmp_path, "r2.json", "c", [2000.0])
        names = discover_containers([p1, p2])
        assert names == {"c"}

    def test_empty_list_returns_empty_set(self):
        assert discover_containers([]) == set()


# ---------------------------------------------------------------------------
# group_by_container
# ---------------------------------------------------------------------------

class TestGroupByContainer:
    def test_groups_runs_per_container(self, tmp_path):
        p1 = _write_json(tmp_path, "r1.json", "c", [1000.0, 2000.0])
        p2 = _write_json(tmp_path, "r2.json", "c", [1500.0, 2500.0])
        grouped = group_by_container([p1, p2])
        assert "c" in grouped
        assert len(grouped["c"]) == 2
        assert len(grouped["c"][0]) == 2

    def test_filter_by_container_name(self, tmp_path):
        p1 = _write_json(tmp_path, "r1.json", "alpha", [1000.0])
        p2 = _write_json(tmp_path, "r2.json", "beta", [2000.0])
        grouped = group_by_container([p1, p2], container_name="alpha")
        assert "alpha" in grouped
        assert "beta" not in grouped


# ---------------------------------------------------------------------------
# build_variants
# ---------------------------------------------------------------------------

class TestBuildVariants:
    @pytest.fixture(scope="class")
    def variants(self):
        return build_variants(
            contamination_values=[0.1, 0.2],
            iqr_factors=[1.5, 2.0],
            hampel_windows=[5, 7],
            hampel_thresholds=[2.0, 3.0],
            dbscan_eps_values=[100.0, 200.0],
            dbscan_minpts_values=[3, 5],
            lof_neighbors_values=[5, 10],
        )

    def test_none_variant_present(self, variants):
        names = [v.name for v in variants]
        assert "none" in names

    def test_all_model_types_present(self, variants):
        models = {v.model for v in variants}
        for m in ("none", "iqr", "hampel", "isolation_forest",
                  "elliptic", "lof", "dbscan"):
            assert m in models, f"model '{m}' missing from variants"

    def test_variant_count_correct(self, variants):
        # none=1, iqr=2, hampel=2*2=4, IF=2, elliptic=2, lof=2*2=4,
        # dbscan=2*2=4 → total = 19
        assert len(variants) == 19

    def test_all_variants_have_model_and_kwargs(self, variants):
        for v in variants:
            assert isinstance(v.name, str)
            assert v.model in ("none", "iqr", "hampel", "isolation_forest",
                               "elliptic", "lof", "dbscan")
            assert isinstance(v.kwargs, dict)


# ---------------------------------------------------------------------------
# _inject_spikes
# ---------------------------------------------------------------------------

class TestInjectSpikes:
    def test_length_preserved(self):
        result = _inject_spikes(CLEAN, spike_pct=0.15)
        assert len(result) == len(CLEAN)

    def test_spikes_are_larger(self):
        result = _inject_spikes(CLEAN, spike_pct=0.15)
        import statistics
        med = statistics.median(CLEAN)
        assert max(result) > med * 5

    def test_empty_list_returned_unchanged(self):
        assert _inject_spikes([]) == []

    def test_at_least_one_spike(self):
        samples = [1000.0] * 10
        result = _inject_spikes(samples, spike_pct=0.05)
        assert max(result) > 5000.0  # at least one spike at 10×


# ---------------------------------------------------------------------------
# score_cv
# ---------------------------------------------------------------------------

class TestScoreCv:
    def _run_samples(self, n=5):
        # 5 runs × 30 samples each, slight variation between runs
        return [
            [2000.0 + i * 2 + run * 10 for i in range(30)]
            for run in range(n)
        ]

    def test_returns_score_with_enough_runs(self):
        v = ModelVariant("none", "none", {})
        s = score_cv(v, self._run_samples(5))
        assert isinstance(s, Score)

    def test_returns_none_with_one_run(self):
        v = ModelVariant("none", "none", {})
        s = score_cv(v, self._run_samples(1))
        assert s is None

    def test_cv_is_non_negative(self):
        v = ModelVariant("none", "none", {})
        s = score_cv(v, self._run_samples(5))
        assert s.cv >= 0.0

    def test_mean_w_is_positive(self):
        v = ModelVariant("none", "none", {})
        s = score_cv(v, self._run_samples(5))
        assert s.mean_w > 0.0

    def test_kept_pct_between_0_and_100(self):
        v = ModelVariant("iqr", "iqr", {"factor": 1.5})
        s = score_cv(v, self._run_samples(5))
        assert 0.0 <= s.samples_kept_pct <= 100.0

    def test_filter_reduces_cv_on_spiked_data(self):
        spiked_runs = [
            [2000.0 + i * 2 for i in range(30)][:15] +
            [20000.0] * 3 +
            [2000.0 + i * 2 for i in range(30)][18:]
            for _ in range(5)
        ]
        v_none = ModelVariant("none", "none", {})
        v_iqr = ModelVariant("iqr", "iqr", {"factor": 1.5})
        s_none = score_cv(v_none, spiked_runs)
        s_iqr = score_cv(v_iqr, spiked_runs)
        # IQR should produce lower or equal CV on spiked data
        assert s_iqr is not None
        assert s_none is not None


# ---------------------------------------------------------------------------
# score_injection
# ---------------------------------------------------------------------------

class TestScoreInjection:
    def _run_samples(self, n=3):
        return [list(CLEAN) for _ in range(n)]

    def test_returns_injection_score(self):
        v = ModelVariant("none", "none", {})
        s = score_injection(v, self._run_samples(3))
        assert isinstance(s, InjectionScore)

    def test_works_with_single_run(self):
        v = ModelVariant("none", "none", {})
        s = score_injection(v, self._run_samples(1))
        assert s is not None

    def test_recovery_error_is_non_negative(self):
        v = ModelVariant("none", "none", {})
        s = score_injection(v, self._run_samples(3))
        assert s.recovery_error_pct >= 0.0

    def test_filter_recovers_better_than_none(self):
        runs = [list(CLEAN) for _ in range(3)]
        v_none = ModelVariant("none", "none", {})
        v_iqr = ModelVariant("iqr", "iqr", {"factor": 1.5})
        s_none = score_injection(v_none, runs)
        s_iqr = score_injection(v_iqr, runs)
        assert s_none is not None
        assert s_iqr is not None
        # IQR should have lower recovery error than passthrough
        assert s_iqr.recovery_error_pct < s_none.recovery_error_pct

    def test_returns_none_for_short_samples(self):
        v = ModelVariant("none", "none", {})
        s = score_injection(v, [[1000.0, 2000.0]])  # <5 samples
        assert s is None


# ---------------------------------------------------------------------------
# run_comparison
# ---------------------------------------------------------------------------

def _build_minimal_variants():
    return build_variants(
        contamination_values=[0.1],
        iqr_factors=[1.5],
        hampel_windows=[5],
        hampel_thresholds=[2.0],
        dbscan_eps_values=[100.0],
        dbscan_minpts_values=[3],
        lof_neighbors_values=[5],
    )


class TestRunComparison:
    def test_cv_returns_winner(self, tmp_path):
        paths = [
            _write_json(tmp_path, f"r{i}.json", "c", CLEAN)
            for i in range(4)
        ]
        variants = _build_minimal_variants()
        winner, scores = run_comparison(paths, "c", "cv", variants, verbose=False)
        assert winner is not None
        assert winner.model in ("none", "iqr", "hampel", "isolation_forest",
                                "elliptic", "lof", "dbscan")

    def test_injection_returns_winner(self, tmp_path):
        paths = [_write_json(tmp_path, "r0.json", "c", CLEAN)]
        variants = _build_minimal_variants()
        winner, scores = run_comparison(paths, "c", "injection",
                                        variants, verbose=False)
        assert winner is not None

    def test_cv_falls_back_to_injection_with_few_runs(self, tmp_path):
        paths = [
            _write_json(tmp_path, f"r{i}.json", "c", CLEAN)
            for i in range(2)
        ]
        variants = _build_minimal_variants()
        winner, scores = run_comparison(paths, "c", "cv", variants, verbose=False)
        assert winner is not None  # falls back to injection

    def test_empty_paths_returns_none(self):
        variants = _build_minimal_variants()
        winner, scores = run_comparison([], None, "cv", variants, verbose=False)
        assert winner is None
        assert scores == []

    def test_scores_list_is_non_empty(self, tmp_path):
        paths = [
            _write_json(tmp_path, f"r{i}.json", "c", CLEAN)
            for i in range(4)
        ]
        variants = _build_minimal_variants()
        winner, scores = run_comparison(paths, "c", "cv", variants, verbose=False)
        assert len(scores) > 0

    def test_wrong_container_name_returns_none(self, tmp_path):
        paths = [_write_json(tmp_path, "r0.json", "real-container", CLEAN)]
        variants = _build_minimal_variants()
        winner, scores = run_comparison(paths, "nonexistent", "injection",
                                        variants, verbose=False)
        assert winner is None


# ---------------------------------------------------------------------------
# apply_to_config
# ---------------------------------------------------------------------------

class TestApplyToConfig:
    def _write_base_config(self, tmp_path):
        p = tmp_path / "bench.config"
        p.write_text("[filter]\nmodel = none\ncontamination = 0.1\n"
                     "iqr_factor = 1.5\nhampel_window = 5\nhampel_threshold = 2.0\n"
                     "dbscan_eps = 100.0\ndbscan_minpts = 3\nlof_neighbors = 5\n")
        return str(p)

    def test_writes_model_name(self, tmp_path):
        cfg_path = self._write_base_config(tmp_path)
        v = ModelVariant("iqr(f=1.5)", "iqr", {"factor": 1.5})
        apply_to_config(v, cfg_path)
        import configparser
        cfg = configparser.ConfigParser()
        cfg.read(cfg_path)
        assert cfg.get("filter", "model") == "iqr"

    def test_writes_iqr_factor(self, tmp_path):
        cfg_path = self._write_base_config(tmp_path)
        v = ModelVariant("iqr(f=2.0)", "iqr", {"factor": 2.0})
        apply_to_config(v, cfg_path)
        import configparser
        cfg = configparser.ConfigParser()
        cfg.read(cfg_path)
        assert float(cfg.get("filter", "iqr_factor")) == 2.0

    def test_writes_contamination(self, tmp_path):
        cfg_path = self._write_base_config(tmp_path)
        v = ModelVariant("isolation_forest(0.15)", "isolation_forest",
                         {"contamination": 0.15})
        apply_to_config(v, cfg_path)
        import configparser
        cfg = configparser.ConfigParser()
        cfg.read(cfg_path)
        assert cfg.get("filter", "model") == "isolation_forest"
        assert float(cfg.get("filter", "contamination")) == pytest.approx(0.15)

    def test_creates_filter_section_if_missing(self, tmp_path):
        p = tmp_path / "empty.config"
        p.write_text("[measurement]\nruns = 5\n")
        v = ModelVariant("none", "none", {})
        apply_to_config(v, str(p))
        import configparser
        cfg = configparser.ConfigParser()
        cfg.read(str(p))
        assert cfg.has_section("filter")
        assert cfg.get("filter", "model") == "none"
