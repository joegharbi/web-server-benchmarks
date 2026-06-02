"""
Tests for tools/bench_profile.py

Covers:
  - All detect_* helpers return correct types
  - run_profile() returns complete findings dict
  - write_config() produces valid INI with required sections and keys
  - load_config() reads a written config correctly
  - resolve() honours three-layer priority: CLI > env var > config > fallback
  - Config template contains no unfilled placeholders
  - Findings dict keys are present and typed correctly
"""

import configparser
import os
import sys
import tempfile

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pytest
from tools.bench_profile import (
    detect_brightness_pct,
    detect_cpu_cores,
    detect_cpu_model,
    detect_cstates,
    detect_hyperthreading,
    detect_kernel,
    detect_memory_gb,
    detect_os,
    detect_cgroup_v2,
    detect_docker,
    load_config,
    recommend_cooldown_temp,
    recommend_runs,
    resolve,
    run_profile,
    write_config,
    CONFIG_TEMPLATE,
)

# ---------------------------------------------------------------------------
# Individual detectors — type checks
# ---------------------------------------------------------------------------

class TestDetectors:
    def test_cpu_model_string(self):
        val = detect_cpu_model()
        assert isinstance(val, str)
        assert len(val) > 0

    def test_cpu_cores_tuple(self):
        physical, logical = detect_cpu_cores()
        assert isinstance(physical, int) and physical >= 1
        assert isinstance(logical, int) and logical >= physical

    def test_hyperthreading_bool(self):
        assert isinstance(detect_hyperthreading(), bool)

    def test_cstates_list(self):
        val = detect_cstates()
        assert isinstance(val, list)

    def test_memory_gb_float(self):
        val = detect_memory_gb()
        assert isinstance(val, float)
        assert val > 0.0

    def test_os_string(self):
        val = detect_os()
        assert isinstance(val, str)
        assert len(val) > 0

    def test_kernel_string(self):
        val = detect_kernel()
        assert isinstance(val, str)
        assert len(val) > 0

    def test_cgroup_v2_bool(self):
        assert isinstance(detect_cgroup_v2(), bool)

    def test_docker_bool(self):
        assert isinstance(detect_docker(), bool)

    def test_brightness_pct_tuple(self):
        found, pct = detect_brightness_pct()
        assert isinstance(found, bool)
        assert isinstance(pct, int)
        assert 0 <= pct <= 100

    def test_recommend_runs_int(self):
        val = recommend_runs()
        assert isinstance(val, int)
        assert val >= 1

    def test_recommend_cooldown_temp_float(self):
        val = recommend_cooldown_temp()
        assert isinstance(val, float)
        assert 30.0 <= val <= 100.0


# ---------------------------------------------------------------------------
# run_profile() findings dict
# ---------------------------------------------------------------------------

class TestRunProfile:
    @pytest.fixture(scope="class")
    def findings(self):
        return run_profile(verbose=False)

    REQUIRED_KEYS = {
        "os", "kernel", "hostname",
        "cpu_model", "cpu_physical", "cpu_logical", "hyperthreading",
        "governor", "turbo", "cstates",
        "memory_gb", "swap_active", "thp",
        "temps", "temp_current", "cooldown_temp",
        "backlight_found", "brightness_pct",
        "active_services",
        "sudo", "cpupower", "docker", "scaphandre", "rapl", "cgroup_v2",
        "isolation_level", "runs",
    }

    def test_all_required_keys_present(self, findings):
        missing = self.REQUIRED_KEYS - set(findings.keys())
        assert not missing, f"Missing keys: {missing}"

    def test_isolation_level_valid(self, findings):
        assert findings["isolation_level"] in ("none", "basic", "full")

    def test_runs_positive_int(self, findings):
        assert isinstance(findings["runs"], int)
        assert findings["runs"] >= 1

    def test_cpu_counts_sane(self, findings):
        assert findings["cpu_physical"] >= 1
        assert findings["cpu_logical"] >= findings["cpu_physical"]

    def test_memory_positive(self, findings):
        assert findings["memory_gb"] > 0

    def test_temps_list(self, findings):
        assert isinstance(findings["temps"], list)

    def test_active_services_list(self, findings):
        assert isinstance(findings["active_services"], list)

    def test_cooldown_temp_range(self, findings):
        assert 30.0 <= findings["cooldown_temp"] <= 100.0


# ---------------------------------------------------------------------------
# write_config() + load_config()
# ---------------------------------------------------------------------------

REQUIRED_SECTIONS = [
    "isolation", "cpu", "memory", "display",
    "services", "thermal", "measurement",
    "filter", "cross_run_filter", "agent", "gui",
]

REQUIRED_KEYS_PER_SECTION = {
    "isolation": ["level"],
    "cpu": ["governor", "disable_turbo", "disable_cstates", "cpuset"],
    "memory": ["disable_thp", "check_swap", "drop_caches"],
    "display": ["brightness", "disable_screensaver"],
    "services": ["stop_before_run"],
    "thermal": ["cooldown_temp_c", "cooldown_cpu_pct", "cooldown_timeout_s"],
    "measurement": ["runs", "baseline_duration_s", "confidence"],
    "filter": ["model", "contamination", "iqr_factor", "hampel_window",
               "hampel_threshold", "dbscan_eps", "dbscan_minpts", "lof_neighbors"],
    "cross_run_filter": ["model", "factor"],
    "agent": ["host", "port", "user"],
    "gui": ["port"],
}


class TestWriteConfig:
    @pytest.fixture
    def config_path(self, tmp_path):
        return str(tmp_path / "bench.config")

    @pytest.fixture
    def findings(self):
        return run_profile(verbose=False)

    def test_file_created(self, findings, config_path):
        write_config(findings, config_path)
        assert os.path.exists(config_path)

    def test_file_not_empty(self, findings, config_path):
        write_config(findings, config_path)
        assert os.path.getsize(config_path) > 100

    def test_all_sections_present(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        for section in REQUIRED_SECTIONS:
            assert cfg.has_section(section), f"Missing section: [{section}]"

    def test_all_keys_present(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        for section, keys in REQUIRED_KEYS_PER_SECTION.items():
            for key in keys:
                assert cfg.has_option(section, key), \
                    f"Missing key '{key}' in [{section}]"

    def test_isolation_level_valid(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        level = cfg.get("isolation", "level")
        assert level in ("none", "basic", "full")

    def test_filter_model_is_none_until_validated(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        model = cfg.get("filter", "model")
        assert model == "none", \
            "Filter model must be 'none' until model_selector validates it"

    def test_runs_is_positive_integer(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        runs = cfg.getint("measurement", "runs")
        assert runs >= 1

    def test_confidence_in_range(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        conf = cfg.getfloat("measurement", "confidence")
        assert 0.8 <= conf <= 0.999

    def test_no_unfilled_template_placeholders(self, findings, config_path):
        write_config(findings, config_path)
        with open(config_path) as f:
            content = f.read()
        import re
        placeholders = re.findall(r'\{[a-z_]+\}', content)
        assert not placeholders, \
            f"Unfilled placeholders in bench.config: {placeholders}"

    def test_agent_host_default_localhost(self, findings, config_path):
        write_config(findings, config_path)
        cfg = load_config(config_path)
        assert cfg.get("agent", "host") == "localhost"


# ---------------------------------------------------------------------------
# load_config() — missing file
# ---------------------------------------------------------------------------

class TestLoadConfig:
    def test_missing_file_returns_empty_parser(self):
        cfg = load_config("/nonexistent/path/bench.config")
        assert isinstance(cfg, configparser.ConfigParser)

    def test_existing_file_loads(self, tmp_path):
        p = tmp_path / "bench.config"
        p.write_text("[measurement]\nruns = 7\n")
        cfg = load_config(str(p))
        assert cfg.getint("measurement", "runs") == 7


# ---------------------------------------------------------------------------
# resolve() — three-layer priority
# ---------------------------------------------------------------------------

class TestResolve:
    @pytest.fixture
    def cfg(self, tmp_path):
        p = tmp_path / "bench.config"
        p.write_text("[measurement]\nruns = 5\n")
        return load_config(str(p))

    def test_fallback_when_nothing_set(self, cfg):
        result = resolve(cfg, "measurement", "nonexistent", "BENCH_NONEXISTENT",
                         cli_val=None, fallback="default_val")
        assert result == "default_val"

    def test_config_wins_over_fallback(self, cfg):
        result = resolve(cfg, "measurement", "runs", "BENCH_RUNS",
                         cli_val=None, fallback="99")
        assert result == "5"

    def test_env_wins_over_config(self, cfg, monkeypatch):
        monkeypatch.setenv("BENCH_RUNS", "8")
        result = resolve(cfg, "measurement", "runs", "BENCH_RUNS",
                         cli_val=None, fallback="99")
        assert result == "8"

    def test_cli_wins_over_env_and_config(self, cfg, monkeypatch):
        monkeypatch.setenv("BENCH_RUNS", "8")
        result = resolve(cfg, "measurement", "runs", "BENCH_RUNS",
                         cli_val="3", fallback="99")
        assert result == "3"

    def test_cli_none_does_not_override(self, cfg):
        result = resolve(cfg, "measurement", "runs", "BENCH_RUNS",
                         cli_val=None, fallback="99")
        assert result == "5"  # config value, not fallback

    def test_env_cleared_falls_back_to_config(self, cfg, monkeypatch):
        monkeypatch.delenv("BENCH_RUNS", raising=False)
        result = resolve(cfg, "measurement", "runs", "BENCH_RUNS",
                         cli_val=None, fallback="99")
        assert result == "5"


# ---------------------------------------------------------------------------
# CONFIG_TEMPLATE sanity
# ---------------------------------------------------------------------------

class TestConfigTemplate:
    def test_template_has_all_sections(self):
        for section in REQUIRED_SECTIONS:
            assert f"[{section}]" in CONFIG_TEMPLATE, \
                f"Section [{section}] missing from CONFIG_TEMPLATE"

    def test_template_has_filter_model_none(self):
        assert "model = none" in CONFIG_TEMPLATE

    def test_template_has_important_comments(self):
        assert "model_selector" in CONFIG_TEMPLATE
        assert "sudo" in CONFIG_TEMPLATE.lower()
