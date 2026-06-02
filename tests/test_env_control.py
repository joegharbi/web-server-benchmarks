"""
Tests for tools/env_control.py

These tests cover everything that can be verified without sudo or hardware:
  - IsolationContext level="none" touches nothing
  - Change recording and restore script generation
  - Temperature and CPU idle readers (smoke tests)
  - Detect helpers return correct types
  - Cooldown exits quickly when thresholds are already met
  - Context manager restores on exception

Hardware-dependent controls (governor, turbo, brightness, services) are
tested for graceful degradation only — they must not crash, even when the
sysfs paths or tools are absent.
"""

import os
import sys
import time
import tempfile

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pytest
from tools.env_control import (
    IsolationContext,
    Change,
    detect_governor,
    detect_turbo,
    detect_thp,
    detect_swap_active,
    detect_temperatures,
    detect_backlight,
    detect_active_services,
    detect_rapl,
    detect_scaphandre_version,
    NOISY_SERVICES,
    RESTORE_SCRIPT,
)


# ---------------------------------------------------------------------------
# IsolationContext — level none
# ---------------------------------------------------------------------------

class TestIsolationNone:
    def test_context_manager_no_crash(self):
        with IsolationContext(level="none", verbose=False) as ctx:
            assert ctx is not None

    def test_no_changes_recorded(self):
        with IsolationContext(level="none", verbose=False) as ctx:
            assert ctx._changes == []

    def test_report_no_crash(self):
        with IsolationContext(level="none", verbose=False) as ctx:
            ctx.report()  # should print gracefully

    def test_no_restore_script_written(self):
        with IsolationContext(level="none", verbose=False):
            pass
        assert not os.path.exists(RESTORE_SCRIPT)


# ---------------------------------------------------------------------------
# IsolationContext — invalid level
# ---------------------------------------------------------------------------

class TestIsolationInvalidLevel:
    def test_raises_on_bad_level(self):
        with pytest.raises(ValueError, match="level must be"):
            IsolationContext(level="ultra")


# ---------------------------------------------------------------------------
# Change recording
# ---------------------------------------------------------------------------

class TestChangeRecording:
    def test_record_adds_change(self):
        ctx = IsolationContext(level="none", verbose=False)
        ctx._record("test change", "echo restored")
        assert len(ctx._changes) == 1
        assert ctx._changes[0].what == "test change"
        assert ctx._changes[0].restore_cmd == "echo restored"
        assert ctx._changes[0].restored is False

    def test_multiple_changes_recorded(self):
        ctx = IsolationContext(level="none", verbose=False)
        ctx._record("change A", "echo A")
        ctx._record("change B", "echo B")
        assert len(ctx._changes) == 2

    def test_restore_script_written_with_changes(self):
        ctx = IsolationContext(level="none", verbose=False)
        ctx._record("test change", "echo restored_value")
        ctx._write_restore_script()
        assert os.path.exists(RESTORE_SCRIPT)
        with open(RESTORE_SCRIPT) as f:
            content = f.read()
        assert "echo restored_value" in content
        assert "#!/bin/bash" in content
        ctx._remove_restore_script()

    def test_restore_script_removed(self):
        ctx = IsolationContext(level="none", verbose=False)
        ctx._record("x", "echo x")
        ctx._write_restore_script()
        assert os.path.exists(RESTORE_SCRIPT)
        ctx._remove_restore_script()
        assert not os.path.exists(RESTORE_SCRIPT)

    def test_restore_script_executable(self):
        ctx = IsolationContext(level="none", verbose=False)
        ctx._record("x", "echo x")
        ctx._write_restore_script()
        mode = oct(os.stat(RESTORE_SCRIPT).st_mode)
        assert "755" in mode or "7" in mode[-3:]
        ctx._remove_restore_script()


# ---------------------------------------------------------------------------
# Context manager restores on exception
# ---------------------------------------------------------------------------

class TestContextManagerException:
    def test_teardown_called_on_exception(self):
        teardown_called = []

        class TrackingContext(IsolationContext):
            def _setup(self):
                pass  # skip real setup in test

            def _teardown(self):
                teardown_called.append(True)

        with pytest.raises(RuntimeError):
            with TrackingContext(level="basic", verbose=False):
                raise RuntimeError("simulated crash")

        assert teardown_called, "teardown must be called even when exception is raised"

    def test_exception_propagates(self):
        class NoopContext(IsolationContext):
            def _setup(self):
                pass  # skip real setup in test

        with pytest.raises(ValueError, match="test error"):
            with NoopContext(level="basic", verbose=False):
                raise ValueError("test error")


# ---------------------------------------------------------------------------
# Temperature and CPU idle readers
# ---------------------------------------------------------------------------

class TestSystemReaders:
    def test_read_temperatures_returns_list(self):
        ctx = IsolationContext(level="none", verbose=False)
        temps = ctx.read_temperatures()
        assert isinstance(temps, list)

    def test_temperatures_are_plausible(self):
        ctx = IsolationContext(level="none", verbose=False)
        temps = ctx.read_temperatures()
        for t in temps:
            assert 0.0 < t < 120.0, f"Temperature {t}°C is implausible"

    def test_current_temp_is_float_or_none(self):
        ctx = IsolationContext(level="none", verbose=False)
        temp = ctx.current_temp()
        assert temp is None or isinstance(temp, float)

    def test_cpu_idle_is_percentage(self):
        ctx = IsolationContext(level="none", verbose=False)
        idle = ctx.current_cpu_idle()
        assert 0.0 <= idle <= 100.0


# ---------------------------------------------------------------------------
# Cooldown (fast exit when thresholds already met)
# ---------------------------------------------------------------------------

class TestCooldown:
    def test_cooldown_exits_fast_when_conditions_met(self):
        # Set extremely relaxed thresholds — should exit on first poll
        # cooldown_cpu=99.0 means "max allowed CPU usage = 99%" → always satisfied
        ctx = IsolationContext(
            level="none",
            cooldown_temp=200.0,   # always satisfied — no CPU reaches 200°C
            cooldown_cpu=99.0,     # always satisfied — CPU usage never hits 100%
            cooldown_timeout=30,
            verbose=False,
        )
        start = time.time()
        result = ctx.cooldown()
        elapsed = time.time() - start
        assert result is True
        assert elapsed < 5.0, f"cooldown took {elapsed:.1f}s — should exit fast"

    def test_cooldown_times_out_with_impossible_threshold(self):
        ctx = IsolationContext(
            level="none",
            cooldown_temp=0.01,    # impossible — CPU will never be this cold
            cooldown_cpu=100.0,    # impossible — CPU is never 100% idle
            cooldown_timeout=3,    # short timeout for test speed
            verbose=False,
        )
        start = time.time()
        result = ctx.cooldown()
        elapsed = time.time() - start
        assert result is False
        assert elapsed >= 3.0


# ---------------------------------------------------------------------------
# Idle baseline (smoke test)
# ---------------------------------------------------------------------------

class TestIdleBaseline:
    def test_measure_idle_returns_float(self):
        ctx = IsolationContext(level="none", verbose=False)
        # Use duration=1 to keep test fast
        result = ctx.measure_idle(duration=1)
        assert isinstance(result, float)
        assert result >= 0.0

    def test_measure_idle_plausible_if_rapl(self):
        ctx = IsolationContext(level="none", verbose=False)
        result = ctx.measure_idle(duration=1)
        if result > 0.0:
            # RAPL available — power should be between 0.1W and 500W
            assert 0.1 <= result <= 500.0, f"Idle power {result}W is implausible"


# ---------------------------------------------------------------------------
# Graceful degradation — hardware controls
# ---------------------------------------------------------------------------

class TestGracefulDegradation:
    """
    These tests verify that each hardware control does not crash even when
    the underlying sysfs path or tool is absent. We test the internal methods
    directly without going through full setup().
    """

    def _ctx(self):
        return IsolationContext(level="none", verbose=False)

    def test_disable_turbo_no_crash(self):
        ctx = self._ctx()
        ctx._disable_turbo()  # must not raise

    def test_disable_thp_no_crash(self):
        ctx = self._ctx()
        ctx._disable_thp()  # must not raise

    def test_drop_caches_no_crash(self):
        ctx = self._ctx()
        ctx._drop_caches()  # must not raise (may warn without sudo)

    def test_set_brightness_no_crash(self):
        ctx = self._ctx()
        ctx._set_brightness(0)  # must not raise if no backlight

    def test_stop_services_no_crash(self):
        ctx = self._ctx()
        ctx._stop_services()  # must not raise

    def test_set_wifi_powersave_no_crash(self):
        ctx = self._ctx()
        ctx._set_wifi_powersave(False)  # must not raise

    def test_set_governor_no_crash(self):
        ctx = self._ctx()
        ctx._set_governor("performance")  # must not raise

    def test_disable_screensaver_no_crash(self):
        ctx = self._ctx()
        ctx._disable_screensaver()  # must not raise


# ---------------------------------------------------------------------------
# Detect helpers
# ---------------------------------------------------------------------------

class TestDetectHelpers:
    def test_detect_governor_string_or_none(self):
        val = detect_governor()
        assert val is None or isinstance(val, str)

    def test_detect_turbo_bool_or_none(self):
        val = detect_turbo()
        assert val is None or isinstance(val, bool)

    def test_detect_thp_string_or_none(self):
        val = detect_thp()
        assert val is None or val in ("always", "madvise", "never")

    def test_detect_swap_bool(self):
        val = detect_swap_active()
        assert isinstance(val, bool)

    def test_detect_temperatures_list_of_floats(self):
        temps = detect_temperatures()
        assert isinstance(temps, list)
        for t in temps:
            assert isinstance(t, float)

    def test_detect_backlight_tuple_or_none(self):
        val = detect_backlight()
        assert val is None or (isinstance(val, tuple) and len(val) == 3)

    def test_detect_active_services_list(self):
        val = detect_active_services(NOISY_SERVICES)
        assert isinstance(val, list)
        for svc in val:
            assert isinstance(svc, str)

    def test_detect_rapl_bool(self):
        val = detect_rapl()
        assert isinstance(val, bool)

    def test_detect_scaphandre_version_string_or_none(self):
        val = detect_scaphandre_version()
        assert val is None or isinstance(val, str)


# ---------------------------------------------------------------------------
# NOISY_SERVICES sanity check
# ---------------------------------------------------------------------------

class TestNoisyServicesList:
    def test_noisy_services_not_empty(self):
        assert len(NOISY_SERVICES) > 0

    def test_no_critical_services_in_list(self):
        forbidden = {"docker", "networkmanager", "network-manager",
                     "scaphandre", "sshd", "ssh"}
        for svc in NOISY_SERVICES:
            name = svc.replace(".service", "").replace(".timer", "").lower()
            assert name not in forbidden, \
                f"Critical service '{svc}' must not be in NOISY_SERVICES"
