"""
Machine profiler for benchmark environment assessment.

Scans the host machine, reports what it finds with traffic-light indicators,
recommends isolation settings, and writes bench.config.

Usage
-----
    python tools/bench_profile.py               # scan + write bench.config
    python tools/bench_profile.py --dry-run     # scan + print, no file written
    python tools/bench_profile.py --out PATH    # write config to a custom path

Output
------
  bench.config   INI file read by measure_docker.py, model_selector.py, and GUI
"""

import argparse
import configparser
import glob
import multiprocessing
import os
import platform
import re
import shutil
import subprocess
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from tools.env_control import (
    NOISY_SERVICES,
    _read,
    _run,
    _sudo_available,
    _tool_available,
    detect_active_services,
    detect_backlight,
    detect_governor,
    detect_rapl,
    detect_scaphandre_version,
    detect_swap_active,
    detect_temperatures,
    detect_thp,
    detect_turbo,
)

DEFAULT_CONFIG_PATH = os.path.join(
    os.path.dirname(__file__), "..", "bench.config"
)

# ANSI colours
_G = "\033[0;32m"   # green
_Y = "\033[1;33m"   # yellow
_R = "\033[0;31m"   # red
_B = "\033[1;34m"   # bold blue
_NC = "\033[0m"     # reset


def _ok(msg):  return f"{_G}[OK]{_NC}    {msg}"
def _warn(msg): return f"{_Y}[WARN]{_NC}  {msg}"
def _bad(msg):  return f"{_R}[BAD]{_NC}   {msg}"
def _info(msg): return f"{_B}[INFO]{_NC}  {msg}"


# ---------------------------------------------------------------------------
# Individual detections
# ---------------------------------------------------------------------------

def detect_cpu_model() -> str:
    val = _read("/proc/cpuinfo")
    if val:
        for line in val.splitlines():
            if "model name" in line:
                return line.split(":", 1)[1].strip()
    return platform.processor() or "unknown"


def detect_cpu_cores() -> tuple[int, int]:
    """Return (physical_cores, logical_cores)."""
    logical = multiprocessing.cpu_count()
    physical = logical
    val = _read("/proc/cpuinfo")
    if val:
        ids = set()
        for line in val.splitlines():
            if line.startswith("core id"):
                ids.add(line.split(":")[1].strip())
        if ids:
            physical = len(ids)
    return physical, logical


def detect_hyperthreading() -> bool:
    physical, logical = detect_cpu_cores()
    return logical > physical


def detect_cstates() -> list[str]:
    """Return list of available C-state names."""
    states = []
    for path in sorted(glob.glob(
        "/sys/devices/system/cpu/cpu0/cpuidle/state*/name"
    )):
        name = _read(path)
        if name:
            states.append(name)
    return states


def detect_memory_gb() -> float:
    val = _read("/proc/meminfo")
    if val:
        for line in val.splitlines():
            if line.startswith("MemTotal"):
                kb = int(line.split()[1])
                return round(kb / 1024 / 1024, 1)
    return 0.0


def detect_os() -> str:
    try:
        result = _run(["lsb_release", "-ds"])
        if result.returncode == 0 and result.stdout.strip():
            return result.stdout.strip().strip('"')
    except Exception:
        pass
    return platform.platform()


def detect_kernel() -> str:
    return platform.release()


def detect_cgroup_v2() -> bool:
    """Return True if cgroups v2 is in use (affects Scaphandre container attribution)."""
    return os.path.exists("/sys/fs/cgroup/cgroup.controllers")


def detect_docker() -> bool:
    return _tool_available("docker")


def detect_brightness_pct() -> tuple[bool, int]:
    """Return (found, current_pct). current_pct is 0-100."""
    info = detect_backlight()
    if info is None:
        return False, 0
    _, current, max_val = info
    if max_val == 0:
        return True, 0
    return True, round((current / max_val) * 100)


def recommend_runs() -> int:
    """Heuristic: 10 runs gives ~95% CI with typical Scaphandre variance."""
    return 10


def recommend_cooldown_temp() -> float:
    """Recommend cooldown temperature threshold based on current temp."""
    temps = detect_temperatures()
    if not temps:
        return 50.0
    current_max = max(temps)
    # 10°C headroom above current idle temperature
    return min(current_max + 15.0, 60.0)


# ---------------------------------------------------------------------------
# Profiler report
# ---------------------------------------------------------------------------

def run_profile(verbose: bool = True) -> dict:
    """
    Scan the machine and return a dict of findings.
    Prints a formatted report if verbose=True.
    """
    findings = {}

    def _section(title):
        if verbose:
            print(f"\n{_B}[{title}]{_NC}")

    def _line(label, value, status_fn, note=""):
        if verbose:
            note_str = f"  → {note}" if note else ""
            print(f"  {label:<18} {value:<30} {status_fn('')}{note_str}")

    def _raw(msg):
        if verbose:
            print(f"  {msg}")

    if verbose:
        print(f"\n{_B}{'='*55}{_NC}")
        print(f"{_B}  BEAM Benchmark Machine Profiler{_NC}")
        print(f"{_B}{'='*55}{_NC}")

    # --- OS ---
    _section("System")
    findings["os"] = detect_os()
    findings["kernel"] = detect_kernel()
    findings["hostname"] = platform.node()
    if verbose:
        _raw(f"  Host      : {findings['hostname']}")
        _raw(f"  OS        : {findings['os']}")
        _raw(f"  Kernel    : {findings['kernel']}")

    # --- CPU ---
    _section("CPU")
    findings["cpu_model"] = detect_cpu_model()
    findings["cpu_physical"], findings["cpu_logical"] = detect_cpu_cores()
    findings["hyperthreading"] = detect_hyperthreading()
    findings["governor"] = detect_governor()
    findings["turbo"] = detect_turbo()
    findings["cstates"] = detect_cstates()

    gov = findings["governor"] or "unknown"
    gov_ok = gov == "performance"
    turbo = findings["turbo"]
    turbo_ok = turbo is False  # False = disabled = good for benchmarks

    if verbose:
        _raw(f"  Model     : {findings['cpu_model']}")
        _raw(f"  Cores     : {findings['cpu_physical']} physical, "
             f"{findings['cpu_logical']} logical"
             + (" (HT enabled)" if findings["hyperthreading"] else ""))
        print(f"  {'Governor':<18} {gov:<30} "
              + (_ok("") if gov_ok else _warn("recommend 'performance'")))
        print(f"  {'Turbo/Boost':<18} "
              + (f"{'disabled':<30} {_ok('')}" if turbo is False
                 else f"{'enabled':<30} {_warn('recommend disabling')}"
                 if turbo is True
                 else f"{'unknown':<30} {_info('could not detect')}"))
        if findings["cstates"]:
            _raw(f"  C-states  : {', '.join(findings['cstates'])}")

    # --- Memory ---
    _section("Memory")
    findings["memory_gb"] = detect_memory_gb()
    findings["swap_active"] = detect_swap_active()
    findings["thp"] = detect_thp()

    swap_ok = not findings["swap_active"]
    thp = findings["thp"] or "unknown"
    thp_ok = thp in ("madvise", "never")

    if verbose:
        _raw(f"  RAM       : {findings['memory_gb']} GB")
        print(f"  {'Swap':<18} "
              + (f"{'not active':<30} {_ok('')}" if swap_ok
                 else f"{'ACTIVE':<30} {_warn('may add I/O noise')}"))
        print(f"  {'THP':<18} {thp:<30} "
              + (_ok("") if thp_ok else _warn("recommend 'madvise' or 'never'")))

    # --- Thermal ---
    _section("Thermal")
    temps = detect_temperatures()
    findings["temps"] = temps
    findings["temp_current"] = max(temps) if temps else None
    findings["cooldown_temp"] = recommend_cooldown_temp()
    temp_found = bool(temps)

    if verbose:
        if temp_found:
            _raw(f"  Zones     : {len(temps)} thermal zone(s)")
            _raw(f"  Current   : {findings['temp_current']:.1f}°C "
                 f"  (cooldown threshold → {findings['cooldown_temp']:.0f}°C)")
            print(f"  {'Sensors':<18} {'found':<30} {_ok('')}")
        else:
            print(f"  {'Sensors':<18} {'not found':<30} "
                  f"{_info('cooldown will use CPU% only')}")

    # --- Display ---
    _section("Display")
    findings["backlight_found"], findings["brightness_pct"] = detect_brightness_pct()
    if verbose:
        if findings["backlight_found"]:
            pct = findings["brightness_pct"]
            print(f"  {'Backlight':<18} {'found':<30} {_ok('')}")
            print(f"  {'Brightness':<18} {str(pct) + '%':<30} "
                  + (_ok("already 0") if pct == 0
                     else _warn("will set to 0% during run")))
        else:
            print(f"  {'Backlight':<18} {'not found':<30} {_info('headless/server')}")

    # --- Services ---
    _section("Services (noisy candidates)")
    findings["active_services"] = detect_active_services(NOISY_SERVICES)
    if verbose:
        if findings["active_services"]:
            for svc in findings["active_services"]:
                print(f"  {svc:<35} {_warn('will stop during run')}")
        else:
            _raw(f"  {_ok('No noisy services running')}")

    # --- Tools ---
    _section("Tools & Environment")
    findings["sudo"] = _sudo_available()
    findings["cpupower"] = _tool_available("cpupower")
    findings["docker"] = detect_docker()
    findings["scaphandre"] = detect_scaphandre_version()
    findings["rapl"] = detect_rapl()
    findings["cgroup_v2"] = detect_cgroup_v2()

    if verbose:
        print(f"  {'sudo':<18} "
              + (f"{'available':<30} {_ok('')}" if findings["sudo"]
                 else f"{'not available':<30} {_warn('full isolation requires sudo')}"))
        print(f"  {'cpupower':<18} "
              + (f"{'found':<30} {_ok('')}" if findings["cpupower"]
                 else f"{'not found':<30} {_warn('install: apt install linux-cpupower')}"))
        print(f"  {'docker':<18} "
              + (f"{'found':<30} {_ok('')}" if findings["docker"]
                 else f"{'NOT FOUND':<30} {_bad('required — install docker')}"))
        scap = findings["scaphandre"] or "not found"
        print(f"  {'scaphandre':<18} {scap:<30} "
              + (_ok("") if findings["scaphandre"]
                 else _bad("required — install scaphandre")))
        print(f"  {'RAPL':<18} "
              + (f"{'available':<30} {_ok('idle baseline supported')}" if findings["rapl"]
                 else f"{'not available':<30} {_info('baseline will be skipped')}"))
        print(f"  {'cgroups v2':<18} "
              + (f"{'yes':<30} {_info('Scaphandre uses cgroup fallback')}" if findings["cgroup_v2"]
                 else f"{'no (v1)':<30} {_ok('direct container attribution')}"))

    # --- Isolation level recommendation ---
    _section("Recommendation")
    if findings["sudo"] and findings["cpupower"]:
        findings["isolation_level"] = "full"
    elif not findings["sudo"]:
        findings["isolation_level"] = "basic"
    else:
        findings["isolation_level"] = "basic"

    findings["runs"] = recommend_runs()

    if verbose:
        level = findings["isolation_level"]
        level_colour = _ok if level == "full" else _warn
        print(f"  {'Isolation level':<18} {level:<30} {level_colour('')}")
        _raw(f"  Runs/config : {findings['runs']} "
             f"(gives ~95% CI with typical Scaphandre variance)")
        _raw(f"  Filter model: not set — run model_selector after first data collection")
        print()

    return findings


# ---------------------------------------------------------------------------
# bench.config writer
# ---------------------------------------------------------------------------

CONFIG_TEMPLATE = """\
# bench.config — generated by tools/bench_profile.py
# Machine : {hostname}
# OS      : {os}
# Kernel  : {kernel}
# CPU     : {cpu_model}
#
# Edit freely. Re-run:  python tools/bench_profile.py
# to regenerate with updated hardware detections.

[isolation]
# Isolation level applied before each benchmark session.
# none  : touch nothing (current behaviour, zero overhead)
# basic : no sudo — screensaver, sync, brightness, swap check
# full  : sudo required — governor, turbo, services, caches, THP, WiFi
level = {isolation_level}

[cpu]
# CPU frequency governor to set before measurement.
# 'performance' eliminates frequency scaling as a noise source.
# Requires cpupower (apt install linux-cpupower) and sudo.
governor = performance

# Disable Intel Turbo Boost / AMD Boost before measurement.
# Burst clocks cause ±15-20% energy variance on short runs.
disable_turbo = {disable_turbo}

# Force CPU to stay in C0 (active) state only.
# Set false if runs are long (>60s) — C-states matter less there.
disable_cstates = false

# Pin benchmark container to specific CPU cores (e.g. 0-3).
# Leave empty to use all cores (default — good for most setups).
cpuset =

[memory]
# Set Transparent Huge Pages to 'never' before measurement.
# Only needed if THP is currently 'always'.
disable_thp = {disable_thp}

# Abort run if swap is active. Swap I/O adds unpredictable latency.
# Set to false to warn instead of abort.
check_swap = true

# Flush kernel page cache before each run.
# Ensures each run starts from the same memory state.
drop_caches = true

[display]
# Set screen brightness to this % before measurement (0 = off).
# Leave empty to skip brightness control (headless/server).
brightness = {brightness}

# Disable screensaver and DPMS to prevent blank/suspend mid-run.
disable_screensaver = true

[services]
# Systemd services to stop before measurement and restart after.
# These are known to add CPU/I/O noise. Never include docker or networking.
stop_before_run = {active_services_csv}

[thermal]
# Wait between runs until CPU temperature drops below this (°C).
cooldown_temp_c = {cooldown_temp}

# Wait between runs until CPU idle rises above (100 - this) %.
# E.g. cooldown_cpu_pct = 5 means wait until CPU usage < 5%.
cooldown_cpu_pct = 5

# Give up waiting for cooldown after this many seconds and warn.
cooldown_timeout_s = 120

[measurement]
# Number of times each container is benchmarked per session.
# More runs = narrower confidence interval.
# 10 gives ~95% CI at typical Scaphandre variance levels.
runs = {runs}

# Seconds of idle power measurement before each run (RAPL baseline).
# Set to 0 to skip if RAPL is not available or sudo not usable.
baseline_duration_s = {baseline_duration}

# Confidence level for interval computation (0.95 = 95%).
confidence = 0.95

[filter]
# Outlier filter applied to raw per-run Scaphandre power samples.
# IMPORTANT: do not set this manually — run model_selector first:
#   python tools/model_selector.py --input output/*.json
# The model selector validates the best filter on YOUR machine data
# and updates this value automatically.
#
# Choices: none | iqr | hampel | isolation_forest | lof | elliptic | dbscan
model = none

# Parameters for each model (only the active model's params are used).
contamination = 0.1
iqr_factor = 1.5
hampel_window = 7
hampel_threshold = 1.5
dbscan_eps = 150
dbscan_minpts = 10
lof_neighbors = 20

[cross_run_filter]
# Reject full runs that are outliers across the N repetitions.
# Uses IQR on total energy values across runs.
model = iqr
factor = 1.5

[agent]
# Connection to the benchmark machine.
# host = localhost  → benchmarks run on this machine (GUI will pause during run)
# host = <IP/hostname> → benchmarks run on remote server (SSH tunnel required)
host = localhost
port = 22
user = {user}

[gui]
# Port the web GUI listens on.
port = 8080
"""


def write_config(findings: dict, path: str) -> None:
    """Write bench.config from profiler findings."""

    active_csv = ",".join(findings.get("active_services", [])) or ""
    brightness = findings["brightness_pct"] if findings["backlight_found"] else ""
    baseline = 10 if findings.get("rapl") else 0
    disable_turbo = "true" if findings.get("turbo") is True else "false"
    thp = findings.get("thp", "madvise")
    disable_thp = "true" if thp == "always" else "false"

    content = CONFIG_TEMPLATE.format(
        hostname=findings.get("hostname", "unknown"),
        os=findings.get("os", "unknown"),
        kernel=findings.get("kernel", "unknown"),
        cpu_model=findings.get("cpu_model", "unknown"),
        isolation_level=findings.get("isolation_level", "basic"),
        disable_turbo=disable_turbo,
        disable_thp=disable_thp,
        brightness=brightness,
        active_services_csv=active_csv,
        cooldown_temp=int(findings.get("cooldown_temp", 50)),
        runs=findings.get("runs", 10),
        baseline_duration=baseline,
        user=os.getenv("USER", "user"),
    )

    with open(path, "w") as f:
        f.write(content)


# ---------------------------------------------------------------------------
# Config loader (bench.config → env vars → CLI args)
# ---------------------------------------------------------------------------

def load_config(path: str = DEFAULT_CONFIG_PATH) -> configparser.ConfigParser:
    """
    Load bench.config. Returns a ConfigParser with defaults pre-populated
    so callers can safely read any key without KeyError.
    """
    cfg = configparser.ConfigParser()
    if os.path.exists(path):
        cfg.read(path)
    return cfg


def resolve(cfg: configparser.ConfigParser,
            section: str,
            key: str,
            env_var: str,
            cli_val=None,
            fallback=None):
    """
    Resolve a config value using the three-layer priority:
      CLI arg (highest) → env var → bench.config → fallback (lowest)

    Parameters
    ----------
    cfg      : loaded ConfigParser
    section  : config section name
    key      : config key name
    env_var  : environment variable name (e.g. "BENCH_RUNS")
    cli_val  : value from argparse (None if not provided by user)
    fallback : default if nothing else is set
    """
    if cli_val is not None:
        return cli_val
    env = os.environ.get(env_var)
    if env is not None:
        return env
    try:
        return cfg.get(section, key)
    except (configparser.NoSectionError, configparser.NoOptionError):
        return fallback


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Scan machine, report benchmark environment, write bench.config"
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Print report only — do not write bench.config"
    )
    parser.add_argument(
        "--out", default=DEFAULT_CONFIG_PATH, metavar="PATH",
        help=f"Config output path (default: {DEFAULT_CONFIG_PATH})"
    )
    parser.add_argument(
        "--quiet", action="store_true",
        help="Suppress report output — only write the config file"
    )
    args = parser.parse_args()

    findings = run_profile(verbose=not args.quiet)

    if args.dry_run:
        print(f"\n{_Y}--dry-run: bench.config not written.{_NC}\n")
        return

    write_config(findings, args.out)

    if not args.quiet:
        print(f"{_G}bench.config written to:{_NC} {args.out}\n")
        print("Next steps:")
        print("  1. Review bench.config and adjust if needed")
        print("  2. make run")
        print("  3. python tools/model_selector.py --input output/*.json")
        print()


if __name__ == "__main__":
    main()
