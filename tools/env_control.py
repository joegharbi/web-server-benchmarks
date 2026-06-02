"""
Environment control for clean benchmark measurements.

IsolationContext is a context manager that:
  1. Saves the current machine state
  2. Applies isolation controls (governor, turbo, services, brightness, etc.)
  3. Writes a crash-recovery shell script to /tmp/bench_restore.sh
  4. On __exit__ (even on exception): restores everything

Isolation levels
----------------
none   : touch nothing — current behaviour, zero overhead
basic  : no sudo required — screensaver, sync, swap check, brightness if writable
full   : sudo required   — governor, turbo, services, caches, THP, WiFi power save

Usage
-----
from tools.env_control import IsolationContext

with IsolationContext(level="full") as ctx:
    ctx.report()          # print what was changed
    ctx.measure_idle(10)  # baseline power for 10 s
    ctx.cooldown()        # wait for temp + CPU to settle
    run_benchmarks()
# restores everything on exit, even if an exception is raised
"""

import os
import re
import glob
import time
import signal
import shutil
import subprocess
import statistics
from dataclasses import dataclass, field
from typing import Optional

RESTORE_SCRIPT = "/tmp/bench_restore.sh"

# Services known to add measurement noise — never include networking or Docker
NOISY_SERVICES = [
    "cups.service",
    "bluetooth.service",
    "avahi-daemon.service",
    "packagekit.service",
    "snapd.service",
    "apt-daily.service",
    "apt-daily-upgrade.service",
    "man-db.timer",
    "fwupd.service",
]

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

def _run(cmd: list[str], sudo: bool = False, check: bool = False) -> subprocess.CompletedProcess:
    if sudo:
        cmd = ["sudo"] + cmd
    return subprocess.run(cmd, capture_output=True, text=True, check=check)


def _write(path: str, value: str, sudo: bool = False) -> bool:
    """Write value to a sysfs path. Returns True on success."""
    try:
        if sudo:
            result = subprocess.run(
                ["sudo", "tee", path],
                input=value, capture_output=True, text=True
            )
            return result.returncode == 0
        else:
            with open(path, "w") as f:
                f.write(value)
            return True
    except (OSError, PermissionError):
        return False


def _read(path: str) -> Optional[str]:
    """Read a sysfs path. Returns None if unavailable."""
    try:
        with open(path) as f:
            return f.read().strip()
    except (OSError, IOError):
        return None


def _sudo_available() -> bool:
    result = subprocess.run(
        ["sudo", "-n", "true"], capture_output=True
    )
    return result.returncode == 0


def _tool_available(name: str) -> bool:
    return shutil.which(name) is not None


# ---------------------------------------------------------------------------
# Change log entry
# ---------------------------------------------------------------------------

@dataclass
class Change:
    what: str        # human-readable description
    restore_cmd: str # shell command to restore (written to bench_restore.sh)
    restored: bool = False


# ---------------------------------------------------------------------------
# IsolationContext
# ---------------------------------------------------------------------------

class IsolationContext:
    """
    Context manager for benchmark environment isolation.

    Parameters
    ----------
    level        : "none" | "basic" | "full"
    gui_host     : host value from bench.config [agent] host
                   if "localhost" the GUI server is stopped before measurement
    gui_port     : port the GUI server listens on
    services     : list of systemd units to stop (overrides NOISY_SERVICES)
    cooldown_temp: max °C before a run starts
    cooldown_cpu : max CPU% before a run starts
    cooldown_timeout: give up waiting after this many seconds
    brightness   : screen brightness to set (0-100, or None to skip)
    cpuset       : CPU cores to pin container to (e.g. "0-3")
    verbose      : print each action as it happens
    """

    def __init__(
        self,
        level: str = "basic",
        gui_host: str = "localhost",
        gui_port: int = 8080,
        services: Optional[list[str]] = None,
        cooldown_temp: float = 50.0,
        cooldown_cpu: float = 5.0,
        cooldown_timeout: int = 120,
        brightness: Optional[int] = 0,
        cpuset: Optional[str] = None,
        verbose: bool = True,
    ):
        if level not in ("none", "basic", "full"):
            raise ValueError(f"level must be 'none', 'basic', or 'full', got '{level}'")
        self.level = level
        self.gui_host = gui_host
        self.gui_port = gui_port
        self.services = services if services is not None else NOISY_SERVICES
        self.cooldown_temp = cooldown_temp
        self.cooldown_cpu = cooldown_cpu
        self.cooldown_timeout = cooldown_timeout
        self.brightness = brightness
        self.cpuset = cpuset
        self.verbose = verbose

        self._changes: list[Change] = []
        self._sudo = _sudo_available()
        self._gui_pid: Optional[int] = None

    # ------------------------------------------------------------------
    # Context manager
    # ------------------------------------------------------------------

    def __enter__(self):
        if self.level == "none":
            return self
        self._setup()
        self._write_restore_script()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.level == "none":
            return False
        self._teardown()
        self._remove_restore_script()
        return False  # do not suppress exceptions

    # ------------------------------------------------------------------
    # Setup
    # ------------------------------------------------------------------

    def _setup(self):
        self._log("=== IsolationContext: setup BEGIN ===")

        # Always: stop GUI server if running locally
        if self.gui_host in ("localhost", "127.0.0.1"):
            self._stop_gui_server()

        # basic + full
        self._disable_screensaver()
        self._sync_disk()
        self._check_swap()
        self._set_brightness(self.brightness)

        if self.level == "full":
            if not self._sudo:
                self._log("WARNING: sudo not available — full isolation requires sudo. "
                          "Falling back to basic.")
            else:
                self._set_governor("performance")
                self._disable_turbo()
                self._disable_thp()
                self._drop_caches()
                self._stop_services()
                self._set_wifi_powersave(False)

        self._log("=== IsolationContext: setup DONE ===")

    # ------------------------------------------------------------------
    # Teardown
    # ------------------------------------------------------------------

    def _teardown(self):
        self._log("=== IsolationContext: teardown BEGIN ===")
        # Restore in reverse order
        for change in reversed(self._changes):
            if not change.restored and change.restore_cmd:
                result = subprocess.run(
                    change.restore_cmd, shell=True,
                    capture_output=True, text=True
                )
                change.restored = True
                ok = "✓" if result.returncode == 0 else "✗"
                self._log(f"  {ok} restored: {change.what}")

        # Restart GUI server last
        if self.gui_host in ("localhost", "127.0.0.1") and self._gui_pid is not None:
            self._start_gui_server()

        self._log("=== IsolationContext: teardown DONE ===")

    # ------------------------------------------------------------------
    # Individual controls
    # ------------------------------------------------------------------

    def _stop_gui_server(self):
        """Stop the FastAPI GUI server if it is running on this machine."""
        result = _run(["pgrep", "-f", "tools/gui/server.py"])
        if result.returncode != 0:
            return  # not running
        pids = result.stdout.strip().split()
        if not pids:
            return
        self._gui_pid = int(pids[0])
        try:
            os.kill(self._gui_pid, signal.SIGTERM)
            time.sleep(1)
            self._log(f"  stopped GUI server (PID {self._gui_pid})")
        except ProcessLookupError:
            pass

    def _start_gui_server(self):
        """Restart the GUI server after measurement."""
        if not _tool_available("uvicorn"):
            return
        subprocess.Popen(
            ["srv/bin/python3", "-m", "uvicorn",
             "tools.gui.server:app", "--host", "0.0.0.0",
             "--port", str(self.gui_port)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        self._log(f"  restarted GUI server on port {self.gui_port}")

    def _set_governor(self, governor: str):
        if not _tool_available("cpupower"):
            self._log("  SKIP: cpupower not found — install linux-cpupower")
            return
        result = _run(["cpupower", "frequency-info", "-p"], check=False)
        current = "unknown"
        for line in result.stdout.splitlines():
            if "The governor" in line:
                match = re.search(r'"(\w+)"', line)
                if match:
                    current = match.group(1)
        ok = _run(["cpupower", "frequency-set", "-g", governor], sudo=True)
        if ok.returncode == 0:
            self._record(
                f"CPU governor → {governor} (was {current})",
                f"sudo cpupower frequency-set -g {current}"
            )
        else:
            self._log(f"  WARN: could not set governor to {governor}")

    def _disable_turbo(self):
        # Intel path
        intel_path = "/sys/devices/system/cpu/intel_pstate/no_turbo"
        amd_path   = "/sys/devices/system/cpu/cpufreq/boost"

        if os.path.exists(intel_path):
            current = _read(intel_path)
            if current == "1":
                self._log("  turbo already disabled (Intel)")
                return
            if _write(intel_path, "1\n", sudo=True):
                self._record(
                    "Intel Turbo Boost disabled",
                    f"echo 0 | sudo tee {intel_path}"
                )
            else:
                self._log("  WARN: could not disable Intel Turbo")

        elif os.path.exists(amd_path):
            current = _read(amd_path)
            if current == "0":
                self._log("  turbo already disabled (AMD)")
                return
            if _write(amd_path, "0\n", sudo=True):
                self._record(
                    "AMD Boost disabled",
                    f"echo 1 | sudo tee {amd_path}"
                )
            else:
                self._log("  WARN: could not disable AMD Boost")
        else:
            self._log("  SKIP: no turbo control path found")

    def _disable_thp(self):
        path = "/sys/kernel/mm/transparent_hugepage/enabled"
        current = _read(path)
        if current is None:
            self._log("  SKIP: THP sysfs path not found")
            return
        # Extract active setting from e.g. "always [madvise] never"
        match = re.search(r'\[(\w+)\]', current)
        active = match.group(1) if match else current
        if active == "never":
            self._log("  THP already 'never'")
            return
        if _write(path, "never\n", sudo=True):
            self._record(
                f"THP disabled (was {active})",
                f"echo {active} | sudo tee {path}"
            )
        else:
            self._log("  WARN: could not disable THP")

    def _sync_disk(self):
        subprocess.run(["sync"], check=False)
        self._log("  disk sync done")

    def _drop_caches(self):
        path = "/proc/sys/vm/drop_caches"
        if _write(path, "3\n", sudo=True):
            self._log("  page caches dropped")
        else:
            self._log("  WARN: could not drop caches (needs sudo)")

    def _check_swap(self):
        if not _tool_available("swapon"):
            # Fall back to /proc/swaps which is always available on Linux
            val = _read("/proc/swaps")
            if val and len(val.splitlines()) > 1:
                self._log("  WARNING: swap is active (/proc/swaps) — may add noise")
            else:
                self._log("  swap: not active ✓")
            return
        result = _run(["swapon", "--show", "--noheadings"])
        if result.stdout.strip():
            self._log(f"  WARNING: swap is active — may add noise:\n{result.stdout.strip()}")
        else:
            self._log("  swap: not active ✓")

    def _disable_screensaver(self):
        # Try gnome
        if _tool_available("gsettings"):
            for schema, key, off_val, on_val in [
                ("org.gnome.desktop.screensaver", "lock-enabled", "false", "true"),
                ("org.gnome.settings-daemon.plugins.power", "sleep-display-ac", "0", None),
            ]:
                try:
                    current = subprocess.run(
                        ["gsettings", "get", schema, key],
                        capture_output=True, text=True
                    ).stdout.strip()
                    subprocess.run(
                        ["gsettings", "set", schema, key, off_val],
                        capture_output=True
                    )
                    restore_val = on_val or current
                    self._record(
                        f"gnome screensaver/sleep disabled ({schema} {key})",
                        f"gsettings set {schema} {key} {restore_val}"
                    )
                except Exception:
                    pass

        # Try xset (X11)
        if _tool_available("xset") and os.environ.get("DISPLAY"):
            subprocess.run(["xset", "s", "off"], capture_output=True)
            subprocess.run(["xset", "-dpms"], capture_output=True)
            self._record(
                "xset screensaver/DPMS disabled",
                "xset s on && xset +dpms"
            )

    def _set_brightness(self, target: Optional[int]):
        if target is None:
            return
        paths = glob.glob("/sys/class/backlight/*/brightness")
        max_paths = glob.glob("/sys/class/backlight/*/max_brightness")
        if not paths:
            self._log("  SKIP: no backlight found")
            return
        path = paths[0]
        max_path = max_paths[0] if max_paths else None
        current_raw = _read(path)
        max_raw = _read(max_path) if max_path else None
        if current_raw is None or max_raw is None:
            return
        current_raw_int = int(current_raw)
        max_val = int(max_raw)
        target_raw = int((target / 100.0) * max_val)
        # Try without sudo first (some systems allow user writes)
        if not _write(path, f"{target_raw}\n", sudo=False):
            _write(path, f"{target_raw}\n", sudo=True)
        self._record(
            f"brightness → {target}% (raw {target_raw}, was {current_raw_int})",
            f"echo {current_raw_int} | sudo tee {path} || echo {current_raw_int} > {path}"
        )

    def _stop_services(self):
        for svc in self.services:
            result = _run(["systemctl", "is-active", "--quiet", svc])
            if result.returncode != 0:
                continue  # not running, skip
            stop = _run(["systemctl", "stop", svc], sudo=True)
            if stop.returncode == 0:
                self._record(
                    f"stopped {svc}",
                    f"sudo systemctl start {svc}"
                )
            else:
                self._log(f"  WARN: could not stop {svc}")

    def _set_wifi_powersave(self, enabled: bool):
        if not _tool_available("iw"):
            self._log("  SKIP: iw not found")
            return
        result = _run(["iw", "dev"])
        interfaces = re.findall(r"Interface\s+(\S+)", result.stdout)
        for iface in interfaces:
            state = "on" if enabled else "off"
            ok = _run(["iw", "dev", iface, "set", "power_save", state], sudo=True)
            if ok.returncode == 0:
                restore_state = "on" if not enabled else "off"
                self._record(
                    f"WiFi power save {state} on {iface}",
                    f"sudo iw dev {iface} set power_save {restore_state}"
                )

    # ------------------------------------------------------------------
    # Temperature and cooldown
    # ------------------------------------------------------------------

    def read_temperatures(self) -> list[float]:
        """Read all available CPU thermal zone temperatures in °C."""
        temps = []
        for path in glob.glob("/sys/class/thermal/thermal_zone*/temp"):
            val = _read(path)
            if val and val.isdigit():
                temps.append(int(val) / 1000.0)
        return temps

    def current_temp(self) -> Optional[float]:
        """Return max temperature across all thermal zones, or None."""
        temps = self.read_temperatures()
        return max(temps) if temps else None

    def current_cpu_idle(self) -> float:
        """Return approximate CPU idle % using /proc/stat (no external tools)."""
        def _read_stat():
            line = _read("/proc/stat")
            if not line:
                return None
            for l in line.splitlines():  # noqa: E741
                if l.startswith("cpu "):
                    parts = l.split()
                    vals = [int(x) for x in parts[1:]]
                    total = sum(vals)
                    idle = vals[3] + (vals[4] if len(vals) > 4 else 0)
                    return total, idle
            return None

        s1 = _read_stat()
        if s1 is None:
            return 100.0
        time.sleep(0.5)
        s2 = _read_stat()
        if s2 is None:
            return 100.0
        total_diff = s2[0] - s1[0]
        idle_diff  = s2[1] - s1[1]
        if total_diff == 0:
            return 100.0
        return (idle_diff / total_diff) * 100.0

    def cooldown(self) -> bool:
        """
        Wait until CPU idle > cooldown_cpu% AND temp < cooldown_temp°C.
        Returns True if settled within timeout, False if timed out.
        """
        self._log(
            f"  cooldown: waiting for CPU idle > {self.cooldown_cpu}% "
            f"and temp < {self.cooldown_temp}°C "
            f"(timeout {self.cooldown_timeout}s)"
        )
        deadline = time.time() + self.cooldown_timeout
        while time.time() < deadline:
            idle = self.current_cpu_idle()
            temp = self.current_temp()
            temp_ok = temp is None or temp < self.cooldown_temp
            cpu_ok  = idle > (100.0 - self.cooldown_cpu)
            if temp_ok and cpu_ok:
                self._log(
                    f"  cooldown done — CPU idle {idle:.1f}%, "
                    f"temp {temp:.1f}°C" if temp else f"  cooldown done — CPU idle {idle:.1f}%"
                )
                return True
            time.sleep(2)
        self._log(f"  WARNING: cooldown timed out after {self.cooldown_timeout}s")
        return False

    # ------------------------------------------------------------------
    # Idle baseline measurement
    # ------------------------------------------------------------------

    def measure_idle(self, duration: int = 10) -> float:
        """
        Measure idle power draw for `duration` seconds by reading
        /sys/class/powercap/intel-rapl (RAPL) if available, otherwise
        return 0.0 (caller should skip baseline subtraction).

        Returns average idle power in watts.
        """
        rapl_paths = glob.glob(
            "/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj"
        )
        if not rapl_paths:
            self._log("  SKIP: RAPL not available for idle baseline")
            return 0.0

        path = rapl_paths[0]

        def _read_uj():
            v = _read(path)
            return int(v) if v and v.isdigit() else None

        e1 = _read_uj()
        if e1 is None:
            return 0.0
        time.sleep(duration)
        e2 = _read_uj()
        if e2 is None:
            return 0.0

        # Handle RAPL counter wrap-around (max ~262 kJ on most Intel CPUs)
        diff_uj = e2 - e1
        if diff_uj < 0:
            max_uj = int(_read(rapl_paths[0].replace("energy_uj", "max_energy_range_uj")) or 0)
            diff_uj += max_uj

        watts = (diff_uj / 1e6) / duration
        self._log(f"  idle baseline: {watts:.3f} W over {duration}s")
        return watts

    # ------------------------------------------------------------------
    # Reporting
    # ------------------------------------------------------------------

    def report(self):
        """Print a table of all changes made during setup."""
        if not self._changes:
            print("[IsolationContext] No changes made (level=none or nothing to change)")
            return
        print("[IsolationContext] Changes applied:")
        for c in self._changes:
            status = "✓ active" if not c.restored else "✓ restored"
            print(f"  {status}  {c.what}")
        temp = self.current_temp()
        idle = self.current_cpu_idle()
        print(f"[IsolationContext] Current state: "
              f"temp={temp:.1f}°C  " if temp else "[IsolationContext] Current state: temp=N/A  ",
              end="")
        print(f"CPU idle={idle:.1f}%")

    # ------------------------------------------------------------------
    # Crash recovery script
    # ------------------------------------------------------------------

    def _write_restore_script(self):
        lines = ["#!/bin/bash", "# Auto-generated by env_control.py — run if benchmark crashed", ""]
        for c in self._changes:
            if c.restore_cmd:
                lines.append(f"# restore: {c.what}")
                lines.append(c.restore_cmd)
                lines.append("")
        # Always restart GUI server at end of restore script
        if self.gui_host in ("localhost", "127.0.0.1"):
            lines.append("# restart GUI server")
            lines.append(
                f"nohup srv/bin/python3 -m uvicorn tools.gui.server:app "
                f"--host 0.0.0.0 --port {self.gui_port} &"
            )
        script = "\n".join(lines)
        try:
            with open(RESTORE_SCRIPT, "w") as f:
                f.write(script)
            os.chmod(RESTORE_SCRIPT, 0o755)
            self._log(f"  crash-recovery script written to {RESTORE_SCRIPT}")
        except OSError:
            self._log(f"  WARN: could not write {RESTORE_SCRIPT}")

    def _remove_restore_script(self):
        try:
            os.remove(RESTORE_SCRIPT)
        except OSError:
            pass

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _record(self, what: str, restore_cmd: str):
        self._changes.append(Change(what=what, restore_cmd=restore_cmd))
        self._log(f"  ✓ {what}")

    def _log(self, msg: str):
        if self.verbose:
            print(f"[env_control] {msg}", flush=True)


# ---------------------------------------------------------------------------
# Standalone system info (used by bench_profile.py)
# ---------------------------------------------------------------------------

def detect_governor() -> Optional[str]:
    """Return current CPU frequency governor or None."""
    paths = glob.glob("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor")
    if paths:
        return _read(paths[0])
    result = _run(["cpupower", "frequency-info", "-p"])
    match = re.search(r'"(\w+)"', result.stdout)
    return match.group(1) if match else None


def detect_turbo() -> Optional[bool]:
    """Return True if turbo/boost is enabled, False if disabled, None if unknown."""
    intel = _read("/sys/devices/system/cpu/intel_pstate/no_turbo")
    if intel is not None:
        return intel.strip() == "0"
    amd = _read("/sys/devices/system/cpu/cpufreq/boost")
    if amd is not None:
        return amd.strip() == "1"
    return None


def detect_thp() -> Optional[str]:
    """Return current THP setting: 'always', 'madvise', 'never', or None."""
    val = _read("/sys/kernel/mm/transparent_hugepage/enabled")
    if val is None:
        return None
    match = re.search(r'\[(\w+)\]', val)
    return match.group(1) if match else val


def detect_swap_active() -> bool:
    """Return True if any swap is active."""
    if _tool_available("swapon"):
        result = _run(["swapon", "--show", "--noheadings"])
        return bool(result.stdout.strip())
    # fallback: /proc/swaps header is always present; data lines mean swap active
    val = _read("/proc/swaps")
    return bool(val and len(val.splitlines()) > 1)


def detect_temperatures() -> list[float]:
    """Return all CPU thermal zone temperatures in °C."""
    temps = []
    for path in glob.glob("/sys/class/thermal/thermal_zone*/temp"):
        val = _read(path)
        if val and val.isdigit():
            temps.append(int(val) / 1000.0)
    return temps


def detect_backlight() -> Optional[tuple[str, int, int]]:
    """Return (path, current_raw, max_raw) or None if no backlight found."""
    paths = glob.glob("/sys/class/backlight/*/brightness")
    if not paths:
        return None
    path = paths[0]
    base = os.path.dirname(path)
    current = _read(path)
    max_val = _read(os.path.join(base, "max_brightness"))
    if current and max_val:
        return path, int(current), int(max_val)
    return None


def detect_active_services(candidates: list[str] = NOISY_SERVICES) -> list[str]:
    """Return which candidate services are currently running."""
    active = []
    for svc in candidates:
        result = _run(["systemctl", "is-active", "--quiet", svc])
        if result.returncode == 0:
            active.append(svc)
    return active


def detect_rapl() -> bool:
    """Return True if Intel RAPL energy counter is available."""
    return bool(glob.glob("/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj"))


def detect_scaphandre_version() -> Optional[str]:
    """Return Scaphandre version string or None."""
    if not _tool_available("scaphandre"):
        return None
    result = _run(["scaphandre", "--version"])
    return result.stdout.strip() or result.stderr.strip() or "unknown"
