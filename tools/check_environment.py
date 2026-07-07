#!/usr/bin/env python3
"""Check whether the machine is in a good state for energy measurement.

Read-only. It changes nothing. It reports the CPU frequency governor, the
turbo/boost state, swap, the current load, other running containers, and time
sync, and marks each OK or WARN with a short note. Run it before a campaign to
know the machine is quiet and steady.

Applying these settings (the CPU governor and turbo) is a separate sudo step,
Step 3 Part B in docs/IMPROVEMENT_PLAN.md. This tool only looks.

Standard library only.
"""
import glob
import os
import subprocess


def read(path):
    try:
        with open(path, encoding="utf-8") as f:
            return f.read().strip()
    except OSError:
        return None


def run(cmd):
    try:
        return subprocess.run(cmd, capture_output=True, text=True, timeout=5).stdout.strip()
    except Exception:
        return ""


def line(status, name, note=""):
    print(f"  [{status:<4}] {name}" + (f"  ({note})" if note else ""))


def check_governor():
    govs = {g for p in glob.glob("/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor")
            for g in [read(p)] if g}
    if not govs:
        line("--", "CPU governor", "not available on this system")
    elif govs == {"performance"}:
        line("OK", "CPU governor", "performance")
    else:
        line("WARN", "CPU governor", f"{', '.join(sorted(govs))}; 'performance' gives steadier energy")


def check_turbo():
    no_turbo = read("/sys/devices/system/cpu/intel_pstate/no_turbo")
    if no_turbo is not None:
        line("OK" if no_turbo == "1" else "WARN", "Turbo (Intel)",
             "off" if no_turbo == "1" else "on; turning it off reduces variance")
        return
    boost = read("/sys/devices/system/cpu/cpufreq/boost")
    if boost is not None:
        line("OK" if boost == "0" else "WARN", "Turbo (boost)",
             "off" if boost == "0" else "on; turning it off reduces variance")
        return
    line("--", "Turbo", "not available on this system")


def check_swap():
    active = [l for l in (read("/proc/swaps") or "").splitlines()[1:] if l.strip()]
    line("WARN" if active else "OK", "Swap",
         "on; swapping can disturb measurements" if active else "off")


def check_load():
    la = read("/proc/loadavg")
    ncpu = os.cpu_count() or 1
    if not la:
        line("--", "Current load")
        return
    one = float(la.split()[0])
    busy = 100 * one / ncpu >= 20
    line("WARN" if busy else "OK", "Current load",
         f"{one:.2f} over {ncpu} cores" + ("; machine is busy" if busy else ""))


def check_containers():
    names = [n for n in run(["docker", "ps", "--format", "{{.Names}}"]).splitlines() if n.strip()]
    if not names:
        line("OK", "Other containers", "none running")
    else:
        line("WARN", "Running containers", f"{len(names)}: {', '.join(names)}; stop unrelated ones")


def check_ntp():
    out = run(["timedatectl", "show", "-p", "NTP", "--value"])
    if out == "":
        line("--", "Time sync (NTP)", "timedatectl not available")
    elif out == "yes":
        line("WARN", "Time sync (NTP)", "on; off is a little steadier")
    else:
        line("OK", "Time sync (NTP)", "off")


def main():
    print("Measurement environment check (read-only, nothing is changed):\n")
    check_governor()
    check_turbo()
    check_swap()
    check_load()
    check_containers()
    check_ntp()
    print("\nWARN items are suggestions, not errors. Step 3 Part B can set the CPU")
    print("governor and turbo for you (needs sudo). The rest are manual for now.")


if __name__ == "__main__":
    main()
