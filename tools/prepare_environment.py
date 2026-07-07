#!/usr/bin/env python3
"""Apply steady-measurement settings, then restore them afterwards (needs sudo).

Two actions:
  apply    - save the current state, then set the CPU governor to performance,
             turn turbo off, and stop other running Docker containers.
  restore  - read the saved state and put everything back: the governor, turbo,
             and the containers that were stopped.

It records what it changed in a small state file, so restore undoes exactly what
apply did and nothing more. Read-only checking is in tools/check_environment.py;
this tool changes the system, so it needs root. Run it with sudo.

It does not touch swap or time sync, which are riskier to flip automatically; the
check tool flags those so you can decide. Standard library only.

  sudo python3 tools/prepare_environment.py apply
  ...run the measurement campaign...
  sudo python3 tools/prepare_environment.py restore
"""
import argparse
import glob
import json
import os
import subprocess
import sys
import tempfile

DEFAULT_STATE = os.path.join(tempfile.gettempdir(), "wseb_env_state.json")
GOV_GLOB = "/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor"
INTEL_NO_TURBO = "/sys/devices/system/cpu/intel_pstate/no_turbo"
BOOST = "/sys/devices/system/cpu/cpufreq/boost"


def read(path):
    try:
        with open(path, encoding="utf-8") as f:
            return f.read().strip()
    except OSError:
        return None


def write(path, value):
    try:
        with open(path, "w", encoding="utf-8") as f:
            f.write(value)
        return True
    except OSError as e:
        print(f"  could not write {path}: {e}")
        return False


def require_root():
    if hasattr(os, "geteuid") and os.geteuid() != 0:
        sys.exit("This changes system settings, so it needs root. Run it with sudo.")


def docker_running():
    try:
        out = subprocess.run(["docker", "ps", "--format", "{{.Names}}"],
                             capture_output=True, text=True, timeout=10)
        return [n for n in out.stdout.splitlines() if n.strip()]
    except Exception:
        return []


def do_apply(args):
    require_root()
    state = {"governors": {}, "turbo": None, "stopped_containers": []}

    gov_files = sorted(glob.glob(GOV_GLOB))
    for p in gov_files:
        state["governors"][p] = read(p)
    if gov_files:
        changed = sum(1 for p in gov_files if write(p, args.governor))
        print(f"CPU governor: set {changed}/{len(gov_files)} cores to '{args.governor}'")
    else:
        print("CPU governor: not available, skipped")

    if read(INTEL_NO_TURBO) is not None:
        state["turbo"] = {"path": INTEL_NO_TURBO, "prev": read(INTEL_NO_TURBO)}
        write(INTEL_NO_TURBO, "1")
        print("Turbo (Intel): off")
    elif read(BOOST) is not None:
        state["turbo"] = {"path": BOOST, "prev": read(BOOST)}
        write(BOOST, "0")
        print("Turbo (boost): off")
    else:
        print("Turbo: not available, skipped")

    keep = {n.strip() for n in (args.keep or "").split(",") if n.strip()}
    to_stop = [n for n in docker_running() if n not in keep]
    if to_stop:
        subprocess.run(["docker", "stop"] + to_stop, capture_output=True, text=True)
        state["stopped_containers"] = to_stop
        print(f"Stopped containers: {', '.join(to_stop)}")
    else:
        print("Containers: none to stop")

    with open(args.state, "w", encoding="utf-8") as f:
        json.dump(state, f)
    print(f"\nSaved previous state to {args.state}")
    print("Run 'restore' after your measurements to put everything back.")


def do_restore(args):
    require_root()
    if not os.path.isfile(args.state):
        sys.exit(f"No saved state at {args.state}; nothing to restore.")
    with open(args.state, encoding="utf-8") as f:
        state = json.load(f)

    restored = sum(1 for p, prev in (state.get("governors") or {}).items() if prev and write(p, prev))
    print(f"CPU governor: restored {restored} core(s)")

    t = state.get("turbo")
    if t and t.get("prev") is not None:
        write(t["path"], t["prev"])
        print("Turbo: restored")

    stopped = state.get("stopped_containers") or []
    if stopped:
        subprocess.run(["docker", "start"] + stopped, capture_output=True, text=True)
        print(f"Restarted containers: {', '.join(stopped)}")

    os.remove(args.state)
    print(f"\nRestored. Removed {args.state}.")


def main():
    ap = argparse.ArgumentParser(description="Apply/restore steady-measurement settings (needs sudo).")
    ap.add_argument("action", choices=["apply", "restore"])
    ap.add_argument("--governor", default="performance",
                    help="CPU governor to set on apply (default: performance)")
    ap.add_argument("--keep", default="",
                    help="Comma-separated container names to keep running on apply")
    ap.add_argument("--state", default=DEFAULT_STATE,
                    help=f"State file path (default: {DEFAULT_STATE})")
    args = ap.parse_args()
    (do_apply if args.action == "apply" else do_restore)(args)


if __name__ == "__main__":
    main()
