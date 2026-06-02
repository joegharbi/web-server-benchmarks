import os
import sys
import time
import subprocess
import requests
import csv
import statistics
from concurrent.futures import ThreadPoolExecutor
from collections import Counter
import argparse
import json
import threading
from datetime import datetime
import logging
import psutil

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from tools.stats import apply_filter, summarise_runs, FILTER_MODELS
from tools.bench_profile import load_config, resolve, DEFAULT_CONFIG_PATH
from tools.env_control import IsolationContext, detect_temperatures

logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger()

def is_measure_quiet():
    """Match scripts/run_benchmarks.sh: full measure_docker logs only when BENCH_MEASURE_QUIET is exactly 0.

    Default when unset is quiet (same as bash ``${BENCH_MEASURE_QUIET:-1}``). Any value other than ``0``
    is treated as quiet so bash and Python stay aligned (e.g. typos do not flip to verbose).
    """
    v = (os.environ.get("BENCH_MEASURE_QUIET") or "1").strip()
    return v != "0"

# [MEASURE] uses magenta so it is distinct from bash [PROGRESS] (cyan).
_M_MAGENTA = "\033[0;35m"
_M_GREEN = "\033[0;32m"
_M_NC = "\033[0m"


def http_max_workers_label(args):
    return "System default" if args.max_workers is None else str(int(args.max_workers))


def measure_quiet_msg(body: str) -> None:
    print(f"{_M_MAGENTA}[MEASURE]{_M_NC} {body}", flush=True)


def measure_quiet_heartbeat_interval_sec():
    try:
        return max(10, int(os.environ.get("MEASURE_HEARTBEAT_SEC", "60")))
    except ValueError:
        return 60


results_counter = Counter()
runtime_data = {}
results_lock = threading.Lock()

def get_binary_path(binary_name):
    result = subprocess.run(["which", binary_name], capture_output=True, text=True, check=True)
    return result.stdout.strip() or f"'{binary_name}' not found"

# Required tools and how to install them (shown when missing)
REQUIRED_TOOLS = {
    "docker": "Install Docker (e.g. apt install docker.io) and ensure the docker daemon is running.",
    "scaphandre": "Install Scaphandre (e.g. cargo install scaphandre) and ensure it is in PATH.",
}

def check_prerequisites():
    """Check all required tools are available; exit with error before any measurement if not."""
    missing = []
    for name, install_hint in REQUIRED_TOOLS.items():
        result = subprocess.run(["which", name], capture_output=True, text=True, check=False)
        path = (result.stdout or "").strip()
        if result.returncode != 0 or not path:
            missing.append((name, install_hint))
    if missing:
        logger.error("The following required tools are missing. Please install them before running measurements.")
        for name, install_hint in missing:
            logger.error("  - %s: %s", name, install_hint)
        sys.exit(1)

def send_request(url, request_num, verbose=False):
    try:
        response = requests.get(url, timeout=5)
        if verbose:
            logger.debug(f'{url} "GET / HTTP/1.1" {response.status_code} {len(response.content)}')
        with results_lock:
            if 200 <= response.status_code < 300:
                results_counter['success'] += 1
            else:
                results_counter['failure'] += 1
    except requests.exceptions.RequestException:
        with results_lock:
            results_counter['failure'] += 1
    finally:
        with results_lock:
            results_counter['total'] += 1

def cleanup_existing_container(container_name, docker_path):
    logger.info(f"Cleaning up any existing container named '{container_name}'...")
    subprocess.run([docker_path, "stop", container_name], capture_output=True, text=True, check=False)
    subprocess.run([docker_path, "rm", "-f", container_name], capture_output=True, text=True, check=False)
    # Wait and check if container is really gone
    for _ in range(5):
        result = subprocess.run([docker_path, "ps", "-a", "--filter", f"name={container_name}", "--format", "{{{{.Names}}}}"], capture_output=True, text=True)
        if container_name not in result.stdout:
            break
        time.sleep(1)
    else:
        logger.warning(f"Container '{container_name}' could not be removed after multiple attempts.")
    time.sleep(3)  # Ensure port and resources released (important after long runs)

def cleanup_existing_scaphandre():
    subprocess.run(["sudo", "pkill", "-9", "scaphandre"], capture_output=True, text=True, check=False)
    time.sleep(2)  # Ensure OS releases resources

def start_scaphandre(output_json, scaphandre_path):
    os.makedirs("output", exist_ok=True)
    cmd = ["sudo", scaphandre_path, "json", "--containers", "-f", output_json]
    scaphandre_process = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    time.sleep(2)
    if scaphandre_process.poll() is not None:
        out, err = scaphandre_process.communicate(timeout=1)
        err = (err or "").strip()
        out = (out or "").strip()
        logger.error("Scaphandre failed to start (exit code %s).", scaphandre_process.returncode)
        if err:
            logger.error("Scaphandre stderr: %s", err)
        if out:
            logger.error("Scaphandre stdout: %s", out)
        raise RuntimeError("Scaphandre failed to start")
    return scaphandre_process

def stop_scaphandre(scaphandre_process):
    scaphandre_process.terminate()
    scaphandre_process.wait(timeout=5)
    time.sleep(2)  # Ensure OS releases resources

def check_container_health(url, retries=None, delay=None, startup_wait=None):
    """Wait for container to respond with HTTP 200. BEAM/Elixir apps often need 15–60s to boot, especially after long runs."""
    startup_wait = int(os.environ.get("MEASURE_STARTUP_WAIT", startup_wait or 15))
    retries = int(os.environ.get("MEASURE_HEALTH_RETRIES", retries or 25))
    delay = int(os.environ.get("MEASURE_HEALTH_DELAY", delay or 2))
    total_max = startup_wait + retries * delay
    logger.info("Waiting up to %ds for container (initial %ds, then %d retries every %ds)...", total_max, startup_wait, retries, delay)
    time.sleep(startup_wait)
    for attempt in range(1, retries + 1):
        try:
            if requests.get(url, timeout=10).status_code == 200:
                logger.info("Container ready after %d attempt(s).", attempt)
                return True
        except requests.exceptions.RequestException:
            if attempt < retries:
                time.sleep(delay)
    return False

def start_server_container(server_image, port_mapping, container_name, docker_path, network="bridge"):
    cleanup_existing_container(container_name, docker_path)
    # --cgroupns=host: needed for Scaphandre to detect container names on cgroups v2
    cmd = [docker_path, "run", "-d", "--cgroupns=host", "--ulimit", "nofile=100000:100000", "--name", container_name]
    if network == "host":
        cmd.extend(["--network", "host"])
    else:
        cmd.extend(["-p", port_mapping])
    cmd.append(server_image)
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        logger.error("Failed to start container. Docker stderr: %s", result.stderr or "(none)")
        logger.error("If --cgroupns=host is unsupported, try: docker run --rm --cgroupns=host hello-world")
        raise RuntimeError("Container failed to start")
    time.sleep(5)

def stop_server_container(container_name, docker_path):
    subprocess.run([docker_path, "stop", container_name], capture_output=True, text=True, check=True)
    subprocess.run([docker_path, "rm", container_name], capture_output=True, text=True, check=True)
    time.sleep(2)  # Ensure Docker/OS releases resources

def collect_resources_docker_stats(container_name, stop_event, docker_path, interval=0.5):
    import re
    cpu_usage = []
    mem_usage = []
    sample_count = 0
    while not stop_event.is_set():
        try:
            stats_format = "{{.CPUPerc}},{{.MemUsage}}"
            cmd = [docker_path, "stats", container_name, "--no-stream", "--format", stats_format]
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
            output = result.stdout.strip()
            if not output:
                cpu_usage.append(0.0)
                mem_usage.append(0.0)
                time.sleep(interval)
                sample_count += 1
                continue
            cpu_str, mem_str = output.split(',')
            cpu_val = float(cpu_str.strip().replace('%',''))
            mem_usage_part = mem_str.strip().split('/')[0].strip()
            mem_match = re.match(r"([\d.]+)([KMG]iB)", mem_usage_part)
            mem_val = 0.0
            if mem_match:
                mem_num = float(mem_match.group(1))
                mem_unit = mem_match.group(2)
                if mem_unit == 'KiB':
                    mem_val = mem_num / 1024
                elif mem_unit == 'MiB':
                    mem_val = mem_num
                elif mem_unit == 'GiB':
                    mem_val = mem_num * 1024
            cpu_usage.append(cpu_val)
            mem_usage.append(mem_val)
        except Exception:
            cpu_usage.append(0.0)
            mem_usage.append(0.0)
        time.sleep(interval)
        sample_count += 1
    cpu_avg = sum(cpu_usage) / len(cpu_usage) if cpu_usage else 0.0
    cpu_peak = max(cpu_usage) if cpu_usage else 0.0
    cpu_total = sum(cpu_usage) * interval if cpu_usage else 0.0  # cumulative CPU (%*s)
    mem_avg = sum(mem_usage) / len(mem_usage) if mem_usage else 0.0
    mem_peak = max(mem_usage) if mem_usage else 0.0
    mem_total = sum(mem_usage) * interval if mem_usage else 0.0  # cumulative memory (MB*s)
    return {'avg': cpu_avg, 'peak': cpu_peak, 'total': cpu_total}, \
           {'avg': mem_avg, 'peak': mem_peak, 'total': mem_total}


def _pid_in_container(pid, container_id):
    """Check if pid belongs to container via /proc/pid/cgroup (fallback when Scaphandre reports container=null)."""
    if not container_id or pid <= 0:
        return False
    try:
        with open(f"/proc/{pid}/cgroup", "r") as f:
            cgroup = f.read()
        return container_id in cgroup
    except (OSError, IOError):
        return False


def parse_json_and_compute_energy(file_name, container_name, runtime, container_id=None):
    """Extract energy from Scaphandre JSON. Prefers Scaphandre's container field; falls back to cgroup when all container=null.

    Returns (energy_joules, avg_power_watts, n_samples, raw_samples_uw)
    raw_samples_uw is the list of per-entry power values in microwatts (>0 only).
    """
    with open(file_name, "r") as file:
        data = json.load(file)

    raw_samples_uw = []
    found_containers = set()

    for entry in data:
        for consumer in entry.get("consumers", []):
            container = consumer.get("container")
            if container:
                found_containers.add(container.get("name"))
            if container and container.get("name") == container_name:
                power = consumer.get("consumption", 0.0)
                if power > 0:
                    raw_samples_uw.append(float(power))

    # Fallback: when Scaphandre reports container=null for all (e.g. cgroups v2), attribute by cgroup path
    if not raw_samples_uw and container_id and not found_containers:
        for entry in data:
            for consumer in entry.get("consumers", []):
                if consumer.get("container"):
                    continue
                pid = consumer.get("pid", 0)
                power = consumer.get("consumption", 0.0)
                if power > 0 and _pid_in_container(pid, container_id):
                    raw_samples_uw.append(float(power))
        if raw_samples_uw:
            logger.info(f"Using cgroup fallback for '{container_name}' (Scaphandre container=null on this system)")

    if not found_containers and not raw_samples_uw:
        logger.warning(f"No containers found in Scaphandre output {file_name}")
    elif found_containers:
        logger.info(f"Containers found in Scaphandre output: {found_containers}")
    if container_name not in found_containers and not raw_samples_uw:
        logger.warning(f"Container '{container_name}' not found in Scaphandre output!")
    if not raw_samples_uw:
        logger.warning(f"No energy samples found for container '{container_name}' in {file_name}")
        return 0.0, 0.0, 0, []

    avg_power_watts = (statistics.mean(raw_samples_uw)) * 1e-6
    total_energy_joules = avg_power_watts * runtime
    return total_energy_joules, avg_power_watts, len(raw_samples_uw), raw_samples_uw

def save_results_to_csv(filename, results, total_energy, average_power, runtime, requests_per_second, total_samples,
                       cpu_metrics, mem_metrics, num_cores, container_name, measurement_type, extra_fields=None):
    extra_fields = extra_fields or {}
    base_headers = ["Container Name", "Type", "Num CPUs", "Total Requests", "Successful Requests", "Failed Requests", "Execution Time (s)", "Requests/s",
               "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%*s)",
               "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB*s)"]
    headers = base_headers + list(extra_fields.keys())
    num_cores_csv = int(num_cores) if num_cores is not None else 1
    new_row = [
        str(container_name),
        str(measurement_type),
        int(num_cores_csv),
        int(results['total']),
        int(results['success']),
        int(results['failure']),
        float(runtime),
        float(requests_per_second),
        float(total_energy),
        float(average_power),
        int(total_samples),
        float(cpu_metrics['avg']),
        float(cpu_metrics['peak']),
        float(cpu_metrics['total']),
        float(mem_metrics['avg']),
        float(mem_metrics['peak']),
        float(mem_metrics['total'])
    ] + list(extra_fields.values())

    if filename is None:
        os.makedirs("results_docker", exist_ok=True)
        filename = os.path.join("results_docker", f"{container_name}.csv")

    if not os.path.isfile(filename) or os.stat(filename).st_size == 0:
        with open(filename, mode='w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(headers)
            writer.writerow(new_row)
        return

    with open(filename, mode='r', newline='') as file:
        existing = list(csv.reader(file))
    if not existing:
        with open(filename, mode='w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(headers)
            writer.writerow(new_row)
        return

    existing_header = existing[0]
    if existing_header == headers:
        with open(filename, mode='a', newline='') as file:
            csv.writer(file).writerow(new_row)
        return

    # Older CSVs without new columns: rewrite with canonical header and padded rows.
    migrated = []
    for row in existing[1:]:
        row_dict = {}
        for i, key in enumerate(existing_header):
            if i < len(row):
                row_dict[key] = row[i]
        migrated.append([row_dict.get(k, '') for k in headers])
    migrated.append(new_row)
    with open(filename, mode='w', newline='') as file:
        w = csv.writer(file)
        w.writerow(headers)
        w.writerows(migrated)

def print_summary(results, total_energy, average_power, runtime, requests_per_second, cpu_metrics, mem_metrics, num_cores, output_json, output_csv, container_name, http_max_workers_label=None):
    logger.info("=== Measurement Summary ===")
    logger.info(f"Container: {container_name}")
    if http_max_workers_label is not None:
        logger.info("HTTP max workers: %s", http_max_workers_label)
    logger.info(f"Total Requests: {results['total']}, Successful: {results['success']}, Failed: {results['failure']}")
    logger.info(f"Execution Time: {runtime:.2f} s, Requests/s: {requests_per_second:.2f}")
    logger.info(f"Energy: Total {total_energy:.2f} J, Avg Power {average_power:.2f} W")
    logger.info(f"CPU: Avg {cpu_metrics['avg']:.2f}%, Peak {cpu_metrics['peak']:.2f}%, Total {cpu_metrics['total']:.2f} %*s")
    logger.info(f"Memory: Avg {mem_metrics['avg']:.2f} MB, Peak {mem_metrics['peak']:.2f} MB, Total {mem_metrics['total']:.2f} MB*s")
    logger.info(f"JSON: {output_json}, CSV: {output_csv or f'results_docker/{container_name}.csv'}")
    logger.info("==========================")


# ---------------------------------------------------------------------------
# Per-iteration runner
# ---------------------------------------------------------------------------

def run_single_iteration(args, docker_path, scaphandre_path, num_cores,
                          run_num, total_runs, output_json):
    """
    Run one complete benchmark iteration (start container → load → stop).

    Returns a dict with keys:
      energy_j, power_w, runtime, requests_total, requests_success,
      raw_samples_uw, temp_peak_c, cpu_metrics, mem_metrics, output_json
    Returns None if the container failed health check.
    """
    global results_counter, runtime_data
    results_counter.clear()

    container_name = args.container_name or args.server_image
    url = ("http://localhost:80/"
           if args.network == "host"
           else f"http://localhost:{args.port_mapping.split(':')[0]}/")

    run_label = f"run {run_num}/{total_runs}" if total_runs > 1 else "run"

    if is_measure_quiet() and not args.verbose:
        measure_quiet_msg(f"{container_name} | {run_label} | Docker start + HTTP readiness wait …")
    logger.info(f"Starting container '{container_name}' ({run_label})...")
    start_server_container(args.server_image, args.port_mapping, container_name, docker_path, args.network)

    if not check_container_health(url):
        logger.error("Container health check failed (no HTTP 200 within wait time).")
        try:
            out = subprocess.run(
                [docker_path, "logs", "--tail", "30", container_name],
                capture_output=True, text=True, timeout=5
            )
            if out.stdout or out.stderr:
                logger.error("Container logs (last 30 lines):\n%s%s", out.stdout or "", out.stderr or "")
        except Exception as e:
            logger.debug("Could not get container logs: %s", e)
        logger.error("To allow more boot time: MEASURE_STARTUP_WAIT=25 MEASURE_HEALTH_RETRIES=30 make run")
        stop_server_container(container_name, docker_path)
        return None

    if is_measure_quiet() and not args.verbose:
        measure_quiet_msg(
            f"{container_name} | {run_label} | Scaphandre + HTTP load | "
            f"{args.num_requests} GET → {url}"
        )
    logger.info("Starting Scaphandre...")
    scaphandre_process = start_scaphandre(output_json, scaphandre_path)

    logger.info(f"Sending {args.num_requests} requests to {url}...")
    time.sleep(2)

    # Record temperature before load
    temps_before = detect_temperatures()

    stop_event = threading.Event()
    resource_results = {'cpu': {}, 'mem': {}}

    def collect():
        cpu_m, mem_m = collect_resources_docker_stats(container_name, stop_event, docker_path)
        resource_results['cpu'] = cpu_m
        resource_results['mem'] = mem_m

    resource_thread = threading.Thread(target=collect)
    resource_thread.start()

    logger.info("Sleeping 1s to let docker stats stabilize...")
    time.sleep(1)

    hb_stop = threading.Event()
    hb_thread = None
    load_t0 = time.time()
    if is_measure_quiet() and not args.verbose:
        iv = measure_quiet_heartbeat_interval_sec()

        def _heartbeat_worker():
            while not hb_stop.wait(iv):
                with results_lock:
                    done = results_counter["total"]
                measure_quiet_msg(
                    f"{container_name} | {run_label} | HTTP requests {done}/{args.num_requests} "
                    f"({int(time.time() - load_t0)}s elapsed)"
                )

        hb_thread = threading.Thread(target=_heartbeat_worker, daemon=True)
        hb_thread.start()

    start_time = time.time()
    try:
        with ThreadPoolExecutor(max_workers=args.max_workers) as executor:
            executor.map(lambda i: send_request(url, i, args.verbose), range(args.num_requests))
    finally:
        if hb_thread is not None:
            hb_stop.set()
            hb_thread.join(timeout=3)
    run_runtime = time.time() - start_time

    time.sleep(3)
    stop_event.set()
    resource_thread.join()

    # Record temperature after load
    temps_after = detect_temperatures()
    all_temps = temps_before + temps_after
    temp_peak = max(all_temps) if all_temps else -1.0

    if is_measure_quiet() and not args.verbose:
        measure_quiet_msg(f"{container_name} | {run_label} | stopping Scaphandre …")
    logger.info("Waiting for Scaphandre...")
    time.sleep(5)
    stop_scaphandre(scaphandre_process)

    container_id = None
    result = subprocess.run(
        [docker_path, "ps", "-q", "-f", f"name={container_name}"],
        capture_output=True, text=True
    )
    if result.returncode == 0 and result.stdout.strip():
        container_id = result.stdout.strip()

    total_energy, average_power, total_samples, raw_samples_uw = parse_json_and_compute_energy(
        output_json, container_name, run_runtime, container_id=container_id
    )
    stop_server_container(container_name, docker_path)

    requests_per_second = results_counter['total'] / run_runtime if run_runtime > 0 else 0

    return {
        "energy_j": total_energy,
        "power_w": average_power,
        "runtime": run_runtime,
        "requests_total": results_counter['total'],
        "requests_success": results_counter['success'],
        "requests_failure": results_counter['failure'],
        "requests_per_second": requests_per_second,
        "raw_samples_uw": raw_samples_uw,
        "temp_peak_c": temp_peak,
        "cpu_metrics": resource_results['cpu'],
        "mem_metrics": resource_results['mem'],
        "total_samples": total_samples,
        "output_json": output_json,
    }


# ---------------------------------------------------------------------------
# Filter kwargs builder
# ---------------------------------------------------------------------------

def _build_filter_kwargs(model: str, args) -> dict:
    kw = {}
    if model == "iqr":
        kw["factor"] = args.iqr_factor
    elif model == "hampel":
        kw["window"] = args.hampel_window
        kw["threshold"] = args.hampel_threshold
    elif model in ("isolation_forest", "elliptic"):
        kw["contamination"] = args.contamination
    elif model == "lof":
        kw["contamination"] = args.contamination
        kw["n_neighbors"] = args.lof_neighbors
    elif model == "dbscan":
        kw["eps"] = args.dbscan_eps
        kw["min_samples"] = args.dbscan_minpts
    return kw


def main():
    parser = argparse.ArgumentParser(description="Measure web server energy with Scaphandre in Docker")
    parser.add_argument('--server_image', type=str, required=True, help="Docker image of the server (e.g., nginx-deb)")
    parser.add_argument('--container_name', type=str, default=None, help="Name of the Docker container (defaults to server_image)")
    parser.add_argument('--port_mapping', type=str, default='8001:80', help="Port mapping (default: 8001:80)")
    parser.add_argument('--network', type=str, default='bridge', choices=['bridge', 'host'], help="Network mode (default: bridge)")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send (default: 500)")
    parser.add_argument('--max_workers', type=int, default=None, help="Max workers for ThreadPoolExecutor (default: None; CSV records System default when unset)")
    parser.add_argument('--output_csv', type=str, default=None, help="Output CSV file path (default: results_docker/<container_name>.csv)")
    parser.add_argument('--output_json', type=str, default=None, help="Output JSON file path (default: output/<timestamp>.json)")
    parser.add_argument('--verbose', action='store_true', help="Enable verbose logging")
    parser.add_argument('--measurement_type', type=str, default=None, help="Type of measurement (static, dynamic, etc.)")

    # --- Repetition and isolation ---
    parser.add_argument('--runs', type=int, default=None,
                        help="Number of benchmark repetitions (default: from bench.config or BENCH_RUNS, fallback 1)")
    parser.add_argument('--isolation', type=str, default=None,
                        choices=["none", "basic", "full"],
                        help="Isolation level (default: from bench.config or BENCH_ISOLATION, fallback none)")

    # --- Filter model ---
    parser.add_argument('--filter-model', type=str, default=None,
                        choices=list(FILTER_MODELS),
                        help="Outlier filter model (default: from bench.config or BENCH_FILTER_MODEL, fallback none)")
    parser.add_argument('--contamination', type=float, default=None,
                        help="Contamination for IF/LOF/Elliptic (default: from bench.config, fallback 0.1)")
    parser.add_argument('--iqr-factor', type=float, default=None,
                        help="IQR factor (default: from bench.config, fallback 1.5)")
    parser.add_argument('--hampel-window', type=int, default=None,
                        help="Hampel filter window (default: from bench.config, fallback 5)")
    parser.add_argument('--hampel-threshold', type=float, default=None,
                        help="Hampel filter threshold (default: from bench.config, fallback 3.0)")
    parser.add_argument('--dbscan-eps', type=float, default=None,
                        help="DBSCAN eps in µW (default: from bench.config, fallback 200.0)")
    parser.add_argument('--dbscan-minpts', type=int, default=None,
                        help="DBSCAN min_samples (default: from bench.config, fallback 5)")
    parser.add_argument('--lof-neighbors', type=int, default=None,
                        help="LOF n_neighbors (default: from bench.config, fallback 10)")

    # --- Thermal ---
    parser.add_argument('--cooldown-temp', type=float, default=None,
                        help="Cooldown temperature ceiling °C (default: from bench.config, fallback 50.0)")
    parser.add_argument('--cooldown-cpu', type=float, default=None,
                        help="Cooldown max CPU%% usage (default: from bench.config, fallback 5.0)")
    parser.add_argument('--cooldown-timeout', type=int, default=None,
                        help="Cooldown timeout seconds (default: from bench.config, fallback 120)")

    # --- Measurement quality ---
    parser.add_argument('--baseline-duration', type=int, default=None,
                        help="Idle baseline measurement duration in seconds (default: from bench.config, fallback 10)")
    parser.add_argument('--confidence', type=float, default=None,
                        help="CI confidence level (default: from bench.config, fallback 0.95)")
    parser.add_argument('--cross-run-factor', type=float, default=None,
                        help="IQR factor for cross-run outlier rejection (default: from bench.config, fallback 1.5)")

    # --- Config path ---
    parser.add_argument('--config', type=str, default=DEFAULT_CONFIG_PATH,
                        help="Path to bench.config (default: bench.config)")

    args = parser.parse_args()

    if args.verbose:
        logger.setLevel(logging.DEBUG)
    elif is_measure_quiet():
        logger.setLevel(logging.WARNING)

    # --- Load bench.config and resolve all parameters ---
    cfg = load_config(args.config)

    def _r(section, key, env_var, cli_val, fallback):
        return resolve(cfg, section, key, env_var, cli_val=cli_val, fallback=str(fallback))

    runs           = int(_r("measurement", "runs",             "BENCH_RUNS",           args.runs,             1))
    isolation      = _r("isolation",   "level",               "BENCH_ISOLATION",      args.isolation,        "none")
    filter_model   = _r("filter",      "model",               "BENCH_FILTER_MODEL",   args.filter_model,     "none")
    contamination  = float(_r("filter", "contamination",      "BENCH_CONTAMINATION",  args.contamination,    0.1))
    iqr_factor_v   = float(_r("filter", "iqr_factor",         "BENCH_IQR_FACTOR",     args.iqr_factor,       1.5))
    hampel_window_v = int(_r("filter", "hampel_window",       "BENCH_HAMPEL_WINDOW",  args.hampel_window,    5))
    hampel_thresh_v = float(_r("filter","hampel_threshold",   "BENCH_HAMPEL_THRESH",  args.hampel_threshold, 3.0))
    dbscan_eps_v   = float(_r("filter", "dbscan_eps",         "BENCH_DBSCAN_EPS",     args.dbscan_eps,       200.0))
    dbscan_minpts_v = int(_r("filter", "dbscan_minpts",       "BENCH_DBSCAN_MINPTS",  args.dbscan_minpts,    5))
    lof_neighbors_v = int(_r("filter", "lof_neighbors",       "BENCH_LOF_NEIGHBORS",  args.lof_neighbors,    10))
    cooldown_temp_v  = float(_r("thermal","cooldown_temp_c",  "BENCH_COOLDOWN_TEMP",  args.cooldown_temp,    50.0))
    cooldown_cpu_v   = float(_r("thermal","cooldown_cpu_pct", "BENCH_COOLDOWN_CPU",   args.cooldown_cpu,     5.0))
    cooldown_to_v    = int(_r("thermal", "cooldown_timeout_s","BENCH_COOLDOWN_TO",    args.cooldown_timeout, 120))
    baseline_dur_v   = int(_r("measurement","baseline_duration_s","BENCH_BASELINE_DUR",args.baseline_duration, 10))
    confidence_v     = float(_r("measurement","confidence",   "BENCH_CONFIDENCE",     args.confidence,       0.95))
    cross_run_fac_v  = float(_r("cross_run_filter","factor",  "BENCH_CROSS_RUN_FACTOR",args.cross_run_factor,1.5))

    # Patch resolved filter params back into args so _build_filter_kwargs can read them
    args.iqr_factor      = iqr_factor_v
    args.hampel_window   = hampel_window_v
    args.hampel_threshold = hampel_thresh_v
    args.contamination   = contamination
    args.dbscan_eps      = dbscan_eps_v
    args.dbscan_minpts   = dbscan_minpts_v
    args.lof_neighbors   = lof_neighbors_v

    filter_kwargs = _build_filter_kwargs(filter_model, args)

    check_prerequisites()
    scaphandre_path = get_binary_path("scaphandre")
    docker_path = get_binary_path("docker")
    num_cores = os.cpu_count()

    container_name = args.container_name or args.server_image

    # Port-in-use check before starting container
    host_port = args.port_mapping.split(":")[0]
    result = subprocess.run(["ss", "-ltn"], capture_output=True, text=True)
    if f":{host_port} " in result.stdout:
        logger.error(f"[ERROR] Port {host_port} is already in use. Please stop the process or container using it before running the benchmark.")
        result2 = subprocess.run(["ss", "-ltnp"], capture_output=True, text=True)
        logger.error("[INFO] The following processes are using port %s:\n%s", host_port, '\n'.join([line for line in result2.stdout.splitlines() if f":{host_port} " in line]))
        result3 = subprocess.run(["docker", "ps", "--filter", f"publish={host_port}"], capture_output=True, text=True)
        logger.error("[INFO] Docker containers using this port:\n%s", result3.stdout)
        exit(1)

    cleanup_existing_scaphandre()

    timestamp = datetime.now().strftime("%Y-%m-%d-%H%M%S")

    # --- Run with IsolationContext wrapping the entire session ---
    with IsolationContext(
        level=isolation,
        cooldown_temp=cooldown_temp_v,
        cooldown_cpu=cooldown_cpu_v,
        cooldown_timeout=cooldown_to_v,
        verbose=args.verbose,
    ) as ctx:

        baseline_power_w = 0.0
        if runs > 1 or isolation != "none":
            if is_measure_quiet() and not args.verbose:
                measure_quiet_msg(f"{container_name} | measuring idle baseline ({baseline_dur_v}s)…")
            baseline_power_w = ctx.measure_idle(duration=baseline_dur_v)
            if baseline_power_w > 0:
                logger.info(f"Idle baseline: {baseline_power_w:.3f} W")

        iteration_results = []

        for run_idx in range(runs):
            run_num = run_idx + 1

            if runs > 1:
                output_json = (args.output_json or
                               os.path.join("output", f"{timestamp}_run{run_num}of{runs}.json"))
            else:
                output_json = args.output_json or os.path.join("output", f"{timestamp}.json")

            result = run_single_iteration(
                args, docker_path, scaphandre_path, num_cores,
                run_num, runs, output_json
            )

            if result is None:
                logger.error(f"Run {run_num}/{runs} failed — skipping.")
                continue

            iteration_results.append(result)

            if is_measure_quiet() and not args.verbose:
                ok = result['requests_success'] == result['requests_total']
                cnt_str = (
                    f"{_M_GREEN}{result['requests_success']}/{result['requests_total']} ok{_M_NC}"
                    if ok
                    else f"{result['requests_success']}/{result['requests_total']}"
                )
                measure_quiet_msg(
                    f"{container_name} | run {run_num}/{runs} | {cnt_str} | "
                    f"{result['runtime']:.1f}s | {result['requests_per_second']:.0f} req/s | "
                    f"temp_peak={result['temp_peak_c']:.1f}°C"
                )

            # Cooldown between runs (not after the last one)
            if run_idx < runs - 1:
                if is_measure_quiet() and not args.verbose:
                    measure_quiet_msg(f"{container_name} | cooldown between runs…")
                ctx.cooldown()

        ctx.report()

    # --- Post-loop: apply filter, cross-run rejection, CI ---
    if not iteration_results:
        logger.error("No successful runs — no CSV written.")
        sys.exit(1)

    # Apply per-run filter and recompute energy from cleaned samples
    run_energies_filtered = []
    run_powers_filtered = []
    run_runtimes = []
    run_requests = []
    run_successes = []
    temp_peaks = []

    for r in iteration_results:
        raw = r["raw_samples_uw"]
        runtime = r["runtime"]
        if raw and filter_model != "none":
            try:
                clean = apply_filter(raw, model=filter_model, **filter_kwargs)
            except Exception:
                clean = raw
        else:
            clean = raw

        if clean:
            avg_w = statistics.mean(clean) * 1e-6
            energy_j = avg_w * runtime
        else:
            avg_w = r["power_w"]
            energy_j = r["energy_j"]

        run_energies_filtered.append(energy_j)
        run_powers_filtered.append(avg_w)
        run_runtimes.append(runtime)
        run_requests.append(r["requests_total"])
        run_successes.append(r["requests_success"])
        temp_peaks.append(r["temp_peak_c"])

    summary = summarise_runs(
        run_energies=run_energies_filtered,
        run_powers=run_powers_filtered,
        run_runtimes=run_runtimes,
        run_requests=run_requests,
        run_successes=run_successes,
        cross_run_factor=cross_run_fac_v,
        confidence=confidence_v,
    )

    # Aggregate CPU/mem across runs (use last run's for single-run compat)
    last = iteration_results[-1]
    cpu_metrics_out = last["cpu_metrics"]
    mem_metrics_out = last["mem_metrics"]

    # Representative results counter (sum across used runs)
    used_indices = [i for i in range(len(iteration_results))
                    if i not in getattr(summary, '_rejected_indices', [])]
    agg_results = Counter()
    agg_results['total']   = int(summary["requests_mean"] * summary["runs_used"])
    agg_results['success'] = int(summary["success_mean"] * summary["runs_used"])
    agg_results['failure'] = agg_results['total'] - agg_results['success']

    total_energy_out  = summary["energy_mean"]
    average_power_out = summary["power_mean"]
    runtime_out       = summary["runtime_mean"]
    rps_out           = (summary["requests_mean"] / runtime_out
                         if runtime_out > 0 else 0.0)
    total_samples_out = last["total_samples"]
    temp_peak_out     = max(t for t in temp_peaks if t >= 0) if temp_peaks else -1.0

    http_workers_label = http_max_workers_label(args)
    measurement_type = getattr(args, 'measurement_type', None) or "unknown"

    extra_fields = {
        "HTTP Max Workers": http_workers_label,
        "Runs Total": summary["runs_total"],
        "Runs Used": summary["runs_used"],
        "Energy Mean (J)": round(summary["energy_mean"], 6),
        "Energy Std (J)": round(summary["energy_std"], 6),
        "Energy CI Lo (J)": round(summary["energy_ci_lo"], 6),
        "Energy CI Hi (J)": round(summary["energy_ci_hi"], 6),
        "Temp Peak (°C)": round(temp_peak_out, 1),
        "Baseline Power (W)": round(baseline_power_w, 4),
        "Filter Model": filter_model,
    }

    save_results_to_csv(
        args.output_csv, agg_results,
        total_energy_out, average_power_out, runtime_out,
        rps_out, total_samples_out,
        cpu_metrics_out, mem_metrics_out,
        num_cores, args.server_image, measurement_type,
        extra_fields=extra_fields,
    )

    csv_disp = args.output_csv or os.path.join("results_docker", f"{container_name}.csv")

    if is_measure_quiet() and not args.verbose:
        ok = agg_results["success"] == agg_results["total"]
        cnt_str = (
            f"{_M_GREEN}{agg_results['success']}/{agg_results['total']} ok{_M_NC}"
            if ok
            else f"{agg_results['success']}/{agg_results['total']}"
        )
        run_info = (f"{summary['runs_used']}/{summary['runs_total']} runs"
                    if summary["runs_total"] > 1 else "")
        ci_info = (f" CI=[{summary['energy_ci_lo']:.3f},{summary['energy_ci_hi']:.3f}]J"
                   if summary["runs_total"] > 1 else "")
        measure_quiet_msg(
            f"{container_name} | {cnt_str} | {runtime_out:.1f}s | {rps_out:.0f} req/s | "
            f"{run_info}{ci_info} | {csv_disp}"
        )
    else:
        print_summary(
            agg_results, total_energy_out, average_power_out,
            runtime_out, rps_out,
            cpu_metrics_out, mem_metrics_out,
            num_cores, last["output_json"], args.output_csv, container_name,
            http_max_workers_label=http_workers_label,
        )
        if summary["runs_total"] > 1:
            logger.info(
                f"Runs: {summary['runs_used']}/{summary['runs_total']} used  "
                f"Energy CI ({int(confidence_v*100)}%%): "
                f"[{summary['energy_ci_lo']:.4f}, {summary['energy_ci_hi']:.4f}] J  "
                f"Filter: {filter_model}"
            )

if __name__ == "__main__":
    main()
