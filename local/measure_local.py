import os
import time
import subprocess
import requests
import csv
from concurrent.futures import ThreadPoolExecutor
from collections import Counter
import argparse
import json
import threading
from datetime import datetime
import logging
import psutil

logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger()

results_counter = Counter()

def get_binary_path(binary_name):
    """Get the full path of a binary using 'sudo which'."""
    try:
        result = subprocess.run(["sudo", "which", binary_name], capture_output=True, text=True, check=True)
        path = result.stdout.strip()
        if not path:
            raise FileNotFoundError(f"Binary '{binary_name}' not found with 'sudo which'")
        return path
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to locate {binary_name}: {e.stderr}") from e

def send_request(url, request_num, verbose=False):
    """Send an HTTP request and track progress."""
    try:
        response = requests.get(url, timeout=5)
        if verbose:
            logger.debug(f'{url} "GET / HTTP/1.1" {response.status_code} {len(response.content)}')
        if 200 <= response.status_code < 300:
            results_counter['success'] += 1
        else:
            results_counter['failure'] += 1
    except requests.exceptions.RequestException:
        results_counter['failure'] += 1
    finally:
        results_counter['total'] += 1

def cleanup_existing_server(server):
    """Stop any existing server process using the setup script."""
    script_path = f"./setup_{server}.sh"
    if not os.path.exists(script_path):
        raise FileNotFoundError(f"Setup script {script_path} not found")
    try:
        subprocess.run(["sudo", script_path, "stop"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        logger.debug(f"Stopped existing {server} process using {script_path} stop")
    except subprocess.CalledProcessError:
        logger.debug(f"No existing {server} process found to stop")

def cleanup_existing_scaphandre():
    """Kill any existing scaphandre processes."""
    try:
        subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        logger.debug("Killed existing scaphandre processes")
    except subprocess.CalledProcessError:
        logger.debug("No existing scaphandre processes found to kill")

def start_scaphandre(output_json, scaphandre_path, step=None, step_nano=None, max_top_consumers=None):
    os.makedirs("output", exist_ok=True)
    scaphandre_command = ["sudo", scaphandre_path, "json", "-f", output_json]
    
    if step is not None:
        if step < 0:
            raise ValueError(f"Step {step} seconds must be non-negative")
        scaphandre_command.extend(["--step", str(step)])
    
    if step_nano is not None:
        if step_nano < 100000:
            raise ValueError(f"Step-nano {step_nano} ns is too small; must be at least 100,000 ns (~100 µs)")
        if step_nano < 0:
            raise ValueError(f"Step-nano {step_nano} nanoseconds must be non-negative")
        if step is None:
            scaphandre_command.extend(["--step", "0"])
        scaphandre_command.extend(["--step-nano", str(step_nano)])
    
    if max_top_consumers is not None:
        if max_top_consumers <= 0:
            raise ValueError(f"Max-top-consumers {max_top_consumers} must be positive")
        scaphandre_command.extend(["--max-top-consumers", str(max_top_consumers)])
    
    scaphandre_process = subprocess.Popen(scaphandre_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(5)
    if scaphandre_process.poll() is not None:
        stdout, stderr = scaphandre_process.communicate()
        raise RuntimeError(f"Scaphandre failed to start: {stderr or stdout}")
    logger.debug("Scaphandre started on host")
    return scaphandre_process

def stop_scaphandre(scaphandre_process):
    logger.debug("Stopping Scaphandre process...")
    scaphandre_process.terminate()
    try:
        scaphandre_process.wait(timeout=5)
        logger.debug(f"Scaphandre (PID {scaphandre_process.pid}) terminated gracefully")
    except subprocess.TimeoutExpired:
        logger.debug(f"Scaphandre (PID {scaphandre_process.pid}) did not stop gracefully; forcing termination...")
        scaphandre_process.kill()
        scaphandre_process.wait(timeout=5)
    subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=False)

def check_server_health(url, retries=5, delay=2):
    """Check if the server is responsive."""
    for i in range(retries):
        try:
            response = requests.get(url, timeout=5)
            if response.status_code == 200:
                logger.debug(f"Server health check passed: {url} responded with status 200")
                return True
        except requests.exceptions.RequestException as e:
            logger.debug(f"Health check attempt {i+1}/{retries} failed: {e}")
            time.sleep(delay)
    logger.error(f"Server health check failed after {retries} attempts")
    return False

def start_local_server(server):
    """Start the local server using the setup script."""
    script_path = f"./setup_{server}.sh"
    if not os.path.exists(script_path):
        raise FileNotFoundError(f"Setup script {script_path} not found")
    
    server_exe_map = {
        'nginx': '/usr/local/nginx/sbin/nginx',
        'yaws': '/usr/local/bin/yaws'
    }
    server_exe = server_exe_map[server]
    if not os.path.exists(server_exe):
        raise RuntimeError(f"{server} binary not found at {server_exe}. Please install it by running: sudo {script_path} install")

    logger.info(f"Starting {server} server...")
    start_cmd = ["sudo", script_path, "run"]
    server_process = subprocess.Popen(start_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout, stderr = server_process.communicate()
    if server_process.returncode != 0:
        raise RuntimeError(f"Failed to start {server}: {stderr}")
    time.sleep(5)

def stop_local_server(server):
    """Stop the local server using the setup script."""
    script_path = f"./setup_{server}.sh"
    if not os.path.exists(script_path):
        raise FileNotFoundError(f"Setup script {script_path} not found")
    logger.debug(f"Stopping {server} server...")
    result = subprocess.run(["sudo", script_path, "stop"], capture_output=True, text=True)
    if result.returncode != 0:
        logger.debug(f"Warning: {server} stop returned non-zero exit code: {result.stderr}")

def collect_resources(server, stop_event, num_cores, interval=0.5):
    """Collect CPU and memory usage for all server processes using psutil."""
    cpu_usage = []
    mem_usage = []
    pgrep_pattern = "nginx" if server == "nginx" else "beam.smp.*yaws"
    
    while not stop_event.is_set():
        try:
            result = subprocess.run(["sudo", "pgrep", "-f", pgrep_pattern], capture_output=True, text=True, check=True)
            pids_raw = result.stdout.strip().split('\n')
            logger.debug(f"Raw PIDs from pgrep '{pgrep_pattern}': {pids_raw}")
            if not pids_raw or pids_raw == ['']:
                logger.debug(f"No PIDs found for {server} with sudo pgrep -f '{pgrep_pattern}'")
            else:
                logger.debug(f"Found {len(pids_raw)} PIDs for {server}")
            
            pids = []
            for pid in pids_raw:
                if not pid or pid == "0":
                    continue
                try:
                    proc = psutil.Process(int(pid))
                    cmd = " ".join(proc.cmdline())
                    logger.debug(f"PID {pid} cmdline: '{cmd}'")
                    if ("nginx" in cmd.lower() if server == "nginx" else "beam.smp" in cmd.lower()) and "sudo" not in cmd and "grep" not in cmd:
                        pids.append(pid)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    logger.debug(f"Skipping PID {pid}: Process not found or access denied")
                    continue
            
            if not pids:
                logger.debug(f"No valid {server} PIDs found after filtering pgrep results")
                cpu_usage.append(0.0)
                mem_usage.append(0.0)
                time.sleep(interval)
                continue
            
            logger.debug(f"Monitoring PIDs {pids} for {server}")
        
        except subprocess.CalledProcessError as e:
            logger.debug(f"pgrep failed for '{pgrep_pattern}': {e}")
            cpu_usage.append(0.0)
            mem_usage.append(0.0)
            time.sleep(interval)
            continue

        try:
            cpu_total = 0.0
            mem_total_mb = 0.0
            pid_count = 0

            for pid in pids:
                try:
                    proc = psutil.Process(int(pid))
                    cpu_percent = proc.cpu_percent(interval=0.1)
                    mem_info = proc.memory_info()
                    mem_mb = mem_info.rss / (1024 * 1024)
                    
                    cpu_total += cpu_percent
                    mem_total_mb += mem_mb
                    pid_count += 1
                    
                    logger.debug(f"PID {pid} CPU usage: {cpu_percent:.2f}%")
                    logger.debug(f"PID {pid} Memory usage: {mem_mb:.2f} MB")
                except (psutil.NoSuchProcess, psutil.AccessDenied) as e:
                    logger.debug(f"Skipping PID {pid} during measurement: {e}")
                    continue

            avg_cpu = cpu_total / pid_count if pid_count > 0 else 0.0
            avg_mem = mem_total_mb if pid_count > 0 else 0.0
            
            cpu_usage.append(avg_cpu)
            mem_usage.append(avg_mem)
            logger.debug(f"Cycle CPU usage (%): {avg_cpu:.2f}")
            logger.debug(f"Cycle Memory usage (MB): {avg_mem:.2f}")
        except Exception as e:
            logger.debug(f"Error collecting stats: {e}")
            cpu_usage.append(0.0)
            mem_usage.append(0.0)
        
        time.sleep(interval)
    
    total_system_capacity = num_cores * 100.0
    cpu_system_pct = [cpu / total_system_capacity * 100.0 for cpu in cpu_usage]
    
    cpu_avg = sum(cpu_system_pct) / len(cpu_system_pct) if cpu_system_pct else 0.0
    cpu_peak = max(cpu_usage) / num_cores if cpu_usage else 0.0
    cpu_total = sum(cpu_usage) if cpu_usage else 0.0
    mem_avg = sum(mem_usage) / len(mem_usage) if mem_usage else 0.0
    mem_peak = max(mem_usage) if mem_usage else 0.0
    mem_total = sum(mem_usage) if mem_usage else 0.0
    
    return {'avg': cpu_avg, 'peak': cpu_peak, 'total': cpu_total}, \
           {'avg': mem_avg, 'peak': mem_peak, 'total': mem_total}

def parse_json_and_compute_energy(file_name, server_exe, runtime):
    """Parse JSON and compute total energy in Joules from power in microwatts."""
    try:
        with open(file_name, "r") as file:
            data = json.load(file)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        logger.error(f"Error reading JSON file: {e}")
        return 0.0, 0.0, 0

    total_power_microwatts = 0.0
    number_samples = 0
    
    for entry in data:
        for consumer in entry.get("consumers", []):
            exe = consumer.get("exe", "")
            cmdline = consumer.get("cmdline", "")
            if server_exe in exe or ("beam.smp" in exe and "yaws" in cmdline.lower()):
                power = consumer.get("consumption", 0.0)
                if power > 0:
                    total_power_microwatts += power
                    number_samples += 1
    
    if number_samples == 0:
        logger.info(f"No non-zero energy data found for {server_exe}")
        return 0.0, 0.0, 0

    avg_power_watts = (total_power_microwatts / number_samples) * 1e-6
    total_energy_joules = avg_power_watts * runtime
    return total_energy_joules, avg_power_watts, number_samples

def save_results_to_csv(filename, results, total_energy, average_power, runtime, requests_per_second, total_samples, 
                       cpu_metrics, mem_metrics, server, measurement_type):
    """Save results to a CSV file in results_local folder."""
    if filename is None:
        os.makedirs("results_local", exist_ok=True)
        filename = os.path.join("results_local", f"{server}.csv")
    
    headers = ["container_name", "type", "Total Requests", "Successful Requests", "Failed Requests", "Execution Time (s)", "Requests/s",
               "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%)",
               "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB)"]
    data = [[server, measurement_type, results['total'], results['success'], results['failure'], runtime, requests_per_second,
             total_energy, average_power, total_samples, cpu_metrics['avg'], cpu_metrics['peak'],
             cpu_metrics['total'], mem_metrics['avg'], mem_metrics['peak'], mem_metrics['total']]]

    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        if not os.path.isfile(filename) or os.stat(filename).st_size == 0:
            writer.writerow(headers)
        writer.writerows(data)

def print_summary(results, total_energy, average_power, runtime, requests_per_second, cpu_metrics, mem_metrics, output_json, output_csv, server):
    logger.info("=== Measurement Summary ===")
    logger.info(f"Server: {server}")
    logger.info(f"Total Requests: {results['total']}, Successful: {results['success']}, Failed: {results['failure']}")
    logger.info(f"Execution Time: {runtime:.2f} s, Requests/s: {requests_per_second:.2f}")
    logger.info(f"Energy: Total {total_energy:.2f} J, Avg Power {average_power:.2f} W")
    logger.info(f"CPU: Avg {cpu_metrics['avg']:.2f}%, Peak {cpu_metrics['peak']:.2f}%, Total {cpu_metrics['total']:.2f}%")
    logger.info(f"Memory: Avg {mem_metrics['avg']:.2f} MB, Peak {mem_metrics['peak']:.2f} MB, Total {mem_metrics['total']:.2f} MB")
    logger.info(f"JSON: {output_json}, CSV: {output_csv or f'results_local/{server}.csv'}")
    logger.info("==========================")

def main():
    parser = argparse.ArgumentParser(description="Measure local web server energy usage")
    parser.add_argument('--server', type=str, required=True, choices=['nginx', 'yaws'], help="Server type to test (nginx or yaws)")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send (default: 500)")
    parser.add_argument('--max_workers', type=int, default=None, help="Max workers for ThreadPoolExecutor (default: None, uses system default)")
    parser.add_argument('--step', type=int, default=None, help="Scaphandre sampling interval in seconds (optional)")
    parser.add_argument('--step_nano', type=int, default=None, help="Sampling interval in nanoseconds (optional, min 100,000 ns)")
    parser.add_argument('--max_top_consumers', type=int, default=None, help="Maximum number of top consuming processes to monitor")
    parser.add_argument('--output_csv', type=str, default=None, help="Output CSV file path (default: results_local/<server>.csv)")
    parser.add_argument('--output_json', type=str, default=None, help="Output JSON file name (default: output/<timestamp>.json)")
    parser.add_argument('--verbose', action='store_true', help="Enable verbose logging")
    parser.add_argument('--measurement_type', type=str, default=None, help="Type of measurement (local, etc.)")
    
    args = parser.parse_args()
    if args.verbose:
        logger.setLevel(logging.DEBUG)

    try:
        scaphandre_path = get_binary_path("scaphandre")
    except (FileNotFoundError, RuntimeError) as e:
        logger.error(f"Error: {e}")
        return

    server_exe_map = {
        'nginx': '/usr/local/nginx/sbin/nginx',
        'yaws': '/usr/local/bin/yaws'
    }
    server_exe = server_exe_map[args.server]
    num_cores = os.cpu_count()
    
    output_json = args.output_json or os.path.join("output", datetime.now().strftime("%Y-%m-%d-%H%M%S") + ".json")
    url = "http://localhost:8001/"

    cleanup_existing_scaphandre()
    cleanup_existing_server(args.server)

    try:
        start_local_server(args.server)
    except (FileNotFoundError, RuntimeError) as e:
        logger.error(f"Server startup failed: {e}")
        return

    if not check_server_health(url):
        stop_local_server(args.server)
        return
    
    logger.info("Starting Scaphandre...")
    try:
        scaphandre_process = start_scaphandre(output_json, scaphandre_path, args.step, args.step_nano, args.max_top_consumers)
    except (ValueError, RuntimeError) as e:
        logger.error(f"Failed to start Scaphandre: {e}")
        stop_local_server(args.server)
        return
    
    logger.info(f"Sending {args.num_requests} requests to {url}...")
    time.sleep(5)

    stop_event = threading.Event()
    resource_results = {'cpu': {}, 'mem': {}}
    def collect():
        cpu_metrics, mem_metrics = collect_resources(args.server, stop_event, num_cores)
        resource_results['cpu'] = cpu_metrics
        resource_results['mem'] = mem_metrics

    resource_thread = threading.Thread(target=collect)
    resource_thread.start()

    start_time = time.time()
    with ThreadPoolExecutor(max_workers=args.max_workers) as executor:
        executor.map(lambda i: send_request(url, i, args.verbose), range(args.num_requests))
    runtime = time.time() - start_time
    results_counter['runtime'] = runtime

    logger.info("Waiting for Scaphandre to collect data...")
    time.sleep(10)
    stop_event.set()
    resource_thread.join()

    requests_per_second = results_counter['total'] / runtime if runtime > 0 else 0
    
    stop_scaphandre(scaphandre_process)
    stop_local_server(args.server)

    total_energy, average_power, total_samples = parse_json_and_compute_energy(output_json, server_exe, runtime)
    measurement_type = args.measurement_type or "local"
    save_results_to_csv(args.output_csv, results_counter, total_energy, average_power, runtime, requests_per_second, 
                       total_samples, resource_results['cpu'], resource_results['mem'], args.server, measurement_type)
    print_summary(results_counter, total_energy, average_power, runtime, requests_per_second, 
                  resource_results['cpu'], resource_results['mem'], output_json, args.output_csv, args.server)

if __name__ == "__main__":
    main()