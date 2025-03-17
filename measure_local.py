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
import psutil

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

def send_request(url, request_num):
    """Send an HTTP request and track progress."""
    try:
        response = requests.get(url, timeout=5)
        if 200 <= response.status_code < 300:
            results_counter['success'] += 1
        else:
            results_counter['failure'] += 1
            print(f"Request {request_num} failed: {response.status_code}, Response: {response.text}")
    except requests.exceptions.RequestException as e:
        results_counter['failure'] += 1
        print(f"Request {request_num} exception: {e}")
    finally:
        results_counter['total'] += 1

def cleanup_existing_server(server):
    """Stop any existing server process using the setup script."""
    script_path = f"./setup_{server}.sh"
    if not os.path.exists(script_path):
        raise FileNotFoundError(f"Setup script {script_path} not found")
    try:
        subprocess.run(["sudo", script_path, "stop"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print(f"Stopped existing {server} process using {script_path} stop")
    except subprocess.CalledProcessError:
        print(f"No existing {server} process found to stop")

def cleanup_existing_scaphandre():
    """Kill any existing scaphandre processes."""
    try:
        subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print("Killed existing scaphandre processes")
    except subprocess.CalledProcessError:
        print("No existing scaphandre processes found to kill")

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
            raise ValueError(f"Max-top-consumers {max_top_consumers} must be a positive integer")
        scaphandre_command.extend(["--max-top-consumers", str(max_top_consumers)])
    
    scaphandre_process = subprocess.Popen(scaphandre_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(5)
    if scaphandre_process.poll() is not None:
        stdout, stderr = scaphandre_process.communicate()
        if stdout:
            print(f"Scaphandre stdout: {stdout}")
        if stderr:
            print(f"Scaphandre stderr: {stderr}")
        raise RuntimeError("Scaphandre failed to start or exited prematurely")
    
    print("Scaphandre started on host")
    return scaphandre_process

def stop_scaphandre(scaphandre_process):
    print("Stopping Scaphandre process...")
    scaphandre_process.terminate()
    try:
        scaphandre_process.wait(timeout=5)
        print(f"Scaphandre (PID {scaphandre_process.pid}) terminated gracefully")
    except subprocess.TimeoutExpired:
        print(f"Scaphandre (PID {scaphandre_process.pid}) did not stop gracefully; forcing termination...")
        scaphandre_process.kill()
        scaphandre_process.wait(timeout=5)
    try:
        subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=False)
        print("Ensured all scaphandre processes are terminated")
    except subprocess.CalledProcessError:
        print("No additional scaphandre processes found to kill")

def check_server_health(url, retries=5, delay=2):
    """Check if the server is responsive by sending a test request."""
    for i in range(retries):
        try:
            response = requests.get(url, timeout=5)
            if response.status_code == 200:
                print(f"Server health check passed: {url} responded with status 200")
                return True
        except requests.exceptions.RequestException as e:
            print(f"Health check attempt {i+1}/{retries} failed: {e}")
            time.sleep(delay)
    print(f"Server health check failed after {retries} attempts")
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

    start_cmd = ["sudo", script_path, "run"]
    server_process = subprocess.Popen(start_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout, stderr = server_process.communicate()
    if server_process.returncode != 0:
        raise RuntimeError(f"Failed to start {server}: {stderr}. If not installed, run: sudo {script_path} install")
    time.sleep(5)
    print(f"Started {server} using {script_path} run")

def stop_local_server(server):
    """Stop the local server using the setup script."""
    script_path = f"./setup_{server}.sh"
    if not os.path.exists(script_path):
        raise FileNotFoundError(f"Setup script {script_path} not found")
    print(f"Stopping {server} server...")
    result = subprocess.run(["sudo", script_path, "stop"], capture_output=True, text=True)
    if result.returncode == 0:
        print(f"{server} server stopped successfully")
    else:
        print(f"Warning: {server} stop returned non-zero exit code: {result.stderr}")

def collect_resources(server, stop_event, num_cores, interval=0.5):
    """Collect CPU and memory usage for all server processes using psutil."""
    cpu_usage = []
    mem_usage = []
    pgrep_pattern = "nginx" if server == "nginx" else "beam.smp.*yaws"
    
    while not stop_event.is_set():
        try:
            result = subprocess.run(["sudo", "pgrep", "-f", pgrep_pattern], capture_output=True, text=True, check=True)
            pids_raw = result.stdout.strip().split('\n')
            if not pids_raw or pids_raw == ['']:
                print(f"No PIDs found for {server} with sudo pgrep -f '{pgrep_pattern}'")
                cpu_usage.append(0.0)
                mem_usage.append(0.0)
                time.sleep(interval)
                continue
            
            pids = []
            for pid in pids_raw:
                if not pid or pid == "0":
                    continue
                try:
                    proc = psutil.Process(int(pid))
                    cmd = " ".join(proc.cmdline())
                    if ("nginx" in cmd if server == "nginx" else "beam.smp" in cmd) and "sudo" not in cmd and "grep" not in cmd:
                        pids.append(pid)
                except (psutil.NoSuchProcess, psutil.AccessDenied, ValueError):
                    continue
            
            if not pids:
                print(f"No valid {server} PIDs found after filtering")
                cpu_usage.append(0.0)
                mem_usage.append(0.0)
                time.sleep(interval)
                continue
            print(f"Monitoring PIDs {pids} for {server}")
        except subprocess.CalledProcessError as e:
            print(f"Could not find PIDs for {server}: {e}")
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
                    
                    print(f"PID {pid} CPU usage: {cpu_percent:.2f}%")
                    print(f"PID {pid} Memory usage: {mem_mb:.2f} MB")
                except (psutil.NoSuchProcess, psutil.AccessDenied) as e:
                    print(f"Skipping PID {pid}: {e}")
                    continue

            avg_cpu = cpu_total / pid_count if pid_count > 0 else 0.0
            avg_mem = mem_total_mb if pid_count > 0 else 0.0
            
            cpu_usage.append(avg_cpu)
            mem_usage.append(avg_mem)
            print(f"Cycle CPU usage (%): {avg_cpu:.2f}")
            print(f"Cycle Memory usage (MB): {avg_mem:.2f}")
        except Exception as e:
            print(f"Error collecting stats: {e}")
            cpu_usage.append(0.0)
            mem_usage.append(0.0)
        
        time.sleep(interval)
    
    total_system_capacity = num_cores * 100.0
    cpu_system_pct = [cpu / total_system_capacity * 100.0 for cpu in cpu_usage]
    cpu_cores = [cpu / 100.0 for cpu in cpu_usage]
    
    print(f"Collected CPU usage (%): {cpu_usage}")
    print(f"Collected Memory usage (MB): {mem_usage}")
    return cpu_system_pct, cpu_cores, mem_usage

def parse_json_and_compute_energy(file_name, server_exe, runtime):
    """Parse JSON and compute total energy in Joules from power in microwatts."""
    json_file_path = file_name
    try:
        with open(json_file_path, "r") as file:
            data = json.load(file)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Error reading JSON file: {e}")
        return 0.0, 0.0, 0

    total_power_microwatts = 0.0
    number_samples = 0
    timestamps = []

    for entry in data:
        consumers = entry.get("consumers", [])
        host_timestamp = entry.get("host", {}).get("timestamp")
        if host_timestamp:
            timestamps.append(host_timestamp)
        for consumer in consumers:
            exe = consumer.get("exe", "")
            cmdline = consumer.get("cmdline", "")
            if server_exe in exe or ("beam.smp" in exe and "yaws" in cmdline.lower()):
                power_microwatts = consumer.get("consumption", 0.0)
                if power_microwatts > 0:
                    total_power_microwatts += power_microwatts
                    number_samples += 1
                    print(f"Added power for {exe} (cmdline: {cmdline}): {power_microwatts:.2f} µW")

    if number_samples == 0:
        print(f"No non-zero energy data found for {server_exe}")
        return 0.0, 0.0, 0

    print(f"Found {number_samples} non-zero samples for {server_exe}")
    average_power_watts = (total_power_microwatts / number_samples) * 1e-6
    total_energy_joules = average_power_watts * runtime
    energy_per_request = total_energy_joules / results_counter['success'] if results_counter['success'] > 0 else 0.0
    print(f"Energy per successful request: {energy_per_request:.6f} J/request")

    if len(timestamps) > 1:
        intervals = [timestamps[i+1] - timestamps[i] for i in range(len(timestamps)-1)]
        avg_interval = sum(intervals) / len(intervals)
        print(f"Average sampling interval: {avg_interval:.6f} seconds")
    else:
        print("Average sampling interval: N/A (insufficient samples)")

    return total_energy_joules, average_power_watts, number_samples

def save_results_to_csv(filename, results, total_energy, average_power, total_runtime, requests_per_second, total_samples, avg_cpu_system_pct, avg_cpu_cores, avg_mem, server=None, num_requests=None):
    """Save results to a CSV file in results_local folder."""
    if filename is None:
        os.makedirs("results_local", exist_ok=True)
        filename = os.path.join("results_local", f"{server}.csv")
    
    headers = ["Total Requests", "Successful Requests", "Failed Requests", "Execution Time (seconds)", "Requests Per Second",
               "Total Energy Consumption (J)", "Average Power Consumption (W)", "Number of Samples",
               "Average CPU Usage (% of System)", "Average CPU Usage (Cores)", "Average Memory Usage (MB)"]
    data = [
        [
            results['total'],
            results['success'],
            results['failure'],
            total_runtime,
            requests_per_second,
            total_energy,
            average_power,
            total_samples,
            avg_cpu_system_pct,
            avg_cpu_cores,
            avg_mem
        ]
    ]
    
    file_exists = os.path.isfile(filename)
    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(headers)
        writer.writerows(data)
    print(f"Results appended to {filename}")

def main():
    parser = argparse.ArgumentParser(description="Measure energy consumption of a locally installed web server with Scaphandre.")
    parser.add_argument('--server', type=str, required=True, choices=['nginx', 'yaws'], help="Server type to test (nginx or yaws)")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send")
    parser.add_argument('--step', type=int, default=None, help="Scaphandre sampling interval in seconds (optional)")
    parser.add_argument('--step_nano', type=int, default=None, help="Sampling interval in nanoseconds (optional, min 100,000 ns)")
    parser.add_argument('--max_top_consumers', type=int, default=None, help="Maximum number of top consuming processes to monitor")
    parser.add_argument('--output_csv', type=str, default=None, help="Output CSV file path (defaults to server.csv in results_local folder)")
    parser.add_argument('--output_json', type=str, default=None, help="Output JSON file name (defaults to timestamp)")

    args = parser.parse_args()

    try:
        scaphandre_path = get_binary_path("scaphandre")
    except (FileNotFoundError, RuntimeError) as e:
        print(f"Error: {e}")
        return

    server_exe_map = {
        'nginx': '/usr/local/nginx/sbin/nginx',
        'yaws': '/usr/local/bin/yaws'
    }
    server_exe = server_exe_map[args.server]

    num_cores = os.cpu_count()
    print(f"Detected {num_cores} CPU cores")

    if args.output_json is None:
        timestamp = datetime.now().strftime("%Y-%m-%d-%H%M%S")
        output_json = os.path.join("output", f"{timestamp}.json")
    else:
        output_json = os.path.join("output", args.output_json)

    url = "http://localhost:8001/"
    n = args.num_requests

    cleanup_existing_scaphandre()
    cleanup_existing_server(args.server)

    print(f"Starting {args.server} server...")
    try:
        start_local_server(args.server)
    except (FileNotFoundError, RuntimeError) as e:
        print(f"Server startup failed: {e}")
        return

    print(f"Checking server health at {url}...")
    if not check_server_health(url):
        print("Server failed health check, stopping and exiting...")
        stop_local_server(args.server)
        return

    print("Starting Scaphandre...")
    try:
        scaphandre_process = start_scaphandre(output_json, scaphandre_path, args.step, args.step_nano, args.max_top_consumers)
    except (ValueError, RuntimeError) as e:
        print(f"Failed to start Scaphandre: {e}")
        stop_local_server(args.server)
        return
    
    print(f"Sending {n} requests to {url}...")
    time.sleep(5)

    stop_event = threading.Event()
    resource_results = {'cpu_system_pct': [], 'cpu_cores': [], 'mem': []}
    def collect():
        cpu_system_pct, cpu_cores, mem_usage = collect_resources(args.server, stop_event, num_cores, 0.5)
        resource_results['cpu_system_pct'] = cpu_system_pct
        resource_results['cpu_cores'] = cpu_cores
        resource_results['mem'] = mem_usage

    resource_thread = threading.Thread(target=collect)
    resource_thread.start()

    start_time = time.time()
    with ThreadPoolExecutor() as executor:
        futures = [executor.submit(send_request, url, i+1) for i in range(n)]
        for _ in futures:
            _.result()
    end_time = time.time()
    runtime = end_time - start_time
    results_counter['runtime'] = runtime

    print("Waiting for Scaphandre to collect data...")
    time.sleep(10)
    stop_event.set()
    resource_thread.join()

    avg_cpu_system_pct = sum(resource_results['cpu_system_pct']) / len(resource_results['cpu_system_pct']) if resource_results['cpu_system_pct'] else 0.0
    avg_cpu_cores = sum(resource_results['cpu_cores']) / len(resource_results['cpu_cores']) if resource_results['cpu_cores'] else 0.0
    avg_mem = sum(resource_results['mem']) / len(resource_results['mem']) if resource_results['mem'] else 0.0

    requests_per_second = results_counter['total'] / runtime if runtime > 0 else 0

    stop_scaphandre(scaphandre_process)
    stop_local_server(args.server)

    total_energy, average_power, total_samples = parse_json_and_compute_energy(output_json, server_exe, runtime)
    save_results_to_csv(args.output_csv, results_counter, total_energy, average_power, runtime, requests_per_second, total_samples,
                        avg_cpu_system_pct, avg_cpu_cores, avg_mem, server=args.server, num_requests=n)

    print("\nSummary:")
    print(f"Total Requests Sent: {results_counter['total']}")
    print(f"Successful Requests: {results_counter['success']}")
    print(f"Failed Requests: {results_counter['failure']}")
    print(f"Execution Time: {runtime:.2f} seconds")
    print(f"Requests Per Second: {requests_per_second:.2f}")
    print(f"Total Energy Consumption: {total_energy:.6f} J")
    print(f"Average Power Consumption: {average_power:.6f} W")
    print(f"Average CPU Usage (% of System): {avg_cpu_system_pct:.2f}%")
    print(f"Average CPU Usage (Cores): {avg_cpu_cores:.2f}")
    print(f"Average Memory Usage: {avg_mem:.2f} MB")
    print(f"JSON output saved to: {output_json}")
    print(f"Measured energy for server: {args.server}")

if __name__ == "__main__":
    main()