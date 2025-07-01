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
    result = subprocess.run(["sudo", "which", binary_name], capture_output=True, text=True, check=True)
    return result.stdout.strip() or f"'{binary_name}' not found"

def send_request(url, request_num, verbose=False):
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

def cleanup_existing_container(container_name, docker_path):
    subprocess.run(["sudo", docker_path, "stop", container_name], capture_output=True, text=True, check=False)
    subprocess.run(["sudo", docker_path, "rm", "-f", container_name], capture_output=True, text=True, check=False)

def cleanup_existing_scaphandre():
    subprocess.run(["sudo", "pkill", "-9", "scaphandre"], capture_output=True, text=True, check=False)

def start_scaphandre(output_json, scaphandre_path):
    os.makedirs("output", exist_ok=True)
    scaphandre_process = subprocess.Popen(["sudo", scaphandre_path, "json", "--containers", "-f", output_json], 
                                          stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(2)
    if scaphandre_process.poll() is not None:
        raise RuntimeError("Scaphandre failed to start")
    return scaphandre_process

def stop_scaphandre(scaphandre_process):
    scaphandre_process.terminate()
    scaphandre_process.wait(timeout=5)

def check_container_health(url, retries=5, delay=1):
    for _ in range(retries):
        try:
            if requests.get(url, timeout=5).status_code == 200:
                return True
        except requests.exceptions.RequestException:
            time.sleep(delay)
    return False

def start_server_container(server_image, port_mapping, container_name, docker_path, network="bridge"):
    cleanup_existing_container(container_name, docker_path)
    cmd = ["sudo", docker_path, "run", "-d", "--name", container_name]
    if network == "host":
        cmd.extend(["--network", "host"])
    else:
        cmd.extend(["-p", port_mapping])
    cmd.append(server_image)
    subprocess.run(cmd, capture_output=True, text=True, check=True)
    time.sleep(5)

def stop_server_container(container_name, docker_path):
    subprocess.run(["sudo", docker_path, "stop", container_name], capture_output=True, text=True, check=True)
    subprocess.run(["sudo", docker_path, "rm", container_name], capture_output=True, text=True, check=True)

def collect_resources(server_image, stop_event, num_cores, interval=0.5):
    cpu_usage = []
    mem_usage = []
    server_name = server_image.split('/')[-1].split(':')[0].lower()
    if "nginx" in server_name:
        pgrep_pattern = "nginx"
    elif "apache2" in server_name or "httpd" in server_name:
        pgrep_pattern = "apache2|httpd"
    elif "yaws" in server_name or "cowboy" in server_name:
        pgrep_pattern = "beam.smp"
    else:
        pgrep_pattern = "beam.smp"
    
    while not stop_event.is_set():
        try:
            result = subprocess.run(["sudo", "pgrep", "-f", pgrep_pattern], capture_output=True, text=True, check=True)
            pids_raw = result.stdout.strip().split('\n')
            logger.debug(f"Raw PIDs from pgrep '{pgrep_pattern}': {pids_raw}")
            if not pids_raw or pids_raw == ['']:
                logger.debug(f"No PIDs found for {server_name} with sudo pgrep -f '{pgrep_pattern}'")
            else:
                logger.debug(f"Found {len(pids_raw)} PIDs for {server_name}")
            
            pids = []
            for pid in pids_raw:
                if not pid or pid == "0":
                    continue
                try:
                    proc = psutil.Process(int(pid))
                    cmd = " ".join(proc.cmdline())
                    logger.debug(f"PID {pid} cmdline: '{cmd}'")
                    if ("nginx" in server_name and "nginx" in cmd.lower()) or \
                       (("apache2" in server_name or "httpd" in server_name) and ("apache" in cmd.lower() or "httpd" in cmd.lower())) or \
                       (("yaws" in server_name or "cowboy" in server_name) and "beam.smp" in cmd.lower()):
                        if "sudo" not in cmd and "grep" not in cmd:
                            pids.append(pid)
                except (psutil.NoSuchProcess, psutil.AccessDenied) as e:
                    logger.debug(f"Skipping PID {pid}: {e}")
                    continue
            
            if not pids:
                logger.debug(f"No valid {server_name} PIDs found after filtering pgrep results")
                for proc in psutil.process_iter(['pid', 'cmdline', 'exe']):
                    try:
                        cmd = " ".join(proc.info['cmdline'] or [])
                        exe = proc.info['exe'] or ""
                        logger.debug(f"Scanning PID {proc.info['pid']} cmdline: '{cmd}', exe: '{exe}'")
                        if ("nginx" in server_name and "nginx" in (cmd.lower() + exe.lower())) or \
                           (("apache2" in server_name or "httpd" in server_name) and ("apache" in (cmd.lower() + exe.lower()) or "httpd" in (cmd.lower() + exe.lower()))) or \
                           (("yaws" in server_name or "cowboy" in server_name) and "beam.smp" in (cmd.lower() + exe.lower())):
                            if "sudo" not in cmd and "grep" not in cmd:
                                pids.append(proc.info['pid'])
                                logger.debug(f"Fallback found PID {proc.info['pid']} for {server_name}")
                    except (psutil.NoSuchProcess, psutil.AccessDenied):
                        continue
            
            if not pids:
                logger.debug(f"No valid {server_name} PIDs found after fallback")
                cpu_usage.append(0.0)
                mem_usage.append(0.0)
                time.sleep(interval)
                continue
            
            logger.debug(f"Monitoring PIDs {pids} for {server_name}")
        
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
    
    non_zero_cpu = [x for x in cpu_system_pct if x > 0.0]
    non_zero_mem = [x for x in mem_usage if x > 0.0]
    
    cpu_avg = sum(non_zero_cpu) / len(non_zero_cpu) if non_zero_cpu else 0.0
    cpu_peak = max(cpu_usage) / num_cores if cpu_usage else 0.0
    cpu_total = sum(cpu_usage) if cpu_usage else 0.0
    mem_avg = sum(non_zero_mem) / len(non_zero_mem) if non_zero_mem else 0.0
    mem_peak = max(mem_usage) if mem_usage else 0.0
    mem_total = sum(mem_usage) if mem_usage else 0.0
    
    return {'avg': cpu_avg, 'peak': cpu_peak, 'total': cpu_total}, \
           {'avg': mem_avg, 'peak': mem_peak, 'total': mem_total}

def parse_json_and_compute_energy(file_name, container_name, runtime):
    with open(file_name, "r") as file:
        data = json.load(file)
    
    total_power_microwatts = 0.0
    number_samples = 0
    
    for entry in data:
        for consumer in entry.get("consumers", []):
            container = consumer.get("container")
            if container and container.get("name") == container_name:
                power = consumer.get("consumption", 0.0)
                if power > 0:
                    total_power_microwatts += power
                    number_samples += 1
    
    if number_samples == 0:
        return 0.0, 0.0, 0
    
    avg_power_watts = (total_power_microwatts / number_samples) * 1e-6
    total_energy_joules = avg_power_watts * runtime
    return total_energy_joules, avg_power_watts, number_samples

def save_results_to_csv(filename, results, total_energy, average_power, runtime, requests_per_second, total_samples, 
                       cpu_metrics, mem_metrics, num_cores, container_name, measurement_type):
    if filename is None:
        os.makedirs("results_docker", exist_ok=True)
        filename = os.path.join("results_docker", f"{container_name}.csv")
    
    headers = ["container_name", "type", "Total Requests", "Successful Requests", "Failed Requests", "Execution Time (s)", "Requests/s",
               "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%)",
               "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB)"]
    data = [[container_name, measurement_type, results['total'], results['success'], results['failure'], runtime, requests_per_second,
             total_energy, average_power, total_samples, cpu_metrics['avg'], cpu_metrics['peak'],
             cpu_metrics['total'], mem_metrics['avg'], mem_metrics['peak'], mem_metrics['total']]]

    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        if not os.path.isfile(filename) or os.stat(filename).st_size == 0:
            writer.writerow(headers)
        writer.writerows(data)

def print_summary(results, total_energy, average_power, runtime, requests_per_second, cpu_metrics, mem_metrics, num_cores, output_json, output_csv, container_name):
    logger.info("=== Measurement Summary ===")
    logger.info(f"Container: {container_name}")
    logger.info(f"Total Requests: {results['total']}, Successful: {results['success']}, Failed: {results['failure']}")
    logger.info(f"Execution Time: {runtime:.2f} s, Requests/s: {requests_per_second:.2f}")
    logger.info(f"Energy: Total {total_energy:.2f} J, Avg Power {average_power:.2f} W")
    logger.info(f"CPU: Avg {cpu_metrics['avg']:.2f}%, Peak {cpu_metrics['peak']:.2f}%, Total {cpu_metrics['total']:.2f}%")
    logger.info(f"Memory: Avg {mem_metrics['avg']:.2f} MB, Peak {mem_metrics['peak']:.2f} MB, Total {mem_metrics['total']:.2f} MB")
    logger.info(f"JSON: {output_json}, CSV: {output_csv or f'results_docker/{container_name}.csv'}")
    logger.info("==========================")

def main():
    parser = argparse.ArgumentParser(description="Measure web server energy with Scaphandre in Docker")
    parser.add_argument('--server_image', type=str, required=True, help="Docker image of the server (e.g., nginx-deb)")
    parser.add_argument('--container_name', type=str, default=None, help="Name of the Docker container (defaults to server_image)")
    parser.add_argument('--port_mapping', type=str, default='8001:80', help="Port mapping (default: 8001:80)")
    parser.add_argument('--network', type=str, default='bridge', choices=['bridge', 'host'], help="Network mode (default: bridge)")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send (default: 500)")
    parser.add_argument('--max_workers', type=int, default=None, help="Max workers for ThreadPoolExecutor (default: None, uses system default)")
    parser.add_argument('--output_csv', type=str, default=None, help="Output CSV file path (default: results_docker/<container_name>.csv)")
    parser.add_argument('--output_json', type=str, default=None, help="Output JSON file path (default: output/<timestamp>.json)")
    parser.add_argument('--verbose', action='store_true', help="Enable verbose logging")
    parser.add_argument('--measurement_type', type=str, default=None, help="Type of measurement (static, dynamic, etc.)")
    
    args = parser.parse_args()
    if args.verbose:
        logger.setLevel(logging.DEBUG)

    scaphandre_path = get_binary_path("scaphandre")
    global docker_path
    docker_path = get_binary_path("docker")
    num_cores = os.cpu_count()
    
    output_json = args.output_json or os.path.join("output", datetime.now().strftime("%Y-%m-%d-%H%M%S") + ".json")
    url = "http://localhost:80/" if args.network == "host" else f"http://localhost:{args.port_mapping.split(':')[0]}/"
    container_name = args.container_name or args.server_image

    cleanup_existing_scaphandre()
    logger.info(f"Starting container '{container_name}'...")
    start_server_container(args.server_image, args.port_mapping, container_name, docker_path, args.network)
    
    if not check_container_health(url):
        logger.error("Container health check failed")
        stop_server_container(container_name, docker_path)
        return
    
    logger.info("Starting Scaphandre...")
    scaphandre_process = start_scaphandre(output_json, scaphandre_path)
    
    logger.info(f"Sending {args.num_requests} requests to {url}...")
    time.sleep(2)

    stop_event = threading.Event()
    resource_results = {'cpu': {}, 'mem': {}}
    def collect():
        cpu_metrics, mem_metrics = collect_resources(args.server_image, stop_event, num_cores)
        resource_results['cpu'] = cpu_metrics
        resource_results['mem'] = mem_metrics

    resource_thread = threading.Thread(target=collect)
    resource_thread.start()

    start_time = time.time()
    with ThreadPoolExecutor(max_workers=args.max_workers) as executor:
        executor.map(lambda i: send_request(url, i, args.verbose), range(args.num_requests))
    runtime = time.time() - start_time
    results_counter['runtime'] = runtime

    time.sleep(3)
    stop_event.set()
    resource_thread.join()

    requests_per_second = results_counter['total'] / runtime if runtime > 0 else 0
    
    logger.info("Waiting for Scaphandre...")
    time.sleep(5)
    
    stop_scaphandre(scaphandre_process)
    stop_server_container(container_name, docker_path)

    total_energy, average_power, total_samples = parse_json_and_compute_energy(output_json, container_name, runtime)
    measurement_type = getattr(args, 'measurement_type', None) or "unknown"
    save_results_to_csv(args.output_csv, results_counter, total_energy, average_power, runtime, requests_per_second, 
                       total_samples, resource_results['cpu'], resource_results['mem'], num_cores, args.server_image, measurement_type)
    print_summary(results_counter, total_energy, average_power, runtime, requests_per_second, 
                  resource_results['cpu'], resource_results['mem'], num_cores, output_json, args.output_csv, container_name)

if __name__ == "__main__":
    main()