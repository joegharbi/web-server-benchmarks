import os
import time
import subprocess
import asyncio
import websockets
import csv
from concurrent.futures import ThreadPoolExecutor
from collections import Counter
import argparse
import json
import threading
from datetime import datetime
import logging
import psutil
import docker

logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger()

results_counter = Counter()

def get_binary_path(binary_name):
    try:
        result = subprocess.run(["sudo", "which", binary_name], capture_output=True, text=True, check=True)
        path = result.stdout.strip()
        if not path:
            raise FileNotFoundError(f"Binary '{binary_name}' not found")
        return path
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to locate {binary_name}: {e.stderr}") from e

async def send_large_data(url, client_num, size_mb, interval_s, duration_s):
    try:
        async with websockets.connect(url, max_size=None) as ws:
            payload = b"x" * (size_mb * 1024 * 1024)
            end_time = time.time() + duration_s
            while time.time() < end_time:
                await ws.send(payload)
                await ws.recv()
                await asyncio.sleep(interval_s)
            results_counter['success'] += 1
    except Exception as e:
        logger.debug(f"Client {client_num} failed: {e}")
        results_counter['failure'] += 1
    finally:
        results_counter['total'] += 1

async def stream_data(url, client_num, rate_mb_s, duration_s):
    try:
        async with websockets.connect(url, max_size=None) as ws:
            chunk_size = 1 * 1024 * 1024  # 1MB chunks
            chunks_per_sec = int(rate_mb_s)
            end_time = time.time() + duration_s
            while time.time() < end_time:
                for _ in range(chunks_per_sec):
                    await ws.send(b"x" * chunk_size)
                    await ws.recv()
                await asyncio.sleep(1)
            results_counter['success'] += 1
    except Exception as e:
        logger.debug(f"Client {client_num} failed: {e}")
        results_counter['failure'] += 1
    finally:
        results_counter['total'] += 1

def run_client(url, client_num, size_mb=None, interval_s=None, rate_mb_s=None, duration_s=300):
    if size_mb and interval_s:
        asyncio.run(send_large_data(url, client_num, size_mb, interval_s, duration_s))
    elif rate_mb_s:
        asyncio.run(stream_data(url, client_num, rate_mb_s, duration_s))

def cleanup_existing_scaphandre():
    try:
        subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        logger.debug("Killed existing scaphandre processes")
    except subprocess.CalledProcessError:
        logger.debug("No existing scaphandre processes found")

def cleanup_existing_container(container_name):
    client = docker.from_env()
    try:
        container = client.containers.get(container_name)
        logger.info(f"Stopping existing container '{container_name}'...")
        container.stop()
        container.remove(force=True)
        logger.info(f"Removed existing container '{container_name}'")
        # Wait and check if container is really gone
        for _ in range(5):
            try:
                client.containers.get(container_name)
                time.sleep(1)
            except docker.errors.NotFound:
                break
        else:
            logger.warning(f"Container '{container_name}' could not be removed after multiple attempts.")
        time.sleep(2)  # Ensure Docker/OS releases resources
    except docker.errors.NotFound:
        logger.debug(f"No existing container '{container_name}' found")
        time.sleep(2)
    except docker.errors.APIError as e:
        logger.error(f"Error cleaning up container '{container_name}': {e}")
        time.sleep(2)

def start_scaphandre(output_json, scaphandre_path):
    os.makedirs("output", exist_ok=True)
    cmd = ["sudo", scaphandre_path, "json", "-f", output_json, "--containers"]
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(5)
    if process.poll() is not None:
        stdout, stderr = process.communicate()
        raise RuntimeError(f"Scaphandre failed: {stderr or stdout}")
    logger.debug("Scaphandre started with container support")
    return process

def stop_scaphandre(scaphandre_process):
    logger.debug("Stopping Scaphandre...")
    scaphandre_process.terminate()
    try:
        scaphandre_process.wait(timeout=5)
        logger.debug(f"Scaphandre (PID {scaphandre_process.pid}) terminated")
    except subprocess.TimeoutExpired:
        scaphandre_process.kill()
        scaphandre_process.wait(timeout=5)
    subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=False)
    time.sleep(2)  # Ensure OS releases resources

def collect_resources_docker_stats(container_name, stop_event, interval=0.5):
    import re
    cpu_usage = []
    mem_usage = []
    sample_count = 0
    docker_path = 'docker'  # Assume docker is in PATH
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
    # Only consider non-zero samples for averages
    non_zero_cpu = [x for x in cpu_usage if x > 0.0]
    non_zero_mem = [x for x in mem_usage if x > 0.0]
    cpu_avg = sum(non_zero_cpu) / len(non_zero_cpu) if non_zero_cpu else 0.0
    cpu_peak = max(cpu_usage) if cpu_usage else 0.0
    cpu_total = sum(cpu_usage) if cpu_usage else 0.0
    mem_avg = sum(non_zero_mem) / len(non_zero_mem) if non_zero_mem else 0.0
    mem_peak = max(mem_usage) if mem_usage else 0.0
    mem_total = sum(mem_usage) if mem_usage else 0.0
    return {'avg': cpu_avg, 'peak': cpu_peak, 'total': cpu_total}, \
           {'avg': mem_avg, 'peak': mem_peak, 'total': mem_total}

def parse_json_and_compute_energy(file_name, container_name, runtime):
    try:
        with open(file_name, "r") as file:
            data = json.load(file)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        logger.error(f"Error reading JSON: {e}")
        return 0.0, 0.0, 0

    total_power_microwatts = 0.0
    number_samples = 0
    for entry in data:
        for consumer in entry.get("consumers", []):
            container_info = consumer.get("container")
            if container_info and container_info.get("name") == container_name:
                power = consumer.get("consumption", 0.0)
                if power > 0:
                    total_power_microwatts += power
                    number_samples += 1

    if number_samples == 0:
        logger.info(f"No energy data for container {container_name}")
        return 0.0, 0.0, 0

    avg_power_watts = (total_power_microwatts / number_samples) * 1e-6
    total_energy_joules = avg_power_watts * runtime
    return total_energy_joules, avg_power_watts, number_samples

def save_results_to_csv(filename, results, total_energy, average_power, runtime, throughput_mb_s, total_samples, cpu_metrics, mem_metrics, container_name, measurement_type, extra_fields=None):
    if filename is None:
        os.makedirs("results_websocket", exist_ok=True)
        filename = os.path.join("results_websocket", f"{container_name}.csv")
    
    headers = ["container_name", "type", "Total Requests", "Successful Requests", "Failed Requests", "Execution Time (s)", "Requests/s",
               "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%)",
               "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB)"]
    row = [container_name, measurement_type, results['total'], results['success'], results['failure'], runtime, throughput_mb_s,
           total_energy, average_power, total_samples, cpu_metrics['avg'], cpu_metrics['peak'],
           cpu_metrics['total'], mem_metrics['avg'], mem_metrics['peak'], mem_metrics['total']]
    if extra_fields:
        headers += list(extra_fields.keys())
        row += list(extra_fields.values())
    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        if not os.path.isfile(filename) or os.stat(filename).st_size == 0:
            writer.writerow(headers)
        writer.writerow(row)

def print_summary(results, total_energy, average_power, runtime, throughput_mb_s, cpu_metrics, mem_metrics, output_json, output_csv, server_image):
    logger.info("=== Measurement Summary ===")
    logger.info(f"Container: {server_image}")
    logger.info(f"Total Clients: {results['total']}, Successful: {results['success']}, Failed: {results['failure']}")
    logger.info(f"Execution Time: {runtime:.2f} s, Throughput: {throughput_mb_s:.2f} MB/s")
    logger.info(f"Energy: Total {total_energy:.2f} J, Avg Power {average_power:.2f} W")
    logger.info(f"CPU: Avg {cpu_metrics['avg']:.2f}%, Peak {cpu_metrics['peak']:.2f}%, Total {cpu_metrics['total']:.2f}%")
    logger.info(f"Memory: Avg {mem_metrics['avg']:.2f} MB, Peak {mem_metrics['peak']:.2f} MB, Total {mem_metrics['total']:.2f} MB")
    logger.info(f"JSON: {output_json}, CSV: {output_csv or f'results_websocket/{server_image}.csv'}")
    logger.info("==========================")

def check_container_health(url, retries=10, delay=2):
    for _ in range(retries):
        try:
            async def check():
                async with websockets.connect(url, max_size=None) as ws:
                    await ws.send(b"ping")
                    await ws.recv()
            asyncio.run(check())
            return True
        except Exception as e:
            logger.debug(f"Health check failed: {e}")
            time.sleep(delay)
    return False

def main():
    parser = argparse.ArgumentParser(description="Measure WebSocket energy consumption in Docker")
    parser.add_argument('--server_image', type=str, required=True, help="Docker image")
    parser.add_argument('--num_clients', type=int, default=5, help="Number of WebSocket clients")
    parser.add_argument('--size_mb', type=int, default=10, help="Payload size per message (MB) for burst mode")
    parser.add_argument('--interval_s', type=float, default=1.0, help="Interval between bursts (seconds)")
    parser.add_argument('--rate_mb_s', type=float, help="Streaming rate (MB/s) per client")
    parser.add_argument('--duration_s', type=int, default=30, help="Test duration (seconds)")
    parser.add_argument('--max_workers', type=int, default=None, help="Max workers for ThreadPoolExecutor")
    parser.add_argument('--output_csv', type=str, default=None, help="Output CSV path")
    parser.add_argument('--output_json', type=str, default=None, help="Output JSON path")
    parser.add_argument('--verbose', action='store_true', help="Enable verbose logging")
    parser.add_argument('--measurement_type', type=str, default=None, help="Type of measurement (websocket, etc.)")

    args = parser.parse_args()
    measurement_type = args.measurement_type or "websocket"
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    if (args.size_mb and not args.interval_s) or (args.interval_s and not args.size_mb):
        parser.error("--size_mb and --interval_s must be used together for burst mode")
    if args.rate_mb_s and (args.size_mb or args.interval_s):
        parser.error("Use --rate_mb_s alone for streaming mode")

    try:
        scaphandre_path = get_binary_path("scaphandre")
    except (FileNotFoundError, RuntimeError) as e:
        logger.error(f"Error: {e}")
        return

    num_cores = os.cpu_count()
    output_json = args.output_json or os.path.join("output", datetime.now().strftime("%Y-%m-%d-%H%M%S") + ".json")
    url = "ws://localhost:8001/ws"  # Fixed to WebSocket protocol
    container_name = args.server_image  # Use image name as container name
    client = docker.from_env()

    cleanup_existing_scaphandre()
    cleanup_existing_container(container_name)  # Added cleanup before starting
    try:
        logger.info(f"Starting container '{container_name}'...")
        container = client.containers.run(args.server_image, name=container_name, ports={'80/tcp': 8001}, 
                                         detach=True, mem_limit="8g")
    except docker.errors.APIError as e:
        logger.error(f"Failed to start container: {e}")
        return

    # Health check
    logger.info(f"Checking container health at {url}...")
    if not check_container_health(url):
        logger.error("Container health check failed")
        container.stop()
        container.remove()
        return

    logger.info("Starting Scaphandre...")
    try:
        scaphandre_process = start_scaphandre(output_json, scaphandre_path)
    except RuntimeError as e:
        logger.error(f"Failed to start Scaphandre: {e}")
        container.stop()
        container.remove()
        return

    logger.info(f"Simulating {args.num_clients} WebSocket clients to {url}...")
    time.sleep(5)

    stop_event = threading.Event()
    resource_results = {'cpu': {}, 'mem': {}}
    def collect():
        cpu_metrics, mem_metrics = collect_resources_docker_stats(container_name, stop_event)
        resource_results['cpu'] = cpu_metrics
        resource_results['mem'] = mem_metrics

    resource_thread = threading.Thread(target=collect)
    resource_thread.start()

    start_time = time.time()
    with ThreadPoolExecutor(max_workers=args.max_workers) as executor:
        if args.size_mb and args.interval_s:
            executor.map(lambda i: run_client(url, i, size_mb=args.size_mb, interval_s=args.interval_s, duration_s=args.duration_s), range(args.num_clients))
        else:
            executor.map(lambda i: run_client(url, i, rate_mb_s=args.rate_mb_s, duration_s=args.duration_s), range(args.num_clients))
    runtime = time.time() - start_time

    logger.info("Waiting for Scaphandre to collect data...")
    time.sleep(10)
    stop_event.set()
    resource_thread.join()

    # total_data_mb = (args.num_clients * args.size_mb * (runtime / args.interval_s)) if args.size_mb else (args.num_clients * args.rate_mb_s * runtime)

    total_data_mb = (args.num_clients * args.size_mb * args.interval_s) if args.size_mb else (args.num_clients * args.rate_mb_s)
    throughput_mb_s = total_data_mb / runtime if runtime > 0 else 0

    stop_scaphandre(scaphandre_process)
    container.stop()
    container.remove()

    total_energy, average_power, total_samples = parse_json_and_compute_energy(output_json, container_name, runtime)
    # save_results_to_csv(args.output_csv, results_counter, total_energy, average_power, runtime, throughput_mb_s, 
    #                     total_samples, resource_results['cpu'], resource_results['mem'], args.server_image)
    print_summary(results_counter, total_energy, average_power, runtime, throughput_mb_s, 
                  resource_results['cpu'], resource_results['mem'], output_json, args.output_csv, args.server_image)

    # Compose output row and header to include all relevant websocket fields and match Docker CSV style
    mode = "burst" if hasattr(args, "size_mb") and args.size_mb and args.size_mb > 0 else ("streaming" if hasattr(args, "rate_mb_s") and args.rate_mb_s and args.rate_mb_s > 0 else "unknown")
    chunk_size = args.size_mb if mode == "burst" else (args.rate_mb_s if mode == "streaming" else 0)
    interval_s = getattr(args, "interval_s", None)
    duration_s = getattr(args, "duration_s", None)
    extra_fields = {
        "Mode": mode,
        "Num Clients": args.num_clients,
        "Chunk Size (MB or MB/s)": chunk_size,
        "Interval (s)": interval_s,
        "Duration (s)": duration_s
    }
    # Ensure num_cores is an int and not None
    num_cores_csv = int(num_cores) if num_cores is not None else 1
    result_row = [
        args.server_image, measurement_type, num_cores_csv, results_counter['total'], results_counter['success'], results_counter['failure'],
        runtime, throughput_mb_s, total_energy, average_power, total_samples,
        resource_results['cpu']['avg'], resource_results['cpu']['peak'], resource_results['cpu']['total'],
        resource_results['mem']['avg'], resource_results['mem']['peak'], resource_results['mem']['total'],
        mode, args.num_clients, chunk_size, interval_s, duration_s
    ]
    header = [
        "Container Name", "Type", "Num CPUs", "Total Requests", "Successful Requests", "Failed Requests", "Execution Time (s)", "Requests/s",
        "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%)",
        "Avg CPU (%) / CPU", "Peak CPU (%) / CPU", "Total CPU (%) / CPU",
        "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB)",
        "Mode", "Num Clients", "Chunk Size (MB or MB/s)", "Interval (s)", "Duration (s)"
    ]
    write_header = not os.path.exists(args.output_csv)
    with open(args.output_csv, "a", newline="") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow(header)
        writer.writerow(result_row)

if __name__ == "__main__":
    main()