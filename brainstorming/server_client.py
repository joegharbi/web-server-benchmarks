import os
import time
import subprocess
import requests
import csv
from concurrent.futures import ThreadPoolExecutor
from collections import Counter
import argparse
import json
import signal
from datetime import datetime

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

def cleanup_existing_container(container_name, docker_path):
    """Check and stop/remove any existing container with the given name."""
    try:
        result = subprocess.run(["sudo", docker_path, "ps", "-q", "-f", f"name={container_name}"], capture_output=True, text=True, check=True)
        if result.stdout.strip():
            print(f"Stopping running container: {container_name}")
            subprocess.run(["sudo", docker_path, "stop", container_name], check=True)
            print(f"Removing stopped container: {container_name}")
            subprocess.run(["sudo", docker_path, "rm", container_name], check=True)
    except subprocess.CalledProcessError:
        pass
    
    try:
        subprocess.run(["sudo", docker_path, "rm", "-f", container_name], check=False, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print(f"Removed any existing container: {container_name}")
    except subprocess.CalledProcessError:
        pass

def cleanup_existing_scaphandre():
    """Kill any existing scaphandre processes."""
    try:
        subprocess.run(["sudo", "pkill", "-9", "scaphandre"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print("Killed existing scaphandre processes")
    except subprocess.CalledProcessError:
        print("No existing scaphandre processes found to kill")

def start_scaphandre(output_json, scaphandre_path, step=None, step_nano=None, max_top_consumers=None):
    os.makedirs("output", exist_ok=True)
    scaphandre_command = ["sudo", scaphandre_path, "json", "--containers", "-f", output_json]
    
    if step is not None:
        if step < 0:
            raise ValueError(f"Step {step} seconds must be non-negative")
        scaphandre_command.extend(["--step", str(step)])
    
    if step_nano is not None:
        if step_nano < 100000:
            raise ValueError(f"Step-nano {step_nano} ns is too small; must be at least 100,000 ns (~100 µs) when specified")
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

def check_container_health(url, retries=5, delay=2):
    """Check if the container is responsive by sending a test request."""
    for i in range(retries):
        try:
            response = requests.get(url, timeout=5)
            if response.status_code == 200:
                print(f"Container health check passed: {url} responded with status 200")
                return True
        except requests.exceptions.RequestException as e:
            print(f"Health check attempt {i+1}/{retries} failed: {e}")
            time.sleep(delay)
    print(f"Container health check failed after {retries} attempts")
    return False

def start_server_container(server_image, detach_mode, port_mapping, server_params, container_name, docker_path):
    cleanup_existing_container(container_name, docker_path)
    server_command = ["sudo", docker_path, "run", detach_mode, "--name", container_name, "-p", port_mapping] + server_params + [server_image]
    server_process = subprocess.Popen(server_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout, stderr = server_process.communicate()
    if server_process.returncode != 0:
        print(f"Error starting container: {stderr.decode('utf-8')}")
        raise RuntimeError(f"Failed to start container '{container_name}': {stderr.decode('utf-8')}")
    time.sleep(10)
    print(f"Server container started with {detach_mode}, name: {container_name}, port mapping: {port_mapping}")
    return True

def stop_server_container(container_name, docker_path):
    print(f"Stopping server container: {container_name}")
    subprocess.run(["sudo", docker_path, "stop", container_name], check=True)
    subprocess.run(["sudo", docker_path, "rm", container_name], check=True)

def parse_json_and_compute_energy(file_name, container_name, runtime):
    """Parse JSON and compute total energy in Joules from power in microwatts, only for non-zero consumption."""
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
            container = consumer.get("container")
            if container and container.get("name") == container_name:
                power_microwatts = consumer.get("consumption", 0.0)
                if power_microwatts > 0:
                    total_power_microwatts += power_microwatts
                    number_samples += 1
                    print(f"Added power for {consumer.get('exe', 'unknown')}: {power_microwatts:.2f} µW")

    if number_samples == 0:
        print(f"No non-zero energy data found for container: {container_name}")
        return 0.0, 0.0, 0

    print(f"Found {number_samples} non-zero samples for container: {container_name}")
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

def save_results_to_csv(filename, results, total_energy, average_power, total_runtime, requests_per_second, total_samples, container_name=None, num_requests=None):
    if filename is None:
        os.makedirs("results", exist_ok=True)
        timestamp = datetime.now().strftime("%Y-%m-%d-%H%M%S")
        filename = os.path.join("results", f"{container_name}_{num_requests}_{timestamp}.csv")
    
    headers = ["Total Requests", "Successful Requests", "Failed Requests", "Execution Time (seconds)", "Requests Per Second",
               "Total Energy Consumption (J)", "Average Power Consumption (W)", "Number of Samples"]
    data = [
        [
            results['total'],
            results['success'],
            results['failure'],
            total_runtime,
            requests_per_second,
            total_energy,
            average_power,
            total_samples
        ]
    ]
    
    os.makedirs(os.path.dirname(filename) or '.', exist_ok=True)
    with open(filename, mode='w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(headers)
        writer.writerows(data)
    print(f"Results saved to {filename}")

def main():
    parser = argparse.ArgumentParser(description="Measure energy consumption of a web server with Scaphandre.")
    parser.add_argument('--server_image', type=str, required=True, help="Server Docker image (required)")
    parser.add_argument('--container_name', type=str, default=None, help="Name of the container to track (defaults to server_image if not specified)")
    parser.add_argument('--server_params', type=str, nargs='*', default=[],
                        help="Additional parameters for the server container (e.g., '-e KEY=VALUE' in quotes)")
    parser.add_argument('--detach_mode', type=str, default='-d',
                        help="Detach mode for Docker (e.g., -d, -it, or -itd)")
    parser.add_argument('--port_mapping', type=str, default='8001:80',
                        help="Port mapping for Docker in host:container format (e.g., 8001:80)")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send")
    parser.add_argument('--step', type=int, default=None, help="Scaphandre sampling interval in seconds (optional, passed to --step)")
    parser.add_argument('--step_nano', type=int, default=None, help="Sampling interval in nanoseconds (optional, passed to --step-nano, min 100,000 ns when specified, adds --step 0 if --step not provided)")
    parser.add_argument('--max_top_consumers', type=int, default=None, help="Maximum number of top consuming processes to monitor (optional, passed to --max-top-consumers, defaults to 10 if not specified)")
    parser.add_argument('--output_csv', type=str, default=None, help="Output CSV file path (defaults to containerName_requestNum_timestamp.csv in results folder if not specified)")
    parser.add_argument('--output_json', type=str, default=None, help="Output JSON file name (defaults to timestamp if not specified)")
    
    args = parser.parse_args()

    try:
        scaphandre_path = get_binary_path("scaphandre")
        docker_path = get_binary_path("docker")
    except (FileNotFoundError, RuntimeError) as e:
        print(f"Error: {e}")
        return

    if args.output_json is None:
        timestamp = datetime.now().strftime("%Y-%m-%d-%H%M%S")
        output_json = os.path.join("output", f"{timestamp}.json")
    else:
        output_json = os.path.join("output", args.output_json)

    host_port = args.port_mapping.split(':')[0]
    url = f"http://localhost:{host_port}/"
    n = args.num_requests

    container_name = args.container_name if args.container_name else args.server_image

    cleanup_existing_scaphandre()

    print("Starting container...")
    try:
        start_server_container(args.server_image, args.detach_mode, args.port_mapping, args.server_params, container_name, docker_path)
    except RuntimeError as e:
        print(f"Container startup failed: {e}")
        return

    # Health check after container starts
    print(f"Checking container health at {url}...")
    if not check_container_health(url):
        print("Container failed health check, stopping and exiting...")
        stop_server_container(container_name, docker_path)
        return

    print("Starting Scaphandre...")
    try:
        scaphandre_process = start_scaphandre(output_json, scaphandre_path, args.step, args.step_nano, args.max_top_consumers)
    except (ValueError, RuntimeError) as e:
        print(f"Failed to start Scaphandre: {e}")
        stop_server_container(container_name, docker_path)
        return
    
    print(f"Sending {n} requests to {url}...")
    time.sleep(15)

    start_time = time.time()
    with ThreadPoolExecutor() as executor:
        futures = [executor.submit(send_request, url, i+1) for i in range(n)]
        for _ in futures:
            _.result()
    end_time = time.time()
    runtime = end_time - start_time
    results_counter['runtime'] = runtime

    requests_per_second = results_counter['total'] / runtime if runtime > 0 else 0

    print("Waiting for Scaphandre to collect data...")
    time.sleep(10)

    stop_scaphandre(scaphandre_process)
    stop_server_container(container_name, docker_path)

    total_energy, average_power, total_samples = parse_json_and_compute_energy(output_json, container_name, runtime)
    save_results_to_csv(args.output_csv, results_counter, total_energy, average_power, runtime, requests_per_second, total_samples,
                       container_name=container_name, num_requests=n)

    print("\nSummary:")
    print(f"Total Requests Sent: {results_counter['total']}")
    print(f"Successful Requests: {results_counter['success']}")
    print(f"Failed Requests: {results_counter['failure']}")
    print(f"Execution Time: {runtime:.2f} seconds")
    print(f"Requests Per Second: {requests_per_second:.2f}")
    print(f"Total Energy Consumption: {total_energy:.6f} J")
    print(f"Average Power Consumption: {average_power:.6f} W")
    print(f"JSON output saved to: {output_json}")
    print(f"Measured energy for container: {container_name}")

if __name__ == "__main__":
    main()