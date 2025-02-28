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
        response = requests.get(url)
        if 200 <= response.status_code < 300:
            results_counter['success'] += 1
        else:
            results_counter['failure'] += 1
            print(f"Request {request_num} failed: {response.status_code}, Response: {response.text}")
        results_counter['total'] += 1
    except requests.exceptions.RequestException as e:
        results_counter['failure'] += 1
        results_counter['total'] += 1
        print(f"Request {request_num} exception: {e}")

def start_scaphandre(output_json, scaphandre_path):
    os.makedirs("output", exist_ok=True)
    scaphandre_command = ["sudo", scaphandre_path, "json", "--containers", "-f", output_json]
    scaphandre_process = subprocess.Popen(scaphandre_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(5)
    print("Scaphandre started on host")
    return scaphandre_process

def stop_scaphandre(scaphandre_process):
    print("Stopping Scaphandre process...")
    scaphandre_process.send_signal(signal.SIGINT)
    scaphandre_process.wait(timeout=5)

def start_server_container(server_image, detach_mode, port_mapping, server_params, container_name, docker_path):
    container_name = container_name if container_name else server_image
    server_command = ["sudo", docker_path, "run", detach_mode, "--name", container_name, "-p", port_mapping, server_image] + server_params
    server_process = subprocess.Popen(server_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout, stderr = server_process.communicate()
    if server_process.returncode != 0:
        print(f"Error starting container: {stderr}")
        return
    time.sleep(10)
    print(f"Server container started with {detach_mode}, name: {container_name}, port mapping: {port_mapping}")

def stop_server_container(container_name, docker_path):
    print(f"Stopping server container: {container_name}")
    subprocess.run(["sudo", docker_path, "stop", container_name], check=True)
    subprocess.run(["sudo", docker_path, "rm", container_name], check=True)

def parse_json_and_compute_energy(file_name, container_name, sample_interval=2.0):
    """Parse JSON and compute total energy in Joules from power in microwatts with error handling."""
    json_file_path = file_name
    try:
        with open(json_file_path, "r") as file:
            data = json.load(file)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Error reading JSON file: {e}")
        return 0.0, 0.0, 0

    total_server_consumption = 0.0
    number_samples = 0

    for entry in data:
        consumers = entry.get("consumers", [])
        for consumer in consumers:
            container = consumer.get("container")
            if container and container.get("name") == container_name:
                power_microwatts = consumer.get("consumption", 0.0)  # µW (power)
                energy_joules = power_microwatts * 1e-6 * sample_interval  # µW to W, then to J
                total_server_consumption += energy_joules
                number_samples += 1
                print(f"Added energy for {consumer.get('exe', 'unknown')}: {energy_joules:.6f} J (from {power_microwatts:.2f} µW)")

    if number_samples == 0:
        print(f"No energy data found for container: {container_name}")
    else:
        print(f"Found {number_samples} samples for container: {container_name}")

    average_energy = total_server_consumption / number_samples if number_samples != 0 else 0
    return total_server_consumption, average_energy, number_samples

def save_results_to_csv(filename, results, total_energy, average_energy, total_runtime, requests_per_second, total_samples, container_name=None, num_requests=None):
    if filename is None:
        os.makedirs("results", exist_ok=True)
        timestamp = datetime.now().strftime("%Y-%m-%d-%H%M%S")
        filename = os.path.join("results", f"{container_name}_{num_requests}_{timestamp}.csv")
    
    headers = ["Total Requests", "Successful Requests", "Failed Requests", "Execution Time (seconds)", "Requests Per Second",
               "Total Energy Consumption (J)", "Average Energy Consumption per Sample (J)", "Number of Samples"]
    data = [
        [
            results['total'],
            results['success'],
            results['failure'],
            total_runtime,
            requests_per_second,
            total_energy,
            average_energy,
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
                        help="Additional parameters for the server container")
    parser.add_argument('--detach_mode', type=str, default='-d',
                        help="Detach mode for Docker (e.g., -d, -it, or -itd)")
    parser.add_argument('--port_mapping', type=str, default='8001:80',
                        help="Port mapping for Docker in host:container format (e.g., 8001:80)")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send")
    parser.add_argument('--sample_interval', type=float, default=2.0, help="Scaphandre sample interval in seconds")
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

    print("Starting container...")
    start_server_container(args.server_image, args.detach_mode, args.port_mapping, args.server_params, container_name, docker_path)
    time.sleep(5)

    print("Starting Scaphandre...")
    scaphandre_process = start_scaphandre(output_json, scaphandre_path)
    
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

    total_energy, average_energy, total_samples = parse_json_and_compute_energy(output_json, container_name, sample_interval=args.sample_interval)
    save_results_to_csv(args.output_csv, results_counter, total_energy, average_energy, runtime, requests_per_second, total_samples,
                       container_name=container_name, num_requests=n)

    print("\nSummary:")
    print(f"Total Requests Sent: {results_counter['total']}")
    print(f"Successful Requests: {results_counter['success']}")
    print(f"Failed Requests: {results_counter['failure']}")
    print(f"Execution Time: {runtime:.2f} seconds")
    print(f"Requests Per Second: {requests_per_second:.2f}")
    print(f"Total Energy Consumption: {total_energy:.6f} J")
    print(f"Average Energy Consumption per Sample: {average_energy:.6f} J")
    print(f"JSON output saved to: {output_json}")
    print(f"Measured energy for container: {container_name}")

if __name__ == "__main__":
    main()