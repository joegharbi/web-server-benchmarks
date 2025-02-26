import os
import time
import subprocess
import requests
import csv
from concurrent.futures import ThreadPoolExecutor
from collections import Counter
import argparse
import json
import signal  # Import signal module for SIGINT

# Shared counters to track requests
results_counter = Counter()

def send_request(url):
    try:
        response = requests.get(url)
        if 200 <= response.status_code < 300:
            results_counter['success'] += 1
        else:
            results_counter['failure'] += 1
            print(f"Request failed: {response.status_code}, Response: {response.text}")
        results_counter['total'] += 1
    except requests.exceptions.RequestException as e:
        results_counter['failure'] += 1
        results_counter['total'] += 1
        print(f"Request exception: {e}")

def start_docker_containers(scaphandre_image, server_image, scaphandre_params, server_params):
    # Start Scaphandre container with volume mount for output
    local_dir = os.getcwd()
    scaphandre_command = [
        "sudo", "docker", "run", "--privileged",
        "-v", "/sys/class/powercap:/sys/class/powercap",
        "-v", "/proc:/proc",
        "-v", f"{local_dir}:/output",  # Mount local dir to /output
        scaphandre_image
    ] + scaphandre_params

    scaphandre_process = subprocess.Popen(scaphandre_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(5)  # Allow some time for Scaphandre to initialize

    # Start the server container
    server_command = ["sudo", "docker", "run", "-d", "-p", "8000:80", server_image] + server_params
    server_process = subprocess.Popen(server_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(5)  # Allow some time for the server to start

    return scaphandre_process, server_process

def stop_docker_containers(scaphandre_process, server_process):
    # Stop Scaphandre gracefully with SIGINT
    print("Stopping Scaphandre container...")
    scaphandre_process.send_signal(signal.SIGINT)  # Use signal.SIGINT
    scaphandre_process.wait(timeout=5)  # Wait for clean exit
    
    # Stop server container
    print("Stopping server container...")
    server_process.terminate()

def parse_json_and_compute_energy(file_name, server_name):
    json_file_path = os.path.join(os.getcwd(), file_name)
    with open(json_file_path, "r") as file:
        data = json.load(file)

    total_server_consumption = 0.0
    number_samples = 0

    for entry in data:
        consumers = entry.get("consumers", [])
        for consumer in consumers:
            exe = consumer.get("exe", "")
            consumption = consumer.get("consumption", 0.0)
            if server_name in exe.lower():
                total_server_consumption += consumption
                number_samples += 1

    average_energy = total_server_consumption / number_samples if number_samples != 0 else 0
    return total_server_consumption, average_energy, number_samples

def save_results_to_csv(filename, results, total_energy, average_energy, total_runtime, total_samples):
    headers = ["Total Requests", "Successful Requests", "Failed Requests", "Execution Time (seconds)",
               "Total Energy Consumption (J)", "Average Energy Consumption (J)", "Number of Samples"]
    data = [
        [
            results['total'],
            results['success'],
            results['failure'],
            total_runtime,
            total_energy,
            average_energy,
            total_samples
        ]
    ]
    
    with open(filename, mode='w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(headers)
        writer.writerows(data)
    print(f"Results saved to {filename}")

def main():
    parser = argparse.ArgumentParser(description="Measure energy consumption of a web server with Scaphandre.")
    parser.add_argument('--scaphandre_image', type=str, default='hubblo/scaphandre', help="Scaphandre Docker image")
    parser.add_argument('--server_image', type=str, default='nginx', help="Server Docker image")
    parser.add_argument('--server_name', type=str, default=None, help="Name of the server process to track")
    parser.add_argument('--scaphandre_params', type=str, nargs='*', default=['json', '-f', '/output/test.json'],
                        help="Additional parameters for the Scaphandre container")
    parser.add_argument('--server_params', type=str, nargs='*', default=[],
                        help="Additional parameters for the server container")
    parser.add_argument('--num_requests', type=int, default=500, help="Number of requests to send")
    parser.add_argument('--output_csv', type=str, default='server_results.csv', help="Output CSV file name")
    
    args = parser.parse_args()

    url = "http://localhost:8000/"
    n = args.num_requests
    server_name = args.server_name if args.server_name else args.server_image

    # Start Docker containers
    print("Starting Docker containers...")
    scaphandre_process, server_process = start_docker_containers(
        args.scaphandre_image, args.server_image, args.scaphandre_params, args.server_params
    )

    print(f"Sending {n} requests to {url}...")
    time.sleep(10)  # Allow time for both containers to stabilize

    # Measure runtime and send requests
    start_time = time.time()
    with ThreadPoolExecutor() as executor:
        futures = [executor.submit(send_request, url) for _ in range(n)]
        for _ in futures:
            _.result()

    end_time = time.time()
    runtime = end_time - start_time
    results_counter['runtime'] = runtime

    # Stop containers to ensure output is written
    stop_docker_containers(scaphandre_process, server_process)

    # Parse JSON and compute energy (use the filename from scaphandre_params)
    output_file = [p for p in args.scaphandre_params if p.startswith('-f')][0].split()[-1].split('/')[-1]  # Extract 'test.json'
    total_energy, average_energy, total_samples = parse_json_and_compute_energy(output_file, server_name)

    # Save results
    save_results_to_csv(args.output_csv, results_counter, total_energy, average_energy, runtime, total_samples)

    # Summary
    print("\nSummary:")
    print(f"Total Requests Sent: {results_counter['total']}")
    print(f"Successful Requests: {results_counter['success']}")
    print(f"Failed Requests: {results_counter['failure']}")
    print(f"Execution Time: {runtime:.2f} seconds")
    print(f"Total Energy Consumption: {total_energy:.2f} J")
    print(f"Average Energy Consumption: {average_energy:.2f} J")

if __name__ == "__main__":
    main()