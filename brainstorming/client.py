import requests
from concurrent.futures import ThreadPoolExecutor
from collections import Counter
import time

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

def main():
    # Configuration
    url = "http://localhost:8001/"
    n = 50000  # Number of requests to send

    print(f"Sending {n} requests to {url}...")

    # Start timing
    start_time = time.time()
    # Use ThreadPoolExecutor for concurrency with instantiation
    with ThreadPoolExecutor() as executor:  # Fixed: added parentheses
        # Submit all tasks at once and store futures
        futures = [executor.submit(send_request, url) for _ in range(n)]
        # Wait for all requests to complete
        for future in futures:
            future.result()  # This blocks until each future is complete

    # Calculate elapsed time
    elapsed_time = time.time() - start_time

    # Summary
    print("\nSummary:")
    print(f"Total Requests Sent: {results_counter['total']}")
    print(f"Successful Requests: {results_counter['success']}")
    print(f"Failed Requests: {results_counter['failure']}")
    print(f"Elapsed Time: {elapsed_time:.2f} seconds")
    print(f"Requests per Second: {results_counter['total'] / elapsed_time:.2f}")

if __name__ == "__main__":
    main()