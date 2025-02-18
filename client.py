import requests
from concurrent.futures import ThreadPoolExecutor
from collections import Counter

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
    url = "http://localhost:8000/"
    n = 50000  # Number of requests to send

    print(f"Sending {n} requests to {url}...")

    # Use ThreadPoolExecutor for concurrency
    # If you don't provide a value for max_workers, 
    # the default behavior is to use a value equal to the number of processors
    # on the machine, multiplied by 5. This default is determined by os.cpu_count().
    # with ThreadPoolExecutor(max_workers=10) as executor:
    with ThreadPoolExecutor as executor:
        futures = [executor.submit(send_request, url) for _ in range(n)]
        for _ in futures:
            _.result()  # Wait for all requests to complete

    # Summary
    print("\nSummary:")
    print(f"Total Requests Sent: {results_counter['total']}")
    print(f"Successful Requests: {results_counter['success']}")
    print(f"Failed Requests: {results_counter['failure']}")

if __name__ == "__main__":
    main()
