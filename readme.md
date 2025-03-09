# Web Server Energy and Performance Benchmarking

## Description
This project centers around the `measure.py` script, a Python tool designed to benchmark the performance and energy efficiency of web servers running inside Docker containers. It targets servers like Nginx, Yaws, and custom Erlang-based implementations, measuring key metrics such as request throughput, energy consumption, CPU utilization, and memory usage. The script leverages Scaphandre for precise energy tracking and Docker’s built-in statistics for resource monitoring, making it a powerful tool for analyzing how web servers behave under load on modern multi-core hardware, such as an Intel i7 vPro with 8 cores.

The script executes a series of HTTP requests against a containerized web server, collects real-time data, and processes it into actionable insights. It calculates energy usage in Joules and Watts, normalizes CPU usage into a percentage of total system capacity and equivalent core count, and tracks memory consumption in megabytes. Results are output both to the console and as CSV files, enabling detailed comparison across different server configurations.

## Goals
The primary objectives of this work are:
- **Performance Evaluation**: Quantify how fast different web servers can handle HTTP requests, expressed as requests per second (RPS), to identify which servers excel under varying loads.
- **Energy Efficiency**: Measure the energy cost (in Joules and Watts) of running web servers, helping to pinpoint configurations that minimize power consumption without sacrificing performance.
- **Resource Utilization**: Assess CPU and memory usage in a multi-core context, providing clear metrics (e.g., % of system capacity and core count) to understand resource demands and scalability.
- **Optimization Insights**: Enable users to compare servers like Nginx (lightweight, static-focused) against Yaws or Erlang-based servers (dynamic, concurrency-driven) to find the best fit for specific workloads, such as static content delivery or high-concurrency applications.
- **Research and Development**: Support experimentation with power-capping (e.g., Docker’s `--cpus` or system-level RAPL) to explore trade-offs between performance and energy use, contributing to greener computing practices.

This tool is particularly useful for developers, system administrators, and researchers interested in sustainable web hosting, cloud optimization, or server-side application design.

## Tutorial: How to Run `measure.py`

This section provides an extensive, step-by-step guide to running the `measure.py` script, with detailed explanations of every aspect, from setup to interpreting results.

### Prerequisites
Before running the script, ensure the following are in place:
- **Operating System**: Linux, preferably a Debian-based distribution (e.g., Ubuntu). The script relies on Linux-specific tools like `docker` and Scaphandre’s energy monitoring.
- **Software Requirements**:
  - **Python 3**: Version 3.6 or higher, installed with `sudo apt install python3 python3-pip`. Verify with `python3 --version`.
  - **Docker**: Container runtime, installed via `sudo apt install docker.io`. Check with `docker --version`. Ensure the Docker daemon is running (`sudo systemctl start docker`).
  - **Scaphandre**: Energy monitoring tool, installed via Rust’s Cargo (`cargo install scaphandre`) or a pre-built binary from [Scaphandre’s GitHub](https://github.com/hubblo-org/scaphandre). Verify with `scaphandre --version`.
- **Hardware**: A multi-core CPU (e.g., 8-core Intel i7 vPro) with RAPL support for energy measurements. Most modern Intel CPUs support this out of the box.
- **Permissions**: Root access is required (`sudo`) because Scaphandre and Docker need elevated privileges to monitor energy and manage containers.

### Preparation
1. **Obtain the Script**:
   - Save `measure.py` in a working directory, such as `~/web-server-benchmarks`. Create the directory if it doesn’t exist: `mkdir ~/web-server-benchmarks && cd ~/web-server-benchmarks`.
2. **Docker Images**:
   - The script assumes images like `nginx-deb`, `yaws-deb`, or `erlang27-deb` are available locally. If you’ve built or stored these in your repository:
     - **Load Images**: If they’re tarballs (e.g., `nginx-deb.tar`), run `sudo docker load -i nginx-deb.tar`.
     - **Build Images**: If Dockerfiles are present, build with `sudo docker build -t nginx-deb .` in the appropriate directory.
   - Verify images are loaded: `sudo docker images` should list them.
3. **Directory Structure**:
   - The script creates `output/` for Scaphandre JSON files and `results/` for CSV outputs. No manual creation is needed—it handles this automatically.
4. **Network**: Ensure port 8001 (or your specified host port) is free, as the script maps it to the container’s port (default 80).

### Running the Script
The script is executed via the command line with various arguments. Here’s how to use it:

#### Basic Command
```bash
sudo python3 measure.py --server_image nginx-deb --num_requests 5000
```
- **Explanation**:
  - `sudo`: Runs as root for Docker and Scaphandre access.
  - `python3 measure.py`: Executes the script.
  - `--server_image nginx-deb`: Specifies the Docker image to test.
  - `--num_requests 5000`: Sets the number of HTTP requests to send.

#### Detailed Options
- **Required**:
  - `--server_image <image>`: The name of the Docker image (e.g., `nginx-deb`, `erlang27-deb`). Must match an available image.
- **Common**:
  - `--num_requests <int>`: Number of requests (default: 500). Higher values increase load and runtime (e.g., 50,000 takes ~47 seconds).
  - `--port_mapping <host:container>`: Maps host port to container port (default: `8001:80`). Use `8001:8080` if the server listens on 8080 (e.g., Erlang).
  - `--output_csv <path>`: Custom CSV output file (default: `results/<image>.csv`, e.g., `results/nginx-deb.csv`).
  - `--output_json <path>`: Custom Scaphandre JSON output (default: `output/<timestamp>.json`, e.g., `output/2025-03-04-123456.json`).
- **Advanced**:
  - `--container_name <name>`: Custom name for the Docker container (default: same as `server_image`).
  - `--detach_mode <mode>`: Docker run mode (default: `-d` for detached). Rarely needs changing.
  - `--server_params "<args>"`: Extra Docker run arguments (e.g., `"--cpus=1.0"` to cap CPU at 1 core). Use quotes for multiple args.
  - `--step <int>`: Scaphandre sampling interval in seconds (e.g., 2).
  - `--step_nano <int>`: Nanosecond precision for Scaphandre sampling (e.g., 500000000 for 0.5s).
  - `--max_top_consumers <int>`: Limit Scaphandre’s process tracking (e.g., 10).

#### Example Runs
1. **Nginx with 5000 Requests**:
   ```bash
   sudo python3 measure.py --server_image nginx-deb --num_requests 5000
   ```
   - Uses default port `8001:80`, outputs to `results/nginx-deb.csv`.
2. **Erlang with 50,000 Requests**:
   ```bash
   sudo python3 measure.py --server_image erlang27-deb --num_requests 50000 --port_mapping 8001:8080
   ```
   - Maps to port 8080, suitable for Erlang-based servers.
3. **Yaws with CPU Cap**:
   ```bash
   sudo python3 measure.py --server_image yaws-deb --num_requests 10000 --server_params "--cpus=1.0"
   ```
   - Limits CPU to 1 core, testing power efficiency under constraint.

### What Happens When You Run It
1. **Initialization**:
   - Checks for `docker` and `scaphandre` binaries.
   - Detects CPU cores (e.g., “Detected 8 CPU cores”).
2. **Container Setup**:
   - Stops/removes any existing container with the same name.
   - Starts the specified image (e.g., `sudo docker run -d --name nginx-deb -p 8001:80 nginx-deb`).
   - Waits 10 seconds for startup.
3. **Health Check**:
   - Sends up to 5 HTTP requests to `http://localhost:8001/` (or specified port), expecting a 200 status. Retries every 2 seconds if it fails.
   - If unsuccessful, stops and exits with an error.
4. **Monitoring**:
   - Starts Scaphandre to log energy data to a JSON file (e.g., `output/2025-03-04-123456.json`).
   - Launches a thread to sample `docker stats` every second, collecting CPU and memory data.
5. **Request Load**:
   - Sends the specified number of requests concurrently using Python’s `ThreadPoolExecutor`.
   - Tracks successes and failures (e.g., timeouts or non-200 responses).
6. **Cleanup**:
   - Stops Scaphandre and the container after a 10-second buffer.
7. **Results**:
   - Processes Scaphandre JSON for energy metrics.
   - Normalizes CPU data based on core count (e.g., 107% raw → 13.43% of 800%, 1.07 cores).
   - Outputs to console and CSV.

### Output Details
- **Console Output**:
  ```
  Detected 8 CPU cores
  Container nginx-deb started
  Health check passed: http://localhost:8001/
  Scaphandre started
  Sending 5000 requests to http://localhost:8001/...
  Scaphandre stopped
  Container nginx-deb stopped
  Results appended to results/nginx-deb.csv

  Summary:
  Total Requests Sent: 5000
  Successful Requests: 5000
  Failed Requests: 0
  Success Rate: 100.00%
  Execution Time: 4.27 seconds
  Requests Per Second: 1170.99
  Total Energy Consumption: 0.097081 J
  Average Power Consumption: 0.022736 W
  Average CPU Usage (% of System): 1.34%
  Average CPU Usage (Cores): 0.11
  Average Memory Usage: 6.42 MB
  ```
  - **Metrics Explained**:
    - **Total Requests Sent**: Number attempted.
    - **Successful/Failed**: Success = HTTP 200; failures include timeouts or errors.
    - **Success Rate**: Successful ÷ Total × 100.
    - **Execution Time**: Time to send all requests (seconds).
    - **RPS**: Total requests ÷ Execution time.
    - **Energy**: Total Joules from Scaphandre; Power = Joules ÷ Time.
    - **CPU % of System**: Raw % ÷ (cores × 100), e.g., 107 ÷ 800 = 13.43%.
    - **CPU Cores**: Raw % ÷ 100, e.g., 107 ÷ 100 = 1.07.
    - **Memory**: Average MB from `docker stats`.
- **CSV Output**:
  - File: `results/<image>.csv` (e.g., `results/nginx-deb.csv`).
  - Columns: `Num Requests`, `Total Requests`, `Successful Requests`, `Failed Requests`, `Success Rate (%)`, `Execution Time (seconds)`, `Requests Per Second`, `Total Energy Consumption (J)`, `Average Power Consumption (W)`, `Number of Samples`, `Average CPU Usage (% of System)`, `Average CPU Usage (Cores)`, `Average Memory Usage (MB)`.

### Troubleshooting
- **No Energy Data**: 
  - Check Scaphandre (`sudo scaphandre json -f test.json`)—needs RAPL support and root.
  - JSON file empty? Hardware may not support energy tracking.
- **CPU/Memory Zeros**:
  - Run `sudo docker stats <container_name>` during execution to confirm data.
  - Container stopping early? Increase startup delay (`time.sleep(10)` in code).
- **Health Check Fails**:
  - Verify server listens on specified port (e.g., 80 or 8080).
  - Test manually: `curl http://localhost:8001/`.
- **Docker Errors**:
  - Image not found? Check `sudo docker images` and load/build as needed.
  - Permission denied? Ensure `sudo` is used.

### Customization Tips
- **More Requests**: Increase `--num_requests` (e.g., 100,000) for heavier loads, but expect longer runtimes.
- **Port Changes**: Match `--port_mapping` to your server’s port (e.g., `8001:8080` for Erlang).
- **Power Capping**: Add `--server_params "--cpus=1.0"` to limit CPU, observing energy/performance trade-offs.
- **Sampling Rate**: Adjust `--step 1` for finer Scaphandre data (default is system-dependent).

## License
MIT License

<!-- ```
Copyright (c) 2025 Youssef

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
``` -->