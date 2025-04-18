# Web Server Energy and Performance Benchmarking

## Overview
This project benchmarks the performance and energy efficiency of web servers running inside Docker containers, locally installed servers, and WebSocket servers. It includes scripts for building Docker images, running benchmarks, and generating visualizations of the results.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Directory Structure](#directory-structure)
3. [Adding Dockerfiles](#adding-dockerfiles)
4. [Installing Docker Images](#installing-docker-images)
5. [Running Benchmarks](#running-benchmarks)
6. [Local Webserver Baseline](#local-webserver-baseline)
7. [WebSocket Implementation](#websocket-implementation)
8. [GUI Graph Generator](#gui-graph-generator)
9. [Troubleshooting](#troubleshooting)

---

## Prerequisites
Before using the scripts, ensure the following are installed:
- **Operating System**: Linux (Debian-based distributions recommended).
- **Python 3**: Version 3.6 or higher.
- **Docker**: Installed via `sudo apt install docker.io`.
- **Scaphandre**: Installed via Rust’s Cargo (`cargo install scaphandre`) or pre-built binaries.

Verify installations:
```bash
python3 --version
docker --version
scaphandre --version
```

---

## Directory Structure
The project is organized as follows:

```
web-server-benchmarks/
├── containers/               # Dockerfiles for containerized web servers
│   ├── static/               # Static web servers (e.g., Apache, Nginx)
│   ├── dynamic/              # Dynamic web servers (e.g., Nginx with dynamic modules)
├── local/                    # Local webserver baseline (e.g., Nginx, Yaws)
│   ├── nginx/                # Configuration for running Nginx locally
│   ├── yaws/                 # Configuration for running Yaws locally
├── web-socket/               # WebSocket implementation and measurement scripts
│   ├── measure_websocket.py  # Script to benchmark WebSocket servers
├── logs/                     # Logs generated during benchmarking
├── output/                   # Output files, such as temporary data or intermediate results
├── results_docker/           # Results for Docker-based benchmarks
├── results_local/            # Results for local webserver benchmarks
├── results_websocket/        # Results for WebSocket benchmarks
├── install_benchmarks.sh     # Script to build Docker images for benchmarking
├── run_benchmarks.sh         # Main script to trigger all benchmarks
├── gui_graph_generator.py    # Script to generate graphs from benchmarking results
└── README.md                 # Project documentation
```

### Explanation of Folders and Files
- **`containers/`**: Contains Dockerfiles for containerized web servers. Subfolders are divided into `static/` and `dynamic/` servers.
- **`local/`**: Contains configurations and scripts for running local webservers (e.g., Nginx, Yaws) as a baseline for benchmarking.
- **`web-socket/`**: Contains the WebSocket implementation and scripts for benchmarking WebSocket servers.
- **`logs/`**: Stores logs generated during benchmarking, such as server logs or error messages.
- **`output/`**: Stores temporary or intermediate files generated during benchmarking.
- **`results_docker/`**: Stores results for Docker-based benchmarks in CSV format.
- **`results_local/`**: Stores results for local webserver benchmarks in CSV format.
- **`results_websocket/`**: Stores results for WebSocket benchmarks in CSV format.
- **`install_benchmarks.sh`**: Script to build Docker images for benchmarking.
- **`run_benchmarks.sh`**: Main script to trigger all benchmarks, including Docker, local, and WebSocket benchmarks.
- **`gui_graph_generator.py`**: Script to generate visualizations (graphs) from benchmarking results.
- **`README.md`**: Documentation for the project.

---

## Adding Dockerfiles
To add a new Dockerfile:
1. Create a directory under `containers/static/` or `containers/dynamic/` based on the server type.
2. Place the `Dockerfile` in the directory. Optionally, name it `Dockerfile.<extension>` to differentiate configurations.
3. Ensure the directory name matches the desired Docker image name.

Example:
```bash
mkdir -p containers/static/my-server
touch containers/static/my-server/Dockerfile
```

---

## Installing Docker Images
Use the `install_benchmarks.sh` script to build Docker images:
```bash
./install_benchmarks.sh
```

### Custom Directories
To include additional directories:
```bash
./install_benchmarks.sh ./custom-dir1 ./custom-dir2
```

The script:
- Searches for `Dockerfile` or `Dockerfile.<extension>` in each directory.
- Builds images with names derived from the directory and Dockerfile.

---

## Running Benchmarks
The `run_benchmarks.sh` script is the main entry point for running all benchmarks. It automatically triggers the corresponding measurement scripts for Docker, local, and WebSocket benchmarks.

### Usage
```bash
./run_benchmarks.sh [--name <target_name>]
```

- `--name <target_name>`: Run benchmarks for a specific target (e.g., `nginx-deb`, `yaws`, `nginx_websocket`). If omitted, all benchmarks are run.

---

## Local Webserver Baseline
The `local` folder contains scripts and configurations for running a local webserver as a baseline for benchmarking. This includes lightweight setups for servers like Nginx and Yaws, which are used to compare performance and energy efficiency against containerized web servers.

You can directly run these local webservers without requiring Docker containers. For example:
- **Nginx**: Use the provided configuration files in the `local/nginx/` directory to start the server locally.
- **Yaws**: Follow the instructions and setup scripts in the `local/yaws/` directory to set up and run the Yaws webserver.

These local setups serve as a baseline for comparison with containerized web servers.

---

## WebSocket Implementation
The `web-socket/` folder contains the implementation for benchmarking WebSocket servers. The main script, `measure_websocket.py`, is used to:
1. Start a WebSocket server.
2. Simulate WebSocket clients sending and receiving messages.
3. Measure performance metrics such as latency, throughput, and resource usage.

### Running the WebSocket Benchmark
To benchmark a WebSocket server, use the following command:
```bash
python3 web-socket/measure_websocket.py --server_image <image_name> --num_clients <number>
```

#### Example:
```bash
python3 web-socket/measure_websocket.py --server_image nginx_websocket --num_clients 10
```

#### Options:
- `--server_image` (required): Docker image name of the WebSocket server.
- `--num_clients`: Number of WebSocket clients to simulate (default: 5).
- `--size_mb`: Payload size per message in burst mode (default: 10 MB).
- `--interval_s`: Interval between bursts in seconds (default: 1.0).
- `--rate_mb_s`: Streaming rate in MB/s per client (optional, for streaming mode).
- `--duration_s`: Test duration in seconds (default: 30).
- `--output_csv`: Path to save the results in CSV format (optional).

---

## GUI Graph Generator
To visualize results, use the GUI graph generator:
1. Ensure results are saved in CSV format.
2. Run the graph generator script:
   ```bash
   python3 gui_graph_generator.py --input_csv <path_to_csv> --output_graph <path_to_graph>
   ```
3. Open the generated graph to analyze performance and energy efficiency.

---

## Troubleshooting
### Common Issues
- **No Energy Data**: Ensure Scaphandre is installed and your hardware supports RAPL.
- **Health Check Fails**: Verify the server is running and accessible on the specified port.
- **Docker Errors**: Check if the image exists using `docker images`.

### Logs
Check logs for detailed error messages:
```bash
docker logs <container_name>
```

---

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.