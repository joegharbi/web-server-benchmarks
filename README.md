# Web Server Energy and Performance Benchmarking

## Overview
This project benchmarks the performance and energy efficiency of web servers running inside Docker containers, locally installed servers, and WebSocket servers. It includes scripts for building Docker images, running benchmarks, and generating visualizations of the results.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Quick Start](#quick-start)
3. [Directory Structure](#directory-structure)
4. [Image/Server Naming and Auto-Discovery](#naming-and-auto-discovery)
5. [Adding Dockerfiles and Servers](#adding-dockerfiles-and-servers)
6. [Installing Docker Images](#installing-docker-images)
7. [Running Benchmarks](#running-benchmarks)
8. [CSV Output Format](#csv-output-format)
9. [GUI Graph Generator](#gui-graph-generator)
10. [Repository Management](#repository-management)
11. [Troubleshooting](#troubleshooting)

---

## Prerequisites
Before using the scripts, ensure the following are installed:
- **Operating System**: Linux (Debian-based distributions recommended).
- **Python 3**: Version 3.6 or higher.
- **Docker**: Installed via `sudo apt install docker.io`.
- **Scaphandre**: Installed via Rust's Cargo (`cargo install scaphandre`) or pre-built binaries.
- **Make**: Usually pre-installed on Linux systems.

Verify installations:
```bash
python3 --version
docker --version
scaphandre --version
make --version
```

---

## Quick Start

### 1. Set up the environment
```bash
# Create and activate virtual environment (recommended)
make setup
source srv/bin/activate

# Or use an existing virtual environment
make ensure-env
```

### 2. Install dependencies
```bash
make install
```

### 3. Build Docker images
```bash
make build
```

### 4. Run a quick test
```bash
make quick-test
```

### 5. Run full benchmarks
```bash
make run-all
```

### 6. Generate graphs
```bash
make graph
```

---

## Directory Structure
The project is organized as follows:

```
web-server-benchmarks/
├── containers/
│   ├── static/         # Static web servers (e.g., Apache, Nginx)
│   │   └── <image_name>/Dockerfile
│   ├── dynamic/        # Dynamic web servers (e.g., Nginx with dynamic modules)
│   │   └── <image_name>/Dockerfile
│   ├── measure_docker.py  # Docker container measurement script
│   └── output/         # Docker benchmark results
├── web-socket/
│   ├── <image_name>/Dockerfile  # WebSocket server images
│   └── measure_websocket.py     # WebSocket measurement script
├── local/              # Local webserver scripts/configs (e.g., nginx, yaws)
│   ├── measure_local.py    # Local server measurement script
│   └── <server_name>   # Script or config file for local server
├── logs/               # Logs generated during benchmarking
├── output/             # Output files, such as temporary data or intermediate results
├── results/            # Results for all benchmarks, organized by timestamp and type
├── requirements.txt    # Python dependencies
├── Makefile           # Build and automation commands
├── install_benchmarks.sh     # Script to build Docker images for benchmarking
├── run_benchmarks.sh         # Main script to trigger all benchmarks
├── gui_graph_generator.py    # Script to generate graphs from benchmarking results
└── README.md                 # Project documentation
```

---

## Naming and Auto-Discovery

The project uses an intelligent auto-discovery system that automatically finds and benchmarks all available servers:

- **Docker images (static, dynamic, websocket):**
  - Place each image in its own folder under the appropriate directory (`containers/static/`, `containers/dynamic/`, or `web-socket/`).
  - The folder name becomes the image name. The folder must contain a file named exactly `Dockerfile`.
  - Example: `containers/static/nginx-deb/Dockerfile`, `web-socket/ws-yaws/Dockerfile`

- **Local servers:**
  - Place a script or config file in `local/` named after the server (e.g., `local/nginx`, `local/yaws`).

- **Sensible Defaults:**
  - Auto-discovered servers use sensible defaults for ports and test parameters.
  - Static servers: port 8001:80, payloads: 100, 1000, 5000, 8000, 10000, 15000, 20000, 30000, 40000, 50000, 60000, 70000, 80000 requests
  - Dynamic servers: port 8001:80, same payload range as static
  - WebSocket servers: burst mode (10/20/50 clients, 10/50/100 MB payloads) and streaming mode (5/10/20 MB/s rates)
  - Local servers: same payload range as static/dynamic

- **Override Arrays:**
  - The benchmark runner uses associative arrays to specify custom port mappings or to override/exclude certain images/servers.
  - If an image/server is listed in the array, its settings are used (and it will not be auto-discovered again).
  - If not in the array, it is auto-discovered from the folder structure and run with sensible defaults.

---

## Adding Dockerfiles and Servers
To add a new Docker image or local server:

### Static/Dynamic/WebSocket Images
1. Create a directory under `containers/static/`, `containers/dynamic/`, or `web-socket/` using the desired image name.
2. Place a file named `Dockerfile` in that directory.

Example:
```bash
mkdir -p containers/static/my-server
nano containers/static/my-server/Dockerfile
```

The server will be automatically discovered and benchmarked with sensible defaults.

### Local Servers
1. Place a script or config file in `local/` named after the server.

Example:
```bash
touch local/myserver
```

### Custom Configuration
To override defaults, add entries to the appropriate arrays in `run_benchmarks.sh`:
```bash
# Example: Custom port and test parameters
declare -A static_images=(
    ["my-server"]="8080:80"
)
```

---

## Installing Docker Images
Use the Makefile or the install script to build Docker images:

```bash
# Using Makefile (recommended)
make build

# Or using the install script directly
./install_benchmarks.sh
```

Both methods auto-discover all subfolders in `containers/static/`, `containers/dynamic/`, and `web-socket/` that contain a file named exactly `Dockerfile` and build them.

---

## Running Benchmarks

### Using Makefile (Recommended)
```bash
# Run all benchmarks
make run-all

# Run specific benchmark types
make run-static
make run-dynamic  
make run-websocket
make run-local

# Run a quick test (single server, reduced load)
make quick-test
```

### Using Scripts Directly
```bash
# Run all benchmarks
./run_benchmarks.sh

# Run specific types
./run_benchmarks.sh static
./run_benchmarks.sh dynamic
./run_benchmarks.sh websocket
./run_benchmarks.sh local
```

The benchmark runner:
- Uses override arrays for custom port mappings or exclusions.
- Auto-discovers all other images/servers from the folder structure.
- Passes the correct `--measurement_type` to each measurement script, ensuring the CSV output is consistent.

---

## WebSocket Benchmark Parameter Explanations

**Burst Mode Parameters:**
- `--clients`    : Number of concurrent WebSocket clients (connections) to the server.
- `--size_kb`    : Size of each WebSocket message sent (in kilobytes).
- `--bursts`     : Number of messages each client sends in a "burst" (as fast as possible, then waits).
- `--interval`   : Time (in seconds) to wait between each burst of messages.

**Streaming Mode Parameters:**
- `--clients`    : Number of concurrent WebSocket clients (connections) to the server.
- `--size_kb`    : Size of each WebSocket message sent (in kilobytes).
- `--rate`       : Number of messages per second each client sends (streaming, not bursty).
- `--duration`   : Total time (in seconds) to run the streaming test.

**Other Inputs:**
- `--server_image`     : The Docker image name of the WebSocket server to test.
- `--pattern`          : The test pattern: 'burst' (bursty traffic) or 'stream' (steady rate).
- `--mode`             : The WebSocket mode: usually 'echo' (server echoes back what it receives).
- `--output_csv`       : Path to the CSV file where results will be saved.
- `--measurement_type` : Label for the type of measurement (for your records).

**Example:**
- Burst:   1 client, 8 KB messages, 3 bursts, 0.5s between bursts
- Stream:  1 client, 8 KB messages, 10 messages/sec, for 5 seconds

---

## CSV Output Format
All measurement scripts output CSV files with the following columns:

```
Container Name,Type,Num CPUs,Total Requests,Successful Requests,Failed Requests,Execution Time (s),Requests/s,Total Energy (J),Avg Power (W),Samples,Avg CPU (%),Peak CPU (%),Total CPU (%),Avg Mem (MB),Peak Mem (MB),Total Mem (MB)
```

- `Container Name`: Name of the Docker image or local server.
- `Type`: Measurement type (`static`, `dynamic`, `websocket`, `local`).
- `Num CPUs`: Number of CPU cores detected.
- The remaining columns are performance and resource metrics.

This makes it easy to aggregate and compare results across all server types.

---

## GUI Graph Generator
To visualize results, use the GUI graph generator:

```bash
# Using Makefile
make graph

# Or directly
python3 gui_graph_generator.py
```

1. Start the GUI with the command above.
2. In the graphical window, use the buttons to select CSV files or folders containing your benchmark results.
3. Choose the columns you want to plot and generate graphs interactively.
4. Save or export the generated graphs as needed.

> **Note:** All file selection and graph generation is done through the graphical interface. No columns are pre-selected by default - you have full control over what gets plotted.

---

## Repository Management

The project includes powerful repository management tools to maintain a clean development environment:

### Cleaning Options

```bash
# Clean Docker builds only (safe)
make clean-build

# Clean entire repository to bare minimum (nuclear option)
make clean-repo
```

**`make clean-build`:**
- Removes Docker containers and images related to benchmarks
- Keeps your source code and local files
- Safe for regular maintenance

**`make clean-repo`:**
- Removes ALL untracked files and directories
- Resets to fresh clone state
- Removes Python virtual environment (`srv/`)
- Removes all logs, results, and build artifacts
- Perfect for getting a completely fresh start

### When to Use Each

- **`clean-build`**: When you want to rebuild Docker images or free up Docker space
- **`clean-repo`**: When you want a completely clean repository (like after `git clone`)

After `make clean-repo`, you'll need to:
1. `make setup` - Recreate virtual environment
2. `source srv/bin/activate` - Activate environment  
3. `make install` - Install dependencies

---

## Makefile Commands

The project includes a comprehensive Makefile for easy automation:

```bash
# Environment setup
make setup          # Create virtual environment
make ensure-env     # Ensure virtual environment is active
make install        # Install Python dependencies

# Docker operations
make build          # Build all Docker images
make clean-build    # Remove benchmark-related Docker containers and images

# Repository management
make clean-repo     # Clean repository to bare minimum (fresh clone state)

# Benchmarking
make quick-test     # Run quick test
make run-all        # Run all benchmarks
make run-static     # Run static server benchmarks
make run-dynamic    # Run dynamic server benchmarks
make run-websocket  # Run WebSocket benchmarks
make run-local      # Run local server benchmarks

# Visualization
make graph          # Start GUI graph generator

# Validation
make validate       # Validate dependencies

# Help
make help           # Show all available commands
```

---

## Troubleshooting

### Common Issues

#### Virtual Environment
- **"Virtual environment not found"**: Run `make setup` to create the environment
- **"Wrong virtual environment"**: Ensure you're using the `srv` environment or update the Makefile

#### Docker Issues
- **Port conflicts**: Check for existing containers with `docker ps` and stop them with `docker stop <container_name>`
- **Build failures**: Check Dockerfile syntax and base image availability
- **Permission errors**: Ensure your user is in the docker group or use `sudo`

#### Energy Measurement
- **No Energy Data**: Ensure Scaphandre is installed and your hardware supports RAPL
- **Zero energy values**: Check Scaphandre permissions and hardware compatibility

#### Performance Issues
- **Health Check Fails**: Verify the server is running and accessible on the specified port
- **Timeout errors**: Increase timeout values in measurement scripts for slower servers

### Logs
Check logs for detailed error messages:
```bash
# Docker container logs
docker logs <container_name>

# Benchmark logs
tail -f logs/run_*.log
```

### Debug Mode
Enable verbose output for debugging:
```bash
# Set debug environment variable
export DEBUG=1
make quick-test
```

---

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.