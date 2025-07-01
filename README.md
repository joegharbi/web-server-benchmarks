# Web Server Energy and Performance Benchmarking

## Overview
This project benchmarks the performance and energy efficiency of web servers running inside Docker containers, locally installed servers, and WebSocket servers. It includes scripts for building Docker images, running benchmarks, and generating visualizations of the results.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Directory Structure](#directory-structure)
3. [Image/Server Naming and Auto-Discovery](#naming-and-auto-discovery)
4. [Adding Dockerfiles and Servers](#adding-dockerfiles-and-servers)
5. [Installing Docker Images](#installing-docker-images)
6. [Running Benchmarks](#running-benchmarks)
7. [CSV Output Format](#csv-output-format)
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
├── containers/
│   ├── static/         # Static web servers (e.g., Apache, Nginx)
│   │   └── <image_name>/Dockerfile
│   ├── dynamic/        # Dynamic web servers (e.g., Nginx with dynamic modules)
│   │   └── <image_name>/Dockerfile
├── web-socket/
│   └── <image_name>/Dockerfile  # WebSocket server images
├── local/              # Local webserver scripts/configs (e.g., nginx, yaws)
│   └── <server_name>   # Script or config file for local server
├── logs/               # Logs generated during benchmarking
├── output/             # Output files, such as temporary data or intermediate results
├── results/            # Results for all benchmarks, organized by timestamp and type
├── install_benchmarks.sh     # Script to build Docker images for benchmarking
├── run_benchmarks.sh         # Main script to trigger all benchmarks
├── gui_graph_generator.py    # Script to generate graphs from benchmarking results
└── README.md                 # Project documentation
```

---

## Naming and Auto-Discovery

- **Docker images (static, dynamic, websocket):**
  - Place each image in its own folder under the appropriate directory (`containers/static/`, `containers/dynamic/`, or `web-socket/`).
  - The folder name is the image name. The folder must contain a file named exactly `Dockerfile`.
  - Example: `containers/static/nginx-deb/Dockerfile`, `web-socket/nginx_websocket/Dockerfile`

- **Local servers:**
  - Place a script or config file in `local/` named after the server (e.g., `local/nginx`, `local/yaws`).

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

### Local Servers
1. Place a script or config file in `local/` named after the server.

Example:
```bash
touch local/myserver
```

---

## Installing Docker Images
Use the `install_benchmarks.sh` script to build Docker images:
```bash
./install_benchmarks.sh
```
- The script auto-discovers all subfolders in `containers/static/`, `containers/dynamic/`, and (optionally) `web-socket/` that contain a file named exactly `Dockerfile` and builds them.
- The image name is taken from the folder name.

---

## Running Benchmarks
The `run_benchmarks.sh` script is the main entry point for running all benchmarks. It:
- Uses override arrays for custom port mappings or exclusions.
- Auto-discovers all other images/servers from the folder structure.
- Passes the correct `--measurement_type` to each measurement script, ensuring the CSV output is consistent.

### Usage
```bash
./run_benchmarks.sh
```
Runs all benchmarks (static, dynamic, websocket, local).

To run only a specific type or image:
```bash
./run_benchmarks.sh static
./run_benchmarks.sh dynamic
./run_benchmarks.sh websocket
./run_benchmarks.sh local
```

---

## CSV Output Format
All measurement scripts output CSV files with the following columns (first two columns always):

```
server_image,type,Total Requests,Successful Requests,Failed Requests,Execution Time (s),Requests/s,Total Energy (J),Avg Power (W),Samples,Avg CPU (%),Peak CPU (%),Total CPU (%),Avg Mem (MB),Peak Mem (MB),Total Mem (MB)
```
- `server_image`: Name of the Docker image or local server.
- `type`: Measurement type (`static`, `dynamic`, `websocket`, `local`).
- The remaining columns are performance and resource metrics.

This makes it easy to aggregate and compare results across all server types.

---

## GUI Graph Generator
To visualize results, use the GUI graph generator:
1. Start the GUI with:
   ```bash
   python3 gui_graph_generator.py
   ```
2. In the graphical window, use the buttons to select CSV files or folders containing your benchmark results.
3. Choose the columns you want to plot and generate graphs interactively.
4. Save or export the generated graphs as needed.

> **Note:** All file selection and graph generation is done through the graphical interface. Do not use command-line arguments like `--input_csv` or `--output_graph` with this tool.

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