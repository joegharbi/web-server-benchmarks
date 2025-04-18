# Web Server Energy and Performance Benchmarking

## Overview
This project benchmarks the performance and energy efficiency of web servers running inside Docker containers. It includes scripts for building Docker images, running benchmarks, and generating visualizations of the results.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Directory Structure](#directory-structure)
3. [Adding Dockerfiles](#adding-dockerfiles)
4. [Installing Docker Images](#installing-docker-images)
5. [Running the Scripts](#running-the-scripts)
6. [Local Webserver Baseline](#local-webserver-baseline)
7. [Measurement Script](#measurement-script)
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
│   ├── static/
│   │   ├── apache-deb/
│   │   ├── nginx-deb/
│   │   └── ...
│   ├── dynamic/
│   │   ├── nginx-dynamic-deb/
│   │   └── ...
├── local/
│   ├── nginx/
│   ├── yaws/
│   └── ...
├── brainstorming/
│   ├── server_client.py
│   └── ...
├── web-socket/
│   ├── measure_websocket.py
│   └── ...
├── install_benchmarks.sh
├── measure.py
└── README.md
```

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
- Skips directories containing the word `brainstorming`.
- Searches for `Dockerfile` or `Dockerfile.<extension>` in each directory.
- Builds images with names derived from the directory and Dockerfile.

---

## Running the Scripts

### 1. **Run Benchmarks**
Use the `measure.py` script to benchmark a server:
```bash
sudo python3 measure.py --server_image <image_name> --num_requests <number>
```

#### Example:
```bash
sudo python3 measure.py --server_image nginx-deb --num_requests 5000
```

#### Options:
- `--server_image`: Docker image name (e.g., `nginx-deb`).
- `--num_requests`: Number of HTTP requests to send.
- `--output_csv`: Custom CSV output file.
- `--port_mapping`: Map host to container port (default: `8001:80`).

---

## Local Webserver Baseline
The `local` folder contains scripts and configurations for running a local webserver as a baseline for benchmarking. This includes lightweight setups for servers like Nginx and Yaws, which are used to compare performance and energy efficiency against containerized web servers.

You can directly run these local webservers without requiring Docker containers. For example:
- **Nginx**: Use the provided configuration files in the `local/nginx/` directory to start the server locally.
- **Yaws**: Follow the instructions and setup scripts in the `local/yaws/` directory to set up and run the Yaws webserver.

These local setups serve as a baseline for comparison with containerized web servers.

---

## Measurement Script
The `measure.py` script:
1. Starts the specified Docker container.
2. Sends HTTP requests to the server.
3. Collects energy, CPU, and memory metrics using Scaphandre and Docker stats.
4. Outputs results to the console and a CSV file.

### Output
- **Console**: Displays metrics like requests per second, energy consumption, and CPU/memory usage.
- **CSV**: Saves detailed results in `results/<image>.csv`.

---

## GUI Graph Generator
To visualize results, use the GUI graph generator:
1. Ensure results are saved in CSV format.
2. Run the graph generator script (if available):
   ```bash
   python3 graph_generator.py --input_csv <path_to_csv> --output_graph <path_to_graph>
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