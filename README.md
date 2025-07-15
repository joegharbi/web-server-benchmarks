# Web Server Energy and Performance Benchmarking Framework

## Overview
A comprehensive benchmarking framework for evaluating the performance and energy efficiency of web servers running in Docker containers, local installations, and WebSocket servers. Features automatic container discovery, intelligent health checks, simplified port management, and extensive automation.

## Key Features
- **🔄 Auto-Discovery**: Automatically finds and benchmarks all containers from directory structure
- **🏥 Intelligent Health Checks**: Comprehensive health validation before benchmarking
- **🔧 Simplified Port Management**: Fixed host port with automatic container port detection
- **📊 Multi-Modal Testing**: Static, dynamic, WebSocket, and local server benchmarks
- **⚡ Energy Measurement**: Integrated Scaphandre for power consumption analysis
- **📈 Visualization**: Interactive GUI for result analysis and graph generation
- **🧹 Repository Management**: Powerful cleaning and maintenance tools

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Quick Start](#quick-start)
3. [Directory Structure](#directory-structure)
4. [Container Auto-Discovery](#container-auto-discovery)
5. [Health Check System](#health-check-system)
6. [Port Management](#port-management)
7. [Adding New Servers](#adding-new-servers)
8. [Running Benchmarks](#running-benchmarks)
9. [WebSocket Testing](#websocket-testing)
10. [Results and Visualization](#results-and-visualization)
11. [Repository Management](#repository-management)
12. [Makefile Commands](#makefile-commands)
13. [Troubleshooting](#troubleshooting)

---

## Prerequisites
- **OS**: Linux (Debian-based recommended)
- **Python 3**: 3.6+ with pip
- **Docker**: `sudo apt install docker.io`
- **Scaphandre**: `cargo install scaphandre` (for energy measurement)
- **Make**: Usually pre-installed

Verify installations:
```bash
python3 --version
docker --version
scaphandre --version
make --version
```

## Prerequisites and System Requirements

- Docker must be installed and running.
- Python 3.8+ required for benchmark scripts.
- All containers are started with `--ulimit nofile=100000:100000` to ensure high concurrency support. This is enforced by the health check system.
- The scripts `run_benchmarks.sh` and `check_health.sh` automatically set `ulimit -n 100000` at the start, so you do not need to set it manually.
- If your system or Docker daemon restricts file descriptor limits, you must increase them (see Troubleshooting).

---

## Quick Start

### 1. Environment Setup

```bash
# Create only the Python virtual environment (no servers)
make setup

# Or set up environment and install dependencies
make set-env

# To install local servers (nginx, yaws, etc.)
make setup-local

# To build all Docker images/containers
make setup-docker
```

- You can combine these as needed. For example, to set up everything:
  ```bash
  make set-env setup-local setup-docker
  ```

### 2. Build and Validate
```bash
# Build all Docker images (if not already done)
make setup-docker

# Run comprehensive health checks (includes ulimit check)
make check-health
```

### 3. Run Benchmarks
```bash
# Quick test (single server, reduced load)
make quick-test

# Full benchmark suite
make run-all

# Specific benchmark types
make run-static
make run-dynamic
make run-websocket
make run-local
```

### 4. Analyze Results
```bash
# Generate interactive graphs
make graph
```

---

## Directory Structure
```
web-server-benchmarks/
├── containers/
│   ├── static/         # Static web servers (Apache, Nginx, etc.)
│   │   └── st-*/Dockerfile
│   ├── dynamic/        # Dynamic web servers (with modules/scripts)
│   │   └── dy-*/Dockerfile
│   └── measure_docker.py
├── web-socket/         # WebSocket servers
│   ├── ws-*/Dockerfile
│   └── measure_websocket.py
├── local/              # Local server configurations
│   ├── measure_local.py
│   └── setup_*.sh
├── results/            # Benchmark results (timestamped)
├── logs/               # Execution logs
├── check_health.sh     # Health check system
├── run_benchmarks.sh   # Main benchmark runner
├── gui_graph_generator.py
├── Makefile           # Automation commands
└── README.md
```

---

## Container Auto-Discovery

The framework automatically discovers all available servers from the directory structure using **true autodiscovery** - no naming conventions required!

### Auto-Discovery Process
1. **Directory Scanning**: Scans `containers/static/`, `containers/dynamic/`, and `web-socket/`
2. **Dockerfile Detection**: Identifies directories containing `Dockerfile`
3. **Port Detection**: Reads `EXPOSE` directive from Dockerfile for container port
4. **Automatic Testing**: Runs health checks and benchmarks with sensible defaults
5. **Dynamic Cleanup**: Removes all discovered containers and images during cleanup

### Autodiscovery Implementation
- ✅ **Build System** (`install_benchmarks.sh`): Discovers and builds all containers
- ✅ **Health Checks** (`check_health.sh`): Tests all discovered containers
- ✅ **Benchmark Runner** (`run_benchmarks.sh`): Runs benchmarks on all discovered containers
- ✅ **Cleanup System**: Removes all discovered containers and images

### Example Structure
```bash
containers/static/my-nginx/Dockerfile      # Auto-discovered, must support high ulimit
containers/dynamic/my-apache/Dockerfile    # Auto-discovered, must support high ulimit
web-socket/my-websocket/Dockerfile         # Auto-discovered, must support high ulimit
```

### Legacy Naming Convention (Optional)
While not required, you can still use the legacy naming convention for organization:
- **Static containers**: `st-*` (e.g., `st-nginx-deb-self`)
- **Dynamic containers**: `dy-*` (e.g., `dy-apache-deb-self`)
- **WebSocket containers**: `ws-*` (e.g., `ws-cowboy-27-self`)

---

## Health Check System

The framework includes a comprehensive health check system that validates containers before benchmarking:

### Health Check Features
- **Container Startup**: Verifies containers start successfully
- **HTTP Response**: Tests for proper HTTP 200 responses
- **WebSocket Handshake**: Validates WebSocket upgrade responses
- **Stability Testing**: Ensures containers remain running
- **ulimit Enforcement**: Checks that `ulimit -n` is set to 100000 inside each container (required for high concurrency)
- **Automatic Cleanup**: Stops and removes test containers
- **ulimit Reporting**: The health check prints the actual `ulimit -n` value inside each container. A container is only reported as healthy if this value is correct.

### Running Health Checks
```bash
# Using Makefile (recommended)
make check-health
make health
make check

# Using script directly
./check_health.sh

# Custom port
HOST_PORT=9001 make check-health
HOST_PORT=9001 ./check_health.sh

# Custom timeouts
./check_health.sh --timeout 60 --startup 15
```

### Health Check Output
```
[INFO] Starting health check for all built containers...
[INFO] Using fixed host port: 8001
[INFO] Found 28 containers to test

[INFO] Testing st-nginx-deb-self...
[SUCCESS] st-nginx-deb-self: Healthy (ready for benchmarking)

[INFO] Testing dy-apache-deb-self...
[SUCCESS] dy-apache-deb-self: Healthy (ready for benchmarking)

[INFO] === HEALTH CHECK SUMMARY ===
[INFO] Total containers tested: 28
[SUCCESS] Healthy containers: 28
[SUCCESS] All containers are healthy! 🎉
```

---

## Port Management

The framework uses a simplified port management system:

### Fixed Host Port
- **Default**: All containers use host port `8001`
- **Configurable**: Set `HOST_PORT` environment variable
- **Container Port**: Automatically detected from Dockerfile `EXPOSE` directive

### Port Configuration
```bash
# Default behavior
./check_health.sh          # Uses port 8001
./run_benchmarks.sh        # Uses port 8001

# Custom port
HOST_PORT=9001 ./check_health.sh
HOST_PORT=9001 ./run_benchmarks.sh

# Session-wide setting
export HOST_PORT=9001
./check_health.sh
./run_benchmarks.sh
```

### Port Mapping Examples
```dockerfile
# Dockerfile with EXPOSE directive
EXPOSE 80
# Results in: 8001:80 mapping

EXPOSE 8080
# Results in: 8001:8080 mapping
```

---

## Adding New Servers

### Docker Containers
1. **Create Directory**: Any name works with autodiscovery
   ```bash
   mkdir -p containers/static/my-server
   mkdir -p containers/dynamic/my-server
   mkdir -p web-socket/my-server
   ```

2. **Add Dockerfile**: Include `EXPOSE` directive and ensure your server can handle high file descriptor limits (ulimit 100000 is enforced)
   ```dockerfile
   FROM nginx:alpine
   COPY nginx.conf /etc/nginx/nginx.conf
   EXPOSE 80
   CMD ["nginx", "-g", "daemon off;"]
   # Your entrypoint or CMD must not lower the ulimit
   ```

3. **Auto-Discovery**: Container will be automatically found, built, tested, and benchmarked
   ```bash
   make build        # Automatically builds your new container
   make check-health # Automatically tests your new container (including ulimit)
   make run-all      # Automatically benchmarks your new container
   ```

### Local Servers
1. **Add Configuration**: Place in `local/` directory
   ```bash
   touch local/my-server
   chmod +x local/my-server
   ```

2. **Auto-Discovery**: Server will be included in local benchmarks

---

## Running Benchmarks

### Using Makefile (Recommended)
```bash
# Complete benchmark suite
make run-all

# Specific benchmark types
make run-static      # Static containers only
make run-dynamic     # Dynamic containers only
make run-websocket   # WebSocket containers only
make run-local       # Local servers only

# Quick testing
make run-quick       # Reduced parameters for fast testing (3 request counts: 1000, 5000, 10000)
make run-super-quick # Single test per container type (1 request count: 1000)
```

### Using Scripts Directly
```bash
# All benchmarks
./run_benchmarks.sh

# Specific types
./run_benchmarks.sh static
./run_benchmarks.sh dynamic
./run_benchmarks.sh websocket
./run_benchmarks.sh local

# Quick mode
./run_benchmarks.sh --quick static
```

### Benchmark Parameters

#### HTTP Benchmarks (Static/Dynamic/Local)
- **Full test**: 13 request counts (100, 1000, 5000, 8000, 10000, 15000, 20000, 30000, 40000, 50000, 60000, 70000, 80000)
- **Quick test**: 3 request counts (1000, 5000, 10000)
- **Super quick test**: 1 request count (1000) - fastest validation

#### WebSocket Benchmarks
- **Full test**: Comprehensive parameter combinations
  - Burst mode: 4 client counts × 7 message sizes × 2 burst counts × 3 intervals
  - Stream mode: 4 client counts × 7 message sizes × 3 rates × 3 durations
- **Quick test**: Moderate parameter combinations
  - Burst mode: 2 client counts × 2 message sizes × 2 burst counts × 1 interval
  - Stream mode: 2 client counts × 2 message sizes × 2 rates × 1 duration
- **Super quick test**: Single parameter combination
  - Burst mode: 1 client × 8KB × 1 burst × 0.5s interval
  - Stream mode: 1 client × 8KB × 10 msg/s × 3s duration

---

## WebSocket Testing

The framework includes comprehensive WebSocket benchmarking:

### Test Modes
1. **Burst Mode**: Rapid message bursts with intervals
2. **Streaming Mode**: Continuous message streams at fixed rates

### Parameters
```bash
# Burst Mode
--clients     # Concurrent WebSocket connections
--size_kb     # Message size in kilobytes
--bursts      # Number of messages per burst
--interval    # Time between bursts (seconds)

# Streaming Mode
--clients     # Concurrent WebSocket connections
--size_kb     # Message size in kilobytes
--rate        # Messages per second
--duration    # Test duration (seconds)
```

### Example Test Scenarios
```bash
# Burst: 10 clients, 8KB messages, 50 bursts, 0.5s intervals
# Stream: 50 clients, 64KB messages, 100 msg/s, 30s duration
```

---

## Results and Visualization

### Results Structure
```
results/
└── 2024-01-15_143022/
    ├── static/          # Static container results (grouped by container name)
    ├── dynamic/         # Dynamic container results (grouped by container name)
    ├── websocket/       # WebSocket results (grouped by container name)
    └── local/           # Local server results (grouped by server name)
```

**File Organization:**
- **Before**: `st-apache-deb-self_1000.csv`, `st-apache-deb-self_5000.csv`, `st-apache-deb-self_10000.csv`
- **After**: `st-apache-deb-self.csv` (single file with multiple rows for different request counts)

### CSV Output Format

#### HTTP Containers (Static/Dynamic/Local)
Results are grouped by container name with multiple rows per file:
```csv
Container Name,Type,Num CPUs,Total Requests,Successful Requests,Failed Requests,Execution Time (s),Requests/s,Total Energy (J),Avg Power (W),Samples,Avg CPU (%),Peak CPU (%),Total CPU (%),Avg Mem (MB),Peak Mem (MB),Total Mem (MB)
```

**Example:** `st-apache-deb-self.csv` contains:
- Row 1: 1000 requests, results...
- Row 2: 5000 requests, results...
- Row 3: 10000 requests, results...

#### WebSocket Containers
WebSocket-specific metrics with latency and throughput data:
```csv
Container Name,Test Type,Num CPUs,Total Messages,Successful Messages,Failed Messages,Execution Time (s),Messages/s,Throughput (MB/s),Avg Latency (ms),Min Latency (ms),Max Latency (ms),Total Energy (J),Avg Power (W),Samples,Avg CPU (%),Peak CPU (%),Total CPU (%),Avg Mem (MB),Peak Mem (MB),Total Mem (MB),Pattern,Num Clients,Message Size (KB),Rate (msg/s),Bursts,Interval (s),Duration (s)
```

**Key WebSocket Metrics:**
- **Latency**: Average, minimum, and maximum round-trip times
- **Throughput**: Messages per second and data throughput in MB/s
- **Pattern Configuration**: Burst or stream mode with specific parameters

### Visualization
```bash
# Interactive graph generator
make graph

# Or directly
python3 gui_graph_generator.py
```

Features:
- **File Selection**: Browse and select CSV files/folders
- **Column Selection**: Choose metrics to visualize
- **Interactive Graphs**: Zoom, pan, and export capabilities
- **Multi-Server Comparison**: Compare multiple servers simultaneously

---

## Repository Management

### Cleaning Options
```bash
# Safe cleaning (Docker only)
make clean-build

# Clean everything: Docker and local servers (autodiscovery)
make clean-all

# Uninstall all local servers (autodiscovery)
make clean-local

# Complete reset (nuclear option)
make clean-repo
```

### Clean Operations
- **`clean-build`**: Removes Docker containers/images, keeps source code
- **`clean-local`**: Uninstalls all local servers that have a `setup_{server}.sh` script with uninstall support (autodiscovery)
- **`clean-all`**: Runs both `clean-build` and `clean-local` for a full clean (Docker + local servers)
- **`clean-repo`**: Complete repository reset to fresh clone state

**Autodiscovery:**
- `clean-local` will automatically find and uninstall any local server for which there is a `setup_{server}.sh` script in `local/` that defines and supports the uninstall command. This makes it easy to add new local servers—just provide a compatible setup script!

### Post-Clean Setup
After `make clean-repo`:
```bash
make setup
source srv/bin/activate
make install
make build
```

---

## Makefile Commands

### Environment Management
```bash
make setup          # Create virtual environment
make ensure-env     # Ensure environment is active
make install        # Install dependencies
make validate       # Validate all prerequisites
```

### Docker Operations
```bash
make build          # Build all Docker images
make clean-build    # Remove Docker containers/images
```

### Benchmarking
```bash
make quick-test     # Quick benchmark test
make run-all        # Complete benchmark suite
make run-static     # Static container benchmarks
make run-dynamic    # Dynamic container benchmarks
make run-websocket  # WebSocket benchmarks
make run-local      # Local server benchmarks
```

### Visualization
```bash
make graph          # Launch graph generator
```

### Repository Management
```bash
make clean-repo     # Complete repository reset
make help           # Show all available commands
```

---

## Troubleshooting

### Common Issues

#### Health Check Failures
```bash
# Check container logs
docker logs health-check-<container-name>

# Verify port availability
netstat -tlnp | grep 8001

# Test manual container startup
docker run -d --rm --ulimit nofile=100000:100000 -p 8001:80 <image-name>

# Check ulimit inside container (should be 100000)
docker exec health-check-<container-name> sh -c 'ulimit -n'
```

- The health check now verifies that `ulimit -n` (open file descriptors) is set to 100000 inside each container. The script prints the actual value for each container. If this is not met, the container will fail the health check and will not be benchmarked.
- If you see ulimit errors, ensure your Docker daemon and host OS allow high file descriptor limits. See below for tips.

#### Docker Issues
```bash
# Check Docker status
docker info

# Clean up containers
docker stop $(docker ps -q)
docker rm $(docker ps -aq)

# Rebuild images
make clean-build
make build
```

#### Energy Measurement
```bash
# Verify Scaphandre installation
scaphandre --version

# Check hardware support
cat /sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj

# Test energy measurement
scaphandre -t 1
```

#### Performance Issues
```bash
# Increase timeouts
./check_health.sh --timeout 60 --startup 20

# Check system resources
htop
free -h
df -h
```

### Debug Mode
```bash
# Enable verbose output
export DEBUG=1
make quick-test

# Check logs
tail -f logs/run_*.log
```

### Getting Help
```bash
# Show all Makefile commands
make help

# Script help
./check_health.sh --help
./run_benchmarks.sh help
```

### ulimit and File Descriptor Limits
- All containers are started with `--ulimit nofile=100000:100000` for high concurrency.
- If you see errors about ulimit or file descriptors, you may need to:
  - Increase your system's open file limit (e.g., edit `/etc/security/limits.conf` on Linux)
  - Configure Docker daemon to allow higher ulimits (see Docker docs: https://docs.docker.com/engine/reference/commandline/dockerd/#default-ulimit)
  - Restart Docker after changing system or daemon limits
- You can check the current limit inside any running container:
  ```bash
  docker exec <container-name> sh -c 'ulimit -n'
  ```
  The health check script will also print this value for you.
- The health check will fail if the limit is not 100000.

---

## Recent Improvements

### v2.1 Enhancements
- **CSV Result Grouping**: Results grouped by container name for easier analysis
- **WebSocket-Specific Metrics**: Enhanced WebSocket CSV format with latency and throughput data
- **Super Quick Testing**: New `run-super-quick` option for fastest validation
- **Improved Local Scripts**: Fixed path resolution for local server setup scripts
- **Enhanced Port Management**: All containers now use port 80 internally for consistency

### v2.0 Enhancements
- **Simplified Port Management**: Fixed host port with automatic container port detection
- **Enhanced Health Checks**: Comprehensive validation with HTTP and WebSocket testing
- **Auto-Discovery**: Intelligent container detection from directory structure
- **Improved Automation**: Streamlined Makefile commands and script integration
- **Better Error Handling**: Robust error detection and reporting
- **Configurable Ports**: Environment variable support for custom ports

### Key Changes
- Removed complex port arrays and manual configuration
- Added real HTTP response validation in health checks
- Implemented WebSocket handshake testing
- Enhanced container discovery and management
- Improved repository cleaning and maintenance tools
- Added comprehensive logging and error reporting

---

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.