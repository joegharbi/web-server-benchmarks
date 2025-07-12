# Web Server Benchmark Framework - Complete Server Coverage

## Overview
This document outlines the complete server coverage across all three categories (Static, Dynamic, WebSocket) in the benchmarking framework. All server types now have consistent implementations across categories.

## Server Type Matrix

| Server Type | Static | Dynamic | WebSocket | Local |
|-------------|--------|---------|-----------|-------|
| **Apache**  | ✅ `st-apache-deb` | ✅ `dy-apache-deb` | ✅ `ws-apache` | ❌ |
| **Nginx**   | ✅ `st-nginx-deb` | ✅ `dy-nginx-deb` | ✅ `ws-nginx` | ✅ `nginx` |
| **Yaws**    | ✅ `st-yaws-deb` | ✅ `dy-yaws-latest-deb` | ✅ `ws-yaws` | ✅ `yaws` |
| **Cowboy**  | ✅ `st-cowboy-play` | ✅ `dy-cowboy-play` | ✅ `ws-cowboy` | ❌ |

## Detailed Server Implementations

### 1. Apache Servers

#### Static Apache (`st-apache-deb`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: Static HTML content, minimal configuration
- **Configuration**: Basic Apache2 configuration for static file serving

#### Dynamic Apache (`dy-apache-deb`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: PHP support, dynamic content generation
- **Configuration**: Apache2 + PHP, dynamic time-based content

#### WebSocket Apache (`ws-apache`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: WebSocket proxy to Python backend
- **Configuration**: Apache2 with mod_proxy_wstunnel, Python WebSocket server

### 2. Nginx Servers

#### Static Nginx (`st-nginx-deb`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: Built from source, static HTML content
- **Configuration**: Minimal Nginx configuration

#### Dynamic Nginx (`dy-nginx-deb`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: Dynamic modules, enhanced configuration
- **Configuration**: Nginx with dynamic content capabilities

#### WebSocket Nginx (`ws-nginx`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: WebSocket proxy to Python backend
- **Configuration**: Nginx proxy + Python WebSocket server

#### WebSocket Nginx + Tornado (`ws-nginx-tornado`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: WebSocket proxy to Tornado backend
- **Configuration**: Nginx proxy + Tornado WebSocket server

#### WebSocket Nginx + Java (`ws-nginx-java`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 8080 (mapped to 8080:8080)
- **Features**: WebSocket proxy to Spring Boot backend
- **Configuration**: Nginx proxy + Java Spring Boot WebSocket server

### 3. Yaws Servers

#### Static Yaws (`st-yaws-deb`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: Erlang-based, static content
- **Configuration**: Basic Yaws configuration

#### Dynamic Yaws (`dy-yaws-latest-deb`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: Latest Yaws with dynamic capabilities
- **Configuration**: Enhanced Yaws configuration

#### WebSocket Yaws (`ws-yaws`)
- **Base Image**: `debian:bookworm-slim`
- **Port**: 80 (mapped to 8001:80)
- **Features**: Native Erlang WebSocket support
- **Configuration**: Yaws with WebSocket handler

### 4. Cowboy Servers

#### Static Cowboy (`st-cowboy-play`)
- **Base Image**: `erlang:25`
- **Port**: 8080 (mapped to 8001:8080)
- **Features**: Erlang-based, lightweight
- **Configuration**: Basic Cowboy application

#### Dynamic Cowboy (`dy-cowboy-play`)
- **Base Image**: `erlang:25`
- **Port**: 8080 (mapped to 8001:8080)
- **Features**: Dynamic content generation, API endpoints
- **Configuration**: Cowboy with dynamic routes and JSON API

#### WebSocket Cowboy (`ws-cowboy`)
- **Base Image**: `erlang:25`
- **Port**: 8080 (mapped to 8080:8080)
- **Features**: Native Erlang WebSocket support
- **Configuration**: Cowboy with WebSocket handler

### 5. Erlang Variants

#### Static Erlang Servers
- `st-erlang23`, `st-erlang26`, `st-erlang27`
- `st-erlindex23`, `st-erlindex26`, `st-erlindex27`

#### Dynamic Erlang Servers
- `dy-erlang23`, `dy-erlang26`, `dy-erlang27`
- `dy-erlindex23`, `dy-erlindex26`, `dy-erlindex27`

## Port Mappings

### Standard Ports
- **HTTP Servers**: `8001:80` (host:container)
- **Erlang/Cowboy**: `8001:8080` (host:container)
- **Java WebSocket**: `8080:8080` (host:container)

### Port Configuration
All port mappings are defined in `run_benchmarks.sh` override arrays:
```bash
DEFAULT_PORT="8001:80"
ERLANG_PORT="8001:8080"
```

## Auto-Discovery System

The framework automatically discovers servers based on directory structure:
- **Static**: `containers/static/*/Dockerfile`
- **Dynamic**: `containers/dynamic/*/Dockerfile`
- **WebSocket**: `web-socket/*/Dockerfile`
- **Local**: `local/*` (scripts/configs)

## Benchmark Coverage

### HTTP Benchmarking
- **Payloads**: 100, 1000, 5000, 8000, 10000, 15000, 20000, 30000, 40000, 50000, 60000, 70000, 80000 requests
- **Metrics**: Performance, energy consumption, CPU usage, memory usage

### WebSocket Benchmarking
- **Burst Mode**: Variable clients, message sizes, burst counts, intervals
- **Stream Mode**: Variable clients, message sizes, rates, durations
- **Metrics**: Latency, throughput, energy consumption, resource usage

## Consistency Features

### 1. Base Images
- **Debian-based**: Apache, Nginx, Yaws use `debian:bookworm-slim`
- **Erlang-based**: Cowboy uses `erlang:25`

### 2. Configuration Patterns
- **Static**: Minimal configuration, static content
- **Dynamic**: Enhanced features, dynamic content generation
- **WebSocket**: Proxy configurations or native WebSocket support

### 3. Port Standardization
- **HTTP**: Port 80 internally, mapped to 8001
- **Erlang**: Port 8080 internally, mapped to 8001
- **Java**: Port 8080 internally, mapped to 8080

### 4. Health Checks
All servers implement consistent health check patterns:
- HTTP servers: `GET /` returns 200 OK
- WebSocket servers: Echo functionality

## Usage Examples

### Run All Servers of a Type
```bash
# All static servers
make run-static

# All dynamic servers  
make run-dynamic

# All WebSocket servers
make run-websocket
```

### Run Specific Server
```bash
# Apache static
./run_benchmarks.sh static st-apache-deb

# Nginx dynamic
./run_benchmarks.sh dynamic dy-nginx-deb

# Cowboy WebSocket
./run_benchmarks.sh websocket ws-cowboy
```

### Quick Test
```bash
# Test one server from each category
make quick-test
```

## Framework Benefits

1. **Complete Coverage**: All major server types represented across categories
2. **Consistent Configuration**: Standardized Docker images and configurations
3. **Auto-Discovery**: Automatic server detection and configuration
4. **Comprehensive Metrics**: Performance, energy, and resource monitoring
5. **Flexible Testing**: Support for both quick tests and full benchmarks
6. **Visualization**: Advanced GUI for result analysis and comparison

This framework now provides a comprehensive and consistent benchmarking environment for comparing web server performance and energy efficiency across different technologies and deployment models. 