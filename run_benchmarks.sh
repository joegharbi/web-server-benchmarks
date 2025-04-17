#!/bin/bash

# Run measurement scripts for all Docker images and local servers based on args
PYTHON_PATH="$(pwd)/srv/bin/python3"  # Absolute path from root directory

# Default port mapping (host:container)
DEFAULT_PORT="8001:80"
ERLANG_PORT="8001:8080"  # For Erlang-based images exposing 8080

# Get all Docker image names
image_names=$(sudo docker images --format "{{.Repository}}")

# Define payloads for measure_docker.py and measure_local.py
payloads=(100 1000 5000 8000 10000 15000 20000)

# Define categories of images with their port mappings
declare -A websocket_images=( 
    ["nginx_websocket"]="" 
    ["nginx-aiohttp"]="" 
    ["nginx-java-websocket"]="" 
    ["nginx-tornado"]="" 
    ["yaws_websocket"]="" 
    ["yaws-socket"]="" 
)
declare -A dynamic_images=( 
    ["nginx-dynamic-deb"]="$DEFAULT_PORT" 
    ["yaws-dynamic-latest-deb"]="$DEFAULT_PORT" 
)
declare -A static_images=( 
    ["apache-deb"]="$DEFAULT_PORT" 
    ["cowboy-play"]="$ERLANG_PORT"      # Erlang-based, uses 8080
    # ["cowboy-test"]="$DEFAULT_PORT" 
    ["erlang-deb"]="$DEFAULT_PORT" 
    ["nginx-deb"]="$DEFAULT_PORT" 
    # ["yaws-apt-slim"]="$DEFAULT_PORT" 
    ["yaws-deb"]="$DEFAULT_PORT" 
    # ["yaws-git-full-27"]="$DEFAULT_PORT" 
    # ["yaws-git-slim-27"]="$DEFAULT_PORT" 
    ["yaws-latest-deb"]="$DEFAULT_PORT" 
    ["erlang27"]="$ERLANG_PORT"         # Erlang-based, uses 8080
    ["erlindex23"]="$ERLANG_PORT"       # Erlang-based, uses 8080
    ["index26"]="$ERLANG_PORT"          # Erlang-based, uses 8080
    ["index27"]="$ERLANG_PORT"          # Erlang-based, uses 8080
    ["erlang23"]="$ERLANG_PORT"         # Erlang-based, uses 8080
    ["erlang26"]="$ERLANG_PORT"         # Erlang-based, uses 8080
)
declare -A local_servers=( 
    ["nginx"]="" 
    ["yaws"]="" 
)

# Ensure required directories exist
mkdir -p results_docker/dynamic results_docker/static results_local results_websocket logs

# Log file setup
TIMESTAMP=$(date +"%Y-%m-%d_%H%M%S")
LOG_FILE="logs/run_${TIMESTAMP}.log"
echo "Logging to $LOG_FILE"
exec > >(tee -a "$LOG_FILE") 2>&1  # Redirect stdout and stderr to log file and console

# Parse arguments
RUN_ALL=1
TARGET=""
if [[ $# -gt 0 ]]; then
    if [[ "$1" == "--name" && $# -ge 2 ]]; then
        RUN_ALL=0
        TARGET="$2"
    elif [[ "$1" != "--"* ]]; then  # If first arg isn't an option, treat it as target
        RUN_ALL=0
        TARGET="$1"
    else
        echo "Unknown parameter: $1 (running all benchmarks by default)"
    fi
fi

# Free up port 8001 if in use (Docker only unless target is local)
echo "Checking and freeing port 8001..."
sudo docker ps -q --filter "publish=8001" | xargs -r sudo docker stop
sudo docker ps -a -q --filter "publish=8001" | xargs -r sudo docker rm
# Stop local servers only if target is in local_servers
if [[ -v "local_servers[$TARGET]" || $RUN_ALL -eq 1 ]]; then
    for server in "${!local_servers[@]}"; do
        if [ -f "local/setup_${server}.sh" ]; then
            echo "Stopping $server..."
            sudo "local/setup_${server}.sh" stop || echo "Warning: Failed to stop $server cleanly"
        fi
    done
fi

# Function to run WebSocket tests (no port_mapping)
run_websocket_tests() {
    local image=$1
    if [ ! -f "./web-socket/measure_websocket.py" ]; then
        echo "Error: ./web-socket/measure_websocket.py not found"
        return 1
    fi
    echo "Running measure_websocket.py for $image in burst mode"
    "$PYTHON_PATH" ./web-socket/measure_websocket.py \
        --server_image "$image" \
        --num_clients 10 \
        --size_mb 10 \
        --interval_s 1 \
        --duration_s 60 \
        --output_csv "results_websocket/$image-burst.csv" || echo "Burst mode failed for $image"

    echo "Running measure_websocket.py for $image in streaming mode"
    "$PYTHON_PATH" ./web-socket/measure_websocket.py \
        --server_image "$image" \
        --num_clients 10 \
        --rate_mb_s 5 \
        --duration_s 60 \
        --size_mb 0 \
        --interval_s 0 \
        --output_csv "results_websocket/$image-streaming.csv" || echo "Streaming mode failed for $image"
}

# Function to run dynamic/static tests (with port_mapping)
run_docker_tests() {
    local image=$1
    local port_mapping=$2
    local category=$3
    if [ ! -f "./containers/measure_docker.py" ]; then
        echo "Error: ./containers/measure_docker.py not found"
        return 1
    fi
    for payload in "${payloads[@]}"; do
        echo "Running measure_docker.py for $image with $payload requests"
        "$PYTHON_PATH" ./containers/measure_docker.py \
            --server_image "$image" \
            --num_requests "$payload" \
            --port_mapping "$port_mapping" \
            --output_csv "results_docker/$category/$image.csv"
    done
}

# Function to run local server tests
run_local_tests() {
    local server=$1
    if [ ! -f "./local/measure_local.py" ]; then
        echo "Error: ./local/measure_local.py not found"
        return 1
    fi
    for payload in "${payloads[@]}"; do
        echo "Running measure_local.py for $server with $payload requests"
        # Run from local/ to match setup script location
        (cd local && "$PYTHON_PATH" measure_local.py \
            --server "$server" \
            --num_requests "$payload" \
            --output_csv "../results_local/$server.csv")
    done
}

# Main logic
for image in $image_names "${!local_servers[@]}"; do
    if [[ $RUN_ALL -eq 0 && "$image" != "$TARGET" ]]; then
        continue
    fi

    echo "Running tests for: $image"
    
    # Docker-specific cleanup only for Docker images
    if [[ ! -v "local_servers[$image]" ]]; then
        sudo docker rm -f "$image" > /dev/null 2>&1
    fi

    # WebSocket images
    if [[ -v "websocket_images[$image]" ]]; then
        run_websocket_tests "$image"
        continue
    fi

    # Dynamic images
    if [[ -v "dynamic_images[$image]" ]]; then
        run_docker_tests "$image" "${dynamic_images[$image]}" "dynamic"
        continue
    fi

    # Static images
    if [[ -v "static_images[$image]" ]]; then
        run_docker_tests "$image" "${static_images[$image]}" "static"
        continue
    fi

    # Local servers
    if [[ -v "local_servers[$image]" ]]; then
        run_local_tests "$image"
        continue
    fi

    # Default to static with DEFAULT_PORT if uncategorized Docker image
    echo "Uncategorized Docker image $image - treating as static with default port $DEFAULT_PORT"
    run_docker_tests "$image" "$DEFAULT_PORT" "static"
done