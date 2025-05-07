#!/bin/bash

# Run measurement scripts for all Docker images and local servers based on args
PYTHON_PATH="$(pwd)/srv/bin/python3"  # Absolute path from root directory

# Default port mapping (host:container)
DEFAULT_PORT="8001:80"
ERLANG_PORT="8001:8080"  # For Erlang-based images exposing 8080

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
    ["cowboy-play"]="$ERLANG_PORT" 
    ["erlang-deb"]="$DEFAULT_PORT" 
    ["nginx-deb"]="$DEFAULT_PORT" 
    ["yaws-deb"]="$DEFAULT_PORT" 
    ["yaws-latest-deb"]="$DEFAULT_PORT" 
    ["erlang27"]="$ERLANG_PORT" 
    ["erlindex23"]="$ERLANG_PORT" 
    ["index26"]="$ERLANG_PORT" 
    ["index27"]="$ERLANG_PORT" 
    ["erlang23"]="$ERLANG_PORT" 
    ["erlang26"]="$ERLANG_PORT" 
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
TARGET_TYPE=""
TARGET_IMAGES=()

if [[ $# -gt 0 ]]; then
    RUN_ALL=0
    TARGET_TYPE="$1"
    shift
    TARGET_IMAGES=("$@")
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
if [[ $RUN_ALL -eq 1 ]]; then
    # Run all images in websocket_images, dynamic_images, static_images, and local_servers
    for image in "${!websocket_images[@]}"; do
        run_websocket_tests "$image"
    done
    for image in "${!dynamic_images[@]}"; do
        run_docker_tests "$image" "${dynamic_images[$image]}" "dynamic"
    done
    for image in "${!static_images[@]}"; do
        run_docker_tests "$image" "${static_images[$image]}" "static"
    done
    for server in "${!local_servers[@]}"; do
        run_local_tests "$server"
    done
else
    # Run specific type or images
    case "$TARGET_TYPE" in
        websocket)
            for image in "${TARGET_IMAGES[@]}"; do
                if [[ -v "websocket_images[$image]" ]]; then
                    run_websocket_tests "$image"
                else
                    echo "Error: $image is not a valid WebSocket image"
                fi
            done
            ;;
        dynamic)
            for image in "${TARGET_IMAGES[@]}"; do
                if [[ -v "dynamic_images[$image]" ]]; then
                    run_docker_tests "$image" "${dynamic_images[$image]}" "dynamic"
                else
                    echo "Error: $image is not a valid dynamic image"
                fi
            done
            ;;
        static)
            for image in "${TARGET_IMAGES[@]}"; do
                if [[ -v "static_images[$image]" ]]; then
                    run_docker_tests "$image" "${static_images[$image]}" "static"
                else
                    echo "Error: $image is not a valid static image"
                fi
            done
            ;;
        local)
            for server in "${TARGET_IMAGES[@]}"; do
                if [[ -v "local_servers[$server]" ]]; then
                    run_local_tests "$server"
                else
                    echo "Error: $server is not a valid local server"
                fi
            done
            ;;
        *)
            echo "Error: Unknown type $TARGET_TYPE. Valid types are: websocket, dynamic, static, local."
            ;;
    esac
fi