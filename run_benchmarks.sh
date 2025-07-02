#!/bin/bash

# Run measurement scripts for all Docker images and local servers based on args
PYTHON_PATH="$(pwd)/srv/bin/python3"  # Absolute path from root directory

# Default port mapping (host:container)
DEFAULT_PORT="8001:80"
ERLANG_PORT="8001:8080"  # For Erlang-based images exposing 8080

# Define payloads for measure_docker.py and measure_local.py
payloads=(100 1000 5000 8000 10000 15000 20000 30000 40000 50000 60000 70000 80000)

# Override arrays: authoritative for custom port mappings or exclusions
# If an image/server is listed here, it is used with these settings
# If not, auto-discover and use sensible defaults

declare -A websocket_images=( 
    ["ws-nginx"]="" 
    ["ws-nginx-java"]="8080:8080" 
    ["ws-nginx-tornado"]="" 
    ["ws-yaws"]="" 
)
declare -A dynamic_images=( 
    ["dy-nginx-deb"]="$DEFAULT_PORT" 
    ["dy-yaws-latest-deb"]="$DEFAULT_PORT" 
    ["dy-apache-deb"]="$DEFAULT_PORT" 
    ["dy-erlang23"]="$ERLANG_PORT" 
    ["dy-erlang26"]="$ERLANG_PORT" 
    ["dy-erlang27"]="$ERLANG_PORT" 
    ["dy-erlindex23"]="$ERLANG_PORT" 
    ["dy-erlindex26"]="$ERLANG_PORT" 
    ["dy-erlindex27"]="$ERLANG_PORT" 
)
declare -A static_images=( 
    ["st-apache-deb"]="$DEFAULT_PORT" 
    ["st-cowboy-play"]="$ERLANG_PORT" 
    ["st-nginx-deb"]="$DEFAULT_PORT" 
    ["st-yaws-deb"]="$DEFAULT_PORT" 
    ["st-yaws-latest-deb"]="$DEFAULT_PORT" 
    ["st-erlang23"]="$ERLANG_PORT" 
    ["st-erlang26"]="$ERLANG_PORT" 
    ["st-erlang27"]="$ERLANG_PORT" 
    ["st-erlindex23"]="$ERLANG_PORT" 
    ["st-erlindex26"]="$ERLANG_PORT" 
    ["st-erlindex27"]="$ERLANG_PORT" 
)
declare -A local_servers=( 
    ["nginx"]="" 
    ["yaws"]="" 
)

# Auto-discover images/servers not in the arrays
# Returns a list of discovered names not present in the override array
function discover_images() {
    local search_dir="$1"
    local -n arr_ref=$2
    local discovered=()
    for d in "$search_dir"/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            local name=$(basename "$d")
            if [[ -z "${arr_ref[$name]+x}" ]]; then
                discovered+=("$name")
            fi
        fi
    done
    echo "${discovered[@]}"
}

function discover_websocket_images() {
    local search_dir="./web-socket"
    local -n arr_ref=$1
    local discovered=()
    for d in "$search_dir"/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            local name=$(basename "$d")
            if [[ -z "${arr_ref[$name]+x}" ]]; then
                discovered+=("$name")
            fi
        fi
    done
    echo "${discovered[@]}"
}

# Optionally, discover local servers (e.g., scripts or configs in local/)
function discover_local_servers() {
    local search_dir="./local"
    local -n arr_ref=$1
    local discovered=()
    for f in "$search_dir"/*; do
        [ -f "$f" ] || continue
        local name=$(basename "$f")
        # Only add if not in array and is a known server script/config
        # Example: skip measure_local.py, setup scripts, etc.
        if [[ "$name" =~ ^[a-zA-Z0-9_-]+$ && -z "${arr_ref[$name]+x}" ]]; then
            discovered+=("$name")
        fi
    done
    echo "${discovered[@]}"
}

# Create a fixed parent directory for results
RESULTS_PARENT_DIR="results"
TIMESTAMP=$(date +"%Y-%m-%d_%H%M%S")
RESULTS_DIR="$RESULTS_PARENT_DIR/$TIMESTAMP"
mkdir -p "$RESULTS_DIR/static" "$RESULTS_DIR/dynamic" "$RESULTS_DIR/local" "$RESULTS_DIR/websocket" logs

# Log file setup
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

    # Burst mode with varying payloads
    for num_clients in 10 20 50; do
        for size_mb in 10 50 100; do
            echo "Running measure_websocket.py for $image in burst mode with $num_clients clients and $size_mb MB payload"
            "$PYTHON_PATH" ./web-socket/measure_websocket.py \
                --server_image "$image" \
                --num_clients "$num_clients" \
                --size_mb "$size_mb" \
                --interval_s 1 \
                --duration_s 60 \
                --output_csv "$RESULTS_DIR/websocket/${image}_burst.csv" \
                --measurement_type websocket || echo "Burst mode failed for $image"
        done
    done

    # Streaming mode with varying rates
    for rate_mb_s in 5 10 20; do
        for num_clients in 10 20 50; do
            echo "Running measure_websocket.py for $image in streaming mode with $num_clients clients and $rate_mb_s MB/s rate"
            "$PYTHON_PATH" ./web-socket/measure_websocket.py \
                --server_image "$image" \
                --num_clients "$num_clients" \
                --rate_mb_s "$rate_mb_s" \
                --duration_s 60 \
                --size_mb 0 \
                --interval_s 0 \
                --output_csv "$RESULTS_DIR/websocket/${image}_streaming.csv" \
                --measurement_type websocket || echo "Streaming mode failed for $image"
        done
    done
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
            --output_csv "$RESULTS_DIR/$category/$image.csv" \
            --measurement_type "$category"
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
            --output_csv "../$RESULTS_DIR/local/$server.csv" \
            --measurement_type local)
    done
}

# Main logic
if [[ $RUN_ALL -eq 1 ]]; then
    # WebSocket images: array + discovered
    websocket_all=("${!websocket_images[@]}" $(discover_websocket_images websocket_images))
    for image in "${websocket_all[@]}"; do
        run_websocket_tests "$image"
    done
    # Dynamic images: array + discovered
    dynamic_all=("${!dynamic_images[@]}" $(discover_images ./containers/dynamic dynamic_images))
    for image in "${dynamic_all[@]}"; do
        port_mapping="${dynamic_images[$image]:-$DEFAULT_PORT}"
        run_docker_tests "$image" "$port_mapping" "dynamic"
    done
    # Static images: array + discovered
    static_all=("${!static_images[@]}" $(discover_images ./containers/static static_images))
    for image in "${static_all[@]}"; do
        port_mapping="${static_images[$image]:-$DEFAULT_PORT}"
        # Use ERLANG_PORT for erlang/erlindex/index images if not overridden
        if [[ "$image" =~ ^(erlang|erlindex|index)[0-9]+$ ]] && [[ -z "${static_images[$image]+x}" ]]; then
            port_mapping="$ERLANG_PORT"
        fi
        run_docker_tests "$image" "$port_mapping" "static"
    done
    # Local servers: array + discovered
    local_all=("${!local_servers[@]}" $(discover_local_servers local_servers))
    for server in "${local_all[@]}"; do
        run_local_tests "$server"
    done
else
    # Run specific type or images
    case "$TARGET_TYPE" in
        websocket)
            if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                websocket_all=("${!websocket_images[@]}" $(discover_websocket_images websocket_images))
                for image in "${websocket_all[@]}"; do
                    run_websocket_tests "$image"
                done
            else
                for image in "${TARGET_IMAGES[@]}"; do
                    if [[ -v "websocket_images[$image]" ]]; then
                        run_websocket_tests "$image"
                    else
                        echo "Error: $image is not a valid WebSocket image"
                    fi
                done
            fi
            ;;
        dynamic)
            if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                dynamic_all=("${!dynamic_images[@]}" $(discover_images ./containers/dynamic dynamic_images))
                for image in "${dynamic_all[@]}"; do
                    port_mapping="${dynamic_images[$image]:-$DEFAULT_PORT}"
                    run_docker_tests "$image" "$port_mapping" "dynamic"
                done
            else
                for image in "${TARGET_IMAGES[@]}"; do
                    if [[ -v "dynamic_images[$image]" ]]; then
                        run_docker_tests "$image" "${dynamic_images[$image]}" "dynamic"
                    else
                        echo "Error: $image is not a valid dynamic image"
                    fi
                done
            fi
            ;;
        static)
            if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                static_all=("${!static_images[@]}" $(discover_images ./containers/static static_images))
                for image in "${static_all[@]}"; do
                    port_mapping="${static_images[$image]:-$DEFAULT_PORT}"
                    # Use ERLANG_PORT for erlang/erlindex/index images if not overridden
                    if [[ "$image" =~ ^(erlang|erlindex|index)[0-9]+$ ]] && [[ -z "${static_images[$image]+x}" ]]; then
                        port_mapping="$ERLANG_PORT"
                    fi
                    run_docker_tests "$image" "$port_mapping" "static"
                done
            else
                for image in "${TARGET_IMAGES[@]}"; do
                    if [[ -v "static_images[$image]" ]]; then
                        run_docker_tests "$image" "${static_images[$image]}" "static"
                    else
                        echo "Error: $image is not a valid static image"
                    fi
                done
            fi
            ;;
        local)
            if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                local_all=("${!local_servers[@]}" $(discover_local_servers local_servers))
                for server in "${local_all[@]}"; do
                    run_local_tests "$server"
                done
            else
                for server in "${TARGET_IMAGES[@]}"; do
                    if [[ -v "local_servers[$server]" ]]; then
                        run_local_tests "$server"
                    else
                        echo "Error: $server is not a valid local server"
                    fi
                done
            fi
            ;;
        *)
            echo "Error: Unknown type $TARGET_TYPE. Valid types are: websocket, dynamic, static, local."
            ;;
    esac
fi