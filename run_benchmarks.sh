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

# Benchmark matrix selection
QUICK_BENCH=0
# Remove --quick from arguments and set QUICK_BENCH if present
args=()
for arg in "$@"; do
    if [[ "$arg" == "--quick" ]]; then
        QUICK_BENCH=1
    else
        args+=("$arg")
    fi
done
set -- "${args[@]}"

# Parse arguments (must be after --quick removal)
RUN_ALL=1
TARGET_TYPE=""
TARGET_IMAGES=()

if [[ $# -gt 0 ]]; then
    RUN_ALL=0
    TARGET_TYPE="$1"
    shift
    TARGET_IMAGES=("$@")
fi

# =====================
# WebSocket Benchmark Parameter Explanations
# =====================
#
# Burst Mode Parameters:
#   --clients    : Number of concurrent WebSocket clients (connections) to the server.
#   --size_kb    : Size of each WebSocket message sent (in kilobytes).
#   --bursts     : Number of messages each client sends in a “burst” (as fast as possible, then waits).
#   --interval   : Time (in seconds) to wait between each burst of messages.
#
# Streaming Mode Parameters:
#   --clients    : Number of concurrent WebSocket clients (connections) to the server.
#   --size_kb    : Size of each WebSocket message sent (in kilobytes).
#   --rate       : Number of messages per second each client sends (streaming, not bursty).
#   --duration   : Total time (in seconds) to run the streaming test.
#
# Other Inputs:
#   --server_image     : The Docker image name of the WebSocket server to test.
#   --pattern          : The test pattern: 'burst' (bursty traffic) or 'stream' (steady rate).
#   --mode             : The WebSocket mode: usually 'echo' (server echoes back what it receives).
#   --output_csv       : Path to the CSV file where results will be saved.
#   --measurement_type : Label for the type of measurement (for your records).
#
# Example:
#   Burst:   1 client, 8 KB messages, 3 bursts, 0.5s between bursts
#   Stream:  1 client, 8 KB messages, 10 messages/sec, for 5 seconds
# =====================
# Function to run WebSocket tests (no port_mapping)
run_websocket_tests() {
    local image=$1
    if [ ! -f "./web-socket/measure_websocket.py" ]; then
        echo "Error: ./web-socket/measure_websocket.py not found"
        return 1
    fi

    # Select parameter arrays based on QUICK_BENCH
    if [[ $QUICK_BENCH -eq 1 ]]; then
        burst_clients=(3)
        burst_sizes=(8)
        burst_bursts=(3)
        burst_intervals=(0.5)
        stream_clients=(3)
        stream_sizes=(8)
        stream_rates=(10)
        stream_durations=(5)
    else
        burst_clients=(1 10 50 200)
        burst_sizes=(1 8 64 256 1024 8192 65536)
        burst_bursts=(10 50)
        burst_intervals=(0.1 0.5 1)
        stream_clients=(1 10 50 200)
        stream_sizes=(1 8 64 256 1024)
        stream_rates=(1 10 100 1000)
        stream_durations=(10 60)
    fi

    # Burst mode
    for num_clients in "${burst_clients[@]}"; do
        for size_kb in "${burst_sizes[@]}"; do
            for bursts in "${burst_bursts[@]}"; do
                for interval in "${burst_intervals[@]}"; do
                    echo "Running burst: clients=$num_clients, size_kb=$size_kb, bursts=$bursts, interval=$interval"
                    "$PYTHON_PATH" ./web-socket/measure_websocket.py \
                        --server_image "$image" \
                        --clients "$num_clients" \
                        --size_kb "$size_kb" \
                        --pattern burst \
                        --mode echo \
                        --bursts "$bursts" \
                        --interval "$interval" \
                        --output_csv "$RESULTS_DIR/websocket/${image}_burst.csv" \
                        --measurement_type websocket || echo "Burst mode failed for $image"
                done
            done
        done
    done

    # Streaming mode
    for num_clients in "${stream_clients[@]}"; do
        for size_kb in "${stream_sizes[@]}"; do
            for rate in "${stream_rates[@]}"; do
                for duration in "${stream_durations[@]}"; do
                    echo "Running stream: clients=$num_clients, size_kb=$size_kb, rate=$rate, duration=$duration"
                    "$PYTHON_PATH" ./web-socket/measure_websocket.py \
                        --server_image "$image" \
                        --clients "$num_clients" \
                        --size_kb "$size_kb" \
                        --pattern stream \
                        --mode echo \
                        --rate "$rate" \
                        --duration "$duration" \
                        --output_csv "$RESULTS_DIR/websocket/${image}_streaming.csv" \
                        --measurement_type websocket || echo "Streaming mode failed for $image"
                done
            done
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