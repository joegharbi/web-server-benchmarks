#!/bin/bash

# Run measurement scripts for all Docker images and local servers based on args
PYTHON_PATH="$(pwd)/srv/bin/python3"  # Absolute path from root directory

# Simple port assignment - start from 8001 and increment for each container
START_PORT=8001

# Define payloads for measure_docker.py and measure_local.py
payloads=(100 1000 5000 8000 10000 15000 20000 30000 40000 50000 60000 70000 80000)

# Function to get container port mapping based on Dockerfile EXPOSE directive
get_container_port_mapping() {
    local image_name=$1
    local host_port=$2
    local container_dir=""
    if [[ $image_name == ws-* ]]; then
        container_dir="./web-socket/${image_name}"
    elif [[ $image_name == dy-* ]]; then
        container_dir="./containers/dynamic/${image_name}"
    elif [[ $image_name == st-* ]]; then
        container_dir="./containers/static/${image_name}"
    fi
    local container_port="80"
    if [ -f "${container_dir}/Dockerfile" ]; then
        local exposed_port=$(grep -i "^EXPOSE" "${container_dir}/Dockerfile" | head -1 | awk '{print $2}')
        if [ -n "$exposed_port" ]; then
            container_port="$exposed_port"
        fi
    fi
    echo "${host_port}:${container_port}"
}

# Auto-discover all containers in the framework
function discover_containers() {
    local container_type=$1
    local discovered=()
    case $container_type in
        "static")
            for d in ./containers/static/*/; do
                [ -d "$d" ] || continue
                if [ -f "$d/Dockerfile" ]; then
                    discovered+=("$(basename "$d")")
                fi
            done
            ;;
        "dynamic")
            for d in ./containers/dynamic/*/; do
                [ -d "$d" ] || continue
                if [ -f "$d/Dockerfile" ]; then
                    discovered+=("$(basename "$d")")
                fi
            done
            ;;
        "websocket")
            for d in ./web-socket/*/; do
                [ -d "$d" ] || continue
                if [ -f "$d/Dockerfile" ]; then
                    discovered+=("$(basename "$d")")
                fi
            done
            ;;
    esac
    echo "${discovered[@]}"
}

clean_repo() {
  echo "Cleaning repository to bare minimum (fresh clone state)..."
  git clean -xfd
  git reset --hard
  echo "Repository is now clean."
}

RESULTS_PARENT_DIR="results"
TIMESTAMP=$(date +"%Y-%m-%d_%H%M%S")
RESULTS_DIR="$RESULTS_PARENT_DIR/$TIMESTAMP"
mkdir -p "$RESULTS_DIR/static" "$RESULTS_DIR/dynamic" "$RESULTS_DIR/local" "$RESULTS_DIR/websocket" logs

LOG_FILE="logs/run_${TIMESTAMP}.log"
echo "Logging to $LOG_FILE"
exec > >(tee -a "$LOG_FILE") 2>&1

QUICK_BENCH=0
args=()
for arg in "$@"; do
    if [[ "$arg" == "--quick" ]]; then
        QUICK_BENCH=1
    else
        args+=("$arg")
    fi
done
set -- "${args[@]}"

RUN_ALL=1
TARGET_TYPE=""
TARGET_IMAGES=()

if [[ $# -gt 0 ]]; then
    # Check for special commands first
    if [[ "$1" == "clean" ]]; then
        clean_repo
        exit 0
    fi
    RUN_ALL=0
    TARGET_TYPE="$1"
    shift
    TARGET_IMAGES=("$@")
fi

run_websocket_tests() {
    local image=$1
    local host_port=$2
    if [ ! -f "./web-socket/measure_websocket.py" ]; then
        echo "Error: ./web-socket/measure_websocket.py not found"
        return 1
    fi
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
        stream_sizes=(1 8 64 256 1024 8192 65536)
        stream_rates=(10 100 1000)
        stream_durations=(5 10 30)
    fi
    echo "Running WebSocket tests for $image on port $host_port"
    local port_mapping=$(get_container_port_mapping $image $host_port)
    local container_port=$(echo $port_mapping | cut -d: -f2)
    local container_name="bench-${image}"
    docker run -d --rm --name "$container_name" -p "$port_mapping" "$image" > /dev/null 2>&1
    sleep 10
    local ws_url="ws://localhost:$host_port/ws"
    for clients in "${burst_clients[@]}"; do
        for size_kb in "${burst_sizes[@]}"; do
            for bursts in "${burst_bursts[@]}"; do
                for interval in "${burst_intervals[@]}"; do
                    echo "  Burst test: $clients clients, ${size_kb}KB, $bursts bursts, ${interval}s interval"
                    $PYTHON_PATH ./web-socket/measure_websocket.py \
                        --server_image "$image" \
                        --pattern burst \
                        --mode echo \
                        --clients $clients \
                        --size_kb $size_kb \
                        --bursts $bursts \
                        --interval $interval \
                        --output_csv "$RESULTS_DIR/websocket/${image}_burst_${clients}_${size_kb}_${bursts}_${interval}.csv" \
                        --measurement_type "burst_${clients}_${size_kb}_${bursts}_${interval}"
                done
            done
        done
    done
    for clients in "${stream_clients[@]}"; do
        for size_kb in "${stream_sizes[@]}"; do
            for rate in "${stream_rates[@]}"; do
                for duration in "${stream_durations[@]}"; do
                    echo "  Stream test: $clients clients, ${size_kb}KB, ${rate}/s, ${duration}s"
                    $PYTHON_PATH ./web-socket/measure_websocket.py \
                        --server_image "$image" \
                        --pattern stream \
                        --mode echo \
                        --clients $clients \
                        --size_kb $size_kb \
                        --rate $rate \
                        --duration $duration \
                        --output_csv "$RESULTS_DIR/websocket/${image}_stream_${clients}_${size_kb}_${rate}_${duration}.csv" \
                        --measurement_type "stream_${clients}_${size_kb}_${rate}_${duration}"
                done
            done
        done
    done
    docker stop "$container_name" > /dev/null 2>&1 || true
}

run_docker_tests() {
    local image=$1
    local host_port=$2
    local test_type=$3
    echo "Running $test_type tests for $image on port $host_port"
    local port_mapping=$(get_container_port_mapping $image $host_port)
    local container_name="bench-${image}"
    docker run -d --rm --name "$container_name" -p "$port_mapping" "$image" > /dev/null 2>&1
    sleep 10
    $PYTHON_PATH ./containers/measure_docker.py \
        --image "$image" \
        --port "$host_port" \
        --payloads "${payloads[*]}" \
        --output "$RESULTS_DIR/$test_type/${image}.csv"
    docker stop "$container_name" > /dev/null 2>&1 || true
}

run_local_tests() {
    local server=$1
    echo "Running local tests for $server"
    $PYTHON_PATH ./local/measure_local.py \
        --server "$server" \
        --payloads "${payloads[*]}" \
        --output "$RESULTS_DIR/local/${server}.csv"
}

main() {
    echo "Starting benchmarks at $(date)"
    echo "Results will be saved to: $RESULTS_DIR"
    echo ""
    if [[ $RUN_ALL -eq 1 ]]; then
        echo "Running all benchmarks..."
        echo "=== Static Container Tests ==="
        local static_containers=($(discover_containers "static"))
        local current_port=$START_PORT
        for container in "${static_containers[@]}"; do
            run_docker_tests "$container" "$current_port" "static"
            current_port=$((current_port + 1))
        done
        echo "=== Dynamic Container Tests ==="
        local dynamic_containers=($(discover_containers "dynamic"))
        for container in "${dynamic_containers[@]}"; do
            run_docker_tests "$container" "$current_port" "dynamic"
            current_port=$((current_port + 1))
        done
        echo "=== WebSocket Tests ==="
        local websocket_containers=($(discover_containers "websocket"))
        for container in "${websocket_containers[@]}"; do
            run_websocket_tests "$container" "$current_port"
            current_port=$((current_port + 1))
        done
        echo "=== Local Server Tests ==="
        run_local_tests "nginx"
        run_local_tests "yaws"
    else
        case $TARGET_TYPE in
            "static")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "static"))
                fi
                local current_port=$START_PORT
                for container in "${TARGET_IMAGES[@]}"; do
                    run_docker_tests "$container" "$current_port" "static"
                    current_port=$((current_port + 1))
                done
                ;;
            "dynamic")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "dynamic"))
                fi
                local current_port=$START_PORT
                for container in "${TARGET_IMAGES[@]}"; do
                    run_docker_tests "$container" "$current_port" "dynamic"
                    current_port=$((current_port + 1))
                done
                ;;
            "websocket")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "websocket"))
                fi
                local current_port=$START_PORT
                for container in "${TARGET_IMAGES[@]}"; do
                    run_websocket_tests "$container" "$current_port"
                    current_port=$((current_port + 1))
                done
                ;;
            "local")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=("nginx" "yaws")
                fi
                for server in "${TARGET_IMAGES[@]}"; do
                    run_local_tests "$server"
                done
                ;;
            *)
                echo "Unknown target type: $TARGET_TYPE"
                echo "Valid types: static, dynamic, websocket, local"
                exit 1
                ;;
        esac
    fi
    echo ""
    echo "Benchmarks completed at $(date)"
    echo "Results saved to: $RESULTS_DIR"
}

case "${1:-}" in
    "help"|"--help"|"-h")
        echo "Usage: $0 [TYPE] [IMAGES...] [OPTIONS]"
        echo ""
        echo "Types:"
        echo "  static      Run static container benchmarks"
        echo "  dynamic     Run dynamic container benchmarks"
        echo "  websocket   Run WebSocket benchmarks"
        echo "  local       Run local server benchmarks"
        echo ""
        echo "Options:"
        echo "  --quick     Run quick benchmarks with reduced parameters"
        echo "  clean       Clean repository to fresh state"
        echo ""
        echo "Examples:"
        echo "  $0                    # Run all benchmarks"
        echo "  $0 static             # Run all static containers"
        echo "  $0 dynamic st-nginx   # Run specific container"
        echo "  $0 --quick static     # Quick static benchmarks"
        echo ""
        echo "Port Assignment:"
        echo "  - Automatic sequential assignment starting from port $START_PORT"
        echo "  - Container port determined from Dockerfile EXPOSE directive"
        echo "  - Default container port: 80"
        exit 0
        ;;
esac

if [[ "${1:-}" == "help" ]] || [[ "${1:-}" == "--help" ]] || [[ "${1:-}" == "-h" ]]; then
    exit 0
fi

main "$@"