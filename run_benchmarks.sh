#!/bin/bash
set -e

# Set high open file descriptor limit for benchmarking session
ulimit -n 100000

# Run measurement scripts for all Docker images and local servers based on args
PYTHON_PATH="$(pwd)/srv/bin/python3"  # Absolute path from root directory

# Check for help first, before any other processing
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
        echo "  --super-quick Run super quick benchmarks with single test per type"
        echo "  clean       Clean repository to fresh state"
        echo ""
        echo "Examples:"
        echo "  $0                    # Run all benchmarks"
        echo "  $0 static             # Run all static containers"
        echo "  $0 dynamic st-nginx   # Run specific container"
        echo "  $0 --quick static     # Quick static benchmarks"
        echo ""
        echo "Port Assignment:"
        echo "  - Fixed host port: ${HOST_PORT:-8001}"
        echo "  - Container port determined from Dockerfile EXPOSE directive"
        echo "  - Default container port: 80"
        exit 0
        ;;
    "concurrency-sweep")
        echo "Run concurrency sweep: test increasing client counts with fixed payload size."
        exit 0
        ;;
    "payload-sweep")
        echo "Run payload sweep: test increasing payload sizes with fixed client count."
        exit 0
        ;;
esac

# Fixed port for all containers (configurable via HOST_PORT env var)
HOST_PORT=${HOST_PORT:-8001}

# Full test parameters for HTTP benchmarks (measure_docker.py and measure_local.py)
full_http_requests=(100 1000 5000 8000 10000 15000 20000 30000 40000 50000 60000 70000 80000)

# Quick test parameters for HTTP benchmarks (was super quick)
quick_http_requests=(1000)

# Full test parameters for WebSocket benchmarks (balanced set)
full_ws_burst_clients=(5 50 100)
full_ws_burst_sizes=(8 1024 65536)
full_ws_burst_bursts=(3)
full_ws_burst_intervals=(0.5)
full_ws_stream_clients=(5 50 100)
full_ws_stream_sizes=(8 1024 65536)
full_ws_stream_rates=(10)
full_ws_stream_durations=(5)

# Quick test parameters for WebSocket benchmarks (was super quick)
quick_ws_burst_clients=(5)
quick_ws_burst_sizes=(8)
quick_ws_burst_bursts=(1)
quick_ws_burst_intervals=(0.5)
quick_ws_stream_clients=(5)
quick_ws_stream_sizes=(8)
quick_ws_stream_rates=(1)
quick_ws_stream_durations=(1)
quick_concurrency_sweep_clients=(100)
quick_concurrency_sweep_size=8
quick_payload_sweep_clients=5
quick_payload_sweep_sizes=(8)

# Concurrency sweep parameters (balanced)
concurrency_sweep_clients=(100 1000 5000)
concurrency_sweep_size=8

# Payload sweep parameters (balanced)
payload_sweep_clients=5
payload_sweep_sizes=(8 1024 65536)

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_status() {
    local status=$1
    local message=$2
    case $status in
        "INFO") echo -e "${BLUE}[INFO]${NC} $message" ;;
        "SUCCESS") echo -e "${GREEN}[SUCCESS]${NC} $message" ;;
        "WARNING") echo -e "${YELLOW}[WARNING]${NC} $message" ;;
        "ERROR") echo -e "${RED}[ERROR]${NC} $message" ;;
    esac
}

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
SUPER_QUICK_BENCH=0
args=()
for arg in "$@"; do
    if [[ "$arg" == "--quick" ]]; then
        QUICK_BENCH=1
    elif [[ "$arg" == "--super-quick" ]]; then
        SUPER_QUICK_BENCH=1
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

check_port_free() {
    local port=$1
    for i in {1..10}; do
        if ! lsof -i :$port >/dev/null 2>&1; then
            return 0
        fi
        echo "[INFO] Port $port is busy, waiting... ($i/10)"
        sleep 1
    done
    return 1
}

run_websocket_tests() {
    local image=$1
    local host_port=$2
    if [ ! -f "./web-socket/measure_websocket.py" ]; then
        echo "Error: ./web-socket/measure_websocket.py not found"
        return 1
    fi
    if [[ $SUPER_QUICK_BENCH -eq 1 ]]; then
        echo -e "${BLUE}--- WebSocket Burst Test (Super Quick) ---${NC}"
        $PYTHON_PATH ./web-socket/measure_websocket.py \
            --server_image "$image" \
            --pattern burst \
            --mode echo \
            --clients ${quick_ws_burst_clients[0]} \
            --size_kb ${quick_ws_burst_sizes[0]} \
            --bursts ${quick_ws_burst_bursts[0]} \
            --interval ${quick_ws_burst_intervals[0]} \
            --output_csv "$RESULTS_DIR/websocket/${image}.csv" \
            --measurement_type "burst_${quick_ws_burst_clients[0]}_${quick_ws_burst_sizes[0]}_${quick_ws_burst_bursts[0]}_${quick_ws_burst_intervals[0]}"
        print_csv_summary "$RESULTS_DIR/websocket/${image}.csv"
        echo -e "${BLUE}--- WebSocket Stream Test (Super Quick) ---${NC}"
        $PYTHON_PATH ./web-socket/measure_websocket.py \
            --server_image "$image" \
            --pattern stream \
            --mode echo \
            --clients ${quick_ws_stream_clients[0]} \
            --size_kb ${quick_ws_stream_sizes[0]} \
            --rate ${quick_ws_stream_rates[0]} \
            --duration ${quick_ws_stream_durations[0]} \
            --output_csv "$RESULTS_DIR/websocket/${image}.csv" \
            --measurement_type "stream_${quick_ws_stream_clients[0]}_${quick_ws_stream_sizes[0]}_${quick_ws_stream_rates[0]}_${quick_ws_stream_durations[0]}"
        print_csv_summary "$RESULTS_DIR/websocket/${image}.csv"
    else
        burst_clients=("${full_ws_burst_clients[@]}")
        burst_sizes=("${full_ws_burst_sizes[@]}")
        burst_bursts=("${full_ws_burst_bursts[@]}")
        burst_intervals=("${full_ws_burst_intervals[@]}")
        stream_clients=("${full_ws_stream_clients[@]}")
        stream_sizes=("${full_ws_stream_sizes[@]}")
        stream_rates=("${full_ws_stream_rates[@]}")
        stream_durations=("${full_ws_stream_durations[@]}")
        echo "Running WebSocket tests for $image on port $host_port"
        local port_mapping=$(get_container_port_mapping $image $host_port)
        local container_port=$(echo $port_mapping | cut -d: -f2)
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
                            --output_csv "$RESULTS_DIR/websocket/${image}.csv" \
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
                            --output_csv "$RESULTS_DIR/websocket/${image}.csv" \
                            --measurement_type "stream_${clients}_${size_kb}_${rate}_${duration}"
                    done
                done
            done
        done
    fi
}

# Helper to print a short summary from the last line of a CSV file
print_csv_summary() {
    local csv_file="$1"
    [ -f "$csv_file" ] || return
    local header last_row
    header=$(head -1 "$csv_file")
    last_row=$(tail -1 "$csv_file")
    IFS=',' read -r -a cols <<EOF
$header
EOF
    IFS=',' read -r -a vals <<EOF
$last_row
EOF
    total_idx=-1; fail_idx=-1; latency_idx=-1; throughput_idx=-1
    for i in $(seq 0 $((${#cols[@]} - 1))); do
        col="${cols[$i]}"
        case "$col" in
            Total\ Requests|Total\ Messages) total_idx=$i ;;
            Failed\ Requests|Failed\ Messages) fail_idx=$i ;;
            Avg\ Latency*) latency_idx=$i ;;
            Throughput*) throughput_idx=$i ;;
        esac
    done
    total="-"; fail="-"; latency="-"; throughput="-"
    [ $total_idx -ge 0 ] && total="${vals[$total_idx]}"
    [ $fail_idx -ge 0 ] && fail="${vals[$fail_idx]}"
    [ $latency_idx -ge 0 ] && latency="${vals[$latency_idx]}"
    [ $throughput_idx -ge 0 ] && throughput="${vals[$throughput_idx]}"
    if [ "$total" = "$fail" ] && [ -n "$total" ]; then
        echo "  -> WARNING: All failed ($fail/$total)"
    else
        echo "  -> Success: $((total-fail))/$total, Avg Latency: $latency ms, Throughput: $throughput MB/s"
    fi
}

run_concurrency_sweep() {
    local image=$1
    local host_port=$2
    if [[ $SUPER_QUICK_BENCH -eq 1 ]]; then
        echo -e "${BLUE}--- WebSocket Concurrency Sweep (Super Quick) ---${NC}"
        local csv_file="$RESULTS_DIR/websocket/${image}_concurrency_sweep.csv"
        $PYTHON_PATH ./web-socket/measure_websocket.py \
            --server_image "$image" \
            --pattern burst \
            --mode echo \
            --clients ${quick_concurrency_sweep_clients[0]} \
            --size_kb $quick_concurrency_sweep_size \
            --bursts 1 \
            --interval 0.5 \
            --output_csv "$csv_file" \
            --measurement_type "concurrency_${quick_concurrency_sweep_clients[0]}_${quick_concurrency_sweep_size}"
        print_csv_summary "$csv_file"
        echo -e "${BLUE}Concurrency sweep completed for $image at $(date)${NC}"
        echo "Results saved to: $csv_file"
    else
        echo -e "${BLUE}\n=== WebSocket Concurrency Sweep: $image ===${NC}"
        local port_mapping=$(get_container_port_mapping $image $host_port)
        local ws_url="ws://localhost:$host_port/ws"
        local ntests=${#concurrency_sweep_clients[@]}
        local idx=1
        for clients in "${concurrency_sweep_clients[@]}"; do
            local csv_file="$RESULTS_DIR/websocket/${image}_concurrency_sweep.csv"
            echo "[$idx/$ntests] Concurrency: $clients clients, ${concurrency_sweep_size}KB payload"
            $PYTHON_PATH ./web-socket/measure_websocket.py \
                --server_image "$image" \
                --pattern burst \
                --mode echo \
                --clients $clients \
                --size_kb $concurrency_sweep_size \
                --bursts 3 \
                --interval 0.5 \
                --output_csv "$csv_file" \
                --measurement_type "concurrency_${clients}_${concurrency_sweep_size}"
            print_csv_summary "$csv_file"
            idx=$((idx+1))
        done
        echo -e "${BLUE}Concurrency sweep completed for $image at $(date)${NC}"
        echo "Results saved to: $RESULTS_DIR/websocket/${image}_concurrency_sweep.csv"
    fi
}

run_payload_sweep() {
    local image=$1
    local host_port=$2
    if [[ $SUPER_QUICK_BENCH -eq 1 ]]; then
        echo -e "${BLUE}--- WebSocket Payload Sweep (Super Quick) ---${NC}"
        local csv_file="$RESULTS_DIR/websocket/${image}_payload_sweep.csv"
        $PYTHON_PATH ./web-socket/measure_websocket.py \
            --server_image "$image" \
            --pattern burst \
            --mode echo \
            --clients $quick_payload_sweep_clients \
            --size_kb ${quick_payload_sweep_sizes[0]} \
            --bursts 1 \
            --interval 0.5 \
            --output_csv "$csv_file" \
            --measurement_type "payload_${quick_payload_sweep_clients}_${quick_payload_sweep_sizes[0]}"
        print_csv_summary "$csv_file"
        echo -e "${BLUE}Payload sweep completed for $image at $(date)${NC}"
        echo "Results saved to: $csv_file"
    else
        echo -e "${BLUE}\n=== WebSocket Payload Sweep: $image ===${NC}"
        local port_mapping=$(get_container_port_mapping $image $host_port)
        local ws_url="ws://localhost:$host_port/ws"
        local ntests=${#payload_sweep_sizes[@]}
        local idx=1
        for size_kb in "${payload_sweep_sizes[@]}"; do
            local csv_file="$RESULTS_DIR/websocket/${image}_payload_sweep.csv"
            echo "[$idx/$ntests] Payload: $payload_sweep_clients clients, ${size_kb}KB payload"
            $PYTHON_PATH ./web-socket/measure_websocket.py \
                --server_image "$image" \
                --pattern burst \
                --mode echo \
                --clients $payload_sweep_clients \
                --size_kb $size_kb \
                --bursts 3 \
                --interval 0.5 \
                --output_csv "$csv_file" \
                --measurement_type "payload_${payload_sweep_clients}_${size_kb}"
            print_csv_summary "$csv_file"
            idx=$((idx+1))
        done
        echo -e "${BLUE}Payload sweep completed for $image at $(date)${NC}"
        echo "Results saved to: $RESULTS_DIR/websocket/${image}_payload_sweep.csv"
    fi
}

# For static, dynamic, and local runs, add test numbering and summary
run_docker_tests() {
    local image=$1
    local host_port=$2
    local test_type=$3
    echo -e "${BLUE}Running $test_type tests for $image on port $host_port${NC}"
    local port_mapping=$(get_container_port_mapping $image $host_port)
    local ntests=0
    local -a test_counts
    if [[ $SUPER_QUICK_BENCH -eq 1 ]]; then
        test_counts=("${quick_http_requests[@]}")
    elif [[ $QUICK_BENCH -eq 1 ]]; then
        test_counts=("${quick_http_requests[@]}")
    else
        test_counts=("${full_http_requests[@]}")
    fi
    ntests=${#test_counts[@]}
    local idx=1
    for num_requests in "${test_counts[@]}"; do
        local csv_file="$RESULTS_DIR/$test_type/${image}.csv"
        echo "[$idx/$ntests] $test_type: $num_requests requests"
        $PYTHON_PATH ./containers/measure_docker.py \
            --server_image "$image" \
            --port_mapping "$port_mapping" \
            --num_requests "$num_requests" \
            --output_csv "$csv_file"
        print_csv_summary "$csv_file"
        idx=$((idx+1))
    done
}

run_local_tests() {
    local server=$1
    echo -e "${BLUE}Running local tests for $server${NC}"
    local ntests=0
    local -a test_counts
    if [[ $SUPER_QUICK_BENCH -eq 1 ]]; then
        test_counts=("${quick_http_requests[@]}")
    elif [[ $QUICK_BENCH -eq 1 ]]; then
        test_counts=("${quick_http_requests[@]}")
    else
        test_counts=("${full_http_requests[@]}")
    fi
    ntests=${#test_counts[@]}
    local idx=1
    for num_requests in "${test_counts[@]}"; do
        local csv_file="$RESULTS_DIR/local/${server}.csv"
        echo "[$idx/$ntests] local: $num_requests requests"
        $PYTHON_PATH ./local/measure_local.py \
            --server "$server" \
            --num_requests "$num_requests" \
            --output_csv "$csv_file"
        print_csv_summary "$csv_file"
        idx=$((idx+1))
    done
}

# After all benchmarks are run, print a summary of containers with 100% failed requests
print_run_summary() {
    local failed_containers=()
    local results_dir="$RESULTS_DIR"
    for csv in "$results_dir"/static/*.csv "$results_dir"/dynamic/*.csv "$results_dir"/websocket/*.csv "$results_dir"/local/*.csv; do
        [ -f "$csv" ] || continue
        # Get the header and the last row (most recent run)
        header=$(head -1 "$csv")
        last_row=$(tail -1 "$csv")
        # Determine column indices
        IFS=',' read -r -a cols <<EOF
$header
EOF
        IFS=',' read -r -a vals <<EOF
$last_row
EOF
        total_idx=-1
        fail_idx=-1
        for i in $(seq 0 $((${#cols[@]} - 1))); do
            col="${cols[$i]}"
            case "$col" in
                Total\ Requests|Total\ Messages) total_idx=$i ;;
                Failed\ Requests|Failed\ Messages) fail_idx=$i ;;
            esac
        done
        if [[ $total_idx -ge 0 ]] && [[ $fail_idx -ge 0 ]]; then
            total="${vals[$total_idx]}"
            fail="${vals[$fail_idx]}"
            if [ "$total" = "$fail" ] && [ -n "$total" ]; then
                container_name="${vals[0]}"
                failed_containers+=("$container_name ($csv)")
            fi
        fi
    done
    if [ ${#failed_containers[@]} -eq 0 ]; then
        echo -e "\n[RUN SUMMARY] All containers ran successfully (no 100% failed requests)."
    else
        echo -e "\n[RUN SUMMARY] The following containers had 100% failed requests:"
        for c in "${failed_containers[@]}"; do
            echo "  - $c"
        done
    fi
}

main() {
    echo "Starting benchmarks at $(date)"
    echo "Results will be saved to: $RESULTS_DIR"
    echo ""
    if [[ $RUN_ALL -eq 1 ]]; then
        echo "Running all benchmarks..."
        echo -e "${BLUE}=== Static Container Tests ===${NC}"
        local static_containers=($(discover_containers "static"))
        for container in "${static_containers[@]}"; do
            if ! check_port_free $HOST_PORT; then
                echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                exit 1
            fi
            # Before starting each container:
            if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                docker stop "$container" > /dev/null 2>&1 || true
                docker rm "$container" > /dev/null 2>&1 || true
                sleep 1
            fi
            run_docker_tests "$container" "$HOST_PORT" "static"
            sleep 1
        done
        echo -e "${BLUE}=== Dynamic Container Tests ===${NC}"
        local dynamic_containers=($(discover_containers "dynamic"))
        for container in "${dynamic_containers[@]}"; do
            if ! check_port_free $HOST_PORT; then
                echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                exit 1
            fi
            # Before starting each container:
            if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                docker stop "$container" > /dev/null 2>&1 || true
                docker rm "$container" > /dev/null 2>&1 || true
                sleep 1
            fi
            run_docker_tests "$container" "$HOST_PORT" "dynamic"
            sleep 1
        done
        echo -e "${BLUE}=== WebSocket Tests ===${NC}"
        local websocket_containers=($(discover_containers "websocket"))
        for container in "${websocket_containers[@]}"; do
            if ! check_port_free $HOST_PORT; then
                echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                exit 1
            fi
            # Before starting each container:
            if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                docker stop "$container" > /dev/null 2>&1 || true
                docker rm "$container" > /dev/null 2>&1 || true
                sleep 1
            fi
            run_websocket_tests "$container" "$HOST_PORT"
            sleep 1
        done
        # Also run sweeps for all websocket servers
        for container in "${websocket_containers[@]}"; do
            if ! check_port_free $HOST_PORT; then
                echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                exit 1
            fi
            # Before starting each container:
            if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                docker stop "$container" > /dev/null 2>&1 || true
                docker rm "$container" > /dev/null 2>&1 || true
                sleep 1
            fi
            run_concurrency_sweep "$container" "$HOST_PORT"
            run_payload_sweep "$container" "$HOST_PORT"
            sleep 1
        done
        echo -e "${BLUE}=== Local Server Tests ===${NC}"
        if ! check_port_free $HOST_PORT; then
            echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
        else
            # Before starting each local server:
            if docker ps -a --format '{{.Names}}' | grep -q "^nginx$"; then
                echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: nginx"
                docker stop "nginx" > /dev/null 2>&1 || true
                docker rm "nginx" > /dev/null 2>&1 || true
                sleep 1
            fi
            run_local_tests "nginx"
            # Before starting each local server:
            if docker ps -a --format '{{.Names}}' | grep -q "^yaws$"; then
                echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: yaws"
                docker stop "yaws" > /dev/null 2>&1 || true
                docker rm "yaws" > /dev/null 2>&1 || true
                sleep 1
            fi
            run_local_tests "yaws"
        fi
    else
        case $TARGET_TYPE in
            "static")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "static"))
                fi
                for container in "${TARGET_IMAGES[@]}"; do
                    if ! check_port_free $HOST_PORT; then
                        echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                        exit 1
                    fi
                    # Before starting each container:
                    if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                        echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                        docker stop "$container" > /dev/null 2>&1 || true
                        docker rm "$container" > /dev/null 2>&1 || true
                        sleep 1
                    fi
                    run_docker_tests "$container" "$HOST_PORT" "static"
                    sleep 1
                done
                ;;
            "dynamic")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "dynamic"))
                fi
                for container in "${TARGET_IMAGES[@]}"; do
                    if ! check_port_free $HOST_PORT; then
                        echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                        exit 1
                    fi
                    # Before starting each container:
                    if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                        echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                        docker stop "$container" > /dev/null 2>&1 || true
                        docker rm "$container" > /dev/null 2>&1 || true
                        sleep 1
                    fi
                    run_docker_tests "$container" "$HOST_PORT" "dynamic"
                    sleep 1
                done
                ;;
            "websocket")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "websocket"))
                fi
                for container in "${TARGET_IMAGES[@]}"; do
                    if ! check_port_free $HOST_PORT; then
                        echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                        exit 1
                    fi
                    # Before starting each container:
                    if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                        echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                        docker stop "$container" > /dev/null 2>&1 || true
                        docker rm "$container" > /dev/null 2>&1 || true
                        sleep 1
                    fi
                    run_websocket_tests "$container" "$HOST_PORT"
                    sleep 1
                done
                ;;
            "local")
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=("nginx" "yaws")
                fi
                for server in "${TARGET_IMAGES[@]}"; do
                    if ! check_port_free $HOST_PORT; then
                        echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                    else
                        # Before starting each local server:
                        if docker ps -a --format '{{.Names}}' | grep -q "^$server$"; then
                            echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $server"
                            docker stop "$server" > /dev/null 2>&1 || true
                            docker rm "$server" > /dev/null 2>&1 || true
                            sleep 1
                        fi
                        run_local_tests "$server"
                    fi
                done
                ;;
            "concurrency-sweep")
                TARGET_TYPE="websocket"
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "websocket"))
                fi
                for container in "${TARGET_IMAGES[@]}"; do
                    if ! check_port_free $HOST_PORT; then
                        echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                        exit 1
                    fi
                    # Before starting each container:
                    if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                        echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                        docker stop "$container" > /dev/null 2>&1 || true
                        docker rm "$container" > /dev/null 2>&1 || true
                        sleep 1
                    fi
                    run_concurrency_sweep "$container" "$HOST_PORT"
                    sleep 1
                done
                echo ""
                echo -e "${BLUE}Concurrency sweep completed at $(date)${NC}"
                echo "Results saved to: $RESULTS_DIR"
                exit 0
                ;;
            "payload-sweep")
                TARGET_TYPE="websocket"
                if [ ${#TARGET_IMAGES[@]} -eq 0 ]; then
                    TARGET_IMAGES=($(discover_containers "websocket"))
                fi
                for container in "${TARGET_IMAGES[@]}"; do
                    if ! check_port_free $HOST_PORT; then
                        echo -e "${RED}[ERROR]${NC} Port $HOST_PORT is already in use. Please free the port and rerun the benchmark."
                        exit 1
                    fi
                    # Before starting each container:
                    if docker ps -a --format '{{.Names}}' | grep -q "^$container$"; then
                        echo -e "${BLUE}[INFO]${NC} Stopping and removing dangling container: $container"
                        docker stop "$container" > /dev/null 2>&1 || true
                        docker rm "$container" > /dev/null 2>&1 || true
                        sleep 1
                    fi
                    run_payload_sweep "$container" "$HOST_PORT"
                    sleep 1
                done
                echo ""
                echo -e "${BLUE}Payload sweep completed at $(date)${NC}"
                echo "Results saved to: $RESULTS_DIR"
                exit 0
                ;;
            *)
                echo "Unknown target type: $TARGET_TYPE"
                echo "Valid types: static, dynamic, websocket, local"
                exit 1
                ;;
        esac
    fi
    echo ""
    echo -e "${BLUE}Benchmarks completed at $(date)${NC}"
    echo "Results saved to: $RESULTS_DIR"
}

main "$@"
print_run_summary