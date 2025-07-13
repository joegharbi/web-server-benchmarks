#!/bin/bash

# Health Check Script for Web Server Benchmarks
# Tests all built containers for proper startup, HTTP response, and health constraints

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
TIMEOUT=30
STARTUP_WAIT=10
HTTP_TIMEOUT=10
HOST_PORT=${HOST_PORT:-8001}

# Results tracking
TOTAL_CONTAINERS=0
HEALTHY_CONTAINERS=0
FAILED_CONTAINERS=0
FAILED_LIST=()

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

# Get container port mapping from Dockerfile EXPOSE
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

check_http_response() {
    local container_name=$1
    local host_port=$2
    local max_attempts=5
    local attempt=1
    local delay=1
    while [ $attempt -le $max_attempts ]; do
        local status_code=$(curl -s -o /dev/null -w "%{http_code}" --max-time 5 "http://localhost:$host_port/" 2>/dev/null || echo "000")
        if [ "$status_code" = "200" ]; then
            return 0
        fi
        if [ $attempt -lt $max_attempts ]; then
            sleep $delay
        fi
        attempt=$((attempt + 1))
    done
    return 1
}

check_container_health() {
    local image_name=$1
    local host_port=$2
    local container_name="health-check-${image_name}"
    local port_mapping=$(get_container_port_mapping $image_name $host_port)
    print_status "INFO" "Testing $image_name..."
    if ! docker run -d --rm --name "$container_name" -p "$port_mapping" "$image_name" > /dev/null 2>&1; then
        print_status "ERROR" "$image_name: Failed to start container"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (startup failed)")
        return 1
    fi
    local wait_time=$STARTUP_WAIT
    if [[ $image_name == *cowboy* ]] || [[ $image_name == *erlang* ]] || [[ $image_name == *erlindex* ]]; then
        wait_time=$((STARTUP_WAIT + 5))
    fi
    sleep $wait_time
    if ! docker ps --format "table {{.Names}}" | grep -q "$container_name"; then
        print_status "ERROR" "$image_name: Container stopped unexpectedly"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (container stopped)")
        return 1
    fi
    if [[ $image_name == ws-* ]]; then
        local ws_test_result=$(curl -s -i --max-time 5 \
            -H "Connection: Upgrade" \
            -H "Upgrade: websocket" \
            -H "Sec-WebSocket-Version: 13" \
            -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
            "http://localhost:$host_port/ws" 2>/dev/null | head -10)
        if [[ "$ws_test_result" == *"101 Switching Protocols"* ]] || [[ "$ws_test_result" == *"Upgrade: websocket"* ]]; then
            print_status "SUCCESS" "$image_name: WebSocket container healthy (ready for benchmarking)"
            HEALTHY_CONTAINERS=$((HEALTHY_CONTAINERS + 1))
        else
            if check_http_response "$container_name" "$host_port"; then
                print_status "SUCCESS" "$image_name: WebSocket container healthy (HTTP ready for benchmarking)"
                HEALTHY_CONTAINERS=$((HEALTHY_CONTAINERS + 1))
            else
                print_status "ERROR" "$image_name: WebSocket container not ready for benchmarking"
                FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
                FAILED_LIST+=("$image_name (health check failed)")
            fi
        fi
    else
        if check_http_response "$container_name" "$host_port"; then
            print_status "SUCCESS" "$image_name: Healthy (ready for benchmarking)"
            HEALTHY_CONTAINERS=$((HEALTHY_CONTAINERS + 1))
        else
            print_status "ERROR" "$image_name: Container not responding with 200 OK"
            FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
            FAILED_LIST+=("$image_name (health check failed)")
        fi
    fi
    docker stop "$container_name" > /dev/null 2>&1 || true
    return 0
}

# Auto-discover all containers in the framework
function discover_containers() {
    local discovered=()
    
    # Discover static containers
    for d in ./containers/static/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            discovered+=("$(basename "$d")")
        fi
    done
    
    # Discover dynamic containers
    for d in ./containers/dynamic/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            discovered+=("$(basename "$d")")
        fi
    done
    
    # Discover websocket containers
    for d in ./web-socket/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            discovered+=("$(basename "$d")")
        fi
    done
    
    echo "${discovered[@]}"
}

main() {
    print_status "INFO" "Starting health check for all built containers..."
    print_status "INFO" "Timeout: ${TIMEOUT}s, Startup wait: ${STARTUP_WAIT}s, HTTP timeout: ${HTTP_TIMEOUT}s"
    print_status "INFO" "Using fixed host port: $HOST_PORT"
    echo ""
    
    # Discover all containers from directory structure
    local discovered_containers=($(discover_containers))
    if [ ${#discovered_containers[@]} -eq 0 ]; then
        print_status "ERROR" "No containers found in benchmark directories. Check containers/static/, containers/dynamic/, and web-socket/ directories."
        exit 1
    fi
    
    # Filter to only built images
    local built_images=()
    for container in "${discovered_containers[@]}"; do
        if docker images --format "{{.Repository}}" | grep -q "^${container}$"; then
            built_images+=("$container")
        fi
    done
    
    if [ ${#built_images[@]} -eq 0 ]; then
        print_status "ERROR" "No built containers found. Run 'make build' first."
        exit 1
    fi
    
    TOTAL_CONTAINERS=${#built_images[@]}
    print_status "INFO" "Found $TOTAL_CONTAINERS built containers to test"
    echo ""
    for image in "${built_images[@]}"; do
        docker stop "health-check-${image}" > /dev/null 2>&1 || true
        docker rm "health-check-${image}" > /dev/null 2>&1 || true
        check_container_health "$image" "$HOST_PORT"
        sleep 1
    done
    echo ""
    print_status "INFO" "=== HEALTH CHECK SUMMARY ==="
    print_status "INFO" "Total containers tested: $TOTAL_CONTAINERS"
    print_status "SUCCESS" "Healthy containers: $HEALTHY_CONTAINERS"
    if [ $FAILED_CONTAINERS -gt 0 ]; then
        print_status "ERROR" "Failed containers: $FAILED_CONTAINERS"
        echo ""
        print_status "ERROR" "Failed containers list:"
        for failed in "${FAILED_LIST[@]}"; do
            echo "  - $failed"
        done
        echo ""
        exit 1
    else
        print_status "SUCCESS" "All containers are healthy! 🎉"
        echo ""
    fi
}

case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [OPTIONS]"
        echo ""
        echo "Options:"
        echo "  --help, -h    Show this help message"
        echo "  --timeout N   Set timeout in seconds (default: 30)"
        echo "  --startup N   Set startup wait time in seconds (default: 10)"
        echo "  --http N      Set HTTP timeout in seconds (default: 10)"
        echo ""
        echo "This script tests all built containers for:"
        echo "  - Container startup"
        echo "  - Container stability"
        echo "  - HTTP response"
        echo "  - WebSocket response (for WebSocket containers)"
        echo ""
        echo "Port assignment:"
        echo "  - Fixed host port: $HOST_PORT"
        echo "  - Container port determined from Dockerfile EXPOSE directive"
        echo "  - Default container port: 80"
        exit 0
        ;;
    --timeout)
        TIMEOUT=$2
        shift 2
        ;;
    --startup)
        STARTUP_WAIT=$2
        shift 2
        ;;
    --http)
        HTTP_TIMEOUT=$2
        shift 2
        ;;
esac

main "$@" 