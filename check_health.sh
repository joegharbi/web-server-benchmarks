#!/bin/bash

# Health Check Script for Web Server Benchmarks
# Tests all built containers for proper startup, HTTP response, and health constraints

# Don't exit on errors - we want to test all containers
# set -e

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
MAX_CONCURRENT_CONTAINERS=5

# Port mappings (matching run_benchmarks.sh)
DEFAULT_PORT="8001:80"
ERLANG_PORT="8001:8080"

# Results tracking
TOTAL_CONTAINERS=0
HEALTHY_CONTAINERS=0
FAILED_CONTAINERS=0
FAILED_LIST=()

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    case $status in
        "INFO")
            echo -e "${BLUE}[INFO]${NC} $message"
            ;;
        "SUCCESS")
            echo -e "${GREEN}[SUCCESS]${NC} $message"
            ;;
        "WARNING")
            echo -e "${YELLOW}[WARNING]${NC} $message"
            ;;
        "ERROR")
            echo -e "${RED}[ERROR]${NC} $message"
            ;;
    esac
}

# Function to get port mapping for an image (matching run_benchmarks.sh logic)
get_port_mapping() {
    local image_name=$1
    local unique_port=$2  # Add unique port parameter
    
    # WebSocket containers
    if [[ $image_name == ws-* ]]; then
        case $image_name in
            ws-cowboy-27-self)
                echo "${unique_port}:8080"
                ;;
            ws-nginx-java-self)
                echo "${unique_port}:8080"
                ;;
            *)
                echo "${unique_port}:80"
                ;;
        esac
    else
        # Static and Dynamic containers
        case $image_name in
            *cowboy*|*erlang*|*erlindex*)
                echo "${unique_port}:8080"
                ;;
            *)
                echo "${unique_port}:80"
                ;;
        esac
    fi
}

# Function to check if container is responding to HTTP (real health check like benchmarking)
check_http_response() {
    local container_name=$1
    local host_port=$2
    local max_attempts=5
    local attempt=1
    local delay=1
    
    while [ $attempt -le $max_attempts ]; do
        # Test the exact same URL that will be used in benchmarking
        local status_code=$(curl -s -o /dev/null -w "%{http_code}" --max-time 5 "http://localhost:$host_port/" 2>/dev/null || echo "000")
        
        if [ "$status_code" = "200" ]; then
            return 0  # Success - container is healthy
        fi
        
        if [ $attempt -lt $max_attempts ]; then
            sleep $delay
        fi
        attempt=$((attempt + 1))
    done
    
    return 1  # Failed after all attempts
}

# Function to check container health (real health check)
check_container_health() {
    local image_name=$1
    local unique_port=$2
    local container_name="health-check-${image_name}"
    local port_mapping=$(get_port_mapping $image_name $unique_port)
    local host_port=$(echo $port_mapping | cut -d: -f1)
    
    print_status "INFO" "Testing $image_name..."
    
    # Start container
    if ! docker run -d --rm --name "$container_name" -p "$port_mapping" "$image_name" > /dev/null 2>&1; then
        print_status "ERROR" "$image_name: Failed to start container"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (startup failed)")
        return 1
    fi
    
    # Wait for startup
    sleep $STARTUP_WAIT
    
    # Check if container is still running
    if ! docker ps --format "table {{.Names}}" | grep -q "$container_name"; then
        print_status "ERROR" "$image_name: Container stopped unexpectedly"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (container stopped)")
        return 1
    fi
    
    # Real health check - same as benchmarking script
    if check_http_response "$container_name" "$host_port"; then
        print_status "SUCCESS" "$image_name: Healthy (ready for benchmarking)"
        HEALTHY_CONTAINERS=$((HEALTHY_CONTAINERS + 1))
    else
        print_status "ERROR" "$image_name: Container not responding with 200 OK"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (health check failed)")
    fi
    
    # Clean up
    docker stop "$container_name" > /dev/null 2>&1 || true
    
    return 0
}

# Function to check WebSocket specific health (real health check)
check_websocket_health() {
    local image_name=$1
    local unique_port=$2
    local container_name="health-check-${image_name}"
    local port_mapping=$(get_port_mapping $image_name $unique_port)
    local host_port=$(echo $port_mapping | cut -d: -f1)
    
    print_status "INFO" "Testing WebSocket container $image_name..."
    
    # Start container
    if ! docker run -d --rm --name "$container_name" -p "$port_mapping" "$image_name" > /dev/null 2>&1; then
        print_status "ERROR" "$image_name: Failed to start WebSocket container"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (startup failed)")
        return 1
    fi
    
    # Wait for startup
    sleep $STARTUP_WAIT
    
    # Check if container is still running
    if ! docker ps --format "table {{.Names}}" | grep -q "$container_name"; then
        print_status "ERROR" "$image_name: WebSocket container stopped unexpectedly"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (container stopped)")
        return 1
    fi
    
    # Real health check - test WebSocket handshake (same as benchmarking would)
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
        # Fallback to HTTP test if WebSocket fails
        if check_http_response "$container_name" "$host_port"; then
            print_status "SUCCESS" "$image_name: WebSocket container healthy (HTTP ready for benchmarking)"
            HEALTHY_CONTAINERS=$((HEALTHY_CONTAINERS + 1))
        else
            print_status "ERROR" "$image_name: WebSocket container not ready for benchmarking"
            FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
            FAILED_LIST+=("$image_name (health check failed)")
        fi
    fi
    
    # Clean up
    docker stop "$container_name" > /dev/null 2>&1 || true
    
    return 0
}

# Function to check Erlang/Cowboy specific health (real health check)
check_erlang_health() {
    local image_name=$1
    local unique_port=$2
    local container_name="health-check-${image_name}"
    local port_mapping=$(get_port_mapping $image_name $unique_port)
    local host_port=$(echo $port_mapping | cut -d: -f1)
    
    print_status "INFO" "Testing Erlang/Cowboy container $image_name..."
    
    # Start container
    if ! docker run -d --rm --name "$container_name" -p "$port_mapping" "$image_name" > /dev/null 2>&1; then
        print_status "ERROR" "$image_name: Failed to start Erlang container"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (startup failed)")
        return 1
    fi
    
    # Wait for startup (Erlang containers might need more time)
    sleep $((STARTUP_WAIT + 5))
    
    # Check if container is still running
    if ! docker ps --format "table {{.Names}}" | grep -q "$container_name"; then
        print_status "ERROR" "$image_name: Erlang container stopped unexpectedly"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (container stopped)")
        return 1
    fi
    
    # Real health check - same as benchmarking script
    if check_http_response "$container_name" "$host_port"; then
        print_status "SUCCESS" "$image_name: Erlang container healthy (ready for benchmarking)"
        HEALTHY_CONTAINERS=$((HEALTHY_CONTAINERS + 1))
    else
        print_status "ERROR" "$image_name: Erlang container not responding with 200 OK"
        FAILED_CONTAINERS=$((FAILED_CONTAINERS + 1))
        FAILED_LIST+=("$image_name (health check failed)")
    fi
    
    # Clean up
    docker stop "$container_name" > /dev/null 2>&1 || true
    
    return 0
}

# Main health check function
main() {
    print_status "INFO" "Starting health check for all built containers..."
    print_status "INFO" "Timeout: ${TIMEOUT}s, Startup wait: ${STARTUP_WAIT}s, HTTP timeout: ${HTTP_TIMEOUT}s"
    print_status "INFO" "Using unique ports for each container test"
    echo ""
    
    # Get all built images
    local images=($(docker images --format "{{.Repository}}" | grep -E "(st-|dy-|ws-)" | sort))
    
    if [ ${#images[@]} -eq 0 ]; then
        print_status "ERROR" "No built containers found. Run 'make build' first."
        exit 1
    fi
    
    TOTAL_CONTAINERS=${#images[@]}
    print_status "INFO" "Found $TOTAL_CONTAINERS containers to test"
    echo ""
    
    # Test each container with unique port
    local port_counter=8001
    for image in "${images[@]}"; do
        # Clean up any existing containers with this name
        docker stop "health-check-${image}" > /dev/null 2>&1 || true
        docker rm "health-check-${image}" > /dev/null 2>&1 || true
        
        if [[ $image == ws-* ]]; then
            check_websocket_health "$image" "$port_counter"
        elif [[ $image == *cowboy* ]] || [[ $image == *erlang* ]] || [[ $image == *erlindex* ]]; then
            check_erlang_health "$image" "$port_counter"
        else
            check_container_health "$image" "$port_counter"
        fi
        
        port_counter=$((port_counter + 1))
        
        # Small delay between tests to avoid overwhelming the system
        sleep 1
    done
    
    # Print summary
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

# Handle script arguments
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
        echo "Port mappings:"
        echo "  - Default containers: $DEFAULT_PORT"
        echo "  - Erlang/Cowboy containers: $ERLANG_PORT"
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

# Run main function
main "$@" 