#!/bin/bash

# Install Docker images and run setup scripts for specified benchmark types

# Remove all containers and images related to the benchmark (safe)
function clean_benchmark_docker_images() {
    echo "Cleaning up benchmark Docker containers and images..."

    # Discover all image names from benchmark directories
    image_names=()
    for d in ./containers/static/*/ ./containers/dynamic/*/ ./web-socket/*/; do
        [ -d "$d" ] || continue
        name=$(basename "$d")
        image_names+=("$name")
    done

    # Remove containers using discovered image names
    for name in "${image_names[@]}"; do
        docker rm -f $(docker ps -aq --filter ancestor="$name") 2>/dev/null || true
    done

    # Remove images using discovered image names
    for name in "${image_names[@]}"; do
        docker rmi -f "$name" 2>/dev/null || true
    done

    # Also remove dangling images that might be related
    docker image prune -f
}

# Build WebSocket Docker images
function process_websocket() {
    for d in ./web-socket/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            local name=$(basename "$d")
            echo "Building Docker image for $d/Dockerfile as $name"
            docker build -t "$name" "$d"
        fi
    done
}

# Build Docker images for all discovered servers
function process_container_folder() {
    local folder="$1"
    for d in "$folder"/*/; do
        [ -d "$d" ] || continue
        if [ -f "$d/Dockerfile" ]; then
            local name=$(basename "$d")
            echo "Building Docker image for $d/Dockerfile as $name"
            docker build -t "$name" "$d"
        fi
    done
}

if [[ $# -gt 0 ]]; then
    for arg in "$@"; do
        case "$arg" in
            clean)
                clean_benchmark_docker_images
                ;;
            websocket)
                process_websocket
                ;;
            static)
                process_container_folder "./containers/static"
                ;;
            dynamic)
                process_container_folder "./containers/dynamic"
                ;;
            *)
                echo "Unknown argument: $arg. Skipping."
                ;;
        esac
    done
else
    # Default: process websocket, static, and dynamic
    process_websocket
    process_container_folder "./containers/static"
    process_container_folder "./containers/dynamic"
fi