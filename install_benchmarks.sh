#!/bin/bash

# Install Docker images and run setup scripts for specified benchmark types

# Remove all containers and images related to the benchmark (safe)
function clean_benchmark_docker_images() {
    echo "Cleaning up benchmark Docker containers and images..."
    docker rm -f $(docker ps -aq --filter ancestor=ws-nginx-python-websockets --filter ancestor=ws-nginx-java --filter ancestor=ws-nginx-tornado --filter ancestor=ws-yaws --filter ancestor=ws-apache --filter ancestor=ws-cowboy --filter ancestor=st-nginx-deb --filter ancestor=st-apache-deb --filter ancestor=st-yaws-26 --filter ancestor=st-yaws-27 --filter ancestor=st-cowboy-27 --filter ancestor=st-erlang23 --filter ancestor=st-erlang26 --filter ancestor=st-erlang27 --filter ancestor=st-erlindex23 --filter ancestor=st-erlindex26 --filter ancestor=st-erlindex27 --filter ancestor=dy-nginx-deb --filter ancestor=dy-yaws-26 --filter ancestor=dy-yaws-27 --filter ancestor=dy-apache-deb --filter ancestor=dy-cowboy-27 --filter ancestor=dy-erlang23 --filter ancestor=dy-erlang26 --filter ancestor=dy-erlang27 --filter ancestor=dy-erlindex23 --filter ancestor=dy-erlindex26 --filter ancestor=dy-erlindex27) 2>/dev/null || true
    docker rmi -f $(docker images --filter=reference='ws-*' --filter=reference='st-*' --filter=reference='dy-*' -q) 2>/dev/null || true
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