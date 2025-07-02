#!/bin/bash

# Install Docker images and run setup scripts for specified benchmark types

clean_benchmark_docker_images() {
    echo "Cleaning Docker containers and images for websocket, static, and dynamic benchmarks..."
    # Collect image names from websocket
    websocket_images=$(find ./web-socket -mindepth 1 -maxdepth 1 -type d | xargs -I{} basename {} | tr '[:upper:]' '[:lower:]' | tr '\n' ' ')
    # Collect image names from static
    static_images=$(find ./containers/static -mindepth 1 -maxdepth 1 -type d | xargs -I{} basename {} | tr '[:upper:]' '[:lower:]' | tr '\n' ' ')
    # Collect image names from dynamic
    dynamic_images=$(find ./containers/dynamic -mindepth 1 -maxdepth 1 -type d | xargs -I{} basename {} | tr '[:upper:]' '[:lower:]' | tr '\n' ' ')
    all_images="$websocket_images $static_images $dynamic_images"
    for image in $all_images; do
        # Remove containers using this image (any tag)
        container_ids=$(sudo docker ps -a --filter ancestor=$image --format "{{.ID}}")
        if [[ ! -z "$container_ids" ]]; then
            echo "Removing containers for image: $image"
            sudo docker rm -f $container_ids
        fi
        # Force remove image by name and by name:latest
        for tag in "" ":latest"; do
            img_name="$image$tag"
            echo "Force removing image: $img_name"
            sudo docker rmi -f $img_name 2>/dev/null || true
        done
    done
    # Prune all dangling and unused images
    echo "Pruning dangling and unused Docker images..."
    sudo docker image prune -a -f
    echo "Selected Docker containers and images have been cleaned."
}

process_websocket() {
    echo "Processing web-socket for Dockerfiles and setup scripts..."
    find ./web-socket -mindepth 1 -maxdepth 1 -type d | while read -r dir; do
        # Skip the "brainstorming" folder
        if [[ "$dir" == *"brainstorming"* ]]; then
            echo "Skipping directory: $dir"
            continue
        fi
        echo "Processing directory: $dir"
        if [[ -f "$dir/Dockerfile" ]]; then
            image_name=$(basename "$dir" | tr '[:upper:]' '[:lower:]')
            echo "Building Docker image for $dir/Dockerfile as $image_name"
            sudo docker build -t "$image_name" -f "$dir/Dockerfile" "$dir"
        else
            echo "No Dockerfile found in $dir, skipping."
            continue
        fi
        if [ -f "$dir/setup_nginx.sh" ]; then
            echo "Running setup_nginx.sh in $dir with 'install' argument"
            sudo bash "$dir/setup_nginx.sh" install
        fi
        if [ -f "$dir/setup_yaws.sh" ]; then
            echo "Running setup_yaws.sh in $dir with 'install' argument"
            sudo bash "$dir/setup_yaws.sh" install
        fi
    done
}

process_container_folder() {
    folder="$1"
    echo "Processing $folder for Dockerfiles..."
    find "$folder" -mindepth 1 -maxdepth 1 -type d | while read -r dir; do
        if [[ "$dir" == *"brainstorming"* ]]; then
            echo "Skipping directory: $dir"
            continue
        fi
        if [[ -f "$dir/Dockerfile" ]]; then
            image_name=$(basename "$dir" | tr '[:upper:]' '[:lower:]')
            echo "Building Docker image for $dir/Dockerfile as $image_name"
            docker build -t "$image_name" -f "$dir/Dockerfile" "$dir"
        else
            echo "No Dockerfile found in $dir, skipping."
            continue
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