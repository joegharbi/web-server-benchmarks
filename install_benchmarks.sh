#!/bin/bash

# Install all Docker images for the benchmarks

# Define the directories containing Dockerfiles
static_dirs=("./containers/static/nginx-deb" "./containers/static/cowboy-test" "./containers/static/yaws-deb" "./containers/static/yaws-git-slim-27" "./containers/static/yaws-git-full-27")

echo "Processing directory: ."

# Loop through each directory and build the Docker image
for dir in "${static_dirs[@]}"; do
    dockerfile_path=$(find "$dir" -name "Dockerfile*" -type f)
    if [[ -z "$dockerfile_path" ]]; then
        echo "No Dockerfile found in $dir, skipping."
        continue
    fi

    dockerfile_name=$(basename "$dockerfile_path")
    image_name=$(basename "$dir")

    # Append the extension to the image name if the Dockerfile has one
    if [[ "$dockerfile_name" != "Dockerfile" ]]; then
        extension="${dockerfile_name#Dockerfile.}"
        image_name+="-$extension"
    fi

    echo "Building Docker image for $dir as $image_name"
    docker build -t "$image_name" -f "$dockerfile_path" "$dir"
done