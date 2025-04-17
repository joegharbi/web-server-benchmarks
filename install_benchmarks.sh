#!/bin/bash

# Install all Docker images for the benchmarks

# Default directories containing Dockerfiles
default_image_dirs=("./containers/static/apache-deb" "./containers/static/nginx-deb" "./containers/static/cowboy-play" "./containers/static/yaws-deb" "./containers/dynamic/nginx-dynamic-deb" "./containers/dynamic/yaws-dynamic-latest-deb")

# Allow additional directories to be passed as arguments
if [[ $# -gt 0 ]]; then
    image_dirs=("$@")
else
    image_dirs=("${default_image_dirs[@]}")
fi

echo "Processing directories: ${image_dirs[*]}"

# Loop through each directory and build the Docker image
for dir in "${image_dirs[@]}"; do
    # Skip the "brainstorming" folder
    if [[ "$dir" == *"brainstorming"* ]]; then
        echo "Skipping directory: $dir"
        continue
    fi

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