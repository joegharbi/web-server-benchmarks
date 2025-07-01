#!/bin/bash

# Install all Docker images for the benchmarks

# If arguments are provided, use them as directories or Dockerfiles to build
if [[ $# -gt 0 ]]; then
    image_dirs=("$@")
else
    # Otherwise, automatically find all subfolders in static and dynamic
    image_dirs=(
        $(find ./containers/static/ -mindepth 1 -maxdepth 1 -type d)
        $(find ./containers/dynamic/ -mindepth 1 -maxdepth 1 -type d)
    )
fi

echo "Processing directories: ${image_dirs[*]}"

# Loop through each directory and build the Docker images
for dir in "${image_dirs[@]}"; do
    # Skip the "brainstorming" folder
    if [[ "$dir" == *"brainstorming"* ]]; then
        echo "Skipping directory: $dir"
        continue
    fi

    # Only build if there is a file named exactly 'Dockerfile' in the directory
    if [[ ! -f "$dir/Dockerfile" ]]; then
        echo "No Dockerfile found in $dir, skipping."
        continue
    fi

    dockerfile_path="$dir/Dockerfile"
    image_name=$(basename "$dir")

    echo "Building Docker image for $dockerfile_path as $image_name"
    docker build -t "$image_name" -f "$dockerfile_path" "$dir"
done