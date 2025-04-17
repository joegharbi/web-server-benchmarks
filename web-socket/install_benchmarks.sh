#!/bin/bash

# Traverse all folders and subfolders to build Docker images and install local servers
find . -type d | while read -r dir; do
    # Skip the brainstorming folder
    if [[ "$dir" == *"brainstorming"* ]]; then
        echo "Skipping directory: $dir"
        continue
    fi

    echo "Processing directory: $dir"

    # Find and build all Dockerfiles in the directory
    find "$dir" -type f -name 'Dockerfile*' | while read -r dockerfile; do
        image_name=$(basename "$dockerfile" | sed 's/Dockerfile\.//;s/Dockerfile//')
        if [ -z "$image_name" ]; then
            image_name=$(basename "$dir")
        fi
        echo "Building Docker image for $dockerfile as $image_name"
        sudo docker build -t "$image_name" -f "$dockerfile" "$dir"
    done

    # Check if setup_nginx.sh exists and run it with 'install' argument
    if [ -f "$dir/setup_nginx.sh" ]; then
        echo "Running setup_nginx.sh in $dir with 'install' argument"
        sudo bash "$dir/setup_nginx.sh" install
    fi

    # Check if setup_yaws.sh exists and run it with 'install' argument
    if [ -f "$dir/setup_yaws.sh" ]; then
        echo "Running setup_yaws.sh in $dir with 'install' argument"
        sudo bash "$dir/setup_yaws.sh" install
    fi

done