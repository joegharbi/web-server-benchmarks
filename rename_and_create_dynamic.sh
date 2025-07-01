#!/bin/bash
# Rename folders to new convention and create dynamic variants with time in response

set -e

# --- Rename web-socket folders ---
cd web-socket
mv nginx-java-websocket WS-nginx-java
mv nginx-tornado WS-nginx-tornado
mv nginx_websocket WS-nginx
mv yaws_websocket WS-yaws
cd ..

# --- Rename static folders ---
cd containers/static
mv apache-deb ST-apache-deb
mv cowboy-play ST-cowboy-play
mv erlang23 ST-erlang23
mv erlang26 ST-erlang26
mv erlang27 ST-erlang27
mv erlindex23 ST-erlindex23
mv index26 ST-index26
mv index27 ST-index27
mv nginx-deb ST-nginx-deb
mv yaws-deb ST-yaws-deb
mv yaws-latest-deb ST-yaws-latest-deb
cd ../..

# --- Rename dynamic folders ---
cd containers/dynamic
mv nginx-dynamic-deb DY-nginx-deb
mv yaws-dynamic-latest-deb DY-yaws-latest-deb
cd ../..

# --- Create new dynamic folders from static ---
cd containers/static
for src in ST-erlang23 ST-erlang26 ST-erlang27 ST-erlindex23 ST-index26 ST-index27; do
    dest="../dynamic/DY-${src#ST-}"
    cp -r "$src" "$dest"
    # Add dynamic time to HTML or response in Dockerfile (simple placeholder)
    if grep -q 'echo' "$dest/Dockerfile"; then
        sed -i '/echo/s/$/ $(date)/' "$dest/Dockerfile"
    fi
    # If there is an index.html, add a placeholder for dynamic time
    if [ -f "$dest/index.html" ]; then
        echo '<!-- Dynamic time: $(date) -->' >> "$dest/index.html"
    fi
    echo "Created $dest with dynamic time placeholder."
done
cd ../..

echo "Renaming and dynamic folder creation complete. Please manually adjust application code to output the actual dynamic time in responses."
