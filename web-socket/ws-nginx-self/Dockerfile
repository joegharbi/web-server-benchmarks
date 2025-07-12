# Use debian:bookworm-slim for consistency
FROM debian:bookworm-slim

# Install nginx and Python dependencies
RUN apt-get update && apt-get install -y \
    nginx \
    python3 \
    python3-pip \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install websockets via pip (with flag for Debian's externally managed environment)
RUN pip3 install --break-system-packages websockets

# Configure Nginx to proxy WebSocket requests to the Python backend
RUN echo "\
    events {}\n\
    http {\n\
        access_log /dev/stdout;\n\
        error_log /dev/stdout;\n\
        server {\n\
            listen 80;\n\
            server_name localhost;\n\
            location /ws {\n\
                proxy_pass http://127.0.0.1:8765;\n\
                proxy_http_version 1.1;\n\
                proxy_set_header Upgrade \$http_upgrade;\n\
                proxy_set_header Connection \"upgrade\";\n\
                proxy_set_header Host \$host;\n\
                proxy_read_timeout 3600s;\n\
            }\n\
            location / {\n\
                root /var/www/html;\n\
                index index.html;\n\
            }\n\
        }\n\
    }" > /etc/nginx/nginx.conf

# Create a basic docroot (for non-WebSocket requests, matching Yaws)
RUN mkdir -p /var/www/html && \
    echo "Nginx WebSocket Server" > /var/www/html/index.html

# Create the app directory and Python WebSocket echo server
RUN mkdir -p /app

# Copy the new websocket_server.py
COPY websocket_server.py /app/websocket_server.py

# Expose port
EXPOSE 80

# Startup script to run Nginx and Python WebSocket server, with logging
RUN echo "#!/bin/sh\n\
    echo \"Starting Nginx and WebSocket server...\"\n\
    nginx -g 'daemon off;' > /dev/stdout 2>&1 &\n\
    python3 /app/websocket_server.py > /dev/stdout 2>&1 &\n\
    if [ \$? -eq 0 ]; then\n\
        echo \"Nginx and WebSocket server started\"\n\
    else\n\
        echo \"Startup failed\"\n\
        exit 1\n\
    fi\n\
    sleep infinity" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"]