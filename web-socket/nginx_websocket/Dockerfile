# Use a lightweight Debian base image for consistency
FROM debian:bookworm-slim

# Install dependencies required for Nginx and Python WebSocket server
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    nginx \
    python3 \
    python3-pip \
    python3-websockets \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

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
RUN mkdir -p /app && \
    echo "\
import asyncio\n\
import websockets\n\
\n\
async def echo(websocket, path):\n\
    print('WebSocket connection established')\n\
    try:\n\
        async for message in websocket:\n\
            print(f'Received: {message}')\n\
            await websocket.send(message)\n\
            print(f'Sent: {message}')\n\
    except websockets.ConnectionClosed:\n\
        print('WebSocket connection closed')\n\
\n\
async def main():\n\
    async with websockets.serve(echo, '0.0.0.0', 8765, max_size=None):\n\
        await asyncio.Future()  # Run forever\n\
\n\
if __name__ == '__main__':\n\
    asyncio.run(main())\n\
" > /app/websocket_server.py

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