# Use a lightweight Debian base image for consistency
FROM debian:bookworm-slim

# Install dependencies for Nginx and Python with aiohttp
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    nginx \
    python3 \
    python3-pip \
    && apt-get clean && rm -rf /var/lib/apt/lists/* \
    && pip3 install aiohttp

# Configure Nginx to proxy WebSocket requests to aiohttp
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
                proxy_buffers 8 1024k;\n\
                proxy_buffer_size 1024k;\n\
            }\n\
            location / {\n\
                root /var/www/html;\n\
                index index.html;\n\
            }\n\
        }\n\
    }" > /etc/nginx/nginx.conf

# Create a basic docroot
RUN mkdir -p /var/www/html && \
    echo "Nginx aiohttp WebSocket Server" > /var/www/html/index.html

# Create the aiohttp WebSocket server
RUN mkdir -p /app && \
    echo "\
from aiohttp import web, WSMsgType\n\
import asyncio\n\
\n\
async def websocket_handler(request):\n\
    ws = web.WebSocketResponse()\n\
    await ws.prepare(request)\n\
    print('WebSocket connection established')\n\
    async for msg in ws:\n\
        if msg.type == WSMsgType.TEXT:\n\
            print(f'Received text: {len(msg.data)} bytes')\n\
            await ws.send_str(msg.data)\n\
        elif msg.type == WSMsgType.BINARY:\n\
            print(f'Received binary: {len(msg.data)} bytes')\n\
            await ws.send_bytes(msg.data)\n\
        elif msg.type == WSMsgType.CLOSED:\n\
            print('WebSocket connection closed')\n\
            break\n\
    return ws\n\
\n\
async def init_app():\n\
    app = web.Application()\n\
    app.router.add_get('/ws', websocket_handler)\n\
    return app\n\
\n\
if __name__ == '__main__':\n\
    app = asyncio.run(init_app())\n\
    web.run_app(app, host='0.0.0.0', port=8765)\n\
" > /app/websocket_server.py

# Expose port
EXPOSE 80

# Startup script to run Nginx and aiohttp
RUN echo "#!/bin/sh\n\
    echo \"Starting Nginx and aiohttp WebSocket server...\"\n\
    nginx -t && nginx -g 'daemon off;' > /dev/stdout 2>&1 &\n\
    python3 /app/websocket_server.py > /dev/stdout 2>&1 &\n\
    sleep 1\n\
    if [ \$? -eq 0 ]; then\n\
        echo \"Nginx and aiohttp WebSocket server started\"\n\
    else\n\
        echo \"Startup failed\"\n\
        exit 1\n\
    fi\n\
    sleep infinity" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"]