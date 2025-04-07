FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    build-essential curl nginx python3 python3-pip python3-websockets \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

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
                proxy_buffer_size 16k;\n\
                proxy_buffers 4 256k;\n\
            }\n\
            location / {\n\
                root /var/www/html;\n\
                index index.html;\n\
            }\n\
        }\n\
    }" > /etc/nginx/nginx.conf

RUN mkdir -p /var/www/html && echo "Nginx WebSocket Server" > /var/www/html/index.html

RUN mkdir -p /app && echo "\
import asyncio\n\
import websockets\n\
async def echo(websocket, path):\n\
    print('WebSocket connection established')\n\
    try:\n\
        async for message in websocket:\n\
            print(f'Received: {len(message)} bytes')\n\
            await websocket.send(message)\n\
            print(f'Sent: {len(message)} bytes')\n\
    except websockets.ConnectionClosed:\n\
        print('WebSocket connection closed')\n\
async def main():\n\
    async with websockets.serve(echo, '0.0.0.0', 8765, max_size=20_000_000):\n\
        await asyncio.Future()\n\
if __name__ == '__main__':\n\
    asyncio.run(main())\n\
" > /app/websocket_server.py

EXPOSE 80

RUN echo "#!/bin/sh\n\
    echo \"Starting Nginx and WebSocket server...\"\n\
    nginx -g 'daemon off;' > /dev/stdout 2>&1 &\n\
    python3 /app/websocket_server.py > /dev/stdout 2>&1 &\n\
    if [ \$? -eq 0 ]; then echo \"Nginx and WebSocket server started\"; else echo \"Startup failed\"; exit 1; fi\n\
    sleep infinity" > /start.sh && chmod +x /start.sh

CMD ["/start.sh"]