# Use a lightweight Debian base image for consistency
FROM debian:bookworm-slim

# Install dependencies for Nginx and Tornado
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    nginx \
    python3 \
    python3-pip \
    && apt-get clean && rm -rf /var/lib/apt/lists/* \
    && pip3 install tornado --break-system-packages

# Configure Nginx to proxy WebSocket requests to Tornado
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
    echo "Nginx WebSocket Server (Tornado)" > /var/www/html/index.html

# Create the Tornado WebSocket echo server
RUN mkdir -p /app && \
    echo "\
import tornado.ioloop\n\
import tornado.web\n\
import tornado.websocket\n\
\n\
class EchoWebSocket(tornado.websocket.WebSocketHandler):\n\
    def open(self):\n\
        print('WebSocket connection opened')\n\
\n\
    def on_message(self, message):\n\
        # Handle both text and binary messages\n\
        if isinstance(message, bytes):\n\
            # Binary message - echo back as binary\n\
            self.write_message(message, binary=True)\n\
        else:\n\
            # Text message - echo back as text\n\
            self.write_message(message)\n\
\n\
    def on_close(self):\n\
        print('WebSocket connection closed')\n\
\n\
def make_app():\n\
    return tornado.web.Application([\n\
        (r'/ws', EchoWebSocket),\n\
    ])\n\
\n\
if __name__ == '__main__':\n\
    app = make_app()\n\
    app.listen(8765, address='0.0.0.0')\n\
    print('Tornado server starting on port 8765...')\n\
    tornado.ioloop.IOLoop.current().start()\n\
" > /app/websocket_server.py

# Expose port
EXPOSE 80

# Startup script
RUN echo "#!/bin/sh\n\
ulimit -n 100000\n\
    echo \"Starting Nginx and Tornado WebSocket server...\"\n\
    # Start Tornado server in background\n\
    python3 /app/websocket_server.py > /dev/stdout 2>&1 &\n\
    TORNADO_PID=\$!\n\
    # Wait a moment for Tornado to start\n\
    sleep 3\n\
    # Check if Tornado is running\n\
    if ! kill -0 \$TORNADO_PID 2>/dev/null; then\n\
        echo \"Tornado server failed to start\"\n\
        exit 1\n\
    fi\n\
    # Start Nginx\n\
    nginx -t && nginx -g 'daemon off;' > /dev/stdout 2>&1 &\n\
    NGINX_PID=\$!\n\
    # Wait a moment for Nginx to start\n\
    sleep 2\n\
    # Check if Nginx is running\n\
    if ! kill -0 \$NGINX_PID 2>/dev/null; then\n\
        echo \"Nginx failed to start\"\n\
        exit 1\n\
    fi\n\
    echo \"Both Nginx and Tornado server started successfully\"\n\
    # Wait for either process to exit\n\
    wait \$TORNADO_PID \$NGINX_PID" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"]