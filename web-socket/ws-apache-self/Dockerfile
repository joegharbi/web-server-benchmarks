# Use a lightweight Debian base image for consistency
FROM debian:bookworm-slim

# Install dependencies required for Apache and Python WebSocket server
RUN apt-get update && apt-get install -y \
    apache2 \
    python3 \
    python3-pip \
    python3-websockets \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Enable required Apache modules
RUN a2enmod proxy
RUN a2enmod proxy_http
RUN a2enmod rewrite

# Configure Apache to proxy WebSocket requests to the Python backend
RUN echo "\
<VirtualHost *:80>\n\
    ServerName localhost\n\
    DocumentRoot /var/www/html\n\
    \n\
    # WebSocket proxy configuration\n\
    ProxyPass /ws ws://127.0.0.1:8765/ws\n\
    ProxyPassReverse /ws ws://127.0.0.1:8765/ws\n\
    \n\
    # Regular HTTP requests\n\
    ProxyPass / http://127.0.0.1:8765/\n\
    ProxyPassReverse / http://127.0.0.1:8765/\n\
    \n\
    # Logging\n\
    ErrorLog \${APACHE_LOG_DIR}/error.log\n\
    CustomLog \${APACHE_LOG_DIR}/access.log combined\n\
</VirtualHost>" > /etc/apache2/sites-available/000-default.conf

# Create a basic docroot
RUN mkdir -p /var/www/html && \
    echo "Apache WebSocket Server" > /var/www/html/index.html

# Create the app directory and Python WebSocket echo server
RUN mkdir -p /app

# Copy the WebSocket server
COPY websocket_server.py /app/websocket_server.py

# Expose port
EXPOSE 80

# Startup script to run Apache and Python WebSocket server
RUN echo "#!/bin/sh\n\
    echo \"Starting Apache and WebSocket server...\"\n\
    apache2ctl -D FOREGROUND > /dev/stdout 2>&1 &\n\
    python3 /app/websocket_server.py > /dev/stdout 2>&1 &\n\
    if [ \$? -eq 0 ]; then\n\
        echo \"Apache and WebSocket server started\"\n\
    else\n\
        echo \"Startup failed\"\n\
        exit 1\n\
    fi\n\
    sleep infinity" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"] 