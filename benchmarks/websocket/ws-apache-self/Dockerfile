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
ulimit -n 100000\n\
    echo \"Starting Apache and WebSocket server...\"\n\
    # Start Python WebSocket server in background\n\
    python3 /app/websocket_server.py > /dev/stdout 2>&1 &\n\
    PYTHON_PID=\$!\n\
    # Wait a moment for Python server to start\n\
    sleep 3\n\
    # Check if Python server is running\n\
    if ! kill -0 \$PYTHON_PID 2>/dev/null; then\n\
        echo \"Python WebSocket server failed to start\"\n\
        exit 1\n\
    fi\n\
    # Start Apache\n\
    apache2ctl -D FOREGROUND > /dev/stdout 2>&1 &\n\
    APACHE_PID=\$!\n\
    # Wait a moment for Apache to start\n\
    sleep 2\n\
    # Check if Apache is running\n\
    if ! kill -0 \$APACHE_PID 2>/dev/null; then\n\
        echo \"Apache failed to start\"\n\
        exit 1\n\
    fi\n\
    echo \"Both Apache and Python WebSocket server started successfully\"\n\
    # Wait for either process to exit\n\
    wait \$PYTHON_PID \$APACHE_PID" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"] 