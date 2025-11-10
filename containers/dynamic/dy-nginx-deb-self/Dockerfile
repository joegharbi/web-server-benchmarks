FROM debian:bookworm-slim

# Install nginx and Python dependencies
RUN apt-get update && apt-get install -y \
    nginx \
    python3 \
    python3-pip \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Python dependencies (with flag for Debian's externally managed environment)
RUN pip3 install --break-system-packages Flask

# Copy Python app
COPY app.py /app/app.py

# Copy nginx configuration
COPY nginx.conf /etc/nginx/nginx.conf

# Copy startup script
COPY start.sh /app/start.sh
RUN chmod +x /app/start.sh

WORKDIR /app

EXPOSE 80

CMD ["/app/start.sh"]