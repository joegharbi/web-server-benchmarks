# Use official Erlang image as base
FROM erlang:26

# Install necessary dependencies
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install Yaws from source
RUN git clone https://github.com/erlang/yaws.git /usr/local/src/yaws \
    && cd /usr/local/src/yaws \
    && ./configure \
    && make \
    && make install

# Create directory structure for our app
RUN mkdir -p /yaws/www/websocket
WORKDIR /yaws/www/websocket

# Create a simple Yaws configuration file
COPY yaws.conf /etc/yaws/yaws.conf

# Copy our WebSocket example files
COPY index.yaws /yaws/www/websocket/
COPY websocket.erl /yaws/www/websocket/

# Expose ports (HTTP and WebSocket)
EXPOSE 8000

# Start Yaws server
CMD ["yaws", "-i", "--conf", "/etc/yaws/yaws.conf"]