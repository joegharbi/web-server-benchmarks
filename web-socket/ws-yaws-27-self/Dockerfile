# Multi-stage build for optimized Yaws WebSocket server
# Builder stage (minimal - just for handler compilation)
FROM erlang:27 AS builder

# Install Yaws from package manager for header files
RUN apt-get update && apt-get install -y \
    yaws \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Create and compile the WebSocket handler
RUN mkdir -p /app && \
    echo "-module(ws_handler).\n\
    -include(\"/usr/lib/yaws-2.1.1/include/yaws_api.hrl\").\n\
    -export([out/1, handle_message/1, terminate/2]).\n\
    out(Arg) ->\n\
        io:format(\"Request received~n\"),\n\
        Headers = Arg#arg.headers,\n\
        io:format(\"Headers: ~p~n\", [Headers]),\n\
        {websocket, ws_handler, []}.\n\
    handle_message({text, Data}) ->\n\
        io:format(\"Received text: ~p~n\", [Data]),\n\
        {reply, {text, Data}};\n\
    handle_message({binary, Data}) ->\n\
        io:format(\"Received binary: ~p~n\", [Data]),\n\
        {reply, {binary, Data}};\n\
    handle_message({close, Status, Reason}) ->\n\
        io:format(\"Closed: ~p~n\", [Status, Reason]),\n\
        {close, Status, Reason}.\n\
    terminate(Reason, _State) ->\n\
        io:format(\"Terminated: ~p~n\", [Reason]),\n\
        ok." > /app/ws_handler.erl \
    && erlc -I /usr/lib/yaws-2.1.1/include -o /app /app/ws_handler.erl

# Final stage - minimal runtime image
FROM debian:bookworm-slim

# Install Yaws and minimal runtime dependencies
RUN apt-get update && apt-get install -y \
    yaws \
    libncurses5 \
    libssl3 \
    ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set up Yaws config
RUN mkdir -p /etc/yaws /var/log/yaws /var/www /app \
    && echo "logdir = /var/log/yaws\n\
    ebin_dir = /app\n\
    <server localhost>\n\
        port = 80\n\
        listen = 0.0.0.0\n\
        docroot = /var/www\n\
        appmods = </ws, ws_handler>\n\
    </server>" > /etc/yaws/yaws.conf

# Copy the compiled handler from builder stage
COPY --from=builder /app/ws_handler.beam /app/ws_handler.beam

# Expose port
EXPOSE 80

# Startup script for daemon mode with logging
RUN echo "#!/bin/sh\n\
    echo \"Starting Yaws...\"\n\
    yaws --daemon --conf /etc/yaws/yaws.conf > /dev/stdout 2>&1\n\
    if [ \$? -eq 0 ]; then\n\
        echo \"Yaws started\"\n\
    else\n\
        echo \"Yaws failed\"\n\
        exit 1\n\
    fi\n\
    sleep infinity" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"]