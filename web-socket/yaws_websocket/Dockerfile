# Use a lightweight Debian base image for consistency
FROM debian:bookworm-slim

# Install dependencies required to build Erlang and Yaws
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    libncurses5-dev \
    libssl-dev \
    autoconf \
    git \
    make \
    automake \
    libtool \
    gcc \
    autotools-dev \
    libpam0g-dev \
    sudo \
    libwxgtk3.2-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Download and build Erlang OTP 27.0.1 from source (your working snippet)
RUN curl -L --http1.1 https://github.com/erlang/otp/releases/download/OTP-27.0.1/otp_src_27.0.1.tar.gz -o erlang.tar.gz && \
    tar -xzf erlang.tar.gz && \
    cd otp_src_27.0.1 && \
    ./configure && \
    make && make install && \
    cd .. && rm -rf erlang.tar.gz otp_src_27.0.1

# Clone and build the latest Yaws from source
RUN git clone https://github.com/erlyaws/yaws.git && \
    cd yaws && \
    autoreconf -fi && \
    ./configure && \
    make all && \
    make install && \
    cd .. && rm -rf yaws

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

# Create and compile the WebSocket handler, dynamically finding the include path
RUN YAWS_INCLUDE=$(find /usr -name yaws_api.hrl | head -n 1 | xargs dirname) && \
    if [ -z "$YAWS_INCLUDE" ]; then echo "Error: yaws_api.hrl not found!"; exit 1; fi && \
    echo "-module(ws_handler).\n\
    -include_lib(\"yaws/include/yaws_api.hrl\").\n\
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
    && erlc -I "$YAWS_INCLUDE" -o /app /app/ws_handler.erl

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