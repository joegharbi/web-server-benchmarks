# Use the same lightweight Debian base image as YAWS
FROM debian:bookworm-slim

# Install dependencies required to build Erlang and Cowboy (match YAWS)
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

# Download and build Erlang OTP 27.0.1 from source (exact match with YAWS)
RUN curl -L --http1.1 https://github.com/erlang/otp/releases/download/OTP-27.0.1/otp_src_27.0.1.tar.gz -o erlang.tar.gz && \
    tar -xzf erlang.tar.gz && \
    cd otp_src_27.0.1 && \
    ./configure && \
    make && make install && \
    cd .. && rm -rf erlang.tar.gz otp_src_27.0.1

# Build Rebar3 from source to ensure compatibility with OTP 27.0.1
RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    mv rebar3 /usr/local/bin/ && \
    cd .. && rm -rf rebar3

# Verify Erlang and Rebar3 versions
RUN erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
RUN rebar3 version || echo "Rebar3 failed to run"

# Set working directory
WORKDIR /app

# Copy configuration and source files
COPY rebar.config .
COPY src/ src/

# Embed the same index.html file as YAWS in /app/
RUN echo '<!DOCTYPE html>' > /app/index.html && \
    echo '<html>' >> /app/index.html && \
    echo '  <head>' >> /app/index.html && \
    echo '    <title>Energy Test</title>' >> /app/index.html && \
    echo '  </head>' >> /app/index.html && \
    echo '  <body>' >> /app/index.html && \
    echo '    <h1>Hello, Energy Test!</h1>' >> /app/index.html && \
    echo '  </body>' >> /app/index.html && \
    echo '</html>' >> /app/index.html && \
    chmod 644 /app/index.html && \
    ls -l /app/index.html && \
    cat /app/index.html

# Build Cowboy with explicit binary check
RUN rebar3 get-deps && \
    rebar3 compile && \
    rebar3 as prod release && \
    ls -l /app/_build/prod/rel/simple_cowboy_app/bin/

# Added a step to verify the application binary exists before running it
RUN test -f /app/_build/prod/rel/simple_cowboy_app/bin/simple_cowboy_app || (echo "Binary not found" && exit 1)

# Expose port 80 (match YAWS)
EXPOSE 80

# Run Cowboy in foreground
CMD ["/app/_build/prod/rel/simple_cowboy_app/bin/simple_cowboy_app", "foreground"]