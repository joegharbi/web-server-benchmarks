FROM debian:bookworm-slim

# Install dependencies and add Bullseye repo for OpenSSL 1.1.1
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    libncurses5-dev \
    autoconf \
    && echo "deb http://deb.debian.org/debian bullseye main" > /etc/apt/sources.list.d/bullseye.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends -t bullseye libssl-dev=1.1.1* && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Erlang from source (OTP 23.3.4.18)
RUN curl -L --http1.1 https://github.com/erlang/otp/releases/download/OTP-23.3.4.18/otp_src_23.3.4.18.tar.gz -o erlang.tar.gz && \
    tar -xzf erlang.tar.gz && \
    cd otp_src_23.3.4.18 && \
    ./configure && \
    make && make install && \
    cd .. && rm -rf erlang.tar.gz otp_src_23.3.4.18

# Set working directory
WORKDIR /app

# Create document root
RUN mkdir -p /var/www/html

# Embed a simple index.html
RUN echo '<!DOCTYPE html>' > /var/www/html/index.html && \
    echo '<html>' >> /var/www/html/index.html && \
    echo '  <head>' >> /var/www/html/index.html && \
    echo '    <title>Energy Test</title>' >> /var/www/html/index.html && \
    echo '  </head>' >> /var/www/html/index.html && \
    echo '  <body>' >> /var/www/html/index.html && \
    echo '    <h1>Hello, Energy Test!</h1>' >> /var/www/html/index.html && \
    echo '  </body>' >> /var/www/html/index.html && \
    echo '</html>' >> /var/www/html/index.html

# Create a simple Erlang HTTP server
RUN echo '-module(hello).' > /app/hello.erl && \
    echo '-export([main/1]).' >> /app/hello.erl && \
    echo 'main(_Args) ->' >> /app/hello.erl && \
    echo '    {ok, Sock} = gen_tcp:listen(8080, [{reuseaddr, true}]),' >> /app/hello.erl && \
    echo '    accept_loop(Sock).' >> /app/hello.erl && \
    echo 'accept_loop(Sock) ->' >> /app/hello.erl && \
    echo '    {ok, Conn} = gen_tcp:accept(Sock),' >> /app/hello.erl && \
    echo '    spawn(fun() -> handle(Conn) end),' >> /app/hello.erl && \
    echo '    accept_loop(Sock).' >> /app/hello.erl && \
    echo 'handle(Conn) ->' >> /app/hello.erl && \
    echo '    {ok, Html} = file:read_file("/var/www/html/index.html"),' >> /app/hello.erl && \
    echo '    Response = <<<<"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n">>/binary, Html/binary>>,' >> /app/hello.erl && \
    echo '    gen_tcp:send(Conn, Response),' >> /app/hello.erl && \
    echo '    gen_tcp:close(Conn).' >> /app/hello.erl

# Compile the Erlang app
RUN erlc -o /app /app/hello.erl

# Expose port 8080
EXPOSE 8080

