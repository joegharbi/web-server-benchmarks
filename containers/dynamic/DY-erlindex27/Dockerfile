FROM debian:bookworm-slim

# Use a lightweight Debian base image
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    libncurses5-dev \
    libssl-dev \
    autoconf \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Erlang from source (OTP 27.0.1)
RUN curl -L --http1.1 https://github.com/erlang/otp/releases/download/OTP-27.0.1/otp_src_27.0.1.tar.gz -o erlang.tar.gz && \
    tar -xzf erlang.tar.gz && \
    cd otp_src_27.0.1 && \
    ./configure && \
    make && make install && \
    cd .. && rm -rf erlang.tar.gz otp_src_27.0.1

WORKDIR /app

RUN mkdir -p /var/www/html

RUN echo '<!DOCTYPE html>' > /var/www/html/index.html && \
    echo '<html>' >> /var/www/html/index.html && \
    echo '  <head>' >> /var/www/html/index.html && \
    echo '    <title>Energy Test</title>' >> /var/www/html/index.html && \
    echo '  </head>' >> /var/www/html/index.html && \
    echo '  <body>' >> /var/www/html/index.html && \
    echo '    <h1>Hello, Energy Test!</h1>' >> /var/www/html/index.html && \
    echo '    <p>Dynamic time: <span id="dyn-time"></span></p>' >> /var/www/html/index.html && \
    echo '  </body>' >> /var/www/html/index.html && \
    echo '</html>' >> /var/www/html/index.html

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
    echo '    {ok, HtmlBin} = file:read_file("/var/www/html/index.html"),' >> /app/hello.erl && \
    echo '    {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_universal_time(erlang:now()),' >> /app/hello.erl && \
    echo '    TimeStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y,Mo,D,H,Mi,S]),' >> /app/hello.erl && \
    echo '    HtmlWithTime = re:replace(HtmlBin, <<"<span id=\"dyn-time\"></span>">>, list_to_binary(TimeStr), [{return, binary}]),' >> /app/hello.erl && \
    echo '    Response = <<"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n", HtmlWithTime/binary>>,' >> /app/hello.erl && \
    echo '    gen_tcp:send(Conn, Response),' >> /app/hello.erl && \
    echo '    gen_tcp:close(Conn).' >> /app/hello.erl

RUN erlc -o /app /app/hello.erl
EXPOSE 8080
CMD ["erl", "-pa", "/app", "-noshell", "-run", "hello", "main", "start"]
