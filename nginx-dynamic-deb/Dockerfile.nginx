FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    git make build-essential autoconf automake libtool gcc \
    autotools-dev libpam0g-dev openssl wget curl sudo \
    python3 python3-pip python3-dev python3-flask python3-gunicorn \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN wget http://nginx.org/download/nginx-1.24.0.tar.gz && \
    tar -xzf nginx-1.24.0.tar.gz && \
    cd nginx-1.24.0 && \
    ./configure --without-http_rewrite_module --without-http_gzip_module && \
    make && \
    make install && \
    cd .. && rm -rf nginx-1.24.0.tar.gz nginx-1.24.0

RUN mkdir -p /var/www/html && \
    chown -R www-data:www-data /var/www/html

RUN mkdir -p /app
COPY app.py /app/app.py
COPY nginx.conf /usr/local/nginx/conf/nginx.conf
COPY start.sh /app/start.sh
RUN chmod +x /app/start.sh
WORKDIR /app

EXPOSE 80

CMD ["/app/start.sh"]