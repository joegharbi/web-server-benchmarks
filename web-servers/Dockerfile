# Use a common base image
FROM debian:bookworm-slim

# Install common dependencies
RUN apt-get update && apt-get install -y \
    git make build-essential autoconf automake libtool gcc \
    autotools-dev libpam0g-dev openssl wget curl sudo \
    erlang-base erlang-dev erlang-nox \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Nginx from source (to match Yaws' build-from-source approach)
RUN wget http://nginx.org/download/nginx-1.24.0.tar.gz && \
    tar -xzf nginx-1.24.0.tar.gz && \
    cd nginx-1.24.0 && \
    ./configure --without-http_rewrite_module --without-http_gzip_module && \
    make && make install && \
    cd .. && rm -rf nginx-1.24.0.tar.gz nginx-1.24.0

# Install Yaws from source
RUN git clone https://github.com/erlyaws/yaws.git && \
    cd yaws && \
    autoreconf -fi && \
    ./configure && \
    make all && make install && \
    cd .. && rm -rf yaws

# Create a unified document root
RUN mkdir -p /var/www/html && \
    chown -R www-data:www-data /var/www/html

# Copy configuration files
COPY yaws.conf /usr/local/etc/yaws/yaws.conf
COPY nginx.conf /usr/local/nginx/conf/nginx.conf

# Copy the same index.html to the unified docroot
COPY index.html /var/www/html/index.html

# Expose port 80
EXPOSE 80

# Default CMD (will be overridden when running the container)
CMD ["bash"]