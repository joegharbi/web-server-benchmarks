# Use a lightweight Debian base image for consistency with Yaws
FROM debian:bookworm-slim

# Install dependencies required to build and run Nginx
# Clean up afterward to reduce image size
RUN apt-get update && apt-get install -y \
    git make build-essential autoconf automake libtool gcc \
    autotools-dev libpam0g-dev openssl wget curl sudo \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Download and build Nginx from source for a controlled version
# Remove source files after installation to minimize image size
RUN wget http://nginx.org/download/nginx-1.24.0.tar.gz && \
    tar -xzf nginx-1.24.0.tar.gz && \
    cd nginx-1.24.0 && \
    ./configure --without-http_rewrite_module --without-http_gzip_module && \
    make && \
    make install && \
    cd .. && rm -rf nginx-1.24.0.tar.gz nginx-1.24.0

# Create a unified document root for serving web content
# Set ownership to the web server user (www-data)
RUN mkdir -p /var/www/html && \
    chown -R www-data:www-data /var/www/html

# Embed a simple index.html file directly in the image
RUN echo '<!DOCTYPE html>' > /var/www/html/index.html && \
    echo '<html>' >> /var/www/html/index.html && \
    echo '  <head>' >> /var/www/html/index.html && \
    echo '    <title>Energy Test</title>' >> /var/www/html/index.html && \
    echo '  </head>' >> /var/www/html/index.html && \
    echo '  <body>' >> /var/www/html/index.html && \
    echo '    <h1>Hello, Energy Test!</h1>' >> /var/www/html/index.html && \
    echo '  </body>' >> /var/www/html/index.html && \
    echo '</html>' >> /var/www/html/index.html

# Copy the Nginx configuration file into the image
COPY nginx.conf /usr/local/nginx/conf/nginx.conf

# Expose port 80 for web traffic
EXPOSE 80

# Start Nginx with the custom configuration, running in foreground
# CMD ["/usr/local/nginx/sbin/nginx", "-c", "/usr/local/nginx/conf/nginx.conf", "-g", "daemon off;"]
CMD /bin/sh -c "/usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf && sleep infinity"