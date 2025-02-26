# Use a lightweight Debian base image for consistency with Nginx
FROM debian:bookworm-slim

# Install dependencies required to build and run Yaws
# Clean up afterward to reduce image size
RUN apt-get update && apt-get install -y \
    git make build-essential autoconf automake libtool gcc \
    autotools-dev libpam0g-dev openssl wget curl sudo \
    erlang-base erlang-dev erlang-nox \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Clone and build Yaws from source for the latest version
# Remove source files after installation to minimize image size
RUN git clone https://github.com/erlyaws/yaws.git && \
    cd yaws && \
    autoreconf -fi && \
    ./configure && \
    make all && \
    make install && \
    cd .. && rm -rf yaws

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

# Copy the Yaws configuration file into the image
COPY yaws.conf /usr/local/etc/yaws/yaws.conf

# Expose port 80 for web traffic
EXPOSE 80

# Start Yaws with the custom configuration file
# CMD ["yaws", "--conf", "/usr/local/etc/yaws/yaws.conf"]
# Start Yaws as a daemon with the custom configuration file
# CMD ["yaws", "--daemon", "--conf", "/usr/local/etc/yaws/yaws.conf"]
# CMD ["yaws", "--daemon", "--heart", "--trace", "--conf", "/usr/local/etc/yaws/yaws.conf"]
# CMD /bin/sh -c "yaws --daemon --conf /usr/local/etc/yaws/yaws.conf && tail -f /var/log/yaws/yaws.log"
CMD /bin/sh -c "yaws --daemon --conf /usr/local/etc/yaws/yaws.conf && sleep infinity"