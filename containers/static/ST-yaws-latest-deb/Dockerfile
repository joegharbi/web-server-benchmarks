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

# Copy the custom Yaws configuration file into the image
COPY yaws.conf /usr/local/etc/yaws/yaws.conf

# Expose port 80 for web traffic
EXPOSE 80

# Start Yaws as a daemon with the custom configuration file
CMD ["/bin/sh", "-c", "yaws --daemon --conf /usr/local/etc/yaws/yaws.conf && sleep infinity"]