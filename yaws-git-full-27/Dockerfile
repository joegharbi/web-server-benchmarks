# Use the official Erlang image as the base image
FROM erlang:27

# Install dependencies
RUN apt-get update && apt-get install -y \
    git \
    make \
    build-essential \
    autoconf \
    automake \
    libtool \
    gcc \
    autotools-dev libpam0g-dev openssl wget curl cadaver #texlive-font-utils \
    sudo

# Clone the Yaws repository and build dependencies
RUN git clone https://github.com/erlyaws/yaws.git && \
    cd yaws && \
    autoreconf -fi && \
    ./configure && \
    make all &&\
    make install


# Create the necessary directory for Yaws docroot
RUN mkdir -p /var/yaws/www

# Set the correct permissions for the docroot
RUN chown -R www-data:www-data /var/yaws/www

# Add the custom Yaws configuration file
COPY yaws.conf /usr/local/etc/yaws/yaws.conf

# Copy your web content into the docroot
COPY ./index.html /var/yaws/www

# Expose the port that Yaws will use
EXPOSE 80

# Start Yaws
# CMD ["yaws", "--interactive", "--conf", "/usr/local/etc/yaws/yaws.conf"]
# CMD ["yaws" ,"-i"]
CMD ["yaws", "-daemon"]
