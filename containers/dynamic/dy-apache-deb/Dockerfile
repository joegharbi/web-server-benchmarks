FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    git make build-essential autoconf automake libtool gcc \
    wget curl sudo apache2 apache2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /var/www/html && \
    chown -R www-data:www-data /var/www/html

# Dynamic index.html with time placeholder
RUN echo '<!DOCTYPE html>' > /var/www/html/index.html && \
    echo '<html>' >> /var/www/html/index.html && \
    echo '  <head>' >> /var/www/html/index.html && \
    echo '    <title>Energy Test</title>' >> /var/www/html/index.html && \
    echo '  </head>' >> /var/www/html/index.html && \
    echo '  <body>' >> /var/www/html/index.html && \
    echo '    <h1>Hello, Energy Test!</h1>' >> /var/www/html/index.html && \
    echo '    <p>Dynamic time: <?php echo date("Y-m-d H:i:s"); ?></p>' >> /var/www/html/index.html && \
    echo '  </body>' >> /var/www/html/index.html && \
    echo '</html>' >> /var/www/html/index.html

COPY apache2.conf /etc/apache2/apache2.conf

RUN apt-get update && apt-get install -y php libapache2-mod-php && a2enmod php7.4 || true

EXPOSE 80

CMD ["apachectl", "-D", "FOREGROUND", "-f", "/etc/apache2/apache2.conf"]
