FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    git make build-essential autoconf automake libtool gcc \
    wget curl sudo apache2 apache2-dev php libapache2-mod-php \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /var/www/html && \
    chown -R www-data:www-data /var/www/html

# Dynamic index.php with time
RUN echo '<!DOCTYPE html>' > /var/www/html/index.php && \
    echo '<html>' >> /var/www/html/index.php && \
    echo '  <head>' >> /var/www/html/index.php && \
    echo '    <title>Energy Test</title>' >> /var/www/html/index.php && \
    echo '  </head>' >> /var/www/html/index.php && \
    echo '  <body>' >> /var/www/html/index.php && \
    echo '    <h1>Hello, Energy Test!</h1>' >> /var/www/html/index.php && \
    echo '    <p>Dynamic time: <?php echo date("Y-m-d H:i:s"); ?></p>' >> /var/www/html/index.php && \
    echo '  </body>' >> /var/www/html/index.php && \
    echo '</html>' >> /var/www/html/index.php

COPY apache2.conf /etc/apache2/apache2.conf

EXPOSE 80

CMD ["apachectl", "-D", "FOREGROUND", "-f", "/etc/apache2/apache2.conf"]
