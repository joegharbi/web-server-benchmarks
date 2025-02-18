# Use the official Erlang image as the base image
FROM debian:bullseye-slim

# Install dependencies
RUN apt-get update && apt-get install -y \
    yaws\
sudo

# Create the necessary directory for Yaws docroot
RUN mkdir -p /var/yaws/www

# Set the correct permissions for the docroot
RUN chown -R www-data:www-data /var/yaws/www

# Add the custom Yaws configuration file
COPY yaws.conf /etc/yaws/yaws.conf

# Copy your web content into the docroot
COPY ./index.html /var/yaws/www

# Expose the port that Yaws will use
EXPOSE 80

# Start Yaws
# CMD ["yaws", "--interactive", "--conf", "/usr/local/etc/yaws/yaws.conf"]
# CMD ["yaws", "-sname", "yaws_node", "-setcookie", "yaws_cookie"]
CMD ["yaws" ,"-i"]
