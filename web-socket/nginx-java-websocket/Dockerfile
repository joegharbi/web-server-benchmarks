# Stage 1: Build the Java WebSocket server with Maven
FROM maven:3.9.9-eclipse-temurin-17 AS builder

WORKDIR /app
COPY pom.xml .
COPY src ./src
RUN mvn clean package -DskipTests

# Stage 2: Runtime with Nginx and Java
FROM debian:bookworm-slim

# Install Nginx and OpenJDK runtime
RUN apt-get update && apt-get install -y \
    nginx \
    openjdk-17-jre \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Configure Nginx to proxy WebSocket requests to Java (port 8080)
RUN echo "\
    events {}\n\
    http {\n\
        access_log /dev/stdout;\n\
        error_log /dev/stdout;\n\
        server {\n\
            listen 80;\n\
            server_name localhost;\n\
            location /ws {\n\
                proxy_pass http://127.0.0.1:8080/ws;\n\
                proxy_http_version 1.1;\n\
                proxy_set_header Upgrade \$http_upgrade;\n\
                proxy_set_header Connection \"upgrade\";\n\
                proxy_set_header Host \$host;\n\
                proxy_read_timeout 3600s;\n\
                proxy_buffers 8 1024k;\n\
                proxy_buffer_size 1024k;\n\
            }\n\
            location / {\n\
                root /var/www/html;\n\
                index index.html;\n\
            }\n\
        }\n\
    }" > /etc/nginx/nginx.conf

# Create a basic docroot
RUN mkdir -p /var/www/html && \
    echo "Nginx Java WebSocket Server" > /var/www/html/index.html

# Copy the built Spring Boot JAR from the builder stage
COPY --from=builder /app/target/websocket-server-0.0.1-SNAPSHOT.jar /app/websocket-server.jar

# Expose port
EXPOSE 80

# Startup script to run Nginx and Java
RUN echo "#!/bin/sh\n\
    echo \"Starting Nginx and Java WebSocket server...\"\n\
    nginx -t && nginx -g 'daemon off;' > /dev/stdout 2>&1 &\n\
    java -jar /app/websocket-server.jar > /dev/stdout 2>&1 &\n\
    sleep 1\n\
    if [ \$? -eq 0 ]; then\n\
        echo \"Nginx and Java WebSocket server started\"\n\
    else\n\
        echo \"Startup failed\"\n\
        exit 1\n\
    fi\n\
    sleep infinity" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"]