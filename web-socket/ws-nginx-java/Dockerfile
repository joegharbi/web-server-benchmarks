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

# Copy Nginx config
COPY nginx.conf /etc/nginx/nginx.conf

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
nginx -g 'daemon off;' > /dev/stdout 2>&1 &\n\
java -Xmx2g -Xms512m -jar /app/websocket-server.jar > /dev/stdout 2>&1 &\n\
if [ \$? -eq 0 ]; then\n\
    echo \"Nginx and Java WebSocket server started\"\n\
else\n\
    echo \"Startup failed\"\n\
    exit 1\n\
fi\n\
sleep infinity" > /start.sh \
    && chmod +x /start.sh

CMD ["/start.sh"]