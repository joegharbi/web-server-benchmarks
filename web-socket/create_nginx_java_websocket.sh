#!/bin/bash

# Directory name for the project
PROJECT_DIR="nginx-java-websocket"

# Create the project directory
echo "Creating directory structure: $PROJECT_DIR"
mkdir -p "$PROJECT_DIR/src/main/java/com/example"

# Navigate to the project directory
cd "$PROJECT_DIR" || exit

# Create Dockerfile
echo "Creating Dockerfile..."
cat << 'EOF' > Dockerfile
# Stage 1: Build the Java WebSocket server with Maven
FROM maven:3.8.6-openjdk-17-slim AS builder

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
EOF

# Create pom.xml
echo "Creating pom.xml..."
cat << 'EOF' > pom.xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" 
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.example</groupId>
    <artifactId>websocket-server</artifactId>
    <version>0.0.1-SNAPSHOT</version>
    <packaging>jar</packaging>

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
        <relativePath/>
    </parent>

    <properties>
        <java.version>17</java.version>
    </properties>

    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-websocket</artifactId>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
            </plugin>
        </plugins>
    </build>
</project>
EOF

# Create WebsocketServerApplication.java
echo "Creating WebsocketServerApplication.java..."
cat << 'EOF' > src/main/java/com/example/WebsocketServerApplication.java
package com.example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class WebsocketServerApplication {
    public static void main(String[] args) {
        SpringApplication.run(WebsocketServerApplication.class, args);
    }
}
EOF

# Create WebSocketConfig.java
echo "Creating WebSocketConfig.java..."
cat << 'EOF' > src/main/java/com/example/WebSocketConfig.java
package com.example;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {
    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(new EchoWebSocketHandler(), "/ws").setAllowedOrigins("*");
    }
}
EOF

# Create EchoWebSocketHandler.java
echo "Creating EchoWebSocketHandler.java..."
cat << 'EOF' > src/main/java/com/example/EchoWebSocketHandler.java
package com.example;

import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.AbstractWebSocketHandler;

public class EchoWebSocketHandler extends AbstractWebSocketHandler {
    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        System.out.println("Received text: " + message.getPayloadLength() + " bytes");
        session.sendMessage(message);
    }

    @Override
    protected void handleBinaryMessage(WebSocketSession session, BinaryMessage message) throws Exception {
        System.out.println("Received binary: " + message.getPayloadLength() + " bytes");
        session.sendMessage(message);
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        System.out.println("WebSocket connection established");
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
        System.out.println("WebSocket connection closed");
    }
}
EOF

# Make the script executable (for future reference)
chmod +x ../create_nginx_java_websocket.sh

echo "Files created successfully in $PROJECT_DIR!"
echo "To build and run:"
echo "  cd $PROJECT_DIR"
echo "  sudo docker build -t nginx_websocket ."
echo "  sudo docker run -d --name nginx-test -p 8001:80 nginx_websocket"
