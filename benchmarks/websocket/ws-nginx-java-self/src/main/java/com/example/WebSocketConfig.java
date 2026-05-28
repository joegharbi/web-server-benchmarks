package com.example;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import org.springframework.web.socket.server.standard.ServletServerContainerFactoryBean;

@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {
    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(new EchoWebSocketHandler(), "/ws").setAllowedOrigins("*");
    }

    // Mimic Python/Tornado: allow many concurrent sessions, large messages, long idle
    @Bean
    public ServletServerContainerFactoryBean createWebSocketContainer() {
        ServletServerContainerFactoryBean container = new ServletServerContainerFactoryBean();
        container.setMaxTextMessageBufferSize(64 * 1024 * 1024); // 64MB
        container.setMaxBinaryMessageBufferSize(64 * 1024 * 1024); // 64MB
        container.setMaxSessionIdleTimeout(600000L); // 10 minutes
        container.setAsyncSendTimeout(30000L); // 30 seconds
        // container.setMaxSessions(500); // Allow up to 500 concurrent sessions (method does not exist)
        return container;
    }
}