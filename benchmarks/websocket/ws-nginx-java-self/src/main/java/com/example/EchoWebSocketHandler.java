package com.example;

import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.AbstractWebSocketHandler;

public class EchoWebSocketHandler extends AbstractWebSocketHandler {
    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        try {
            System.out.println("Received text: " + message.getPayloadLength() + " bytes");
            session.sendMessage(message);
        } catch (Exception e) {
            System.err.println("Exception in handleTextMessage: ");
            e.printStackTrace();
            throw e;
        }
    }

    @Override
    protected void handleBinaryMessage(WebSocketSession session, BinaryMessage message) throws Exception {
        try {
            System.out.println("Received binary: " + message.getPayloadLength() + " bytes");
            session.sendMessage(message);
        } catch (Exception e) {
            System.err.println("Exception in handleBinaryMessage: ");
            e.printStackTrace();
            throw e;
        }
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
