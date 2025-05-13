package com.duelo.client.utils;

import java.util.ArrayList;
import java.util.List;

public class MessageParser {
    public static Object parseMessage(String message) {
        if (!message.startsWith("!")) {
            return message;
        }

        List<Object> parsed = new ArrayList<>();
        String[] tokens = message.trim().split("\\s+");

        if (tokens.length == 0) {
            System.out.println("Received empty server message");
            return null;
        }

        for (String token : tokens) {
            parsed.add(parseToken(token));
        }

        return parsed;
    }

    private static Object parseToken(String token) {
        // Try Integer
        try {
            return Integer.parseInt(token);
        } catch (NumberFormatException ignored) {
        }

        // Try Float
        try {
            return Float.parseFloat(token);
        } catch (NumberFormatException ignored) {
        }

        // Return as string
        return token;
    }
}
