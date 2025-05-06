package com.duelo.client;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class MessageParser {
    private static final Set<String> RESERVED_KEYWORDS = Set.of(
        "!pos", "!proj"
    );

    public static Object parseMessage(String message) {
        if (!message.startsWith("!")) {
            return message;
        }

        List<Object> parsed = new ArrayList<>();
        String[] tokens = message.trim().split("\\s+");

        if (tokens.length == 0 || !RESERVED_KEYWORDS.contains(tokens[0])) {
            System.out.println("Invalid server message");
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
        } catch (NumberFormatException ignored) {}

        // Try Float
        try {
            return Float.parseFloat(token);
        } catch (NumberFormatException ignored) {}

        // Return as string
        return token;
    }
}
