package com.duelo.client.utils;

import java.io.IOException;

import com.duelo.client.core.Game;

public class Auth {
    private final Game game;
    private boolean isLoggedIn = false;
    private String currentUser = null;

    public Auth(Game game) {
        this.game = game;
    }

    public String register(String username, String password) {
        try {
            if (!game.connect())
                return "";

            game.sendCommand("/c " + username + " " + password);
            String response = game.readResponse();

            if (!response.startsWith("!")) {
                return response;
            }

            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return "";
        }
    }

    public String login(String username, String password) {
        try {
            if (!game.connect())
                return "";

            game.sendCommand("/l " + username + " " + password);
            String response = game.readResponse();

            if (!response.startsWith("!")) {
                return response;
            }

            isLoggedIn = true;
            currentUser = username;
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return "";
        }
    }

    public boolean logout() {
        if (!isLoggedIn) {
            return false;
        }

        try {
            game.sendCommand("/e");
            String response = game.readResponse();

            if (!response.startsWith("!")) {
                return false;
            }

            isLoggedIn = false;
            currentUser = null;
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean unregister(String username) {
        if (!isLoggedIn || !username.equals(currentUser)) {
            return false;
        }

        try {
            game.sendCommand("/d");
            String response = game.readResponse();

            if (!response.startsWith("!")) {
                return false;
            }

            isLoggedIn = false;
            currentUser = null;
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public String changePassword(String oldPassword, String newPassword) {
        try {
            if (!isLoggedIn) {
                return "You must be logged in.";
            }
            if (!game.connect())
                return "Connection error.";

            // O servidor espera: /c <newPassword>
            game.sendCommand("/c " + newPassword);
            String response = game.readResponse();

            if (!response.startsWith("!")) {
                return response; // Error message from server
            }
            return null; // Success
        } catch (IOException e) {
            e.printStackTrace();
            return "Error communicating with server.";
        }
    }

    // Getters
    public boolean isLoggedIn() {
        return isLoggedIn;
    }
    public String getCurrentUser() {
        return currentUser;
    }
}
