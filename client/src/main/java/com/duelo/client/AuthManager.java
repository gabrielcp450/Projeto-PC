package com.duelo.client;

import java.io.*;
import java.net.Socket;

public class AuthManager {
    private static AuthManager instance;
    private GameManager gameManager;

    private AuthManager() {
        this.gameManager = GameManager.getInstance();
    }

    public static AuthManager getInstance() {
        if (instance == null) {
            instance = new AuthManager();
        }
        return instance;
    }

    public boolean register(String username, String password) {
        try {
            if (!gameManager.connect()) return false;

            // Envia comando de registro com username e password
            gameManager.getOut().println("/c " + username + " " + password);
            String response = gameManager.getIn().readLine();
            System.out.println("Register response: " + response);

            // Envia comando de salvar
            gameManager.getOut().println("/save");
            response = gameManager.getIn().readLine();
            System.out.println("Save response: " + response);

            return response.equals("OK");
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean login(String username, String password) {
        try {
            if (!gameManager.connect()) return false;

            // Envia comando de login com username e password
            gameManager.getOut().println("/l " + username + " " + password);
            String response = gameManager.getIn().readLine();
            System.out.println("Login response: " + response);

            return response.equals("user logged in");
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean unregister(String username) {
        try {
            if (!gameManager.connect()) return false;

            // Envia comando de remoção de conta com username
            gameManager.getOut().println("/r " + username);
            String response = gameManager.getIn().readLine();
            System.out.println("Unregister response: " + response);

            // Envia comando save
            gameManager.getOut().println("/save");
            response = gameManager.getIn().readLine();
            System.out.println("Save response: " + response);

            return response.equals("OK");
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }
} 