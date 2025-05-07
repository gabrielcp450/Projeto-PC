package com.duelo.client;

import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class AuthManager {
    private static AuthManager instance;
    private GameManager gameManager;
    private boolean isLoggedIn = false;
    private String currentUser = null;

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

            if (response.equals("user created")) {
                // Espera pela resposta do save que o servidor envia automaticamente
            response = gameManager.getIn().readLine();
            System.out.println("Save response: " + response);
                return response.equals("save completed");
            }
            return false;
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

            if (response.equals("user logged in")) {
                isLoggedIn = true;
                currentUser = username;
                return true;
            }
            return false;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean logout(String username) {
        if (!isLoggedIn || !username.equals(currentUser)) {
            return false;
        }

        try {
            // Envia comando de logout usando a conexão existente
            gameManager.getOut().println("/e");
            String response = gameManager.getIn().readLine();
            System.out.println("Logout response: " + response);

            if (response.equals("user logged out")) {
                isLoggedIn = false;
                currentUser = null;
                return true;
            }
            return false;
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
            // Envia comando de remoção de conta com username
            gameManager.getOut().println("/d");
            String response = gameManager.getIn().readLine();
            System.out.println("Unregister response: " + response);

            if (response.equals("user deleted")) {
                isLoggedIn = false;
                currentUser = null;
                return true;
            }
            return false;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean isLoggedIn() {
        return isLoggedIn;
    }

    public String getCurrentUser() {
        return currentUser;
    }

    public List<RankingEntry> getRankings() {
        try {
            // Conecta ao servidor
            Socket socket = new Socket("localhost", 1234);
            ObjectOutputStream out = new ObjectOutputStream(socket.getOutputStream());
            ObjectInputStream in = new ObjectInputStream(socket.getInputStream());
            
            // Envia requisição de rankings
            out.writeObject("top10");
            out.flush();
            
            // Recebe lista de rankings
            @SuppressWarnings("unchecked")
            List<Object[]> rankings = (List<Object[]>) in.readObject();
            
            // Converte para RankingEntry
            List<RankingEntry> entries = new ArrayList<>();
            for (Object[] entry : rankings) {
                String username = (String) entry[0];
                int level = (int) entry[1];
                entries.add(new RankingEntry(username, level));
            }
            
            // Fecha conexão
            socket.close();
            
            return entries;
        } catch (Exception e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }
} 