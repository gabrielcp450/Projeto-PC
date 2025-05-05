package com.duelo.client;

import java.io.*;
import java.net.Socket;

public class AuthManager {
    private static final String SERVER_HOST = "localhost";
    private static final int SERVER_PORT = 12446;

    public AuthManager() {
    }

    public boolean register(String username, String password) {
        try (Socket sock = new Socket(SERVER_HOST, SERVER_PORT);
             PrintWriter out = new PrintWriter(sock.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()))) {
            
            // Lê a primeira linha (bem-vindo)
            String welcome = in.readLine();
            System.out.println("Server welcome: " + welcome);
            
            // Envia comando de criação de conta
            out.println("/c " + username + " " + password);
            String response = in.readLine();
            System.out.println("Register response: " + response);
            
            // Se o usuário já existe, retorna false imediatamente
            if (response != null && response.equals("username already used")) {
                return false;
            }
            
            // Se chegou aqui, o usuário foi criado com sucesso
            // Envia comando save
            out.println("/save");
            response = in.readLine();
            System.out.println("Save response: " + response);
            
            return response != null && response.equals("OK");
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean login(String username, String password) {
        try (Socket sock = new Socket(SERVER_HOST, SERVER_PORT);
             PrintWriter out = new PrintWriter(sock.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()))) {
            
            // Lê a primeira linha (bem-vindo)
            String welcome = in.readLine();
            System.out.println("Server welcome: " + welcome);
            
            // Envia comando de login
            out.println("/l " + username + " " + password);
            String response = in.readLine();
            System.out.println("Login response: " + response);
            
            return response != null && response.equals("user logged in");
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean unregister(String username) {
        try (Socket sock = new Socket(SERVER_HOST, SERVER_PORT);
             PrintWriter out = new PrintWriter(sock.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()))) {
            
            // Lê a primeira linha (bem-vindo)
            String welcome = in.readLine();
            System.out.println("Server welcome: " + welcome);
            
            // Envia comando de remoção de conta
            out.println("/r " + username);
            String response = in.readLine();
            System.out.println("Unregister response: " + response);
            
            // Envia comando save
            out.println("/save");
            response = in.readLine();
            System.out.println("Save response: " + response);
            
            return response != null && response.equals("OK");
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }
} 