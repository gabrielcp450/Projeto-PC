package com.duelo.client;

import java.io.*;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.swing.SwingUtilities;

public class GameManager {
    private static final String SERVER_HOST = "localhost";
    private static final int SERVER_PORT = 12547;
    
    private static GameManager instance;
    private Socket socket;
    private PrintWriter out;
    private BufferedReader in;
    private String username;
    private GamePanel gamePanel;
    private ExecutorService executor;
    private boolean inGame = false;

    private GameManager() {
        this.executor = Executors.newSingleThreadExecutor();
    }

    public static GameManager getInstance() {
        if (instance == null) {
            instance = new GameManager();
        }
        return instance;
    }

    public Socket getSocket() {
        return socket;
    }

    public PrintWriter getOut() {
        return out;
    }

    public BufferedReader getIn() {
        return in;
    }

    public boolean connect() {
        try {
            socket = new Socket(SERVER_HOST, SERVER_PORT);
            out = new PrintWriter(socket.getOutputStream(), true);
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            
            // Lê a mensagem de boas-vindas do servidor
            String welcomeMessage = in.readLine();
            System.out.println("Server: " + welcomeMessage);
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    public void setGamePanel(GamePanel gamePanel) {
        this.gamePanel = gamePanel;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void enterQueue() {
        executor.execute(() -> {
            try {
                // Envia comando para entrar na fila
                out.println("/s");
                System.out.println("Entered queue");

                // Aguarda resposta do servidor
                String response;
                while ((response = in.readLine()) != null) {
                    System.out.println("Server: " + response);
                    if (response.startsWith("match found:")) {
                        inGame = true;
                        SwingUtilities.invokeLater(() -> {
                            if (gamePanel != null) {
                                gamePanel.onGameStart();
                            }
                        });
                        break;
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    public void leaveQueue() {
        if (inGame) {
            out.println("/leave");
            inGame = false;
        }
    }

    public void sendPlayerPosition(double x, double y) {
        if (inGame) {
            out.println(String.format("/pos %.2f %.2f", x, y));
        }
    }

    public void sendProjectile(double x, double y, double angle) {
        if (inGame) {
            out.println(String.format("/proj %.2f %.2f %.2f", x, y, angle));
        }
    }

    public boolean isInGame() {
        return inGame;
    }

    public void disconnect() {
        try {
            leaveQueue();
            if (socket != null && !socket.isClosed()) {
                socket.close();
            }
            executor.shutdown();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
} 