package com.duelo.client;

import java.io.*;
import java.net.Socket;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.ArrayList;

public class GameManager {
    private static final String SERVER_HOST = "localhost";
    private static final int SERVER_PORT = 13556;
    
    private static GameManager instance;
    private Socket socket;
    private PrintWriter out;
    private BufferedReader in;
    private String username;
    private Game game;
    private ExecutorService executor;
    private boolean isInQueue = false;
    private String opponent = null;
    private List<RankingEntry> rankings = new ArrayList<>();

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
            if (socket != null && !socket.isClosed()) {
                return true;
            }
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

    public void setGame(Game game) {
        this.game = game;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void enterQueue() {
        if (!isInQueue && connect()) {
            out.println("/s"); // Comando para procurar partida
            isInQueue = true;
            System.out.println("Entering queue");
            startQueueListener();
        }
    }

    public void leaveQueue() {
        if (isInQueue && connect()) {
            out.println("/s-");
        }
    }

    private void startQueueListener() {
        new Thread(() -> {
            try {
                while (isInQueue) {
                    String response = in.readLine();
                    if (response != null) {
                        System.out.println("Server: " + response);
                        Object obj = MessageParser.parseMessage(response);
                        if (obj instanceof List) {
                            List<?> list = (List<?>)obj;

                            System.out.println("Received: " + list);
                            
                            if (((String)list.get(0)).compareTo("!found") == 0) {
                                int myid = (int)list.get(1);
                                String opponent = (String)list.get(2);
                                if (game != null) game.onMatchFound(myid, opponent);
                            }
                            else if (((String)list.get(0)).compareTo("!player_pos") == 0) {
                                if (game != null) game.onPlayerPositionChange((int)list.get(1), (float)list.get(2), (float)list.get(3));
                                System.out.println("Position update for "+ list.get(1) +" : " + list.get(2) + " : " + list.get(3));
                            }
                            else if (((String)list.get(0)).compareTo("!finished") == 0) {
                                System.out.println("Match finished with: " + list.get(1));
                                game.goToMenu();
                                isInQueue = false;
                            }
                            else if (((String)list.get(0)).compareTo("!cancelled") == 0) {
                                System.out.println("Match search cancelled");
                                game.goToMenu();
                                isInQueue = false;
                            }
                        }
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
                isInQueue = false;
            }
        }).start();
    }

    public boolean isInQueue() {
        return isInQueue;
    }

    public String getOpponent() {
        return opponent;
    }

    public void logout() throws IOException {
        if (socket == null || socket.isClosed()) {
            throw new IOException("Not connected to server");
        }
        if (out == null || in == null) {
            throw new IOException("Streams not initialized");
        }

        // Envia comando de logout
        System.out.println("Sending logout command");
        out.println("/e");

        // Espera a resposta do servidor
        String response = in.readLine();
        System.out.println("Server response to logout: " + response);

        // Fecha a conexão
        disconnect();
    }

    public void disconnect() {
        try {
            leaveQueue();
            if (socket != null && !socket.isClosed()) {
                socket.close();
            }
            executor.shutdown();
            // Limpa as referências
            socket = null;
            out = null;
            in = null;
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void sendCommand(String command) throws IOException {
        if (socket == null || socket.isClosed()) {
            throw new IOException("Not connected to server");
        }
        if (out == null) {
            throw new IOException("Output stream not initialized");
        }
        System.out.println("Sending command: " + command);
        out.println(command);
    }

    public List<RankingEntry> getRankings() {
        if (!connect()) {
            System.out.println("[DEBUG] Não foi possível conectar ao servidor.");
            return rankings;
        }

        try {
            System.out.println("[DEBUG] Enviando comando /t para obter rankings...");
            out.println("/t");
            StringBuilder sb = new StringBuilder();
            String response;
            boolean started = false;
            while ((response = in.readLine()) != null) {
                System.out.println("[DEBUG] Linha recebida: " + response);
                if (response.contains("Top10:")) {
                    started = true;
                    response = response.substring(response.indexOf("Top10:") + 6).trim();
                }
                if (started) {
                    sb.append(response);
                    if (response.trim().endsWith("}]")) {
                        break;
                    }
                }
            }
            String fullResponse = sb.toString();
            System.out.println("[DEBUG] Resposta completa: " + fullResponse);

            if (!fullResponse.isEmpty() && fullResponse.startsWith("[")) {
                rankings.clear();
                // Remove [ e ]
                fullResponse = fullResponse.substring(1, fullResponse.length() - 1);
                System.out.println("[DEBUG] Resposta após remover colchetes: " + fullResponse);
                // Separa cada entrada do ranking
                String[] entries = fullResponse.split("\\},\\s*\\{");
                for (String entry : entries) {
                    entry = entry.replaceAll("[\\{\\}\"]", ""); // remove {, }, "
                    System.out.println("[DEBUG] Entrada bruta: " + entry);
                    String[] parts = entry.split(",");
                    if (parts.length >= 4) {
                        String username = parts[0].trim();
                        int level = Integer.parseInt(parts[1].trim());
                        int winStreak = Integer.parseInt(parts[2].trim());
                        int lossStreak = Integer.parseInt(parts[3].trim());
                        System.out.println("[DEBUG] Parsed: " + username + " | " + level + " | " + winStreak + " | " + lossStreak);
                        rankings.add(new RankingEntry(username, level, winStreak, lossStreak));
                    } else {
                        System.out.println("[DEBUG] Entrada ignorada (partes < 4): " + entry);
                    }
                }
            } else {
                System.out.println("[DEBUG] Resposta inesperada ou nula do servidor.");
            }
        } catch (IOException | NumberFormatException e) {
            e.printStackTrace();
        }
        return rankings;
    }

    public static class RankingEntry {
        private String username;
        private int level;
        private int winStreak;
        private int lossStreak;

        public RankingEntry(String username, int level, int winStreak, int lossStreak) {
            this.username = username;
            this.level = level;
            this.winStreak = winStreak;
            this.lossStreak = lossStreak;
        }

        public String getUsername() { return username; }
        public int getLevel() { return level; }
        public int getWinStreak() { return winStreak; }
        public int getLossStreak() { return lossStreak; }
    }
} 