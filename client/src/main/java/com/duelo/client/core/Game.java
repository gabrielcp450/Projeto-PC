package com.duelo.client.core;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

import com.duelo.client.entities.RankingEntry;
import com.duelo.client.states.LoginState;
import com.duelo.client.states.MenuState;
import com.duelo.client.states.PlayState;
import com.duelo.client.states.QueueState;
import com.duelo.client.states.RankingsState;
import com.duelo.client.ui.Constants;
import com.duelo.client.utils.MessageParser;

import processing.core.PApplet;
import processing.event.MouseEvent;

public class Game extends PApplet {
    private static final String SERVER_HOST = "localhost";
    private static final int SERVER_PORT = 13556;

    private static final int WINDOW_WIDTH = 900;
    private static final int WINDOW_HEIGHT = 600;
    
    private static Game instance;
    private Auth authManager;
    private Socket socket;
    private PrintWriter out;
    private BufferedReader in;
    private boolean isListening = false;
    
    // States
    private GameState currentState = GameState.LOGIN;
    private MenuState menuState;
    private LoginState loginState;
    private QueueState queueState;
    private PlayState playState;
    private RankingsState rankingsState;
    
    // Game data
    private List<RankingEntry> rankings = new ArrayList<>();
    
    public Game() {
        this.authManager = new Auth(this);
    }
    
    public static Game getInstance() {
        if (instance == null) {
            instance = new Game();
        }
        return instance;
    }

    public Auth getAuthManager() {
        return authManager;
    }
    
    // Connection and server communication methods
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
    
    // State management
    public void changeState(GameState state) {
        this.currentState = state;
    }
    
    // Processing methods
    public void settings() {
        size(WINDOW_WIDTH, WINDOW_HEIGHT);
    }
    
    public void setup() {
        if (this.args.length == 2) {
            if (authManager.login(args[0], args[1]) == null) {
                changeState(GameState.MENU);
            }
        } else {
            System.out.println("Usage: java -jar client.jar <username> <password>");
            exit();
        }

        menuState = new MenuState(this);
        loginState = new LoginState(this);
        queueState = new QueueState(this);
        playState = new PlayState(this);
        rankingsState = new RankingsState(this);
    }
    
    public void draw() {
        background(Constants.BACKGROUND_COLOR);

        switch (currentState) {
            case LOGIN: loginState.draw(); break;
            case MENU: menuState.draw(); break;
            case QUEUE: queueState.draw(); break;
            case PLAY: playState.draw(); break;
            case RANKINGS: rankingsState.draw(); break;
        }
    }

    public void keyPressed() {
        switch (currentState) {
            case LOGIN: loginState.keyPressed(key, keyCode); break;
            case PLAY: playState.keyPressed(key, keyCode); break;
        }
    }
    
    public void keyReleased() {
        switch (currentState) {
            case PLAY: playState.keyReleased(key, keyCode); break;
        }
    }
    
    public void keyTyped() {
    }

    public void mouseWheel(MouseEvent event) {
        switch (currentState) {
            case RANKINGS: rankingsState.mouseWheel(event); break;
        }
    }

    public void mousePressed() {
        switch (currentState) {
            case LOGIN: loginState.mousePressed(); break;
            case MENU: menuState.mousePressed(); break;
            case QUEUE: queueState.mousePressed(); break;
            case RANKINGS: rankingsState.mousePressed(); break;
            case PLAY: playState.mousePressed(); break;
        }
    }

    public void sendCommand(String command) {
        // System.out.println("[CLIENT] Sending: " + command);
        out.println(command);
    }

    public String readResponse() throws IOException {
        return in.readLine();
    }

    public void enterQueue() {
        if (connect()) {
            out.println("/s"); // Comando para procurar partida
            System.out.println("Entering queue");
            changeState(GameState.QUEUE);
            startAsyncListener();
        }
    }

    public void leaveQueue() {
        if (connect()) {
            out.println("/s-");
        }
    }
    
    public String getUsername() {
        return authManager.getCurrentUser();
    }

    private void startAsyncListener() {
        isListening = true;
        new Thread(() -> {
            try {
                while (isListening) {
                    String response = in.readLine();
                    if (response != null) {
                        handleServerMessage(response);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
                isListening = false;
            }
        }).start();
    }

    // Message handling from server
    private void handleServerMessage(String message) {
        Object obj = MessageParser.parseMessage(message);
        if (obj instanceof List) {
            List<?> list = (List<?>)obj;
            String command = (String)list.get(0);

            // System.out.println("Received " + list);
            
            switch (command) {
                case "!found": // Match found: !found <playerId> <opponentName>
                    if (list.size() >= 3) {
                        int playerId = (Integer) list.get(1);
                        String opponent = (String) list.get(2);
                        playState.onMatchFound(playerId, opponent);
                        changeState(GameState.PLAY);
                    }
                    break;
                    
                case "!player_pos": // Player position update: !player_pos <id> <x> <y>
                    if (list.size() >= 4) {
                        int playerId = (Integer) list.get(1);
                        float x = list.get(2) instanceof Float ? (Float) list.get(2) : ((Integer) list.get(2)).floatValue();
                        float y = list.get(3) instanceof Float ? (Float) list.get(3) : ((Integer) list.get(3)).floatValue();
                        playState.onPlayerPositionChange(playerId, x, y);
                    }
                    break;

                case "!modifier_pos": // Modifier position update: !modifier_pos <id> <x> <y>
                    if (list.size() >= 4) {
                        int modId = (Integer) list.get(1);
                        float x = list.get(2) instanceof Float ? (Float) list.get(2) : ((Integer) list.get(2)).floatValue();
                        float y = list.get(3) instanceof Float ? (Float) list.get(3) : ((Integer) list.get(3)).floatValue();
                        System.out.println("[DEBUG] Modifier position update: " + modId + " " + x + " " + y);
                        // playState.onModifierPositionUpdate(modId, x, y);
                    }
                    break;

                case "!player_aim": // Player aim update: !player_aim <id> <x> <y>
                    if (list.size() >= 4) {
                        int playerId = (Integer) list.get(1);
                        float x = list.get(2) instanceof Float ? (Float) list.get(2) : ((Integer) list.get(2)).floatValue();
                        float y = list.get(3) instanceof Float ? (Float) list.get(3) : ((Integer) list.get(3)).floatValue();
                        playState.onPlayerAimChange(playerId, x, y);
                    }
                    break;

                case "!proj_pos": // Projectile position: !proj_pos <id> <x> <y>
                    if (list.size() >= 4) {
                        int projId = (Integer) list.get(1);
                        float x = list.get(2) instanceof Float ? (Float) list.get(2) : ((Integer) list.get(2)).floatValue();
                        float y = list.get(3) instanceof Float ? (Float) list.get(3) : ((Integer) list.get(3)).floatValue();
                        playState.onProjPositionChange(projId, x, y);
                    }
                    break;

                case "!proj_rem": // Projectile removed: !proj_rem <id>
                    if (list.size() >= 2) {
                        int projId = (Integer) list.get(1);
                        playState.onProjRemoved(projId);
                    }
                    break;
                    
                case "!score": // Score update: !score <playerScore> <opponentScore>
                    if (list.size() >= 3) {
                        int score0 = (Integer) list.get(1);
                        int score1 = (Integer) list.get(2);
                        if (playState != null) {
                            playState.updateScore(score0, score1);
                        }
                    } else {
                        System.out.println("[DEBUG] Invalid score message format: " + list);
                    }
                    break;
                    
                case "!finished": // Match finished: !finished <result>
                    if (list.size() >= 2) {
                        String result = (String) list.get(1);
                        // playState.onMatchFinished(result);
                        isListening = false;
                        changeState(GameState.MENU);
                    }
                    break;
                    
                case "!cancelled": // Queue cancelled
                    changeState(GameState.MENU);
                    isListening = false;
                    break;
                    
                default:
                    System.err.println("Unknown server command: " + command);
            }
        }
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
}