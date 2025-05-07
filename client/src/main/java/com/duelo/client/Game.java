package com.duelo.client;

import processing.core.PApplet;
import processing.core.PFont;
import processing.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe principal do jogo Duelo implementada com Processing.
 * Gerencia os estados do jogo, interface do usuário e lógica principal.
 * 
 * Estados do jogo:
 * - LOGIN: Tela de autenticação
 * - MENU: Menu principal
 * - QUEUE: Busca de partida
 * - GAME: Jogo em andamento
 * - RANKINGS: Visualização de rankings
 * - PROFILE: Perfil do jogador
 * 
 * Controles:
 * - Setas: Movimento do jogador
 * - Mouse: Mira e tiro
 * - TAB: Alternar entre campos de login
 * - ENTER: Confirmar login
 */
public class Game extends PApplet {
    // Window dimensions
    private static final int WINDOW_WIDTH = 900;
    private static final int WINDOW_HEIGHT = 600;
    
    // UI Colors
    private int BACKGROUND_COLOR;
    private int PRIMARY_COLOR;
    private int SECONDARY_COLOR;
    private int ACCENT_COLOR;
    private int TEXT_COLOR;
    private int ERROR_COLOR;
    
    // UI Dimensions
    private static final int BUTTON_WIDTH = 250;
    private static final int BUTTON_HEIGHT = 50;
    private static final int INPUT_WIDTH = 300;
    private static final int INPUT_HEIGHT = 40;
    private static final int PADDING = 20;
    
    // Game states
    private enum GameState {
        LOGIN, MENU, QUEUE, GAME, RANKINGS, PROFILE
    }
    
    // Current state
    private GameState currentState;
    private String currentUsername;
    
    // UI elements
    private String usernameInput = "";
    private String passwordInput = "";
    private boolean isTypingUsername = true;
    private String errorMessage = "";
    private PFont titleFont;
    private PFont buttonFont;
    private PFont inputFont;
    
    // Auth manager
    private AuthManager authManager;
    private GameManager gameManager;
    
    // Game elements
    private Player[] players = {new Player(0, 0, 0xff0000ff), new Player(0, 0, 0xffff0000)};
    private int myPlayerId;
    // private List<Modifier> modifiers;
    
    // Cache
    private List<GameManager.RankingEntry> cachedRankings = new ArrayList<>();
    private boolean rankingsLoaded = false;
    
    // Scroll variables
    private float rankingScrollY = 0;
    private float rankingMaxScroll = 0;

    public void settings() {
        size(WINDOW_WIDTH, WINDOW_HEIGHT);
    }
    
    public void setup() {
        if (this.args != null) {
            for (int i = 0; i < this.args.length; i++) {
                System.out.println("Argument " + i + " : " + this.args[i]);
            }
        }
        if (this.args.length == 2) {
            this.usernameInput = args[0];
            this.passwordInput = args[1];
        }
        surface.setResizable(false);
        currentState = GameState.LOGIN;
        textAlign(CENTER, CENTER);
        rectMode(CENTER);
        
        // Inicializa cores
        BACKGROUND_COLOR = color(240);
        PRIMARY_COLOR = color(50, 150, 200);
        SECONDARY_COLOR = color(100, 150, 200);
        ACCENT_COLOR = color(200, 100, 100);
        TEXT_COLOR = color(50);
        ERROR_COLOR = color(200, 50, 50);
        
        // Carrega fontes
        titleFont = createFont("Arial-Bold", 32);
        buttonFont = createFont("Arial-Bold", 20);
        inputFont = createFont("Arial", 16);
        
        // Initialize managers
        authManager = AuthManager.getInstance();
        gameManager = GameManager.getInstance();
        gameManager.setGame(this);
        
        // Initialize game elements
        // modifiers = new ArrayList<>();
        if (authManager.login(usernameInput, passwordInput)) {
            currentUsername = usernameInput;
            currentState = GameState.MENU;
            errorMessage = "";
        } else {
            errorMessage = "Invalid username or password";
        }
    }
    
    public void draw() {
        background(BACKGROUND_COLOR);
        
        switch (currentState) {
            case LOGIN:
                drawLogin();
                break;
            case MENU:
                drawMenu();
                break;
            case QUEUE:
                drawQueue();
                break;
            case GAME:
                updateGame();
                drawGame();
                break;
            case RANKINGS:
                drawRankings();
                break;
            case PROFILE:
                drawProfile();
                break;
        }
    }
    
    private void drawLogin() {
        // Title
        textFont(titleFont);
        fill(TEXT_COLOR);
        text("Duelo Game", width/2, 100);
        
        // Error message
        if (!errorMessage.isEmpty()) {
            textFont(inputFont);
            fill(ERROR_COLOR);
            text(errorMessage, width/2, 150);
        }
        
        // Input fields
        drawInputField(width/2, 250, "Username", usernameInput, isTypingUsername);
        drawInputField(width/2, 320, "Password", "*".repeat(passwordInput.length()), !isTypingUsername);
        
        // Buttons
        drawButton(width/2, 400, "Login", PRIMARY_COLOR);
        drawButton(width/2, 470, "Register", SECONDARY_COLOR);
    }
    
    private void drawMenu() {
        // Title
        textFont(titleFont);
        fill(TEXT_COLOR);
        text("Main Menu", width/2, 100);
        
        // Welcome message
        textFont(inputFont);
        text("Welcome, " + currentUsername, width/2, 150);
        
        // Menu buttons
        drawButton(width/2, 250, "Play", PRIMARY_COLOR);
        drawButton(width/2, 330, "Rankings", SECONDARY_COLOR);
        drawButton(width/2, 410, "Profile", SECONDARY_COLOR);
        drawButton(width/2, 490, "Logout", ACCENT_COLOR);
    }
    
    private void drawInputField(float x, float y, String label, String value, boolean isSelected) {
        // Label
        textFont(inputFont);
        fill(TEXT_COLOR);
        textAlign(RIGHT, CENTER);
        text(label + ":", x - INPUT_WIDTH/2 - PADDING, y);
        textAlign(CENTER, CENTER);
        
        // Input box
        stroke(isSelected ? PRIMARY_COLOR : color(200));
        strokeWeight(2);
        fill(255);
        rect(x, y, INPUT_WIDTH, INPUT_HEIGHT, 5);
        
        // Input text
        fill(TEXT_COLOR);
        textAlign(LEFT, CENTER);
        text(value, x - INPUT_WIDTH/2 + PADDING, y);
        textAlign(CENTER, CENTER);
    }
    
    private void drawButton(float x, float y, String label, int color) {
        // Button background
        noStroke();
        fill(color);
        rect(x, y, BUTTON_WIDTH, BUTTON_HEIGHT, 5);
        
        // Button text
        textFont(buttonFont);
        fill(255);
        text(label, x, y);
    }
    
    private void drawQueue() {
        // Title
        textFont(titleFont);
        fill(TEXT_COLOR);
        text("Searching for Match", width/2, height/2 - 50);
        
        // Loading animation
        float size = 50 + 10 * sin(frameCount * 0.1f);
        noFill();
        stroke(PRIMARY_COLOR);
        strokeWeight(3);
        ellipse(width/2, height/2 + 50, size, size);
        
        // Cancel button
        drawButton(width/2, height - 100, "Cancel Search", ACCENT_COLOR);
    }
    
    private void drawGame() {
        // Game area
        background(200);
        
        // // Draw modifiers
        // for (Modifier modifier : modifiers) {
        //     modifier.draw(this);
        // }
        
        // Draw players if they exist
        for (Player player : players) {
            player.draw(this);
        }
    }
    
    private void drawRankings() {
        background(BACKGROUND_COLOR);
        
        // Título
        textFont(titleFont);
        fill(TEXT_COLOR);
        textAlign(CENTER, CENTER);
        text("Rankings", width/2, 60);
        
        // Parâmetros da tabela
        int tableWidth = 700;
        int colRank = width/2 - tableWidth/2 + 40;
        int colPlayer = colRank + 80;
        int colLevel = colPlayer + 220;
        int colWin = colLevel + 100;
        int colLoss = colWin + 100;
        int rowStart = 130; // Cabeçalho ainda mais acima
        int rowHeight = 48;
        int headerHeight = 54;
        int numCols = 5;
        int visibleRows = 7;
        int tableX = width/2 - tableWidth/2;
        int tableY = rowStart - headerHeight/2;
        int tableHeight = headerHeight + visibleRows * rowHeight;
        int usersAreaTop = rowStart + headerHeight/2 + 16; // Limite superior da área dos users (mais abaixo)
        int usersAreaBottom = usersAreaTop + visibleRows * rowHeight - 16; // Limite inferior (mais restrito)

        // Fundo da tabela
        fill(255, 255, 255, 220);
        stroke(220);
        strokeWeight(2);
        rect(width/2, tableY + tableHeight/2, tableWidth, tableHeight + 20, 18);

        // Cabeçalho (fixo)
        fill(PRIMARY_COLOR);
        noStroke();
        rect(width/2, rowStart, tableWidth, headerHeight, 16);
        textFont(buttonFont);
        fill(255);
        textAlign(LEFT, CENTER);
        text("Rank", colRank, rowStart);
        text("Player", colPlayer, rowStart);
        text("Level", colLevel, rowStart);
        text("Win Streak", colWin, rowStart);
        text("Loss Streak", colLoss, rowStart);

        // Dados
        List<GameManager.RankingEntry> rankings = cachedRankings;
        textFont(inputFont);
        int totalRows = rankings.size();
        rankingMaxScroll = max(0, (totalRows - visibleRows) * rowHeight);
        rankingScrollY = constrain(rankingScrollY, 0, rankingMaxScroll);
        for (int i = 0; i < totalRows; i++) {
            int y = rowStart + headerHeight/2 + (i+1)*rowHeight - (int)rankingScrollY;
            // Só desenha linhas de dados dentro da área visível (abaixo do cabeçalho)
            if (y < usersAreaTop || y > usersAreaBottom) continue;
            // Linhas alternadas
            if (i % 2 == 0) {
                fill(240, 245, 255, 180);
            } else {
                fill(255, 255, 255, 180);
            }
            noStroke();
            rect(width/2, y, tableWidth-8, rowHeight-6, 12);

            // Destaque para o top 3
            if (i == 0) fill(255, 215, 0); // Ouro
            else if (i == 1) fill(192, 192, 192); // Prata
            else if (i == 2) fill(205, 127, 50); // Bronze
            else fill(TEXT_COLOR);
            textAlign(LEFT, CENTER);
            text((i+1) + ".", colRank, y);

            fill(TEXT_COLOR);
            text(rankings.get(i).getUsername(), colPlayer, y);
            text(String.valueOf(rankings.get(i).getLevel()), colLevel, y);
            text(String.valueOf(rankings.get(i).getWinStreak()), colWin, y);
            text(String.valueOf(rankings.get(i).getLossStreak()), colLoss, y);
        }

        // Botão voltar
        int btnW = 220, btnH = 54;
        int btnY = height - 70;
        fill(PRIMARY_COLOR);
        stroke(180);
        strokeWeight(2);
        rect(width/2, btnY, btnW, btnH, 16);
        fill(255);
        textFont(buttonFont);
        textAlign(CENTER, CENTER);
        text("Back", width/2, btnY);
    }
    
    private void drawProfile() {
        // TODO: Implement profile display
        textFont(titleFont);
        fill(TEXT_COLOR);
        text("Profile", width/2, height/2);
    }
    
    public void mousePressed() {
        switch (currentState) {
            case LOGIN:
                handleLoginClick();
                break;
            case MENU:
                handleMenuClick();
                break;
            case QUEUE:
                handleQueueClick();
                break;
            case GAME:
                handleGameClick();
                break;
            case RANKINGS:
                handleRankingsClick();
                break;
        }
    }
    
    private void handleLoginClick() {
        // Login button
        if (isMouseOver(width/2, 400, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            if (usernameInput.isEmpty() || passwordInput.isEmpty()) {
                errorMessage = "Please fill in all fields";
                return;
            }
            
            if (authManager.login(usernameInput, passwordInput)) {
                currentUsername = usernameInput;
                currentState = GameState.MENU;
                errorMessage = "";
            } else {
                errorMessage = "Invalid username or password";
            }
        }
        
        // Register button
        if (isMouseOver(width/2, 470, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            if (usernameInput.isEmpty() || passwordInput.isEmpty()) {
                errorMessage = "Please fill in all fields";
                return;
            }
            
            if (authManager.register(usernameInput, passwordInput)) {
                errorMessage = "Registration successful! Please login.";
                usernameInput = "";
                passwordInput = "";
            } else {
                errorMessage = "Registration failed. Username might be taken.";
            }
        }
        
        // Input fields
        if (isMouseOver(width/2, 250, INPUT_WIDTH, INPUT_HEIGHT)) {
            isTypingUsername = true;
        } else if (isMouseOver(width/2, 320, INPUT_WIDTH, INPUT_HEIGHT)) {
            isTypingUsername = false;
        }
    }
    
    private void handleMenuClick() {
        // Play button
        if (isMouseOver(width/2, 250, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            currentState = GameState.QUEUE;
            gameManager.enterQueue();
        }
        
        // Rankings button
        if (isMouseOver(width/2, 330, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            currentState = GameState.RANKINGS;
            if (!rankingsLoaded) {
                cachedRankings = gameManager.getRankings();
                rankingsLoaded = true;
            }
        }
        
        // Profile button
        if (isMouseOver(width/2, 410, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            currentState = GameState.PROFILE;
        }
        
        // Logout button
        if (isMouseOver(width/2, 490, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            if (authManager.logout(currentUsername)) {
                currentState = GameState.LOGIN;
                usernameInput = "";
                passwordInput = "";
                errorMessage = "";
            }
        }
    }
    
    private boolean isMouseOver(float x, float y, float w, float h) {
        return mouseX > x - w/2 && mouseX < x + w/2 &&
               mouseY > y - h/2 && mouseY < y + h/2;
    }
    
    private void handleGameClick() {
    }
    
    private void handleRankingsClick() {
        // Back button
        if (isMouseOver(width/2, height - 70, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            currentState = GameState.MENU;
            rankingsLoaded = false;
            rankingScrollY = 0;
        }
    }
    
    private void handleQueueClick() {
        // Cancel button
        if (isMouseOver(width/2, height - 100, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            gameManager.leaveQueue();
        }
    }

    public void goToMenu() {
        currentState = GameState.MENU;
    }
    
    public void onGameStart() {
        currentState = GameState.GAME;
    }

    public void onMatchFound(int myid, String opponent) {
        currentState = GameState.GAME;
        myPlayerId = myid;
    }

    public void onPlayerPositionChange(int id, float x, float y) {
        players[id].setPosition((int)(x * WINDOW_WIDTH), (int)(y * WINDOW_HEIGHT));
    }
    
    public void keyPressed() {
        if (currentState == GameState.LOGIN) {
            handleLoginKey(true);
        } else if (currentState == GameState.GAME) {
            handleGameKey(true);
        }
    }
    
    private void handleLoginKey(boolean pressed) {
        if (key == BACKSPACE) {
            if (isTypingUsername && usernameInput.length() > 0) {
                usernameInput = usernameInput.substring(0, usernameInput.length() - 1);
            } else if (!isTypingUsername && passwordInput.length() > 0) {
                passwordInput = passwordInput.substring(0, passwordInput.length() - 1);
            }
        } else if (key == TAB) {
            isTypingUsername = !isTypingUsername;
        } else if (key == ENTER) {
            // Try to login
            if (authManager.login(usernameInput, passwordInput)) {
                currentUsername = usernameInput;
                currentState = GameState.MENU;
                errorMessage = "";
            } else {
                errorMessage = "Invalid username or password";
            }
        } else if (key != CODED && (key >= ' ' && key <= '~')) {
            if (isTypingUsername) {
                usernameInput += key;
            } else {
                passwordInput += key;
            }
        }
    }
    
    private void handleGameKey(boolean pressed) {
        if (key == CODED) {
            switch (keyCode) {
                case UP:
                    break;
                case DOWN:
                    break;
                case LEFT:
                    break;
                case RIGHT:
                    break;
            }
        } else {
            switch (key) {
                case 'w':
                case 'W':
                    break;
                case 's':
                case 'S':
                    break;
                case 'a':
                case 'A':
                    break;
                case 'd':
                case 'D':
                    break;
            }
        }
    }
    
    public void keyReleased() {
        if (currentState == GameState.GAME) {
            handleGameKey(false);
        }
    }

    
    private void updateGame() {
    }
    
    public void mouseWheel(MouseEvent event) {
        if (currentState == GameState.RANKINGS) {
            float e = event.getCount();
            rankingScrollY += e * 32; // 32 pixels por scroll
            rankingScrollY = constrain(rankingScrollY, 0, rankingMaxScroll);
            System.out.println("[DEBUG] mouseWheel chamado: e=" + e + ", rankingScrollY=" + rankingScrollY);
        }
    }
} 