package com.duelo.client;

import processing.core.PApplet;
import processing.core.PFont;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Classe principal do jogo Duelo implementada com Processing.
 * Gerencia os estados do jogo, interface do usuário e lógica principal.
 * 
 * Estados do jogo:
 * - LOGIN: Tela de autenticação
 * - MENU: Menu principal
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
        LOGIN, MENU, GAME, RANKINGS, PROFILE
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
    
    // Game elements
    private Player player;
    private Player opponent;
    private List<Modifier> modifiers;
    private Random random;
    private long lastModifierSpawn;
    private static final int MODIFIER_SPAWN_INTERVAL = 5000; // 5 segundos
    private static final int MAX_MODIFIERS_PER_TYPE = 3;
    
    public void settings() {
        size(WINDOW_WIDTH, WINDOW_HEIGHT);
    }
    
    public void setup() {
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
        
        // Initialize auth manager
        authManager = AuthManager.getInstance();
        
        // Initialize game elements
        random = new Random();
        modifiers = new ArrayList<>();
        lastModifierSpawn = 0;
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
    
    private void drawGame() {
        // Game area
        background(200);
        
        // Draw modifiers
        for (Modifier modifier : modifiers) {
            modifier.draw(this);
        }
        
        // Draw players if they exist
        if (player != null) {
            player.draw(this);
        }
        if (opponent != null) {
            opponent.draw(this);
        }
    }
    
    private void drawRankings() {
        // Title
        textFont(titleFont);
        fill(TEXT_COLOR);
        text("Rankings", width/2, 80);
        
        // Back button
        drawButton(width/2, height - 50, "Back to Menu", SECONDARY_COLOR);
        
        // Rankings table header
        textFont(buttonFont);
        fill(TEXT_COLOR);
        textAlign(LEFT, CENTER);
        text("Rank", width/4, 150);
        text("Player", width/2, 150);
        text("Level", width * 3/4, 150);
        
        // Get rankings from server
        List<RankingEntry> rankings = authManager.getRankings();
        
        // Draw rankings
        textFont(inputFont);
        for (int i = 0; i < rankings.size(); i++) {
            RankingEntry entry = rankings.get(i);
            float y = 200 + i * 40;
            
            // Rank number
            fill(TEXT_COLOR);
            textAlign(LEFT, CENTER);
            text("#" + (i + 1), width/4, y);
            
            // Player name
            text(entry.getUsername(), width/2, y);
            
            // Level
            textAlign(RIGHT, CENTER);
            text("Level " + entry.getLevel(), width * 3/4, y);
        }
        
        // Reset text alignment
        textAlign(CENTER, CENTER);
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
            currentState = GameState.GAME;
            initializeGame();
        }
        
        // Rankings button
        if (isMouseOver(width/2, 330, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            currentState = GameState.RANKINGS;
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
        if (player != null) {
            player.shoot(mouseX, mouseY);
        }
    }
    
    private void handleRankingsClick() {
        // Back button
        if (isMouseOver(width/2, height - 50, BUTTON_WIDTH, BUTTON_HEIGHT)) {
            currentState = GameState.MENU;
        }
    }
    
    public void onGameStart() {
        currentState = GameState.GAME;
        initializeGame();
    }
    
    private void initializeGame() {
        player = new Player(100, height/2, color(0, 0, 255), this); // Blue player
        opponent = new Player(width-100, height/2, color(255, 0, 0), this); // Red opponent
        modifiers.clear();
        lastModifierSpawn = System.currentTimeMillis();
    }
    
    public void keyPressed() {
        if (currentState == GameState.LOGIN) {
            handleLoginKeyPress();
        } else if (currentState == GameState.GAME) {
            handleGameKeyPress();
        }
    }
    
    private void handleLoginKeyPress() {
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
    
    private void handleGameKeyPress() {
        if (key == CODED) {
            switch (keyCode) {
                case UP:
                    player.setMovingUp(true);
                    break;
                case DOWN:
                    player.setMovingDown(true);
                    break;
                case LEFT:
                    player.setMovingLeft(true);
                    break;
                case RIGHT:
                    player.setMovingRight(true);
                    break;
            }
        } else {
            switch (key) {
                case 'w':
                case 'W':
                    player.setMovingUp(true);
                    break;
                case 's':
                case 'S':
                    player.setMovingDown(true);
                    break;
                case 'a':
                case 'A':
                    player.setMovingLeft(true);
                    break;
                case 'd':
                case 'D':
                    player.setMovingRight(true);
                    break;
            }
        }
    }
    
    public void keyReleased() {
        if (currentState == GameState.GAME) {
            handleGameKeyRelease();
        }
    }
    
    private void handleGameKeyRelease() {
        if (key == CODED) {
            switch (keyCode) {
                case UP:
                    player.setMovingUp(false);
                    break;
                case DOWN:
                    player.setMovingDown(false);
                    break;
                case LEFT:
                    player.setMovingLeft(false);
                    break;
                case RIGHT:
                    player.setMovingRight(false);
                    break;
            }
        } else {
            switch (key) {
                case 'w':
                case 'W':
                    player.setMovingUp(false);
                    break;
                case 's':
                case 'S':
                    player.setMovingDown(false);
                    break;
                case 'a':
                case 'A':
                    player.setMovingLeft(false);
                    break;
                case 'd':
                case 'D':
                    player.setMovingRight(false);
                    break;
            }
        }
    }
    
    private void updateGame() {
        if (player != null) {
            player.update();
            
            // Spawn modifiers
            long currentTime = System.currentTimeMillis();
            if (currentTime - lastModifierSpawn > MODIFIER_SPAWN_INTERVAL) {
                spawnModifier();
                lastModifierSpawn = currentTime;
            }
            
            // Check modifier collisions
            for (Modifier modifier : modifiers) {
                if (modifier.isActive() && modifier.checkCollision(player.getX(), player.getY(), player.getSize())) {
                    modifier.collect();
                    applyModifier(modifier.getType());
                }
            }
        }
        if (opponent != null) {
            opponent.update();
        }
    }
    
    private void spawnModifier() {
        // Count active modifiers of each type
        int[] counts = new int[Modifier.Type.values().length];
        for (Modifier m : modifiers) {
            if (m.isActive()) {
                counts[m.getType().ordinal()]++;
            }
        }
        
        // Create list of available types
        List<Modifier.Type> availableTypes = new ArrayList<>();
        for (Modifier.Type type : Modifier.Type.values()) {
            if (counts[type.ordinal()] < MAX_MODIFIERS_PER_TYPE) {
                availableTypes.add(type);
            }
        }
        
        // Spawn new modifier if possible
        if (!availableTypes.isEmpty()) {
            Modifier.Type type = availableTypes.get(random.nextInt(availableTypes.size()));
            float x = random.nextFloat() * (width - 100) + 50;
            float y = random.nextFloat() * (height - 100) + 50;
            modifiers.add(new Modifier(x, y, type));
        }
    }
    
    private void applyModifier(Modifier.Type type) {
        if (player != null) {
            player.applyModifier(type);
        }
    }
} 