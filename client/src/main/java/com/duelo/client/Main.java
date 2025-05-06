package com.duelo.client;

import processing.core.PApplet;
import processing.core.PFont;
import java.util.ArrayList;
import java.util.List;

public class Main extends PApplet {
    private static final int WINDOW_WIDTH = 900;
    private static final int WINDOW_HEIGHT = 600;
    
    private GameState currentState;
    private String currentUsername;
    private PFont mainFont;
    
    // Login UI elements
    private String usernameInput = "";
    private String passwordInput = "";
    private boolean isTypingUsername = true;
    private boolean showPassword = false;
    private int cursorBlinkRate = 30;
    private int cursorBlinkCounter = 0;
    private boolean showCursor = true;
    private String errorMessage = "";
    
    // Menu UI elements
    private final int BUTTON_WIDTH = 250;
    private final int BUTTON_HEIGHT = 60;
    private final int BUTTON_SPACING = 80;
    
    // Game UI elements
    private final int GAME_BOARD_SIZE = 400;
    private final int CELL_SIZE = 50;
    private int[][] gameBoard;
    private boolean isPlayerTurn = true;
    private String gameStatus = "Your turn";
    
    // Rankings UI elements
    private List<PlayerRank> rankings;
    private final int RANKING_ROW_HEIGHT = 40;
    private final int RANKING_START_Y = 150;
    
    // Profile UI elements
    private int wins = 0;
    private int losses = 0;
    private int draws = 0;
    private int currentRank = 0;
    private int totalScore = 0;
    
    // Auth manager
    private AuthManager authManager;
    
    public static void main(String[] args) {
        PApplet.main("com.duelo.client.Main");
    }
    
    public void settings() {
        size(WINDOW_WIDTH, WINDOW_HEIGHT);
    }
    
    public void setup() {
        surface.setResizable(false);
        currentState = GameState.LOGIN;
        textAlign(CENTER, CENTER);
        rectMode(CENTER);
        
        // Use system font instead of loading custom font
        textFont(createFont("SansSerif", 16));
        
        initializeGameBoard();
        initializeRankings();
        initializeProfile();
        
        // Initialize auth manager
        authManager = AuthManager.getInstance();
    }
    
    private void initializeGameBoard() {
        gameBoard = new int[8][8];
        // Initialize empty board
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++) {
                gameBoard[i][j] = 0;
            }
        }
    }
    
    private void initializeRankings() {
        rankings = new ArrayList<>();
        // TODO: Load actual rankings from server
        rankings.add(new PlayerRank("Player1", 1000));
        rankings.add(new PlayerRank("Player2", 850));
        rankings.add(new PlayerRank("Player3", 720));
        rankings.add(new PlayerRank("Player4", 650));
        rankings.add(new PlayerRank("Player5", 500));
    }
    
    private void initializeProfile() {
        // TODO: Load actual profile data from server
        wins = 10;
        losses = 5;
        draws = 2;
        currentRank = 3;
        totalScore = 720;
    }
    
    public void draw() {
        background(240);
        
        // Update cursor blink
        cursorBlinkCounter++;
        if (cursorBlinkCounter >= cursorBlinkRate) {
            showCursor = !showCursor;
            cursorBlinkCounter = 0;
        }
        
        switch (currentState) {
            case LOGIN:
                drawLogin();
                break;
            case MENU:
                drawMenu();
                break;
            case GAME:
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
        fill(0);
        textSize(32);
        text("Duelo Game", width/2, 100);
        
        // Error message
        if (!errorMessage.isEmpty()) {
            fill(255, 0, 0);
            textSize(16);
            text(errorMessage, width/2, 150);
        }
        
        // Input fields background
        fill(255);
        stroke(isTypingUsername ? color(50, 150, 200) : color(200));
        rect(width/2, 250, 300, 40, 10);
        stroke(isTypingUsername ? color(200) : color(50, 150, 200));
        rect(width/2, 320, 300, 40, 10);
        
        // Labels
        fill(100);
        textAlign(LEFT);
        textSize(14);
        text("Username:", width/2 - 140, 230);
        text("Password:", width/2 - 140, 300);
        
        // Input text
        fill(0);
        textAlign(LEFT);
        textSize(16);
        float usernameX = width/2 - 140;
        float usernameY = 250;
        text(usernameInput, usernameX, usernameY);
        
        // Draw cursor for username
        if (isTypingUsername && showCursor) {
            float cursorX = usernameX + textWidth(usernameInput);
            stroke(0);
            line(cursorX, usernameY - 10, cursorX, usernameY + 10);
        }
        
        // Password text
        if (showPassword) {
            text(passwordInput, width/2 - 140, 320);
        } else {
            text("*".repeat(passwordInput.length()), width/2 - 140, 320);
        }
        
        // Draw cursor for password
        if (!isTypingUsername && showCursor) {
            float cursorX = width/2 - 140 + textWidth(showPassword ? passwordInput : "*".repeat(passwordInput.length()));
            stroke(0);
            line(cursorX, 320 - 10, cursorX, 320 + 10);
        }
        
        // Login button
        fill(50, 150, 200);
        rect(width/2, 400, 200, 50, 10);
        fill(255);
        textAlign(CENTER);
        text("Login", width/2, 400);
        
        // Register button
        fill(100);
        rect(width/2, 470, 200, 50, 10);
        fill(255);
        text("Register", width/2, 470);
    }
    
    private void drawMenu() {
        // Title
        fill(0);
        textSize(32);
        text("Main Menu", width/2, 100);
        
        // Welcome message
        textSize(16);
        text("Welcome, " + currentUsername, width/2, 150);
        
        // Menu buttons
        int startY = 250;
        
        // Play button
        fill(50, 150, 200);
        rect(width/2, startY, BUTTON_WIDTH, BUTTON_HEIGHT, 10);
        fill(255);
        text("Play Game", width/2, startY);
        
        // Rankings button
        fill(100, 150, 200);
        rect(width/2, startY + BUTTON_SPACING, BUTTON_WIDTH, BUTTON_HEIGHT, 10);
        fill(255);
        text("Rankings", width/2, startY + BUTTON_SPACING);
        
        // Profile button
        fill(150, 150, 200);
        rect(width/2, startY + BUTTON_SPACING * 2, BUTTON_WIDTH, BUTTON_HEIGHT, 10);
        fill(255);
        text("Profile", width/2, startY + BUTTON_SPACING * 2);
        
        // Logout button
        fill(200, 100, 100);
        rect(width/2, startY + BUTTON_SPACING * 3, BUTTON_WIDTH, BUTTON_HEIGHT, 10);
        fill(255);
        text("Logout", width/2, startY + BUTTON_SPACING * 3);
    }
    
    private void drawGame() {
        // Title
        fill(0);
        textSize(24);
        text("Game Board", width/2, 50);
        
        // Game status
        textSize(16);
        text(gameStatus, width/2, 80);
        
        // Draw game board
        int boardX = width/2 - GAME_BOARD_SIZE/2;
        int boardY = height/2 - GAME_BOARD_SIZE/2;
        
        // Draw board background
        fill(200);
        rect(width/2, height/2, GAME_BOARD_SIZE, GAME_BOARD_SIZE);
        
        // Draw grid
        stroke(0);
        for (int i = 0; i <= 8; i++) {
            line(boardX + i * CELL_SIZE, boardY, boardX + i * CELL_SIZE, boardY + GAME_BOARD_SIZE);
            line(boardX, boardY + i * CELL_SIZE, boardX + GAME_BOARD_SIZE, boardY + i * CELL_SIZE);
        }
        
        // Draw pieces
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++) {
                if (gameBoard[i][j] != 0) {
                    if (gameBoard[i][j] == 1) {
                        fill(255, 0, 0); // Red for player
                    } else {
                        fill(0, 0, 255); // Blue for opponent
                    }
                    ellipse(boardX + j * CELL_SIZE + CELL_SIZE/2,
                           boardY + i * CELL_SIZE + CELL_SIZE/2,
                           CELL_SIZE * 0.8f,
                           CELL_SIZE * 0.8f);
                }
            }
        }
        
        // Back button
        fill(200, 100, 100);
        rect(width - 100, 50, 150, 40, 10);
        fill(255);
        text("Back to Menu", width - 100, 50);
    }
    
    private void drawRankings() {
        // Title
        fill(0);
        textSize(32);
        text("Rankings", width/2, 80);
        
        // Column headers
        textSize(16);
        textAlign(LEFT);
        text("Rank", 100, RANKING_START_Y - 20);
        text("Player", 200, RANKING_START_Y - 20);
        text("Score", 400, RANKING_START_Y - 20);
        
        // Draw rankings
        for (int i = 0; i < rankings.size(); i++) {
            PlayerRank rank = rankings.get(i);
            int y = RANKING_START_Y + i * RANKING_ROW_HEIGHT;
            
            // Highlight current player
            if (rank.username.equals(currentUsername)) {
                fill(200, 200, 255);
                rect(width/2, y, width - 100, RANKING_ROW_HEIGHT - 5);
            }
            
            fill(0);
            textAlign(LEFT);
            text((i + 1) + ".", 100, y);
            text(rank.username, 200, y);
            text(rank.score, 400, y);
        }
        
        // Back button
        fill(200, 100, 100);
        rect(width - 100, 50, 150, 40, 10);
        fill(255);
        textAlign(CENTER);
        text("Back to Menu", width - 100, 50);
    }
    
    private void drawProfile() {
        // Title
        fill(0);
        textSize(32);
        text("Profile", width/2, 80);
        
        // Username
        textSize(24);
        text(currentUsername, width/2, 130);
        
        // Stats
        textSize(16);
        textAlign(LEFT);
        
        // Rank
        fill(0);
        text("Current Rank:", 100, 200);
        fill(50, 150, 200);
        text("#" + currentRank, 250, 200);
        
        // Score
        fill(0);
        text("Total Score:", 100, 240);
        fill(50, 150, 200);
        text(totalScore, 250, 240);
        
        // Game stats
        fill(0);
        text("Games Played:", 100, 280);
        fill(50, 150, 200);
        text(wins + losses + draws, 250, 280);
        
        // Wins
        fill(0);
        text("Wins:", 100, 320);
        fill(0, 200, 0);
        text(wins, 250, 320);
        
        // Losses
        fill(0);
        text("Losses:", 100, 360);
        fill(200, 0, 0);
        text(losses, 250, 360);
        
        // Draws
        fill(0);
        text("Draws:", 100, 400);
        fill(150, 150, 150);
        text(draws, 250, 400);
        
        // Win rate
        fill(0);
        text("Win Rate:", 100, 440);
        fill(50, 150, 200);
        float winRate = (wins + losses + draws) > 0 ? (wins * 100.0f) / (wins + losses + draws) : 0;
        text(String.format("%.1f%%", winRate), 250, 440);
        
        // Back button
        fill(200, 100, 100);
        rect(width - 100, 50, 150, 40, 10);
        fill(255);
        textAlign(CENTER);
        text("Back to Menu", width - 100, 50);
    }
    
    public void mousePressed() {
        if (currentState == GameState.LOGIN) {
            // Check if clicking on input fields
            if (mouseY > 230 && mouseY < 270) {
                isTypingUsername = true;
                errorMessage = "";
            } else if (mouseY > 300 && mouseY < 340) {
                isTypingUsername = false;
                errorMessage = "";
            }
            
            // Check if clicking login button
            if (mouseX > width/2 - 100 && mouseX < width/2 + 100 &&
                mouseY > 375 && mouseY < 425) {
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
            
            // Check if clicking register button
            if (mouseX > width/2 - 100 && mouseX < width/2 + 100 &&
                mouseY > 445 && mouseY < 495) {
                if (usernameInput.isEmpty() || passwordInput.isEmpty()) {
                    errorMessage = "Please fill in all fields";
                    return;
                }
                
                // Try to register
                if (authManager.register(usernameInput, passwordInput)) {
                    errorMessage = "Registration successful! Please login.";
                    usernameInput = "";
                    passwordInput = "";
                } else {
                    errorMessage = "Registration failed. Username might be taken.";
                }
            }
        } else if (currentState == GameState.MENU) {
            int startY = 250;
            
            // Play button
            if (mouseX > width/2 - BUTTON_WIDTH/2 && mouseX < width/2 + BUTTON_WIDTH/2 &&
                mouseY > startY - BUTTON_HEIGHT/2 && mouseY < startY + BUTTON_HEIGHT/2) {
                currentState = GameState.GAME;
                initializeGameBoard();
            }
            
            // Rankings button
            if (mouseX > width/2 - BUTTON_WIDTH/2 && mouseX < width/2 + BUTTON_WIDTH/2 &&
                mouseY > startY + BUTTON_SPACING - BUTTON_HEIGHT/2 && mouseY < startY + BUTTON_SPACING + BUTTON_HEIGHT/2) {
                currentState = GameState.RANKINGS;
            }
            
            // Profile button
            if (mouseX > width/2 - BUTTON_WIDTH/2 && mouseX < width/2 + BUTTON_WIDTH/2 &&
                mouseY > startY + BUTTON_SPACING * 2 - BUTTON_HEIGHT/2 && mouseY < startY + BUTTON_SPACING * 2 + BUTTON_HEIGHT/2) {
                currentState = GameState.PROFILE;
            }
            
            // Logout button
            if (mouseX > width/2 - BUTTON_WIDTH/2 && mouseX < width/2 + BUTTON_WIDTH/2 &&
                mouseY > startY + BUTTON_SPACING * 3 - BUTTON_HEIGHT/2 && mouseY < startY + BUTTON_SPACING * 3 + BUTTON_HEIGHT/2) {
                // Send logout command to server
                if (authManager.logout(currentUsername)) {
                    currentState = GameState.LOGIN;
                    usernameInput = "";
                    passwordInput = "";
                    errorMessage = "";
                } else {
                    errorMessage = "Error logging out. Please try again.";
                }
            }
        } else if (currentState == GameState.GAME) {
            // Back button
            if (mouseX > width - 175 && mouseX < width - 25 &&
                mouseY > 30 && mouseY < 70) {
                currentState = GameState.MENU;
                return;
            }
            
            // Game board interaction
            if (isPlayerTurn) {
                int boardX = width/2 - GAME_BOARD_SIZE/2;
                int boardY = height/2 - GAME_BOARD_SIZE/2;
                
                if (mouseX >= boardX && mouseX < boardX + GAME_BOARD_SIZE &&
                    mouseY >= boardY && mouseY < boardY + GAME_BOARD_SIZE) {
                    
                    int col = (mouseX - boardX) / CELL_SIZE;
                    int row = (mouseY - boardY) / CELL_SIZE;
                    
                    if (gameBoard[row][col] == 0) {
                        gameBoard[row][col] = 1; // Player's move
                        isPlayerTurn = false;
                        gameStatus = "Opponent's turn";
                        
                        // TODO: Send move to server
                    }
                }
            }
        } else if (currentState == GameState.RANKINGS) {
            // Back button
            if (mouseX > width - 175 && mouseX < width - 25 &&
                mouseY > 30 && mouseY < 70) {
                currentState = GameState.MENU;
            }
        } else if (currentState == GameState.PROFILE) {
            // Back button
            if (mouseX > width - 175 && mouseX < width - 25 &&
                mouseY > 30 && mouseY < 70) {
                currentState = GameState.MENU;
            }
        }
    }
    
    public void keyPressed() {
        if (currentState == GameState.LOGIN) {
            if (key == BACKSPACE) {
                if (isTypingUsername) {
                    if (usernameInput.length() > 0) {
                        usernameInput = usernameInput.substring(0, usernameInput.length() - 1);
                    }
                } else {
                    if (passwordInput.length() > 0) {
                        passwordInput = passwordInput.substring(0, passwordInput.length() - 1);
                    }
                }
            } else if (key == ENTER) {
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
            } else if (key == TAB) {
                isTypingUsername = !isTypingUsername;
            } else if (key != CODED) {
                if (isTypingUsername) {
                    usernameInput += key;
                } else {
                    passwordInput += key;
                }
            }
        }
    }
    
    private enum GameState {
        LOGIN, MENU, GAME, RANKINGS, PROFILE
    }
    
    private static class PlayerRank {
        String username;
        int score;
        
        PlayerRank(String username, int score) {
            this.username = username;
            this.score = score;
        }
    }
} 