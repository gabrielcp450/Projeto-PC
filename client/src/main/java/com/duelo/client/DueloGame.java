package com.duelo.client;

import processing.core.PApplet;
import processing.core.PVector;

public class DueloGame extends PApplet {
    private static DueloGame instance;
    private static final int PLAYER_SIZE = 40;
    private static final int PROJECTILE_SIZE = 10;
    private static final float PLAYER_SPEED = 5.0f;
    private static final float PROJECTILE_SPEED = 10.0f;
    private static final int SHOOT_COOLDOWN = 500; // milissegundos entre tiros

    private PVector player1Pos;
    private PVector player2Pos;
    private java.util.List<Projectile> projectiles;
    private boolean[] keysPressed;
    private long lastShootTime;
    private String username;
    private GameManager gameManager;
    private boolean gameStarted = false;
    private String statusMessage = "Waiting for opponent...";
    private Menu currentMenu;
    private boolean inGame = false;
    private float gameBackgroundOffset = 0;
    private int player1Color = 0xFF4CAF50;
    private int player2Color = 0xFFE91E63;
    private int projectile1Color = 0xFF4CAF50;
    private int projectile2Color = 0xFFE91E63;

    public static DueloGame getInstance() {
        return instance;
    }

    private class Projectile {
        PVector pos;
        PVector vel;
        boolean isMine;
        float trailProgress = 0;

        Projectile(float x, float y, float angle, boolean isMine) {
            this.pos = new PVector(x, y);
            this.vel = PVector.fromAngle(angle).mult(PROJECTILE_SPEED);
            this.isMine = isMine;
        }

        void update() {
            pos.add(vel);
            trailProgress += 0.1f;
            if (trailProgress > 1) trailProgress = 0;
        }

        void draw() {
            // Desenha o rastro do projétil
            for (int i = 0; i < 5; i++) {
                float alpha = 255 * (1 - (i * 0.2f));
                float size = PROJECTILE_SIZE * (1 - (i * 0.15f));
                fill(isMine ? projectile1Color : projectile2Color, alpha);
                noStroke();
                ellipse(pos.x - vel.x * i * 0.2f, pos.y - vel.y * i * 0.2f, size, size);
            }
            
            // Desenha o projétil
            fill(isMine ? projectile1Color : projectile2Color);
            noStroke();
            ellipse(pos.x, pos.y, PROJECTILE_SIZE, PROJECTILE_SIZE);
            
            // Desenha o brilho
            fill(255, 100);
            ellipse(pos.x, pos.y, PROJECTILE_SIZE * 1.5f, PROJECTILE_SIZE * 1.5f);
        }
    }

    public void settings() {
        size(800, 600);
    }

    public void setup() {
        instance = this;
        frameRate(60);
        projectiles = new java.util.ArrayList<>();
        keysPressed = new boolean[256];
        lastShootTime = 0;

        // Inicializa os jogadores em posições opostas
        player1Pos = new PVector(50, height/2);
        player2Pos = new PVector(width - 50, height/2);

        // Inicia com o menu de login
        showLoginMenu();
    }

    public void draw() {
        if (inGame) {
            drawGame();
        } else if (currentMenu != null) {
            currentMenu.draw();
        }
    }

    private void drawGame() {
        // Desenha o fundo animado
        drawGameBackground();
        
        // Desenha os jogadores
        drawPlayer(player1Pos, player1Color, true);
        drawPlayer(player2Pos, player2Color, false);

        // Atualiza e desenha os projéteis
        for (Projectile p : projectiles) {
            p.update();
            p.draw();
        }

        // Desenha a interface do jogo
        drawGameUI();

        // Atualiza a posição do jogador baseado nas teclas pressionadas
        if (gameStarted) {
            updatePlayerPosition();
        }
    }

    private void drawGameBackground() {
        // Atualiza o offset do fundo
        gameBackgroundOffset += 0.5f;
        if (gameBackgroundOffset > 100) gameBackgroundOffset = 0;
        
        // Desenha o padrão de fundo
        stroke(0xFF1A1A1A);
        strokeWeight(2);
        for (int i = 0; i < width; i += 50) {
            for (int j = 0; j < height; j += 50) {
                float x = i + (gameBackgroundOffset % 50);
                float y = j + (gameBackgroundOffset % 50);
                point(x, y);
            }
        }
        strokeWeight(1);
    }

    private void drawPlayer(PVector pos, int color, boolean isPlayer1) {
        // Desenha o brilho do jogador
        fill(color, 50);
        noStroke();
        ellipse(pos.x, pos.y, PLAYER_SIZE * 2, PLAYER_SIZE * 2);
        
        // Desenha o jogador
        fill(color);
        noStroke();
        rectMode(CENTER);
        rect(pos.x, pos.y, PLAYER_SIZE, PLAYER_SIZE, 10);
        
        // Desenha o indicador de direção
        float angle = atan2(mouseY - pos.y, mouseX - pos.x);
        float indicatorX = pos.x + cos(angle) * (PLAYER_SIZE/2 + 5);
        float indicatorY = pos.y + sin(angle) * (PLAYER_SIZE/2 + 5);
        fill(255);
        ellipse(indicatorX, indicatorY, 5, 5);
        
        // Desenha o nome do jogador
        fill(255);
        textAlign(CENTER, CENTER);
        textSize(12);
        text(isPlayer1 ? username : "Opponent", pos.x, pos.y - PLAYER_SIZE/2 - 15);
    }

    private void drawGameUI() {
        // Desenha o status
        fill(255);
        textAlign(CENTER, TOP);
        textSize(24);
        text(statusMessage, width/2, 20);
        
        // Desenha o cooldown do tiro
        if (millis() - lastShootTime < SHOOT_COOLDOWN) {
            float cooldownProgress = (float)(millis() - lastShootTime) / SHOOT_COOLDOWN;
            float cooldownWidth = 100 * cooldownProgress;
            
            // Desenha a barra de cooldown
            noFill();
            stroke(255);
            strokeWeight(2);
            rect(width/2 - 50, 50, 100, 10);
            
            // Desenha o progresso
            fill(0xFF4CAF50);
            noStroke();
            rect(width/2 - 50, 50, cooldownWidth, 10);
        }
    }

    private void updatePlayerPosition() {
        PVector movement = new PVector(0, 0);
        
        if (keysPressed['w'] || keysPressed['W']) movement.y -= PLAYER_SPEED;
        if (keysPressed['s'] || keysPressed['S']) movement.y += PLAYER_SPEED;
        if (keysPressed['a'] || keysPressed['A']) movement.x -= PLAYER_SPEED;
        if (keysPressed['d'] || keysPressed['D']) movement.x += PLAYER_SPEED;

        if (movement.mag() > 0) {
            player1Pos.add(movement);
            // Limita o jogador dentro da tela
            player1Pos.x = constrain(player1Pos.x, PLAYER_SIZE/2, width - PLAYER_SIZE/2);
            player1Pos.y = constrain(player1Pos.y, PLAYER_SIZE/2, height - PLAYER_SIZE/2);
            
            // Envia a posição para o servidor
            gameManager.sendPlayerPosition(player1Pos.x, player1Pos.y);
        }
    }

    public void keyPressed() {
        if (inGame) {
            keysPressed[key] = true;
        } else if (currentMenu instanceof LoginMenu) {
            ((LoginMenu) currentMenu).keyPressed();
        }
    }

    public void keyReleased() {
        keysPressed[key] = false;
    }

    public void mousePressed() {
        if (inGame && gameStarted && millis() - lastShootTime > SHOOT_COOLDOWN) {
            float angle = atan2(mouseY - player1Pos.y, mouseX - player1Pos.x);
            projectiles.add(new Projectile(player1Pos.x, player1Pos.y, angle, true));
            gameManager.sendProjectile(player1Pos.x, player1Pos.y, angle);
            lastShootTime = millis();
        } else if (currentMenu != null) {
            currentMenu.mousePressed();
        }
    }

    public void mouseMoved() {
        if (currentMenu != null) {
            currentMenu.mouseMoved();
        }
    }

    public void onGameStart() {
        gameStarted = true;
        statusMessage = "Game Started! Use WASD to move and click to shoot!";
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void showLoginMenu() {
        currentMenu = new LoginMenu(this);
        inGame = false;
    }

    public void showMainMenu() {
        currentMenu = new MainMenu(this);
        inGame = false;
    }

    public void showRankingsMenu() {
        currentMenu = new RankingsMenu(this);
        inGame = false;
    }

    public void showProfileMenu() {
        currentMenu = new ProfileMenu(this);
        inGame = false;
    }

    public void startGame() {
        inGame = true;
        currentMenu = null;
        try {
            gameManager = GameManager.getInstance();
            gameManager.setGame(this);
            gameManager.setUsername(username);
            gameManager.enterQueue();
        } catch (Exception e) {
            e.printStackTrace();
            statusMessage = "Error connecting to server";
        }
    }

    public static void main(String[] args) {
        PApplet.main(DueloGame.class);
    }
} 