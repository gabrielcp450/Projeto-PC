package com.duelo.client;

import processing.core.PApplet;

public class ProfileMenu extends Menu {
    private Button backButton;
    private String username;
    private int wins = 0;
    private int losses = 0;
    private int points = 0;
    private float cardWidth = 400;
    private float cardHeight = 300;
    private float statCardWidth = 180;
    private float statCardHeight = 120;
    private float[] statCardYPositions;
    private float[] statCardHoverProgress;

    public ProfileMenu(DueloGame game) {
        super(game, "Profile");
        float buttonWidth = 200;
        float buttonHeight = 40;
        float centerX = game.width/2 - buttonWidth/2;

        backButton = new Button(centerX, game.height - 100, buttonWidth, buttonHeight, "Back", () -> {
            game.showMainMenu();
        });

        buttons = new Button[] { backButton };

        // Inicializa as posições e progressos dos cards
        statCardYPositions = new float[3];
        statCardHoverProgress = new float[3];
        float startY = game.height/2 - 50;
        for (int i = 0; i < 3; i++) {
            statCardYPositions[i] = startY;
            statCardHoverProgress[i] = 0;
        }
    }

    @Override
    public void draw() {
        super.draw();

        // Desenha o card principal
        drawMainCard();

        // Desenha os cards de estatísticas
        drawStatCards();
    }

    private void drawMainCard() {
        float centerX = game.width/2;
        float centerY = game.height/2 - 150;
        
        // Desenha o fundo do card
        game.fill(0xFF2C2C2C);
        game.noStroke();
        game.rect(centerX - cardWidth/2, centerY - cardHeight/2, cardWidth, cardHeight, 20);
        
        // Desenha a borda do card
        game.noFill();
        game.stroke(0xFF4CAF50);
        game.strokeWeight(2);
        game.rect(centerX - cardWidth/2, centerY - cardHeight/2, cardWidth, cardHeight, 20);
        game.strokeWeight(1);
        
        // Desenha o avatar (placeholder)
        game.fill(0xFF4CAF50);
        game.ellipse(centerX, centerY - 50, 80, 80);
        
        // Desenha o nome do usuário
        game.fill(255);
        game.textAlign(PApplet.CENTER, PApplet.CENTER);
        game.textSize(24);
        game.text(username, centerX, centerY + 20);
        
        // Desenha o nível do usuário
        game.textSize(18);
        game.text("Level " + calculateLevel(), centerX, centerY + 50);
    }

    private void drawStatCards() {
        float centerX = game.width/2;
        float startX = centerX - statCardWidth - 20;
        float[] statValues = {wins, losses, points};
        String[] statLabels = {"Wins", "Losses", "Points"};
        String[] statIcons = {"🏆", "💔", "⭐"};
        
        for (int i = 0; i < 3; i++) {
            float x = startX + (i * (statCardWidth + 40));
            float y = statCardYPositions[i];
            
            // Atualiza o progresso do hover
            boolean isHovered = game.mouseY >= y - statCardHeight/2 && game.mouseY <= y + statCardHeight/2 &&
                              game.mouseX >= x && game.mouseX <= x + statCardWidth;
            float targetHover = isHovered ? 1 : 0;
            statCardHoverProgress[i] += (targetHover - statCardHoverProgress[i]) * 0.2f;
            
            // Desenha o fundo do card
            game.fill(0xFF2C2C2C);
            if (isHovered) {
                game.fill(game.lerpColor(0xFF2C2C2C, 0xFF3C3C3C, statCardHoverProgress[i]));
            }
            game.noStroke();
            game.rect(x, y - statCardHeight/2, statCardWidth, statCardHeight, 15);
            
            // Desenha a borda do card
            game.noFill();
            game.stroke(0xFF4CAF50, 100 + statCardHoverProgress[i] * 155);
            game.strokeWeight(2);
            game.rect(x, y - statCardHeight/2, statCardWidth, statCardHeight, 15);
            game.strokeWeight(1);
            
            // Desenha o ícone
            game.fill(0xFF4CAF50);
            game.textAlign(PApplet.CENTER, PApplet.CENTER);
            game.textSize(32);
            game.text(statIcons[i], x + statCardWidth/2, y - 20);
            
            // Desenha o valor
            game.fill(255);
            game.textSize(24);
            game.text(String.valueOf(statValues[i]), x + statCardWidth/2, y + 10);
            
            // Desenha o label
            game.textSize(16);
            game.text(statLabels[i], x + statCardWidth/2, y + 40);
            
            // Efeito de brilho quando hover
            if (isHovered) {
                game.noFill();
                game.stroke(0xFF4CAF50, 30);
                game.strokeWeight(3);
                game.rect(x - 5, y - statCardHeight/2 - 5, statCardWidth + 10, statCardHeight + 10, 20);
                game.strokeWeight(1);
            }
        }
    }

    private int calculateLevel() {
        return (wins + losses) / 10 + 1;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setStats(int wins, int losses, int points) {
        this.wins = wins;
        this.losses = losses;
        this.points = points;
    }
} 