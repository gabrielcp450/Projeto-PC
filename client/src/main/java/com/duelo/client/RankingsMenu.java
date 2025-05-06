package com.duelo.client;

import processing.core.PApplet;

public class RankingsMenu extends Menu {
    private Button backButton;
    private String[] rankings = {
        "1. Player1 - 1000 points",
        "2. Player2 - 800 points",
        "3. Player3 - 600 points",
        "4. Player4 - 400 points",
        "5. Player5 - 200 points"
    };
    private float tableWidth = 500;
    private float tableHeight = 300;
    private float rowHeight = 50;
    private float[] rowYPositions;
    private float[] rowHoverProgress;

    public RankingsMenu(DueloGame game) {
        super(game, "Rankings");
        float buttonWidth = 200;
        float buttonHeight = 40;
        float centerX = game.width/2 - buttonWidth/2;

        backButton = new Button(centerX, game.height - 100, buttonWidth, buttonHeight, "Back", () -> {
            game.showMainMenu();
        });

        buttons = new Button[] { backButton };

        // Inicializa as posições e progressos das linhas
        rowYPositions = new float[rankings.length];
        rowHoverProgress = new float[rankings.length];
        float startY = game.height/2 - (tableHeight/2);
        for (int i = 0; i < rankings.length; i++) {
            rowYPositions[i] = startY + (i * rowHeight);
            rowHoverProgress[i] = 0;
        }
    }

    @Override
    public void draw() {
        super.draw();

        // Desenha o cabeçalho da tabela
        drawTableHeader();

        // Desenha as linhas da tabela
        for (int i = 0; i < rankings.length; i++) {
            drawTableRow(rankings[i], i);
        }
    }

    private void drawTableHeader() {
        float centerX = game.width/2;
        float startY = game.height/2 - (tableHeight/2) - 40;
        
        // Desenha o fundo do cabeçalho
        game.fill(0xFF2C2C2C);
        game.noStroke();
        game.rect(centerX - tableWidth/2, startY, tableWidth, 40, 10, 10, 0, 0);
        
        // Desenha o texto do cabeçalho
        game.fill(0xFF4CAF50);
        game.textAlign(PApplet.CENTER, PApplet.CENTER);
        game.textSize(20);
        game.text("Player Rankings", centerX, startY + 20);
    }

    private void drawTableRow(String ranking, int index) {
        float centerX = game.width/2;
        float y = rowYPositions[index];
        
        // Atualiza o progresso do hover
        boolean isHovered = game.mouseY >= y && game.mouseY <= y + rowHeight &&
                          game.mouseX >= centerX - tableWidth/2 && game.mouseX <= centerX + tableWidth/2;
        float targetHover = isHovered ? 1 : 0;
        rowHoverProgress[index] += (targetHover - rowHoverProgress[index]) * 0.2f;
        
        // Desenha o fundo da linha
        game.fill(0xFF2C2C2C);
        if (isHovered) {
            game.fill(game.lerpColor(0xFF2C2C2C, 0xFF3C3C3C, rowHoverProgress[index]));
        }
        game.noStroke();
        game.rect(centerX - tableWidth/2, y, tableWidth, rowHeight);
        
        // Desenha a borda da linha
        game.noFill();
        game.stroke(0xFF4CAF50, 50);
        game.strokeWeight(1);
        game.line(centerX - tableWidth/2, y + rowHeight, centerX + tableWidth/2, y + rowHeight);
        
        // Desenha o texto da linha
        game.fill(255);
        game.textAlign(PApplet.LEFT, PApplet.CENTER);
        game.textSize(18);
        
        // Separa o ranking em partes
        String[] parts = ranking.split(" - ");
        String rankAndName = parts[0];
        String points = parts[1];
        
        // Desenha o ranking e nome
        game.text(rankAndName, centerX - tableWidth/2 + 20, y + rowHeight/2);
        
        // Desenha os pontos
        game.textAlign(PApplet.RIGHT, PApplet.CENTER);
        game.text(points, centerX + tableWidth/2 - 20, y + rowHeight/2);
        
        // Efeito de brilho quando hover
        if (isHovered) {
            game.noFill();
            game.stroke(0xFF4CAF50, 30);
            game.strokeWeight(2);
            game.rect(centerX - tableWidth/2, y, tableWidth, rowHeight, 5);
            game.strokeWeight(1);
        }
    }
} 