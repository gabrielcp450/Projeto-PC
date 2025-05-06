package com.duelo.client;

import processing.core.PApplet;

public class MainMenu extends Menu {
    private Button playButton;
    private Button rankingsButton;
    private Button profileButton;
    private Button logoutButton;
    private float buttonSpacing = 80;
    private float buttonWidth = 250;
    private float buttonHeight = 50;
    private float centerX;
    private float centerY;
    private float[] buttonYPositions;
    private float[] buttonHoverProgress;

    public MainMenu(DueloGame game) {
        super(game, "Main Menu");
        centerX = game.width/2 - buttonWidth/2;
        centerY = game.height/2 - (buttonSpacing * 1.5f);
        
        buttonYPositions = new float[4];
        buttonHoverProgress = new float[4];
        
        for (int i = 0; i < 4; i++) {
            buttonYPositions[i] = centerY + (i * buttonSpacing);
            buttonHoverProgress[i] = 0;
        }

        playButton = new Button(centerX, buttonYPositions[0], buttonWidth, buttonHeight, "Play", () -> {
            game.startGame();
        });

        rankingsButton = new Button(centerX, buttonYPositions[1], buttonWidth, buttonHeight, "Rankings", () -> {
            game.showRankingsMenu();
        });

        profileButton = new Button(centerX, buttonYPositions[2], buttonWidth, buttonHeight, "Profile", () -> {
            game.showProfileMenu();
        });

        logoutButton = new Button(centerX, buttonYPositions[3], buttonWidth, buttonHeight, "Logout", () -> {
            game.showLoginMenu();
        });

        buttons = new Button[] { playButton, rankingsButton, profileButton, logoutButton };
    }

    @Override
    public void draw() {
        super.draw();

        // Desenha o subtítulo
        game.fill(200);
        game.textAlign(PApplet.CENTER, PApplet.CENTER);
        game.textSize(24);
        game.text("Welcome to Duelo!", game.width/2, titleY + 60);

        // Atualiza e desenha os botões com animações
        for (int i = 0; i < buttons.length; i++) {
            Button button = buttons[i];
            float targetY = buttonYPositions[i];
            float currentY = button.y;
            
            // Suaviza o movimento do botão
            button.y += (targetY - currentY) * 0.2f;
            
            // Atualiza o progresso do hover
            float targetHover = button.isHovered ? 1 : 0;
            buttonHoverProgress[i] += (targetHover - buttonHoverProgress[i]) * 0.2f;
            
            // Desenha o botão com efeitos
            drawButton(button, i);
        }
    }

    private void drawButton(Button button, int index) {
        float x = button.x;
        float y = button.y;
        float w = button.width;
        float h = button.height;
        
        // Desenha o fundo do botão com gradiente
        for (int i = 0; i < h; i++) {
            float inter = PApplet.map(i, 0, h, 0, 1);
            int c = game.lerpColor(0xFF2C2C2C, 0xFF1A1A1A, inter);
            if (button.isHovered) {
                c = game.lerpColor(c, 0xFF4CAF50, buttonHoverProgress[index] * 0.3f);
            }
            game.stroke(c);
            game.line(x, y + i, x + w, y + i);
        }
        
        // Desenha a borda do botão
        game.noFill();
        game.stroke(0xFF4CAF50, 100 + buttonHoverProgress[index] * 155);
        game.strokeWeight(2);
        game.rect(x, y, w, h, 10);
        game.strokeWeight(1);
        
        // Desenha o ícone do botão
        game.fill(0xFFFFFFFF);
        game.textAlign(PApplet.LEFT, PApplet.CENTER);
        game.textSize(24);
        String icon = getButtonIcon(index);
        game.text(icon, x + 20, y + h/2);
        
        // Desenha o texto do botão
        game.textAlign(PApplet.CENTER, PApplet.CENTER);
        game.textSize(20);
        game.text(button.text, x + w/2, y + h/2);
        
        // Efeito de brilho quando hover
        if (button.isHovered) {
            game.noFill();
            game.stroke(0xFF4CAF50, 50);
            game.strokeWeight(3);
            game.rect(x - 5, y - 5, w + 10, h + 10, 15);
            game.strokeWeight(1);
        }
    }

    private String getButtonIcon(int index) {
        switch (index) {
            case 0: return "▶"; // Play
            case 1: return "🏆"; // Rankings
            case 2: return "👤"; // Profile
            case 3: return "↪"; // Logout
            default: return "";
        }
    }
} 