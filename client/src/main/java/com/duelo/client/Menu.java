package com.duelo.client;

import processing.core.PApplet;

public abstract class Menu {
    protected DueloGame game;
    protected String title;
    protected Button[] buttons;
    protected float titleY = 50;
    protected float titleSize = 48;
    protected int backgroundColor = 0xFF1A1A1A;
    protected int accentColor = 0xFF4CAF50;
    protected int textColor = 0xFFFFFFFF;

    public Menu(DueloGame game, String title) {
        this.game = game;
        this.title = title;
    }

    public void draw() {
        // Desenha o fundo com gradiente
        drawBackground();
        
        // Desenha o título com efeito de sombra
        drawTitle();
        
        // Desenha os botões
        if (buttons != null) {
            for (Button button : buttons) {
                button.draw();
            }
        }
    }

    protected void drawBackground() {
        // Gradiente de cima para baixo
        for (int y = 0; y < game.height; y++) {
            float inter = PApplet.map(y, 0, game.height, 0, 1);
            int c = game.lerpColor(backgroundColor, 0xFF000000, inter);
            game.stroke(c);
            game.line(0, y, game.width, y);
        }
    }

    protected void drawTitle() {
        // Sombra do título
        game.fill(0, 100);
        game.textAlign(PApplet.CENTER, PApplet.TOP);
        game.textSize(titleSize);
        game.text(title, game.width/2 + 2, titleY + 2);
        
        // Título principal
        game.fill(textColor);
        game.text(title, game.width/2, titleY);
    }

    public void mousePressed() {
        if (buttons != null) {
            for (Button button : buttons) {
                if (button.isMouseOver()) {
                    button.onClick();
                }
            }
        }
    }

    public void mouseMoved() {
        if (buttons != null) {
            for (Button button : buttons) {
                button.checkHover(game.mouseX, game.mouseY);
            }
        }
    }

    protected static class Button {
        protected float x, y, width, height;
        protected String text;
        protected boolean isHovered;
        protected Runnable onClickAction;
        protected float hoverProgress = 0;
        protected static final float HOVER_SPEED = 0.2f;

        public Button(float x, float y, float width, float height, String text, Runnable onClickAction) {
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
            this.text = text;
            this.onClickAction = onClickAction;
        }

        public void draw() {
            DueloGame game = DueloGame.getInstance();
            
            // Atualiza a animação de hover
            float targetHover = isHovered ? 1 : 0;
            hoverProgress += (targetHover - hoverProgress) * HOVER_SPEED;
            
            // Desenha o fundo do botão com gradiente
            for (int i = 0; i < height; i++) {
                float inter = PApplet.map(i, 0, height, 0, 1);
                int c = game.lerpColor(0xFF2C2C2C, 0xFF1A1A1A, inter);
                if (isHovered) {
                    c = game.lerpColor(c, 0xFF4CAF50, hoverProgress * 0.3f);
                }
                game.stroke(c);
                game.line(x, y + i, x + width, y + i);
            }
            
            // Borda do botão
            game.noFill();
            game.stroke(0xFF4CAF50, 100 + hoverProgress * 155);
            game.strokeWeight(2);
            game.rect(x, y, width, height, 10);
            game.strokeWeight(1);
            
            // Texto do botão
            game.fill(0xFFFFFFFF);
            game.textAlign(PApplet.CENTER, PApplet.CENTER);
            game.textSize(20);
            game.text(text, x + width/2, y + height/2);
        }

        public boolean isMouseOver() {
            DueloGame game = DueloGame.getInstance();
            return game.mouseX >= x && game.mouseX <= x + width &&
                   game.mouseY >= y && game.mouseY <= y + height;
        }

        public void checkHover(float mouseX, float mouseY) {
            isHovered = mouseX >= x && mouseX <= x + width &&
                       mouseY >= y && mouseY <= y + height;
        }

        public void onClick() {
            if (onClickAction != null) {
                onClickAction.run();
            }
        }
    }
} 