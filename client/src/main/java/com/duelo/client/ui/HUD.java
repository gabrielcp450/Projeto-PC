package com.duelo.client.ui;

import processing.core.PApplet;
import processing.core.PFont;

/**
 * Heads-Up Display (HUD) for the game. Displays game information such as scores in a clean and organized way.
 */
public class HUD {
    private final PApplet p;
    private PFont font;
    private int playerScore;
    private int opponentScore;
    private int playerColor;
    private int opponentColor;
    private int myPlayerId = 0;
    private static final int FONT_SIZE = 32;
    private static final int PADDING = 20;
    private static final int BOX_HEIGHT = 56;
    private static final int BOX_WIDTH = 64;
    private static final int CENTER_BOX_WIDTH = 120;
    private static final int RADIUS = 12;

    public HUD(PApplet p) {
        this.p = p;
        // Use system font instead of custom font
        this.font = p.createFont("Arial-Bold", FONT_SIZE);
        this.playerScore = 0;
        this.opponentScore = 0;
        // Default colors (can be overridden)
        this.playerColor = p.color(0, 120, 255);
        this.opponentColor = p.color(255, 120, 0);
    }

    public void setColors(int playerColor, int opponentColor) {
        this.playerColor = playerColor;
        this.opponentColor = opponentColor;
    }

    public void setLocalPlayerId(int id) {
        this.myPlayerId = id;
    }

    /**
     * Updates the scores displayed in the HUD
     *
     * @param playerScore
     *            The player's current score
     * @param opponentScore
     *            The opponent's current score
     */
    public void updateScores(int playerScore, int opponentScore) {
        System.out.println("Updating scores - Player: " + playerScore + ", Opponent: " + opponentScore);
        this.playerScore = playerScore;
        this.opponentScore = opponentScore;
    }

    /**
     * Renders the HUD with current scores
     */
    public void render() {
        p.pushStyle();
        p.textFont(font);
        p.textAlign(p.CENTER, p.CENTER);

        float centerX = p.width / 2f;
        float y = PADDING + BOX_HEIGHT / 2f;
        float spacing = 6;

        int blackTrans = p.color(0, 0, 0, 180);
        int white = p.color(255);
        int highlight = p.color(255, 214, 0); // Amarelo vibrante

        // Retângulo central (placar)
        p.noStroke();
        p.fill(blackTrans);
        p.rectMode(PApplet.CENTER);
        p.rect(centerX, y, CENTER_BOX_WIDTH, BOX_HEIGHT, RADIUS);

        // Retângulo do player 0 (esquerda)
        float leftX = centerX - CENTER_BOX_WIDTH / 2 - BOX_WIDTH / 2 + spacing;
        // Retângulo do player 1 (direita)
        float rightX = centerX + CENTER_BOX_WIDTH / 2 + BOX_WIDTH / 2 - spacing;

        // Destaque do player local
        p.rectMode(PApplet.CENTER);
        if (myPlayerId == 0) {
            p.stroke(highlight);
            p.strokeWeight(6);
            p.noFill();
            p.rect(leftX, y, BOX_WIDTH, BOX_HEIGHT, RADIUS);
        } else if (myPlayerId == 1) {
            p.stroke(highlight);
            p.strokeWeight(6);
            p.noFill();
            p.rect(rightX, y, BOX_WIDTH, BOX_HEIGHT, RADIUS);
        }

        // Retângulo do player 0
        p.noStroke();
        p.fill(playerColor);
        p.rect(leftX, y, BOX_WIDTH, BOX_HEIGHT, RADIUS);
        // Retângulo do player 1
        p.fill(opponentColor);
        p.rect(rightX, y, BOX_WIDTH, BOX_HEIGHT, RADIUS);

        // Scores
        p.fill(white);
        p.textSize(FONT_SIZE);
        p.textAlign(p.CENTER, p.CENTER);
        p.text(playerScore, leftX, y);
        p.text(opponentScore, rightX, y);

        // Placar central (só score, mas pode ser timer futuramente)
        String scoreText = String.format("-");
        p.text(scoreText, centerX, y);

        p.popStyle();
    }
}