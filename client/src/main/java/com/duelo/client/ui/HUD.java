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

    // Timer related fields
    private int gameTime = 0; // Time in seconds
    private boolean isCountdown = false; // Whether the timer counts down or up
    private int initialTime = 300; // 5 minutes in seconds
    private static final int TIMER_BOX_WIDTH = 100;
    private static final int TIMER_BOX_HEIGHT = 40; // Slightly smaller height for top position
    private static final int TIMER_COLOR = 0xFF3C3C3C; // Dark gray color for timer box
    private static final int TIMER_Y_OFFSET = 10; // Distance from top of screen

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
        // System.out.println("Updating scores - Player: " + playerScore + ", Opponent: " + opponentScore);
        this.playerScore = playerScore;
        this.opponentScore = opponentScore;
    }

    public int getPlayerScore() {
        return playerScore;
    }

    public int getOpponentScore() {
        return opponentScore;
    }

    public void reset() {
        this.playerScore = 0;
        this.opponentScore = 0;
        this.gameTime = 0;
    }

    /**
     * Sets the timer mode and initial time
     *
     * @param isCountdown
     *            Whether the timer should count down
     * @param initialTime
     *            Initial time in seconds (only used for countdown mode)
     */
    public void setTimerMode(boolean isCountdown, int initialTime) {
        this.isCountdown = isCountdown;
        this.initialTime = initialTime;
        this.gameTime = isCountdown ? initialTime : 0;
    }

    /**
     * Updates the game time
     *
     * @param time
     *            Current time in seconds
     */
    public void updateTime(int time) {
        this.gameTime = time;
    }

    /**
     * Formats time in MM:SS format
     */
    private String formatTime(int seconds) {
        int minutes = seconds / 60;
        int remainingSeconds = seconds % 60;
        return String.format("%02d:%02d", minutes, remainingSeconds);
    }

    /**
     * Renders the HUD with current scores and timer
     *
     * @param playAreaY
     *            The Y position where the play area starts
     */
    public void render(int playAreaY) {
        p.pushStyle();
        p.textFont(font);

        // Timer centralizado acima da área de jogo
        float timerX = p.width / 2f;
        float timerY = Math.max(TIMER_Y_OFFSET + TIMER_BOX_HEIGHT / 2f, playAreaY - TIMER_BOX_HEIGHT / 2f - 10);

        // Fundo do timer centralizado
        p.rectMode(PApplet.CENTER);
        p.noStroke();
        p.fill(TIMER_COLOR, 200);
        p.rect(timerX, timerY, TIMER_BOX_WIDTH, TIMER_BOX_HEIGHT, RADIUS);

        // Texto do timer centralizado
        String timeText = formatTime(gameTime);
        p.textAlign(PApplet.CENTER, PApplet.CENTER);
        p.textSize(FONT_SIZE - 4);

        // Efeito de alerta
        if (gameTime <= 30) {
            float glowIntensity = 0.5f + 0.5f * p.sin(p.frameCount * 0.2f);
            p.fill(255, 50, 50, (int) (glowIntensity * 255));
            p.textSize(FONT_SIZE - 2);
            p.text(timeText, timerX, timerY);
            p.fill(255);
            p.textSize(FONT_SIZE - 4);
        }
        p.fill(255);
        p.text(timeText, timerX, timerY);

        // Draw score boxes
        float leftX = PADDING + BOX_WIDTH / 2f;
        float rightX = p.width - PADDING - BOX_WIDTH / 2f;
        float y = PADDING + BOX_HEIGHT / 2f;

        int white = p.color(255);
        int highlight = p.color(255, 214, 0); // Amarelo vibrante

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

        // Retângulo do player 0 (esquerda)
        p.noStroke();
        p.fill(playerColor);
        p.rect(leftX, y, BOX_WIDTH, BOX_HEIGHT, RADIUS);
        // Retângulo do player 1 (direita)
        p.fill(opponentColor);
        p.rect(rightX, y, BOX_WIDTH, BOX_HEIGHT, RADIUS);

        // Scores
        p.fill(white);
        p.textSize(FONT_SIZE);
        p.textAlign(p.CENTER, p.CENTER);
        p.text(playerScore, leftX, y);
        p.text(opponentScore, rightX, y);

        p.popStyle();
    }
}