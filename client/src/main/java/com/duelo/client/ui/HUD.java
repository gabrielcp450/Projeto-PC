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
     * @param isCountdown Whether the timer should count down
     * @param initialTime Initial time in seconds (only used for countdown mode)
     */
    public void setTimerMode(boolean isCountdown, int initialTime) {
        this.isCountdown = isCountdown;
        this.initialTime = initialTime;
        this.gameTime = isCountdown ? initialTime : 0;
    }

    /**
     * Updates the game time
     * @param time Current time in seconds
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
     */
    public void render() {
        p.pushStyle();
        p.textFont(font);
        p.textAlign(p.CENTER, p.CENTER);

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

        // Timer
        p.fill(white);
        p.textSize(FONT_SIZE);
        p.textAlign(p.CENTER, p.CENTER);
        String timeText = formatTime(gameTime);
        p.text(timeText, p.width / 2, y);

        p.popStyle();
    }
}