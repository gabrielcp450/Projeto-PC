package com.duelo.client.entities;

import com.duelo.client.ui.Constants;

import processing.core.PApplet;

/**
 * Classe que representa um jogador no jogo.
 */
public class Player {
    private float x, y;
    private float aimX, aimY;
    private int color;
    private String name;

    public Player(String name, float x, float y, int color) {
        this.x = x;
        this.y = y;
        this.aimX = x;
        this.aimY = y;
        this.color = color;
        this.name = name;
    }

    public void setAim(float aimX, float aimY) {
        this.aimX = aimX;
        this.aimY = aimY;
    }

    public void draw(PApplet p, int playAreaX, int playAreaY, int playAreaSize) {
        // Draw player
        p.ellipseMode(PApplet.RADIUS);
        p.fill(color);
        p.noStroke();
        float mappedX = playAreaX + x * playAreaSize;
        float mappedY = playAreaY + y * playAreaSize;
        p.ellipse(mappedX, mappedY, Constants.PLAYER_RADIUS * playAreaSize, Constants.PLAYER_RADIUS * playAreaSize);

        // Draw aim direction
        p.stroke(50);
        p.strokeWeight(2);
        float mappedAimX = playAreaX + (x + aimX * 0.05f) * playAreaSize;
        float mappedAimY = playAreaY + (y + aimY * 0.05f) * playAreaSize;
        p.stroke(1);
        p.strokeWeight(2.5f);
        p.line(mappedX, mappedY, mappedAimX, mappedAimY);

        // Draw name box
        p.textAlign(PApplet.CENTER, PApplet.CENTER);
        p.textSize(12);
        p.fill(128, 128, 128, 150); // Gray with transparency
        p.noStroke();
        float boxWidth = p.textWidth(name) + 10; // Add padding
        float boxHeight = 20;
        p.rectMode(PApplet.CENTER);
        p.rect(mappedX, mappedY - Constants.PLAYER_RADIUS * playAreaSize - boxHeight / 2 - 5, boxWidth, boxHeight, 5); // Rounded
                                                                                                                       // corners
        p.fill(255); // White text
        p.text(name, mappedX, mappedY - Constants.PLAYER_RADIUS * playAreaSize - boxHeight / 2 - 5);
        p.strokeWeight(1);
    }

    public void setPosition(float x, float y) {
        this.x = x;
        this.y = y;
    }

    public float getX() {
        return x;
    }

    public float getY() {
        return y;
    }

    public int getColor() {
        return color;
    }
}