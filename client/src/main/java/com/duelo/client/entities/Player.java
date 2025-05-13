package com.duelo.client.entities;

import processing.core.PApplet;

/**
 * Classe que representa um jogador no jogo.
 */
public class Player {
    private static final int SIZE = 40;

    private float x, y;
    private float aimX, aimY;
    private int color;

    public Player(float x, float y, int color) {
        this.x = x;
        this.y = y;
        this.aimX = x;
        this.aimY = y;
        this.color = color;
    }

    public void setAim(float aimX, float aimY) {
        this.aimX = aimX;
        this.aimY = aimY;
    }

    public void draw(PApplet p, int playAreaX, int playAreaY, int playAreaSize) {
        // Draw player
        p.ellipseMode(PApplet.CENTER);
        p.fill(color);
        p.noStroke();
        float mappedX = playAreaX + x * playAreaSize;
        float mappedY = playAreaY + y * playAreaSize;
        p.ellipse(mappedX, mappedY, SIZE, SIZE);

        // Draw aim direction
        float mappedAimX = playAreaX + (x + aimX * 0.05f) * playAreaSize;
        float mappedAimY = playAreaY + (y + aimY * 0.05f) * playAreaSize;
        p.stroke(1);
        p.line(mappedX, mappedY, mappedAimX, mappedAimY);
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