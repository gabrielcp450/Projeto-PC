package com.duelo.client.entities;

import com.duelo.client.core.Game;

import processing.core.PApplet;

public class Projectile {
    private Game game;
    private PApplet p;
    private float x, y;

    public Projectile(Game game, float x, float y) {
        this.game = game;
        this.p = game;
        this.x = x;
        this.y = y;
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

    public void draw(PApplet p, int playAreaX, int playAreaY, int playAreaSize) {
        // Draw projectile
        p.ellipseMode(PApplet.CENTER);
        p.fill(255, 0, 0);
        p.noStroke();
        float mappedX = playAreaX + x * playAreaSize;
        float mappedY = playAreaY + y * playAreaSize;
        p.ellipse(mappedX, mappedY, 10, 10); // Size of the projectile
    }
}
