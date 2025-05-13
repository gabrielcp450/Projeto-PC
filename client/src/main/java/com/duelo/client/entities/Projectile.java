package com.duelo.client.entities;

import com.duelo.client.ui.Constants;

import processing.core.PApplet;

public class Projectile {
    private float x, y;

    public Projectile(float x, float y) {
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
        p.ellipseMode(PApplet.RADIUS);
        p.fill(255, 0, 0);
        p.noStroke();
        float mappedX = playAreaX + x * playAreaSize;
        float mappedY = playAreaY + y * playAreaSize;
        p.ellipse(mappedX, mappedY, Constants.PROJECTILE_RADIUS * playAreaSize,
                Constants.PROJECTILE_RADIUS * playAreaSize); // Size of the projectile
    }
}
