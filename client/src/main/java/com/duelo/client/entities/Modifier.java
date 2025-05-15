package com.duelo.client.entities;

import com.duelo.client.ui.Constants;

import processing.core.PApplet;

public class Modifier {
    private int type;
    private float x;
    private float y;

    public Modifier(int type, float x, float y) {
        this.type = type;
        this.x = x;
        this.y = y;
    }

    public float getX() {
        return x;
    }

    public float getY() {
        return y;
    }

    public int getType() {
        return type;
    }

    public void draw(PApplet p, int playAreaX, int playAreaY, int playAreaSize) {
        // Draw modifier
        p.ellipseMode(PApplet.RADIUS);
        p.noStroke();
        switch (type) {
            case 0:
                p.fill(0, 255, 0); // Green
                break;
            case 1:
                p.fill(255, 165, 0); // Orange
                break;
            case 2:
                p.fill(0, 0, 255); // Blue
                break;
            case 3:
                p.fill(255, 0, 0); // Red
                break;
            default:
                p.fill(128, 128, 128); // Default gray
                break;
        }
        float mappedX = playAreaX + x * playAreaSize;
        float mappedY = playAreaY + y * playAreaSize;
        p.ellipse(mappedX, mappedY, Constants.MODIFIER_RADIUS * playAreaSize, Constants.MODIFIER_RADIUS * playAreaSize); // Size
                                                                                                                         // of
                                                                                                                         // the
                                                                                                                         // modifier
    }
}
