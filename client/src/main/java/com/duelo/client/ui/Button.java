package com.duelo.client.ui;

import processing.core.PApplet;
import processing.core.PFont;

public class Button {
    private final float x, y, w, h;
    private final String label;
    private final int color;
    private final Runnable action;
    private final PApplet p;
    private final PFont buttonFont;

    public Button(PApplet p, float x, float y, float w, float h, String label, int color, PFont buttonFont,
            Runnable action) {
        this.p = p;
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.label = label;
        this.color = color;
        this.buttonFont = buttonFont;
        this.action = action;
    }

    public void draw() {
        p.rectMode(PApplet.CENTER);

        // Button background
        p.noStroke();
        p.fill(color);
        p.rect(x, y, w, h, 5);

        // Button text
        p.textFont(buttonFont);
        p.fill(255);
        p.textAlign(PApplet.CENTER, PApplet.CENTER);
        p.text(label, x, y);
    }

    public boolean checkClick() {
        if (p.mouseX > x - w / 2 && p.mouseX < x + w / 2 && p.mouseY > y - h / 2 && p.mouseY < y + h / 2) {
            action.run();
            return true;
        }
        return false;
    }
}
