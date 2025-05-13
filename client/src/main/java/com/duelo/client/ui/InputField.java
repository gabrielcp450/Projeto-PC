package com.duelo.client.ui;

import processing.core.PApplet;
import processing.core.PFont;

public class InputField {
    private final float x, y, w, h;
    private final String label;
    private final java.util.function.Supplier<String> valueGetter;
    private final java.util.function.Consumer<String> valueSetter;
    private final PApplet p;
    private boolean active;
    private PFont textFont;
    private PFont inputFont;

    public InputField(PApplet p, float x, float y, float w, float h, String label,
            java.util.function.Supplier<String> valueGetter, java.util.function.Consumer<String> valueSetter,
            boolean startsActive, PFont textFont, PFont inputFont) {
        this.p = p;
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.label = label;
        this.valueGetter = valueGetter;
        this.valueSetter = valueSetter;
        this.active = startsActive;
        this.textFont = textFont;
        this.inputFont = inputFont;
    }

    public void draw() {
        p.rectMode(PApplet.CENTER);

        // Label
        p.textFont(textFont);
        p.fill(0xFF323232);
        p.textAlign(PApplet.RIGHT, PApplet.CENTER);
        p.text(label + ":", x - w / 2 - Constants.PADDING, y);
        p.textAlign(PApplet.CENTER, PApplet.CENTER);

        // Input box
        p.stroke(active ? 0xFF3296C8 : 0xFFC8C8C8);
        p.strokeWeight(2);
        p.fill(255);
        p.rect(x, y, w, h, 5);

        // Input text
        p.textFont(inputFont);
        p.fill(0xFF323232);
        p.textAlign(PApplet.LEFT, PApplet.CENTER);
        p.text(valueGetter.get(), x - w / 2 + Constants.PADDING, y);
        p.textAlign(PApplet.CENTER, PApplet.CENTER);
    }

    public boolean checkClick() {
        boolean clicked = p.mouseX > x - w / 2 && p.mouseX < x + w / 2 && p.mouseY > y - h / 2 && p.mouseY < y + h / 2;
        this.active = clicked;
        return clicked;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public void handleKey(char key, int keyCode) {
        if (!active)
            return;

        String current = valueGetter.get();
        if (key == PApplet.BACKSPACE) {
            if (current.length() > 0)
                valueSetter.accept(current.substring(0, current.length() - 1));
        } else if (key == PApplet.ENTER) {
            active = false;
        } else if (key != PApplet.CODED) {
            valueSetter.accept(current + key);
        }
    }
}
