package com.duelo.client.core;

import processing.event.MouseEvent;

public abstract class State {
    protected final Game game;

    public State(Game game) {
        this.game = game;
    }

    public abstract void draw();

    public void keyPressed() {
        // Default implementation (can be overridden)
    }

    public void keyReleased() {
        // Default implementation (can be overridden)
    }

    public void mousePressed() {
        // Default implementation (can be overridden)
    }

    public void mouseMoved() {
        // Default implementation (can be overridden)
    }

    public void mouseWheel(MouseEvent event) {
        // Default implementation (can be overridden)
    }
}
