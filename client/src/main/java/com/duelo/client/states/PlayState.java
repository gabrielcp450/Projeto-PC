package com.duelo.client.states;

import com.duelo.client.core.Game;
import com.duelo.client.entities.Player;
import com.duelo.client.ui.Constants;

import processing.core.PApplet;

public class PlayState {
    private Game game;
    private PApplet p;
    private Player[] players;
    private int myPlayerId;
    private boolean[] keysPressed = new boolean[256];
    private float playAreaSize;
    private float playAreaX, playAreaY;
    
    public PlayState(Game game) {
        this.game = game;
        this.p = game;
        this.players = new Player[2];
        calculatePlayArea();
    }

    // Calculate largest square that fits in window
    private void calculatePlayArea() {
        float padding = 0;
        playAreaSize = Math.min(p.width, p.height) - padding*2;
        playAreaX = (p.width - playAreaSize)/2;
        playAreaY = (p.height - playAreaSize)/2;
    }
    
    public void draw() {
        p.background(Constants.BACKGROUND_COLOR);

        // Draw game elements
        for (Player player : players) {
            if (player != null) {
                Player mappedPlayer = player.clone();

                // Map normalized position (0-1) to play area coordinates
                float mappedX = playAreaX + mappedPlayer.getX() * playAreaSize;
                float mappedY = playAreaY + mappedPlayer.getY() * playAreaSize;

                mappedPlayer.setPosition(mappedX, mappedY);
                mappedPlayer.draw(p);
            }
        }

        // Draw black bars
        p.rectMode(PApplet.CORNER);
        p.fill(0);
        if (playAreaX > 0) {
            // Vertical bars
            p.rect(0, 0, playAreaX, p.height); // Left bar
            p.rect(playAreaX + playAreaSize, 0, playAreaX, p.height); // Right bar
        } 
        if (playAreaY > 0) {
            // Horizontal bars
            p.rect(0, 0, p.width, playAreaY); // Top bar
            p.rect(0, playAreaY + playAreaSize, p.width, playAreaY); // Bottom bar
        }
    }
    
    public void onMatchFound(int myId, String opponent) {
        this.myPlayerId = myId;

        // Initialize players
        players[0] = new Player(0, 0, 0xffff0000);
        players[1] = new Player(0, 0, 0xff0000ff);
    }
    
    public void onPlayerPositionChange(int id, float x, float y) {
        if (players[id] != null) {
            players[id].setPosition(x, y);
        }
    }

    public void keyPressed(char key, int keyCode) {
        if (key >= 0 && key < 256 && !keysPressed[key]) {
            game.sendCommand("/pressed " + key);
            keysPressed[key] = true;
        }
    }

    public void keyReleased(char key, int keyCode) {
        game.sendCommand("/unpressed " + key);
        if (key >= 0 && key < 256) keysPressed[key] = false;
    }
}