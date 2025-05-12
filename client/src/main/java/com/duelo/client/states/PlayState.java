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
    
    public PlayState(Game game) {
        this.game = game;
        this.p = game;
        this.players = new Player[2];
    }
    
    public void setup() {
        // Initialize game
    }
    
    public void draw() {
        p.background(Constants.BACKGROUND_COLOR);

        // Draw game elements
        for (Player player : players) {
            if (player != null) player.draw(p);
        }
    }
    
    public void onMatchFound(int myId, String opponent) {
        this.myPlayerId = myId;

        // Initialize players
        players[0] = new Player(0, 0, 0xffff0000);
        players[1] = new Player(0, 0, 0xff0000ff);
    }
    
    public void onPlayerPositionChange(int id, int x, int y) {
        if (players[id] != null) {
            players[id].setPosition(x, y);
        }
    }
    public void keyPressed(char key, int keyCode) {
        if (keyCode == 'w') {
            game.sendCommand("/pressed w");
        }
        else if (keyCode == 's') {
            game.sendCommand("/pressed s");
        }
        else if (keyCode == 'a') {
            game.sendCommand("/pressed s");
        }
        else if (keyCode == 'd') {
            game.sendCommand("/pressed s");
        }
    }
    public void keyReleased(char key, int keyCode) {
        if (keyCode == 'w') {
            game.sendCommand("/unpressed w");
        }
        else if (keyCode == 's') {
            game.sendCommand("/unpressed s");
        }
        else if (keyCode == 'a') {
            game.sendCommand("/unpressed s");
        }
        else if (keyCode == 'd') {
            game.sendCommand("/unpressed s");
        }
    }
}