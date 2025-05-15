package com.duelo.client.states;

import java.util.HashMap;
import java.util.Map;

import com.duelo.client.core.Game;
import com.duelo.client.core.State;
import com.duelo.client.entities.Modifier;
import com.duelo.client.entities.Player;
import com.duelo.client.entities.Projectile;
import com.duelo.client.ui.Constants;
import com.duelo.client.ui.HUD;

import processing.core.PApplet;
import processing.sound.*;

public class PlayState extends State {
    private final Player[] players = new Player[2];
    private final Map<Integer, Projectile> projs = new HashMap<>();
    private final Map<Integer, Modifier> modifiers = new HashMap<>();
    private final HUD hud;
    private final boolean[] keysPressed = new boolean[256];
    private final SoundFile projHitSound;
    private final SoundFile wallHitSound;

    private int playAreaSize, playAreaX, playAreaY;
    private int myPlayerId;

    public PlayState(Game game) {
        super(game);
        this.hud = new HUD(game);
        this.projHitSound = new SoundFile(game, "assets/proj_hit.mp3");
        this.wallHitSound = new SoundFile(game, "assets/wall_hit.mp3");
        Sound.volume(0.5f);
        calculatePlayArea();
    }

    private void calculatePlayArea() {
        int padding = 0;
        playAreaSize = (int) (Math.min(game.width, game.height) - padding * 2);
        playAreaX = (game.width - playAreaSize) / 2;
        playAreaY = (game.height - playAreaSize) / 2;
    }

    @Override
    public void draw() {
        game.background(Constants.BACKGROUND_COLOR);

        // Update aim direction based on mouse position
        // Normalize mouse position to the playArea
        float normalizedX = (game.mouseX - playAreaX) / (float) playAreaSize;
        float normalizedY = (game.mouseY - playAreaY) / (float) playAreaSize;
        game.sendCommand("/aim " + normalizedX + " " + normalizedY);

        // Draw game elements
        synchronized (this) {
            modifiers.values().forEach(mod -> mod.draw(game, playAreaX, playAreaY, playAreaSize));
            for (Player player : players) {
                if (player != null)
                    player.draw(game, playAreaX, playAreaY, playAreaSize);
            }
            projs.values().forEach(proj -> proj.draw(game, playAreaX, playAreaY, playAreaSize));
        }

        // Draw black bars
        game.rectMode(PApplet.CORNER);
        game.fill(0);
        if (playAreaX > 0) {
            // Vertical bars
            game.rect(0, 0, playAreaX, game.height); // Left bar
            game.rect(playAreaX + playAreaSize, 0, playAreaX, game.height); // Right bar
        }
        if (playAreaY > 0) {
            // Horizontal bars
            game.rect(0, 0, game.width, playAreaY); // Top bar
            game.rect(0, playAreaY + playAreaSize, game.width, playAreaY); // Bottom bar
        }

        // Draw HUD
        hud.render();
    }

    @Override
    public void keyPressed() {
        if (game.key >= 0 && game.key < 256 && !keysPressed[game.key]) {
            game.sendCommand("/pressed " + game.key);
            keysPressed[game.key] = true;
        }
    }

    @Override
    public void keyReleased() {
        game.sendCommand("/unpressed " + game.key);
        if (game.key >= 0 && game.key < 256)
            keysPressed[game.key] = false;
    }

    @Override
    public void mousePressed() {
        float normalizedX = (game.mouseX - playAreaX) / (float) playAreaSize;
        float normalizedY = (game.mouseY - playAreaY) / (float) playAreaSize;
        game.sendCommand("/clicked " + normalizedX + " " + normalizedY);
    }

    public void onMatchFound(int myId, String opponent) {
        this.myPlayerId = myId;

        String player0Name = myId == 0 ? "You" : opponent;
        String player1Name = myId == 1 ? "You" : opponent;

        players[0] = new Player(player0Name, 0, 0, 0xffff0000);
        players[1] = new Player(player1Name, 0, 0, 0xff0000ff);
        hud.setColors(players[0].getColor(), players[1].getColor());
        hud.setLocalPlayerId(myPlayerId);
    }

    public void onMatchFinished() {
        // reset everything
        for (int i = 0; i < players.length; i++) {
            players[i] = null;
        }
        projs.clear();
        modifiers.clear();
        hud.reset();
        for (int i = 0; i < keysPressed.length; i++) {
            keysPressed[i] = false;
        }
    }

    synchronized public void onPlayerPositionChange(int id, float x, float y) {
        if (players[id] != null) {
            players[id].setPosition(x, y);
        }
    }

    synchronized public void onPlayerAimChange(int id, float x, float y) {
        if (players[id] != null) {
            players[id].setAim(x, y);
        }
    }

    synchronized public void onProjPositionChange(int id, float x, float y) {
        if (projs.containsKey(id)) {
            projs.get(id).setPosition(x, y);
        } else {
            projs.put(id, new Projectile(x, y));
        }
    }

    synchronized public void onProjRemoved(int id) {
        projs.remove(id);
    }

    synchronized public void onModifierCreate(int id, int type, float x, float y) {
        if (!modifiers.containsKey(id)) {
            modifiers.put(id, new Modifier(type, x, y));
        }
    }

    synchronized public void onModifierRemove(int id) {
        modifiers.remove(id);
    }

    public void updateScore(int score0, int score1) {
        int prevScore0, prevScore1;
        if (myPlayerId == 0) {
            prevScore0 = hud.getPlayerScore();
            prevScore1 = hud.getOpponentScore();
            hud.updateScores(score0, score1);
        } else {
            prevScore0 = hud.getOpponentScore();
            prevScore1 = hud.getPlayerScore();
            hud.updateScores(score1, score0);
        }

        // if (score0 == prevScore0 + 2 || score1 == prevScore1 + 2) {
        // wallHitSound.play();
        // } else {
        // projHitSound.play();
        // }
    }
}