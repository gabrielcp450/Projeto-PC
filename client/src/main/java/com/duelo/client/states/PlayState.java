package com.duelo.client.states;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.duelo.client.core.Game;
import com.duelo.client.core.State;
import com.duelo.client.entities.Modifier;
import com.duelo.client.entities.Player;
import com.duelo.client.entities.Projectile;
import com.duelo.client.entities.RankingEntry;
import com.duelo.client.ui.Constants;
import com.duelo.client.ui.HUD;

import processing.core.PApplet;
import processing.core.PFont;
import processing.sound.Sound;
import processing.sound.SoundFile;

public class PlayState extends State {
    private final Player[] players = new Player[2];
    private final Map<Integer, Projectile> projs = new HashMap<>();
    private final Map<Integer, Modifier> modifiers = new HashMap<>();
    private final HUD hud;
    private final boolean[] keysPressed = new boolean[256];
    private final SoundFile projHitSound;
    private final SoundFile wallHitSound;
    private final PFont inputFont, buttonFont;

    private int playAreaSize, playAreaX, playAreaY;
    private int myPlayerId;
    private boolean scoreTableVisible = false;
    private List<RankingEntry> rankings = new ArrayList<RankingEntry>();

    // Timer related fields
    private int matchStartTime = 0;
    private static final int GAME_DURATION = 120; // 2 minutes in seconds

    public PlayState(Game game) {
        super(game);
        this.hud = new HUD(game);
        this.projHitSound = new SoundFile(game, "assets/proj_hit.mp3");
        this.wallHitSound = new SoundFile(game, "assets/wall_hit.mp3");
        Sound.volume(0.5f);
        calculatePlayArea();

        this.inputFont = game.createFont("Arial", 16);
        this.buttonFont = game.createFont("Arial-Bold", 20);
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

        // Update timer
        int currentTime = game.millis() / 1000; // Convert to seconds
        int elapsedTime = currentTime - matchStartTime;
        int remainingTime = GAME_DURATION - elapsedTime;
        
        if (remainingTime <= 0) {
            // Game time is up
            game.sendCommand("/timeup");
            remainingTime = 0;
        }
        hud.updateTime(remainingTime);

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

        // Draw áreas não jogáveis (laterais) com cor aesthetic
        game.rectMode(PApplet.CORNER);
        game.noStroke();
        game.fill(60, 62, 90); // azul arroxeado aesthetic
        if (playAreaX > 0) {
            game.rect(0, 0, playAreaX, game.height); // Esquerda
            game.rect(playAreaX + playAreaSize, 0, playAreaX, game.height); // Direita
        }
        if (playAreaY > 0) {
            game.fill(60, 62, 90);
            game.rect(0, 0, game.width, playAreaY); // Topo
            game.rect(0, playAreaY + playAreaSize, game.width, playAreaY); // Baixo
        }

        // Linha vermelha grossa (zona proibida)
        game.stroke(220, 40, 40); // vermelho forte
        game.strokeWeight(8);
        game.line(playAreaX, 0, playAreaX, game.height); // Esquerda
        game.line(playAreaX + playAreaSize, 0, playAreaX + playAreaSize, game.height); // Direita
        game.noStroke();

        // Draw HUD
        hud.render(playAreaY);

        if (scoreTableVisible) {
            drawScoreTable();
        }
    }

    @Override
    public void keyPressed() {
        if (game.keyCode == PApplet.TAB) {
            game.sendCommand("/t");
        } else if (game.key >= 0 && game.key < 256 && !keysPressed[game.key]) {
            game.sendCommand("/pressed " + game.key);
            keysPressed[game.key] = true;
        }
    }

    @Override
    public void keyReleased() {
        if (game.keyCode == PApplet.TAB) {
            scoreTableVisible = false;
        } else if (game.key >= 0 && game.key < 256) {
            game.sendCommand("/unpressed " + game.key);
            keysPressed[game.key] = false;
        }
    }

    @Override
    public void mousePressed() {
        float normalizedX = (game.mouseX - playAreaX) / (float) playAreaSize;
        float normalizedY = (game.mouseY - playAreaY) / (float) playAreaSize;
        game.sendCommand("/clicked " + normalizedX + " " + normalizedY);
    }

    public void onMatchFound(int myId, String opponent) {
        this.myPlayerId = myId;
        this.matchStartTime = game.millis() / 1000; // Start counting from now

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
        matchStartTime = 0; // Reset match start time
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

    public void showRankings(List<RankingEntry> rankings) {
        this.rankings = rankings;
        scoreTableVisible = true;
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

        if (score0 == prevScore0 + 2 || score1 == prevScore1 + 2) {
            wallHitSound.play();
        } else {
            projHitSound.play();
        }
    }

    public void drawScoreTable() {
        // Semi-transparent grey overlay
        game.fill(50, 50, 50, 180); // dark grey with alpha
        game.noStroke();
        game.rectMode(PApplet.CORNER);
        game.rect(80, 80, game.width - 160, game.height - 160, 20);

        // Table layout
        int tableWidth = game.width - 200;
        int tableX = 100;
        int tableY = 140;
        int headerHeight = 52;
        int rowHeight = 44;
        int visibleRows = 7;

        // Column positions
        int colRank = tableX + 20;
        int colPlayer = colRank + 80;
        int colLevel = colPlayer + 220;
        int colWin = colLevel + 100;
        int colLoss = colWin + 100;

        // Header background
        game.fill(100, 100, 100, 220); // darker semi-transparent
        game.noStroke();
        game.rect(tableX, tableY, tableWidth, headerHeight, 12);

        // Header text
        game.textFont(buttonFont);
        game.fill(255);
        game.textAlign(PApplet.LEFT, PApplet.CENTER);
        int headerY = tableY + headerHeight / 2;
        game.text("Rank", colRank, headerY);
        game.text("Player", colPlayer, headerY);
        game.text("Level", colLevel, headerY);
        game.text("Win Streak", colWin, headerY);
        game.text("Loss Streak", colLoss, headerY);

        // Body rows
        game.textFont(inputFont);
        int rowY = tableY + headerHeight;

        for (int i = 0; i < PApplet.min(visibleRows, rankings.size()); i++) {
            int currentY = rowY + i * rowHeight + rowHeight / 2;

            // Row background
            if (i % 2 == 0)
                game.fill(255, 255, 255, 80);
            else
                game.fill(220, 220, 220, 80);

            game.noStroke();
            game.rect(tableX, rowY + i * rowHeight, tableWidth, rowHeight, 10);

            // Highlight top 3
            if (i == 0)
                game.fill(255, 215, 0);
            else if (i == 1)
                game.fill(192, 192, 192);
            else if (i == 2)
                game.fill(205, 127, 50);
            else
                game.fill(255);

            game.textAlign(PApplet.LEFT, PApplet.CENTER);
            game.text((i + 1) + ".", colRank, currentY);

            // Player data
            game.fill(255);
            RankingEntry entry = rankings.get(i);
            game.text(entry.getUsername(), colPlayer, currentY);
            game.text(String.valueOf(entry.getLevel()), colLevel, currentY);
            game.text(String.valueOf(entry.getWinStreak()), colWin, currentY);
            game.text(String.valueOf(entry.getLossStreak()), colLoss, currentY);
        }
    }
}