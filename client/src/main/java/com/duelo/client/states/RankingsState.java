package com.duelo.client.states;

import java.util.ArrayList;
import java.util.List;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.entities.RankingEntry;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;

import processing.core.PApplet;
import processing.core.PFont;
import processing.event.MouseEvent;

public class RankingsState {
    private final Game game;
    private final PApplet p;

    private List<RankingEntry> rankings = new ArrayList<>();
    private boolean rankingsLoaded = false;
    private float rankingScrollY = 0;
    private float rankingMaxScroll = 0;

    // Fonts
    private PFont titleFont;
    private PFont textFont;
    private PFont buttonFont;
    private PFont inputFont;
    
    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public RankingsState(Game game) {
        this.game = game;
        this.p = game;

        // Load fonts
        titleFont = p.createFont("Arial-Bold", 32);
        textFont = p.createFont("Arial-Bold", 18);
        buttonFont = p.createFont("Arial-Bold", 20);
        inputFont = p.createFont("Arial", 16);

        // Back Button
        buttons.add(new Button(p, p.width/2, p.height - 100, 220, 54, "Back", Constants.PRIMARY_COLOR, buttonFont, () -> {
            rankingsLoaded = false;
            rankingScrollY = 0;
            game.changeState(GameState.MENU);
        }));
    }

    public void draw() {
        if (!rankingsLoaded) {
            rankings = game.getRankings();
            rankingsLoaded = true;
        }

        p.background(Constants.BACKGROUND_COLOR);
        
        // Title
        p.textFont(titleFont);
        p.fill(Constants.TEXT_COLOR);
        p.textAlign(PApplet.CENTER, PApplet.CENTER);
        p.text("Rankings", p.width/2, 60);

        // Table parameters
        int tableWidth = 700;
        int colRank = p.width/2 - tableWidth/2 + 40;
        int colPlayer = colRank + 80;
        int colLevel = colPlayer + 220;
        int colWin = colLevel + 100;
        int colLoss = colWin + 100;
        int rowStart = 130;
        int rowHeight = 48;
        int headerHeight = 54;
        int visibleRows = 7;
        int tableX = p.width/2 - tableWidth/2;
        int tableY = rowStart - headerHeight/2;
        int tableHeight = headerHeight + visibleRows * rowHeight;
        int usersAreaTop = rowStart + headerHeight/2 + 16;
        int usersAreaBottom = usersAreaTop + visibleRows * rowHeight - 16;

        p.rectMode(PApplet.CENTER); 

        // Table background
        p.fill(255, 255, 255, 220);
        p.stroke(220);
        p.strokeWeight(2);
        p.rect(p.width/2, tableY + tableHeight/2, tableWidth, tableHeight + 20, 18);

        // Header (fixed position)
        p.fill(Constants.PRIMARY_COLOR);
        p.noStroke();
        p.rect(p.width/2, rowStart, tableWidth, headerHeight, 16);
        
        // Header text
        p.textFont(buttonFont);
        p.fill(255);
        p.textAlign(PApplet.LEFT, PApplet.CENTER);
        p.text("Rank", colRank, rowStart);
        p.text("Player", colPlayer, rowStart);
        p.text("Level", colLevel, rowStart);
        p.text("Win Streak", colWin, rowStart);
        p.text("Loss Streak", colLoss, rowStart);

        // Ranking data
        p.textFont(inputFont);
        int totalRows = rankings.size();
        rankingMaxScroll = PApplet.max(0, (totalRows - visibleRows) * rowHeight);
        rankingScrollY = PApplet.constrain(rankingScrollY, 0, rankingMaxScroll);

        // Draw visible rows only
        int startRow = PApplet.floor(rankingScrollY / rowHeight);
        int endRow = PApplet.min(startRow + visibleRows + 1, totalRows);

        for (int i = startRow; i < endRow; i++) {
            int y = rowStart + headerHeight/2 + (i - startRow + 1) * rowHeight - (int)(rankingScrollY % rowHeight);

            // Only draw if visible in the scroll area
            if (y >= usersAreaTop && y <= usersAreaBottom) {
                // Alternate row colors
                if (i % 2 == 0) {
                    p.fill(240, 245, 255, 180);
                } else {
                    p.fill(255, 255, 255, 180);
                }
                
                // Row background
                p.noStroke();
                p.rect(p.width/2, y, tableWidth - 8, rowHeight - 6, 12);

                // Highlight top 3
                if (i == 0) p.fill(255, 215, 0);       // Gold
                else if (i == 1) p.fill(192, 192, 192); // Silver
                else if (i == 2) p.fill(205, 127, 50);  // Bronze
                else p.fill(Constants.TEXT_COLOR);
                
                // Rank number
                p.textAlign(PApplet.LEFT, PApplet.CENTER);
                p.text((i + 1) + ".", colRank, y);

                // Player data
                p.fill(Constants.TEXT_COLOR);
                RankingEntry entry = rankings.get(i);
                p.text(entry.getUsername(), colPlayer, y);
                p.text(String.valueOf(entry.getLevel()), colLevel, y);
                p.text(String.valueOf(entry.getWinStreak()), colWin, y);
                p.text(String.valueOf(entry.getLossStreak()), colLoss, y);
            }
        }

        // Scroll bar (if needed)
        if (rankingMaxScroll > 0) {
            float scrollbarHeight = (visibleRows * rowHeight) * (visibleRows / (float)totalRows);
            float scrollbarY = PApplet.map(rankingScrollY, 0, rankingMaxScroll, 
                                        usersAreaTop, usersAreaBottom - scrollbarHeight);
            
            p.fill(200, 200, 200, 150);
            p.rect(p.width/2 + tableWidth/2 - 15, usersAreaTop, 8, usersAreaBottom - usersAreaTop, 4);
            p.fill(150, 150, 150, 200);
            p.rect(p.width/2 + tableWidth/2 - 15, scrollbarY, 8, scrollbarHeight, 4);
        }

        // Draw buttons (back button)
        buttons.forEach(Button::draw);
    }

    public void keyPressed(char key, int keyCode) {
        System.out.println("login menu key " + key);
    }

    public void mousePressed() {
        buttons.forEach(Button::checkClick);
    }

    public void mouseWheel(MouseEvent event) {
        float e = event.getCount();
        rankingScrollY += e * 32; // 32 pixels por scroll
        rankingScrollY = PApplet.constrain(rankingScrollY, 0, rankingMaxScroll);
        System.out.println("[DEBUG] mouseWheel chamado: e=" + e + ", rankingScrollY=" + rankingScrollY);
    }
}
