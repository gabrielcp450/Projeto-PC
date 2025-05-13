package com.duelo.client.states;

import java.util.ArrayList;
import java.util.List;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.core.State;
import com.duelo.client.entities.RankingEntry;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;

import processing.core.PApplet;
import processing.core.PFont;
import processing.event.MouseEvent;

public class RankingsState extends State {
    private List<RankingEntry> rankings = new ArrayList<>();
    private boolean rankingsLoaded = false;
    private float rankingScrollY = 0;
    private float rankingMaxScroll = 0;

    // Fonts
    private PFont titleFont;
    private PFont buttonFont;
    private PFont inputFont;

    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public RankingsState(Game game) {
        super(game);

        // Load fonts
        titleFont = game.createFont("Arial-Bold", 32);
        buttonFont = game.createFont("Arial-Bold", 20);
        inputFont = game.createFont("Arial", 16);

        // Back Button
        buttons.add(new Button(game, game.width / 2, game.height - 100, 220, 54, "Back", Constants.PRIMARY_COLOR,
                buttonFont, () -> {
                    rankingsLoaded = false;
                    rankingScrollY = 0;
                    game.changeState(GameState.MENU);
                }));
    }

    @Override
    public void draw() {
        if (!rankingsLoaded) {
            rankings = game.getRankings();
            rankingsLoaded = true;
        }

        game.background(Constants.BACKGROUND_COLOR);

        // Title
        game.textFont(titleFont);
        game.fill(Constants.TEXT_COLOR);
        game.textAlign(PApplet.CENTER, PApplet.CENTER);
        game.text("Rankings", game.width / 2, 60);

        // Table parameters
        int tableWidth = 700;
        int colRank = game.width / 2 - tableWidth / 2 + 40;
        int colPlayer = colRank + 80;
        int colLevel = colPlayer + 220;
        int colWin = colLevel + 100;
        int colLoss = colWin + 100;
        int rowStart = 130;
        int rowHeight = 48;
        int headerHeight = 54;
        int visibleRows = 7;
        int tableX = game.width / 2 - tableWidth / 2;
        int tableY = rowStart - headerHeight / 2;
        int tableHeight = headerHeight + visibleRows * rowHeight;
        int usersAreaTop = rowStart + headerHeight / 2 + 16;
        int usersAreaBottom = usersAreaTop + visibleRows * rowHeight - 16;

        game.rectMode(PApplet.CENTER);

        // Table background
        game.fill(255, 255, 255, 220);
        game.stroke(220);
        game.strokeWeight(2);
        game.rect(game.width / 2, tableY + tableHeight / 2, tableWidth, tableHeight + 20, 18);

        // Header (fixed position)
        game.fill(Constants.PRIMARY_COLOR);
        game.noStroke();
        game.rect(game.width / 2, rowStart, tableWidth, headerHeight, 16);

        // Header text
        game.textFont(buttonFont);
        game.fill(255);
        game.textAlign(PApplet.LEFT, PApplet.CENTER);
        game.text("Rank", colRank, rowStart);
        game.text("Player", colPlayer, rowStart);
        game.text("Level", colLevel, rowStart);
        game.text("Win Streak", colWin, rowStart);
        game.text("Loss Streak", colLoss, rowStart);

        // Ranking data
        game.textFont(inputFont);
        int totalRows = rankings.size();
        rankingMaxScroll = PApplet.max(0, (totalRows - visibleRows) * rowHeight);
        rankingScrollY = PApplet.constrain(rankingScrollY, 0, rankingMaxScroll);

        // Draw visible rows only
        int startRow = PApplet.floor(rankingScrollY / rowHeight);
        int endRow = PApplet.min(startRow + visibleRows + 1, totalRows);

        for (int i = startRow; i < endRow; i++) {
            int y = rowStart + headerHeight / 2 + (i - startRow + 1) * rowHeight - (int) (rankingScrollY % rowHeight);

            // Only draw if visible in the scroll area
            if (y >= usersAreaTop && y <= usersAreaBottom) {
                // Alternate row colors
                if (i % 2 == 0) {
                    game.fill(240, 245, 255, 180);
                } else {
                    game.fill(255, 255, 255, 180);
                }

                // Row background
                game.noStroke();
                game.rect(game.width / 2, y, tableWidth - 8, rowHeight - 6, 12);

                // Highlight top 3
                if (i == 0)
                    game.fill(255, 215, 0); // Gold
                else if (i == 1)
                    game.fill(192, 192, 192); // Silver
                else if (i == 2)
                    game.fill(205, 127, 50); // Bronze
                else
                    game.fill(Constants.TEXT_COLOR);

                // Rank number
                game.textAlign(PApplet.LEFT, PApplet.CENTER);
                game.text((i + 1) + ".", colRank, y);

                // Player data
                game.fill(Constants.TEXT_COLOR);
                RankingEntry entry = rankings.get(i);
                game.text(entry.getUsername(), colPlayer, y);
                game.text(String.valueOf(entry.getLevel()), colLevel, y);
                game.text(String.valueOf(entry.getWinStreak()), colWin, y);
                game.text(String.valueOf(entry.getLossStreak()), colLoss, y);
            }
        }

        // Scroll bar (if needed)
        if (rankingMaxScroll > 0) {
            float scrollbarHeight = (visibleRows * rowHeight) * (visibleRows / (float) totalRows);
            float scrollbarY = PApplet.map(rankingScrollY, 0, rankingMaxScroll, usersAreaTop,
                    usersAreaBottom - scrollbarHeight);

            game.fill(200, 200, 200, 150);
            game.rect(game.width / 2 + tableWidth / 2 - 15, usersAreaTop, 8, usersAreaBottom - usersAreaTop, 4);
            game.fill(150, 150, 150, 200);
            game.rect(game.width / 2 + tableWidth / 2 - 15, scrollbarY, 8, scrollbarHeight, 4);
        }

        // Draw buttons (back button)
        buttons.forEach(Button::draw);
    }

    @Override
    public void mousePressed() {
        buttons.forEach(Button::checkClick);
    }

    @Override
    public void mouseWheel(MouseEvent event) {
        float e = event.getCount();
        rankingScrollY += e * 32; // 32 pixels por scroll
        rankingScrollY = PApplet.constrain(rankingScrollY, 0, rankingMaxScroll);
    }
}
