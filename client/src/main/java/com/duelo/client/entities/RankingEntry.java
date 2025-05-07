package com.duelo.client.entities;

public class RankingEntry {
    private String username;
    private int level;
    private int winStreak;
    private int lossStreak;

    public RankingEntry(String username, int level, int winStreak, int lossStreak) {
        this.username = username;
        this.level = level;
        this.winStreak = winStreak;
        this.lossStreak = lossStreak;
    }

    public String getUsername() { return username; }
    public int getLevel() { return level; }
    public int getWinStreak() { return winStreak; }
    public int getLossStreak() { return lossStreak; }
}