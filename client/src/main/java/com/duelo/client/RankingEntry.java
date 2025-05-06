package com.duelo.client;

/**
 * Classe que representa uma entrada no ranking.
 * Armazena informações sobre o jogador e seu nível.
 */
public class RankingEntry {
    private String username;
    private int level;
    
    public RankingEntry(String username, int level) {
        this.username = username;
        this.level = level;
    }
    
    public String getUsername() {
        return username;
    }
    
    public int getLevel() {
        return level;
    }
} 