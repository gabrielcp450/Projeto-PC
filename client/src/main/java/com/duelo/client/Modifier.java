package com.duelo.client;

import processing.core.PApplet;

/**
 * Classe que representa modificadores (power-ups) no jogo.
 * 
 * Tipos de modificadores:
 * - VERDE: Aumenta a velocidade dos projéteis
 * - LARANJA: Diminui a velocidade dos projéteis
 * - AZUL: Diminui o intervalo entre tiros
 * - VERMELHO: Aumenta o intervalo entre tiros
 * 
 * Cada modificador tem uma duração limitada e afeta
 * as características do jogador que o coleta.
 */
public class Modifier {
    public enum Type {
        GREEN(0, 255, 0),    // Aumenta velocidade dos projéteis
        ORANGE(255, 165, 0), // Diminui velocidade dos projéteis
        BLUE(0, 0, 255),     // Diminui intervalo entre tiros
        RED(255, 0, 0);      // Aumenta intervalo entre tiros
        
        private final int r, g, b;
        
        Type(int r, int g, int b) {
            this.r = r;
            this.g = g;
            this.b = b;
        }
    }
    
    private static final int SIZE = 25; // Tamanho entre projéteis e jogadores
    
    private float x, y;
    private Type type;
    private boolean active;
    private long spawnTime;
    private static final long MAX_LIFETIME = 10000; // 10 segundos
    
    public Modifier(float x, float y, Type type) {
        this.x = x;
        this.y = y;
        this.type = type;
        this.active = true;
        this.spawnTime = System.currentTimeMillis();
    }
    
    public void draw(PApplet app) {
        if (!active) return;
        
        app.fill(type.r, type.g, type.b);
        app.noStroke();
        app.ellipse(x, y, SIZE, SIZE);
    }
    
    public boolean checkCollision(float playerX, float playerY, float playerSize) {
        if (!active) return false;
        
        float dx = x - playerX;
        float dy = y - playerY;
        float distance = (float) Math.sqrt(dx * dx + dy * dy);
        
        return distance < (SIZE + playerSize) / 2;
    }
    
    public void collect() {
        active = false;
    }
    
    public boolean isActive() {
        return active;
    }
    
    public Type getType() {
        return type;
    }
    
    public float getX() {
        return x;
    }
    
    public float getY() {
        return y;
    }
    
    public boolean isExpired() {
        return System.currentTimeMillis() - spawnTime > MAX_LIFETIME;
    }
} 