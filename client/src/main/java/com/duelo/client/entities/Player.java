package com.duelo.client.entities;

import processing.core.PApplet;

/**
 * Classe que representa um jogador no jogo.
 * Gerencia movimento, tiros e modificadores do jogador.
 * 
 * Características:
 * - Movimento suave e responsivo
 * - Movimento diagonal otimizado
 * - Sistema de tiro com cooldown
 * - Gerenciamento de projéteis
 * - Sistema de modificadores temporários
 * 
 * Modificadores:
 * - Velocidade de projétil
 * - Cooldown de tiro
 */
public class Player {
    private static final int SIZE = 40;
    
    private float x, y;
    private int color;
    
    public Player(float x, float y, int color) {
        this.x = x;
        this.y = y;
        this.color = color;
    }

    public Player clone() {
        return new Player(x, y, color);
    }
    
    // public void shoot(float targetX, float targetY) {
    //     long currentTime = System.currentTimeMillis();
    //     if (currentTime - lastShootTime >= getCurrentShootCooldown()) {
    //         float dx = targetX - x;
    //         float dy = targetY - y;
    //         float angle = (float) Math.atan2(dy, dx);
            
    //         projectiles.add(new Projectile(x, y, angle, getCurrentProjectileSpeed()));
    //         lastShootTime = currentTime;
    //     }
    // }
    
    public void draw(PApplet p) {
        // Draw player
        p.ellipseMode(PApplet.CENTER);
        p.fill(color);
        p.noStroke();
        p.ellipse(x, y, SIZE, SIZE);
        
        // // Draw projectiles
        // for (Projectile p : projectiles) {
        //     p.draw(app);
        // }
    }

    public void setPosition(float x, float y) {
        this.x = x;
        this.y = y;
    }

    public float getX() {
        return x;
    }

    public float getY() {
        return y;
    }

    public int getColor() {
        return color;
    }
} 