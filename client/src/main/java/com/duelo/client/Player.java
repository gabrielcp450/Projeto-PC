package com.duelo.client;

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
    
    public void draw(PApplet app) {
        // Draw player
        app.fill(color);
        app.noStroke();
        app.ellipse(x, y, SIZE, SIZE);
        
        // // Draw projectiles
        // for (Projectile p : projectiles) {
        //     p.draw(app);
        // }
    }

    public void setPosition(int x, int y) {
        this.x = x;
        this.y = y;
    }
} 