package com.duelo.client;

import processing.core.PApplet;
import java.util.ArrayList;
import java.util.List;

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
    private static final float SPEED = 6.0f;
    private static final int SIZE = 40;
    private static final int BASE_SHOOT_COOLDOWN = 500; // milliseconds
    private static final float BASE_PROJECTILE_SPEED = 10.0f;
    
    private float x, y;
    private float vx, vy;
    private int color;
    private long lastShootTime;
    private List<Projectile> projectiles;
    private PApplet app;
    
    // Controles
    private boolean movingUp = false;
    private boolean movingDown = false;
    private boolean movingLeft = false;
    private boolean movingRight = false;
    
    // Modifier effects
    private float projectileSpeedMultiplier = 1.0f;
    private float shootCooldownMultiplier = 1.0f;
    private long modifierEndTime = 0;
    private static final long MODIFIER_DURATION = 5000; // 5 seconds
    
    public Player(float x, float y, int color, PApplet app) {
        this.x = x;
        this.y = y;
        this.vx = 0;
        this.vy = 0;
        this.color = color;
        this.lastShootTime = 0;
        this.projectiles = new ArrayList<>();
        this.app = app;
    }
    
    public void setMovingUp(boolean moving) {
        this.movingUp = moving;
        updateVelocity();
    }
    
    public void setMovingDown(boolean moving) {
        this.movingDown = moving;
        updateVelocity();
    }
    
    public void setMovingLeft(boolean moving) {
        this.movingLeft = moving;
        updateVelocity();
    }
    
    public void setMovingRight(boolean moving) {
        this.movingRight = moving;
        updateVelocity();
    }
    
    private void updateVelocity() {
        // Calcula a direção baseada nas teclas pressionadas
        float dx = 0;
        float dy = 0;
        
        if (movingUp) dy -= 1;
        if (movingDown) dy += 1;
        if (movingLeft) dx -= 1;
        if (movingRight) dx += 1;
        
        // Normaliza o movimento diagonal
        float length = (float) Math.sqrt(dx * dx + dy * dy);
        if (length > 0) {
            dx = dx / length;
            dy = dy / length;
        }
        
        // Aplica a velocidade
        vx = dx * SPEED;
        vy = dy * SPEED;
    }
    
    private float constrain(float value, float min, float max) {
        return Math.min(Math.max(value, min), max);
    }
    
    public void update() {
        // Atualiza posição
        x += vx;
        y += vy;
        
        // Mantém o jogador dentro dos limites
        x = constrain(x, SIZE/2, app.width - SIZE/2);
        y = constrain(y, SIZE/2, app.height - SIZE/2);
        
        // Update projectiles
        for (int i = projectiles.size() - 1; i >= 0; i--) {
            Projectile p = projectiles.get(i);
            p.update();
            if (p.isOutOfBounds()) {
                projectiles.remove(i);
            }
        }
        
        // Check if modifier effects have expired
        if (System.currentTimeMillis() > modifierEndTime) {
            resetModifiers();
        }
    }
    
    public void shoot(float targetX, float targetY) {
        long currentTime = System.currentTimeMillis();
        if (currentTime - lastShootTime >= getCurrentShootCooldown()) {
            float dx = targetX - x;
            float dy = targetY - y;
            float angle = (float) Math.atan2(dy, dx);
            
            projectiles.add(new Projectile(x, y, angle, getCurrentProjectileSpeed()));
            lastShootTime = currentTime;
        }
    }
    
    public void draw(PApplet app) {
        // Draw player
        app.fill(color);
        app.noStroke();
        app.ellipse(x, y, SIZE, SIZE);
        
        // Draw projectiles
        for (Projectile p : projectiles) {
            p.draw(app);
        }
    }
    
    public void applyModifier(Modifier.Type type) {
        switch (type) {
            case GREEN:
                projectileSpeedMultiplier = 1.5f;
                break;
            case ORANGE:
                projectileSpeedMultiplier = 0.75f;
                break;
            case BLUE:
                shootCooldownMultiplier = 0.75f;
                break;
            case RED:
                shootCooldownMultiplier = 1.5f;
                break;
        }
        modifierEndTime = System.currentTimeMillis() + MODIFIER_DURATION;
    }
    
    private void resetModifiers() {
        projectileSpeedMultiplier = 1.0f;
        shootCooldownMultiplier = 1.0f;
    }
    
    private int getCurrentShootCooldown() {
        return (int) (BASE_SHOOT_COOLDOWN * shootCooldownMultiplier);
    }
    
    private float getCurrentProjectileSpeed() {
        return BASE_PROJECTILE_SPEED * projectileSpeedMultiplier;
    }
    
    public float getX() { return x; }
    public float getY() { return y; }
    public float getSize() { return SIZE; }
    public List<Projectile> getProjectiles() { return projectiles; }
    
    private class Projectile {
        private static final int SIZE = 10;
        
        private float x, y;
        private float vx, vy;
        
        public Projectile(float x, float y, float angle, float speed) {
            this.x = x;
            this.y = y;
            this.vx = (float) Math.cos(angle) * speed;
            this.vy = (float) Math.sin(angle) * speed;
        }
        
        public void update() {
            x += vx;
            y += vy;
        }
        
        public void draw(PApplet app) {
            app.fill(255);
            app.noStroke();
            app.ellipse(x, y, SIZE, SIZE);
        }
        
        public boolean isOutOfBounds() {
            return x < 0 || x > app.width || y < 0 || y > app.height;
        }
    }
} 