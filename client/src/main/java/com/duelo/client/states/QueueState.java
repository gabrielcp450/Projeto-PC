package com.duelo.client.states;

import java.util.ArrayList;
import java.util.List;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;

import processing.core.PApplet;
import processing.core.PFont;

public class QueueState {
    private final Game game;
    private final PApplet p;

    // Fonts
    private PFont titleFont;
    private PFont textFont;
    private PFont buttonFont;
    private PFont inputFont;
    
    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public QueueState(Game game) {
        this.game = game;
        this.p = game;

        // Load fonts
        titleFont = p.createFont("Arial-Bold", 32);
        textFont = p.createFont("Arial-Bold", 18);
        buttonFont = p.createFont("Arial-Bold", 20);
        inputFont = p.createFont("Arial", 16);

        // Cancel Button
        buttons.add(new Button(p, p.width/2, p.height - 100, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Cancel Search", Constants.ACCENT_COLOR, buttonFont, () -> {
            game.leaveQueue();
        }));
    }

    public void draw() {
        p.background(Constants.BACKGROUND_COLOR);

        // Title
        p.textFont(titleFont);
        p.fill(Constants.TEXT_COLOR);
        p.text("Searching for Match", p.width/2, p.height/2 - 50);
        
        // Loading animation
        float size = 50 + 10 * PApplet.sin(p.frameCount * 0.1f);
        p.noFill();
        p.stroke(Constants.PRIMARY_COLOR);
        p.strokeWeight(3);
        p.ellipse(p.width/2, p.height/2 + 50, size, size);
        
        // Draw cancel button
        buttons.forEach(Button::draw);
    }

    public void keyPressed(char key, int keyCode) {
        System.out.println("login menu key " + key);
    }

    public void mousePressed() {
        buttons.forEach(Button::checkClick);
    }
}
