package com.duelo.client.states;

import java.util.ArrayList;
import java.util.List;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;
import com.duelo.client.ui.InputField;

import processing.core.PApplet;
import processing.core.PFont;

public class MenuState {
    private final Game game;
    private final PApplet p;

    // Fonts
    private PFont titleFont;
    private PFont textFont;
    private PFont buttonFont;
    private PFont inputFont;
    
    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public MenuState(Game game) {
        this.game = game;
        this.p = game;

        // Load fonts
        titleFont = p.createFont("Arial-Bold", 32);
        textFont = p.createFont("Arial-Bold", 18);
        buttonFont = p.createFont("Arial-Bold", 20);
        inputFont = p.createFont("Arial", 16);

        // Play Button
        buttons.add(new Button(p, p.width/2, 250, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Play", Constants.PRIMARY_COLOR, buttonFont, () -> {
            game.enterQueue();
        }));
        
        // Rankings Button
        buttons.add(new Button(p, p.width/2, 330, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Rankings", Constants.SECONDARY_COLOR, buttonFont, () -> {
            game.changeState(GameState.RANKINGS);
        }));
        
        // Profile Button
        buttons.add(new Button(p, p.width/2, 410, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Profile", Constants.SECONDARY_COLOR, buttonFont, () -> {
            game.changeState(GameState.PROFILE);
        }));
        
        // Logout Button
        buttons.add(new Button(p, p.width/2, 490, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Logout", Constants.ACCENT_COLOR, buttonFont, () -> {
            if (game.getAuthManager().logout()) {
                game.changeState(GameState.LOGIN);
            }
        }));
    }

    public void draw() {
        p.background(Constants.BACKGROUND_COLOR);

        // Title
        p.textFont(titleFont);
        p.fill(Constants.TEXT_COLOR);
        p.text("Main Menu", p.width/2, 100);
        
        // Welcome message
        p.textFont(textFont);
        p.text("Welcome, " + game.getUsername(), p.width/2, 150);
        
        // Draw buttons
        buttons.forEach(Button::draw);
    }

    public void keyPressed(char key, int keyCode) {
        System.out.println("login menu key " + key);
    }

    public void mousePressed() {
        buttons.forEach(Button::checkClick);
    }
}
