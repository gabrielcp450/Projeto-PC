package com.duelo.client.states;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.core.State;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;

import java.util.ArrayList;
import java.util.List;

import processing.core.PFont;

public class MenuState extends State {
    // Fonts
    private PFont titleFont;
    private PFont textFont;
    private PFont buttonFont;

    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public MenuState(Game game) {
        super(game);

        // Load fonts
        titleFont = game.createFont("Arial-Bold", 32);
        textFont = game.createFont("Arial-Bold", 18);
        buttonFont = game.createFont("Arial-Bold", 20);

        // Play Button
        buttons.add(new Button(game, game.width / 2, 250, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Play",
                Constants.PRIMARY_COLOR, buttonFont, () -> {
                    game.enterQueue();
                }));

        // Rankings Button
        buttons.add(new Button(game, game.width / 2, 330, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Rankings",
                Constants.SECONDARY_COLOR, buttonFont, () -> {
                    game.changeState(GameState.RANKINGS);
                }));

        // Profile Button
        buttons.add(new Button(game, game.width / 2, 410, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Profile",
                Constants.SECONDARY_COLOR, buttonFont, () -> {
                    game.changeState(GameState.PROFILE);
                }));

        // Logout Button
        buttons.add(new Button(game, game.width / 2, 490, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Logout",
                Constants.ACCENT_COLOR, buttonFont, () -> {
                    if (game.getAuthManager().logout()) {
                        game.changeState(GameState.LOGIN);
                    }
                }));
    }

    @Override
    public void draw() {
        game.background(Constants.BACKGROUND_COLOR);

        // Title
        game.textFont(titleFont);
        game.fill(Constants.TEXT_COLOR);
        game.text("Main Menu", game.width / 2, 100);

        // Welcome message
        game.textFont(textFont);
        game.text("Welcome, " + game.getUsername(), game.width / 2, 150);

        // Draw buttons
        buttons.forEach(Button::draw);
    }

    @Override
    public void mousePressed() {
        buttons.forEach(Button::checkClick);
    }
}
