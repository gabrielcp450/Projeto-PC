package com.duelo.client.states;

import com.duelo.client.core.Game;
import com.duelo.client.core.State;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;

import java.util.ArrayList;
import java.util.List;

import processing.core.PApplet;
import processing.core.PFont;

public class QueueState extends State {
    // Fonts
    private PFont titleFont;
    private PFont buttonFont;

    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public QueueState(Game game) {
        super(game);

        // Load fonts
        titleFont = game.createFont("Arial-Bold", 32);
        buttonFont = game.createFont("Arial-Bold", 20);

        // Cancel Button
        buttons.add(new Button(game, game.width / 2, game.height - 100, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT,
                "Cancel Search", Constants.ACCENT_COLOR, buttonFont, () -> {
                    game.leaveQueue();
                }));
    }

    @Override
    public void draw() {
        game.background(Constants.BACKGROUND_COLOR);

        // Title
        game.textFont(titleFont);
        game.fill(Constants.TEXT_COLOR);
        game.text("Searching for Match", game.width / 2, game.height / 2 - 50);

        // Loading animation
        float size = 50 + 10 * PApplet.sin(game.frameCount * 0.1f);
        game.noFill();
        game.stroke(Constants.PRIMARY_COLOR);
        game.strokeWeight(3);
        game.ellipse(game.width / 2, game.height / 2 + 50, size, size);

        // Draw cancel button
        buttons.forEach(Button::draw);
    }

    @Override
    public void mousePressed() {
        buttons.forEach(Button::checkClick);
    }
}
