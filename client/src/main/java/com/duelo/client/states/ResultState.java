package com.duelo.client.states;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.core.State;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;

import processing.core.PApplet;
import processing.core.PFont;

public class ResultState extends State {
    private int result = 0;
    private Button backButton;
    private PFont buttonFont;

    public ResultState(Game game) {
        super(game);

        buttonFont = game.createFont("Arial-Bold", 20);

        backButton = new Button(game, game.width / 2, game.height - 100, 220, 54, "Back", Constants.PRIMARY_COLOR,
                buttonFont, () -> {
                    game.changeState(GameState.MENU);
                });
    }

    public void setResult(int result) {
        this.result = result;
    }

    @Override
    public void draw() {
        // Implement the drawing logic for the result state
        // This could include displaying the results of a match, player stats, etc.

        game.background(Constants.BACKGROUND_COLOR);

        game.textAlign(PApplet.CENTER, PApplet.CENTER);
        game.textSize(32);
        game.fill(0);
        String scoreText = String.format("Result: " + (result == 1 ? "Win" : result == -1 ? "Loss" : "Draw"));
        game.text(scoreText, game.width / 2, game.height / 2);

        backButton.draw();
    }

    @Override
    public void keyPressed() {
        // Handle key presses specific to the result state
    }

    @Override
    public void keyReleased() {
        // Handle key releases specific to the result state
    }

    @Override
    public void mousePressed() {
        // Handle mouse presses specific to the result state
        backButton.checkClick();
    }

    @Override
    public void mouseMoved() {
        // Handle mouse movements specific to the result state
    }

}
