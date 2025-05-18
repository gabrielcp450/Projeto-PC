package com.duelo.client.states;

import com.duelo.client.core.Game;
import com.duelo.client.core.GameState;
import com.duelo.client.core.State;
import com.duelo.client.ui.Button;
import com.duelo.client.ui.Constants;
import com.duelo.client.ui.InputField;

import java.util.ArrayList;
import java.util.List;

import processing.core.PFont;

public class ChangePasswordState extends State {
    private PFont titleFont;
    private PFont buttonFont;
    private PFont inputFont;
    private final List<InputField> inputFields = new ArrayList<>();
    private final List<Button> buttons = new ArrayList<>();
    private String oldPassword = "";
    private String newPassword = "";
    private String confirmPassword = "";
    private String message = "";

    public ChangePasswordState(Game game) {
        super(game);
        titleFont = game.createFont("Arial-Bold", 28);
        buttonFont = game.createFont("Arial-Bold", 20);
        inputFont = game.createFont("Arial", 16);

        inputFields.add(new InputField(game, game.width / 2, 220, Constants.INPUT_WIDTH, Constants.INPUT_HEIGHT,
                "Old Password", () -> oldPassword, v -> oldPassword = v, false, buttonFont, inputFont));
        inputFields.add(new InputField(game, game.width / 2, 290, Constants.INPUT_WIDTH, Constants.INPUT_HEIGHT,
                "New Password", () -> newPassword, v -> newPassword = v, false, buttonFont, inputFont));
        inputFields.add(new InputField(game, game.width / 2, 360, Constants.INPUT_WIDTH, Constants.INPUT_HEIGHT,
                "Confirm Password", () -> confirmPassword, v -> confirmPassword = v, false, buttonFont, inputFont));

        buttons.add(new Button(game, game.width / 2, 440, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Submit",
                Constants.PRIMARY_COLOR, buttonFont, () -> {
                    if (oldPassword.isEmpty() || newPassword.isEmpty() || confirmPassword.isEmpty()) {
                        message = "All fields are required.";
                        return;
                    }
                    if (!newPassword.equals(confirmPassword)) {
                        message = "New passwords do not match.";
                        return;
                    }
                    String result = game.getAuthManager().changePassword(oldPassword, newPassword);
                    if (result == null) {
                        message = "Password changed successfully!";
                        // Optionally, return to menu: game.changeState(GameState.MENU);
                    } else {
                        message = result;
                    }
                }));
        buttons.add(new Button(game, game.width / 2, 510, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Back",
                Constants.SECONDARY_COLOR, buttonFont, () -> {
                    game.changeState(GameState.MENU);
                }));
    }

    @Override
    public void draw() {
        game.background(Constants.BACKGROUND_COLOR);
        game.textFont(titleFont);
        game.fill(Constants.TEXT_COLOR);
        game.textAlign(game.CENTER, game.CENTER);
        game.text("Change Password", game.width / 2, 120);

        inputFields.forEach(InputField::draw);
        buttons.forEach(Button::draw);

        if (!message.isEmpty()) {
            game.textFont(inputFont);
            game.fill(Constants.ERROR_COLOR);
            game.text(message, game.width / 2, 580);
        }
    }

    @Override
    public void mousePressed() {
        inputFields.forEach(InputField::checkClick);
        buttons.forEach(Button::checkClick);
    }

    @Override
    public void keyPressed() {
        // Permite navegação entre campos
        if (game.key == '\t') {
            int active = -1;
            for (int i = 0; i < inputFields.size(); i++) {
                if (inputFields.get(i).isActive()) {
                    inputFields.get(i).setActive(false);
                    active = i;
                    break;
                }
            }
            int next = (active + 1) % inputFields.size();
            inputFields.get(next).setActive(true);
        } else {
            inputFields.stream().filter(InputField::isActive).findFirst().ifPresent(f -> f.handleKey(game.key, game.keyCode));
        }
    }
} 