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

public class LoginState {
    private final PApplet p;

    // Fonts
    private PFont titleFont;
    private PFont textFont;
    private PFont buttonFont;
    private PFont inputFont;
    
    // Strings
    private String usernameInput = "";
    private String passwordInput = "";
    private String errorMessage = "";

    // Input Fields
    private final List<InputField> inputFields = new ArrayList<>();

    // Buttons
    private final List<Button> buttons = new ArrayList<>();

    public LoginState(Game game) {
        this.p = game;

        // Load fonts
        titleFont = p.createFont("Arial-Bold", 32);
        textFont = p.createFont("Arial-Bold", 18);
        buttonFont = p.createFont("Arial-Bold", 20);
        inputFont = p.createFont("Arial", 16);

        // Username Input
        inputFields.add(new InputField(p, p.width/2, 250, Constants.INPUT_WIDTH, Constants.INPUT_HEIGHT, "Username", () -> usernameInput, (v) -> usernameInput = v, true, textFont, inputFont));
        
        // Password Input
        inputFields.add(new InputField(p, p.width/2, 320, Constants.INPUT_WIDTH, Constants.INPUT_HEIGHT, "Password", () -> passwordInput, (v) -> passwordInput = v, false, textFont, inputFont));
        
        // Login Button
        buttons.add(new Button(p, p.width/2, 400, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Login", Constants.PRIMARY_COLOR, buttonFont, () -> {
            if (usernameInput.isEmpty() || passwordInput.isEmpty()) {
                errorMessage = "Please fill in all fields";
                return;
            }
            
            errorMessage = game.getAuthManager().login(usernameInput, passwordInput);
            if (errorMessage == null) {
                errorMessage = "";
                game.changeState(GameState.MENU);
            }
        }));
        
        // Register Button
        buttons.add(new Button(p, p.width/2, 470, Constants.BUTTON_WIDTH, Constants.BUTTON_HEIGHT, "Register", Constants.SECONDARY_COLOR, buttonFont, () -> {
            if (usernameInput.isEmpty() || passwordInput.isEmpty()) {
                errorMessage = "Please fill in all fields";
                return;
            }
            
            errorMessage = game.getAuthManager().register(usernameInput, passwordInput);
            if (errorMessage == null) errorMessage = "";
        }));
    }

    public void draw() {
        p.background(Constants.BACKGROUND_COLOR);

        // Title
        p.textFont(titleFont);
        p.fill(Constants.TEXT_COLOR);
        p.textAlign(PApplet.CENTER, PApplet.CENTER);
        p.text("Duelo Game", p.width/2, 100);
        
        // Error message
        if (!errorMessage.isEmpty()) {
            p.textFont(textFont);
            p.fill(Constants.ERROR_COLOR);
            p.text(errorMessage, p.width/2, 150);
        }
        
        // Draw input fields and buttons
        inputFields.forEach(InputField::draw);
        buttons.forEach(Button::draw);
    }

    private void toggleActiveInputField() {
        InputField activeField = inputFields.stream()
            .filter(InputField::isActive)
            .findFirst()
            .orElse(null);
        int pos = 0;
        for (InputField field : inputFields) {
            if (field == activeField) {
                field.setActive(false);
                break;
            }
            pos++;
        }
        int npos = (pos + 1) % inputFields.size();
        inputFields.get(npos).setActive(true);
    }

    public void keyPressed(char key, int keyCode) {
        System.out.println("login menu key " + key);
        if (key == PApplet.TAB) {
            toggleActiveInputField();
        } else {
            InputField activeField = inputFields.stream()
                .filter(InputField::isActive)
                .findFirst()
                .orElse(null);
            
            if (activeField != null) {
                activeField.handleKey(key, keyCode);
            }
        }
    }

    public void mousePressed() {
        inputFields.forEach(InputField::checkClick);
        buttons.forEach(Button::checkClick);
    }
}
