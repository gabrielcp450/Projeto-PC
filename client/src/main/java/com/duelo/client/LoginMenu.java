package com.duelo.client;

import processing.core.PApplet;

public class LoginMenu extends Menu {
    private String username = "";
    private String password = "";
    private boolean isUsernameSelected = true;
    private Button loginButton;
    private Button registerButton;
    private float usernameY, passwordY;
    private float fieldWidth = 300;
    private float fieldHeight = 40;
    private float fieldSpacing = 60;
    private float cursorBlink = 0;
    private boolean showCursor = true;

    public LoginMenu(DueloGame game) {
        super(game, "Login");
        float buttonWidth = 200;
        float buttonHeight = 40;
        float centerX = game.width/2 - buttonWidth/2;
        float centerY = game.height/2;

        usernameY = centerY - 50;
        passwordY = centerY + 10;

        loginButton = new Button(centerX, centerY + 80, buttonWidth, buttonHeight, "Login", () -> {
            if (!username.isEmpty() && !password.isEmpty()) {
                game.setUsername(username);
                game.showMainMenu();
            }
        });

        registerButton = new Button(centerX, centerY + 130, buttonWidth, buttonHeight, "Register", () -> {
            // TODO: Implementar registro
        });

        buttons = new Button[] { loginButton, registerButton };
    }

    @Override
    public void draw() {
        super.draw();

        // Atualiza o cursor piscante
        cursorBlink += 0.1f;
        if (cursorBlink >= 1) {
            cursorBlink = 0;
            showCursor = !showCursor;
        }

        // Desenha os campos de texto
        drawTextField("Username", username, usernameY, isUsernameSelected);
        drawTextField("Password", "*".repeat(password.length()), passwordY, !isUsernameSelected);

        // Desenha mensagem de erro se os campos estiverem vazios
        if (username.isEmpty() || password.isEmpty()) {
            game.fill(255, 0, 0, 150);
            game.textAlign(PApplet.CENTER, PApplet.CENTER);
            game.textSize(16);
            game.text("Please fill in all fields", game.width/2, passwordY + 40);
        }
    }

    private void drawTextField(String label, String value, float y, boolean isSelected) {
        float centerX = game.width/2;
        
        // Desenha o label
        game.fill(200);
        game.textAlign(PApplet.LEFT, PApplet.CENTER);
        game.textSize(16);
        game.text(label, centerX - fieldWidth/2, y - 25);

        // Desenha o campo de texto
        game.noFill();
        game.stroke(isSelected ? accentColor : 100);
        game.strokeWeight(2);
        game.rect(centerX - fieldWidth/2, y - fieldHeight/2, fieldWidth, fieldHeight, 10);
        game.strokeWeight(1);

        // Desenha o texto
        game.fill(255);
        game.textAlign(PApplet.LEFT, PApplet.CENTER);
        game.textSize(20);
        game.text(value, centerX - fieldWidth/2 + 10, y);

        // Desenha o cursor se o campo estiver selecionado
        if (isSelected && showCursor) {
            float textWidth = game.textWidth(value);
            game.stroke(255);
            game.line(centerX - fieldWidth/2 + 10 + textWidth, y - 10, 
                     centerX - fieldWidth/2 + 10 + textWidth, y + 10);
        }
    }

    @Override
    public void mousePressed() {
        super.mousePressed();

        // Verifica se clicou nos campos de texto
        float centerX = game.width/2;
        if (game.mouseX >= centerX - fieldWidth/2 && game.mouseX <= centerX + fieldWidth/2) {
            if (game.mouseY >= usernameY - fieldHeight/2 && game.mouseY <= usernameY + fieldHeight/2) {
                isUsernameSelected = true;
            } else if (game.mouseY >= passwordY - fieldHeight/2 && game.mouseY <= passwordY + fieldHeight/2) {
                isUsernameSelected = false;
            }
        }
    }

    public void keyPressed() {
        if (isUsernameSelected) {
            if (game.key == PApplet.BACKSPACE) {
                if (username.length() > 0) {
                    username = username.substring(0, username.length() - 1);
                }
            } else if (game.key != PApplet.CODED && game.key != PApplet.ENTER && game.key != PApplet.TAB) {
                username += game.key;
            }
        } else {
            if (game.key == PApplet.BACKSPACE) {
                if (password.length() > 0) {
                    password = password.substring(0, password.length() - 1);
                }
            } else if (game.key != PApplet.CODED && game.key != PApplet.ENTER && game.key != PApplet.TAB) {
                password += game.key;
            }
        }
    }
} 