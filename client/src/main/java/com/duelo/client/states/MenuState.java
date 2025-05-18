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

    // Labels, colors e actions
    private final List<String> labels = List.of("Play", "Rankings", "Change Password", "Delete Account", "Logout");
    private final List<Integer> colors = List.of(
        Constants.PRIMARY_COLOR,
        Constants.SECONDARY_COLOR,
        Constants.SECONDARY_COLOR,
        Constants.ACCENT_COLOR,
        Constants.ACCENT_COLOR
    );
    private final List<Runnable> actions = new ArrayList<>();
    private final int btnWidth = Constants.BUTTON_WIDTH;
    private final int btnHeight = Constants.BUTTON_HEIGHT;
    // Lista temporária para os botões do frame atual
    private List<Button> tempButtons = new ArrayList<>();

    public MenuState(Game game) {
        super(game);
        // Load fonts
        titleFont = game.createFont("Arial-Bold", 32);
        textFont = game.createFont("Arial-Bold", 18);
        buttonFont = game.createFont("Arial-Bold", 20);

        // Define actions
        actions.add(() -> game.enterQueue());
        actions.add(() -> game.changeState(GameState.RANKINGS));
        actions.add(() -> game.changeState(GameState.CHANGE_PASSWORD));
        actions.add(() -> {
            if (game.getAuthManager().unregister(game.getUsername())) {
                game.changeState(GameState.LOGIN);
            }
        });
        actions.add(() -> {
            if (game.getAuthManager().logout()) {
                game.changeState(GameState.LOGIN);
            }
        });
    }

    @Override
    public void draw() {
        game.background(Constants.BACKGROUND_COLOR);
        game.textFont(titleFont);
        game.fill(Constants.TEXT_COLOR);
        game.textAlign(game.CENTER, game.CENTER);
        game.text("Main Menu", game.width / 2, game.height * 0.15f);

        game.textFont(textFont);
        game.text("Welcome, " + game.getUsername(), game.width / 2, game.height * 0.25f);

        // Cria e desenha botões dinamicamente
        tempButtons = new ArrayList<>();
        float startY = game.height * 0.38f;
        float spacing = game.height * 0.10f;
        for (int i = 0; i < labels.size(); i++) {
            int x = game.width / 2;
            int y = (int) (startY + i * spacing);
            Button btn = new Button(game, x, y, btnWidth, btnHeight, labels.get(i), colors.get(i), buttonFont, actions.get(i));
            tempButtons.add(btn);
            btn.draw();
        }
    }

    @Override
    public void mousePressed() {
        if (tempButtons != null) {
            tempButtons.forEach(Button::checkClick);
        }
    }
}
