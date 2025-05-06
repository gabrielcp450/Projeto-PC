package com.duelo.client;

import javax.swing.*;
import java.awt.*;

public class MainFrame extends JFrame {
    private CardLayout cardLayout;
    private JPanel mainPanel;
    private LoginPanel loginPanel;
    private MainMenuPanel mainMenuPanel;
    private GamePanel gamePanel;
    private RankingsPanel rankingsPanel;
    private ProfilePanel profilePanel;
    private String currentUsername;

    public MainFrame() {
        setTitle("Duelo Game");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setResizable(false);
        setSize(900, 600);
        setLocationRelativeTo(null);
        getContentPane().setBackground(new Color(240, 240, 240));

        // Setup card layout for switching between panels
        cardLayout = new CardLayout();
        mainPanel = new JPanel(cardLayout);
        mainPanel.setOpaque(false);

        // Create panels
        loginPanel = new LoginPanel(this);
        mainMenuPanel = new MainMenuPanel(this);
        rankingsPanel = new RankingsPanel(this, currentUsername);
        
        // Add panels to card layout
        mainPanel.add(loginPanel, "LOGIN");
        mainPanel.add(mainMenuPanel, "MENU");
        mainPanel.add(rankingsPanel, "RANKINGS");

        // Add main panel to frame
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Show login panel by default
        cardLayout.show(mainPanel, "LOGIN");
    }

    public void showLogin() {
        cardLayout.show(mainPanel, "LOGIN");
        loginPanel.requestFocusInWindow();
    }

    public void showMainMenu(String username) {
        this.currentUsername = username;
        mainMenuPanel.setUsername(username);
        cardLayout.show(mainPanel, "MENU");
        mainMenuPanel.requestFocusInWindow();
    }

    public void showGamePanel(String username) {
        if (gamePanel != null) {
            mainPanel.remove(gamePanel);
        }
        gamePanel = new GamePanel(this, username);
        mainPanel.add(gamePanel, "GAME");
        cardLayout.show(mainPanel, "GAME");
        gamePanel.requestFocusInWindow();
    }

    public void showRankings() {
        if (rankingsPanel != null) {
            mainPanel.remove(rankingsPanel);
        }
        rankingsPanel = new RankingsPanel(this, currentUsername);
        mainPanel.add(rankingsPanel, "RANKINGS");
        cardLayout.show(mainPanel, "RANKINGS");
        rankingsPanel.requestFocusInWindow();
    }

    public void showProfile(String username) {
        if (profilePanel != null) {
            mainPanel.remove(profilePanel);
        }
        profilePanel = new ProfilePanel(this, username);
        mainPanel.add(profilePanel, "PROFILE");
        cardLayout.show(mainPanel, "PROFILE");
        profilePanel.requestFocusInWindow();
    }

    public String getCurrentUsername() {
        return currentUsername;
    }
} 