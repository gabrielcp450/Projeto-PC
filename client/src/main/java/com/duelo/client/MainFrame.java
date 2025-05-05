package com.duelo.client;

import javax.swing.*;
import java.awt.*;

public class MainFrame extends JFrame {
    private CardLayout cardLayout;
    private JPanel mainPanel;
    private LoginPanel loginPanel;
    private MainMenuPanel mainMenuPanel;

    public MainFrame() {
        setTitle("Duelo");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(900, 600);
        setLocationRelativeTo(null);
        setResizable(false);
        getContentPane().setBackground(new Color(240, 240, 240));

        // Setup card layout for switching between panels
        cardLayout = new CardLayout();
        mainPanel = new JPanel(cardLayout);
        mainPanel.setOpaque(false);

        // Create panels
        loginPanel = new LoginPanel(this);
        mainMenuPanel = new MainMenuPanel(this);

        // Add panels to card layout
        mainPanel.add(loginPanel, "LOGIN");
        mainPanel.add(mainMenuPanel, "MAIN_MENU");

        // Add main panel to frame
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Show login panel by default
        showLoginPanel();
    }

    public void showLoginPanel() {
        loginPanel.clearFields();
        cardLayout.show(mainPanel, "LOGIN");
    }

    public void showMainMenuPanel(String username) {
        mainMenuPanel.setUsername(username);
        cardLayout.show(mainPanel, "MAIN_MENU");
    }
} 