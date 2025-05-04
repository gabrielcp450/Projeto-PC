package com.duelo.client;

import javax.swing.SwingUtilities;

public class Main {
    public static void main(String[] args) {
        // Garante que a interface seja criada na thread de eventos do Swing
        SwingUtilities.invokeLater(() -> {
            String username = AuthManager.loadSession();
            if (username != null) {
                new MainMenuFrame(username).setVisible(true);
            } else {
                new LoginFrame().setVisible(true);
            }
        });
    }
} 