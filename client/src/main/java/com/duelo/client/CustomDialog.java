package com.duelo.client;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyAdapter;

public class CustomDialog {
    public static void showErrorDialog(Component parent, String message) {
        JDialog dialog = createDialog(parent, "Error", message, new Color(231, 76, 60));
        JButton okButton = createButton("OK", new Color(231, 76, 60));
        setupDialog(dialog, okButton);
    }

    public static void showSuccessDialog(Component parent, String message) {
        JDialog dialog = createDialog(parent, "Success", message, new Color(46, 204, 113));
        JButton okButton = createButton("OK", new Color(46, 204, 113));
        setupDialog(dialog, okButton);
    }

    public static void showInfoDialog(Component parent, String message) {
        JDialog dialog = createDialog(parent, "Information", message, new Color(52, 152, 219));
        JButton okButton = createButton("OK", new Color(52, 152, 219));
        setupDialog(dialog, okButton);
    }

    private static JDialog createDialog(Component parent, String title, String message, Color color) {
        JFrame frame = (JFrame) SwingUtilities.getWindowAncestor(parent);
        JDialog dialog = new JDialog(frame, title, true);
        dialog.setLayout(new BorderLayout());
        dialog.setResizable(false);
        dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

        JPanel messagePanel = new JPanel(new BorderLayout());
        messagePanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
        messagePanel.setBackground(Color.WHITE);

        JLabel messageLabel = new JLabel(message);
        messageLabel.setFont(new Font("Arial", Font.PLAIN, 14));
        messageLabel.setForeground(new Color(52, 73, 94));
        messagePanel.add(messageLabel, BorderLayout.CENTER);

        dialog.add(messagePanel, BorderLayout.CENTER);
        return dialog;
    }

    private static JButton createButton(String text, Color color) {
        JButton button = new JButton(text);
        button.setFont(new Font("Arial", Font.BOLD, 14));
        button.setBackground(color);
        button.setForeground(Color.WHITE);
        button.setFocusPainted(true);
        button.setBorderPainted(false);
        button.setOpaque(true);
        button.setCursor(new Cursor(Cursor.HAND_CURSOR));
        button.setPreferredSize(new Dimension(100, 35));
        button.setMnemonic(KeyEvent.VK_O); // Alt+O para OK
        return button;
    }

    private static void setupDialog(JDialog dialog, JButton okButton) {
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setBackground(Color.WHITE);
        buttonPanel.add(okButton);

        dialog.add(buttonPanel, BorderLayout.SOUTH);

        // Adiciona KeyListener para Enter e Escape
        dialog.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER || e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    dialog.dispose();
                }
            }
        });

        // Adiciona KeyListener para Enter no botão
        okButton.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    dialog.dispose();
                }
            }
        });

        okButton.addActionListener(e -> dialog.dispose());

        dialog.pack();
        dialog.setLocationRelativeTo(dialog.getOwner());
        dialog.setVisible(true);
    }
} 