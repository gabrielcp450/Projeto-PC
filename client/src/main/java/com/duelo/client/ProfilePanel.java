package com.duelo.client;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;

public class ProfilePanel extends JPanel {
    private MainFrame mainFrame;
    private String username;

    public ProfilePanel(MainFrame mainFrame, String username) {
        this.mainFrame = mainFrame;
        this.username = username;
        setLayout(new BorderLayout());
        setBackground(new Color(44, 62, 80));
        setBorder(new EmptyBorder(30, 30, 30, 30));

        // Título
        JLabel titleLabel = new JLabel("Player Profile", SwingConstants.CENTER) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
                g2d.setColor(new Color(0, 0, 0, 50));
                g2d.setFont(getFont());
                FontMetrics fm = g2d.getFontMetrics();
                int x = (getWidth() - fm.stringWidth(getText())) / 2;
                int y = (getHeight() - fm.getHeight()) / 2 + fm.getAscent() + 2;
                g2d.drawString(getText(), x, y);
                g2d.setColor(Color.WHITE);
                g2d.drawString(getText(), x, y - 2);
                g2d.dispose();
            }
        };
        titleLabel.setFont(new Font("Arial", Font.BOLD, 36));
        titleLabel.setBorder(new EmptyBorder(0, 0, 30, 0));
        add(titleLabel, BorderLayout.NORTH);

        // Painel central arredondado
        JPanel centerPanel = new JPanel(new BorderLayout()) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g;
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2d.setColor(new Color(236, 240, 241));
                g2d.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 30, 30));
                g2d.setColor(new Color(189, 195, 199));
                g2d.setStroke(new BasicStroke(2));
                g2d.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 30, 30));
            }
        };
        centerPanel.setOpaque(false);
        centerPanel.setBorder(new EmptyBorder(30, 30, 30, 30));

        // Dados fictícios
        String level = "10";
        String xp = "950/1000";
        String rank = "1";
        String winRate = "83.3%";
        String wins = "15";
        String losses = "3";
        String avgPoints = "1200";
        String[][] recentMatches = {
            {"davide", "Win", "+1300"},
            {"player3", "Loss", "+900"},
            {"player4", "Win", "+1100"},
            {"player5", "Win", "+1200"},
            {"player2", "Win", "+1300"}
        };

        // Painel de estatísticas
        JPanel statsPanel = new JPanel();
        statsPanel.setOpaque(false);
        statsPanel.setLayout(new GridLayout(0, 2, 20, 10));
        statsPanel.setBorder(new EmptyBorder(0, 0, 20, 0));
        Font labelFont = new Font("Arial", Font.BOLD, 18);
        Font valueFont = new Font("Arial", Font.PLAIN, 18);
        Color labelColor = new Color(52, 73, 94);
        Color valueColor = new Color(41, 128, 185);

        statsPanel.add(createLabel("Username:", labelFont, labelColor));
        statsPanel.add(createLabel(username, valueFont, valueColor));
        statsPanel.add(createLabel("Level:", labelFont, labelColor));
        statsPanel.add(createLabel(level, valueFont, valueColor));
        statsPanel.add(createLabel("XP:", labelFont, labelColor));
        statsPanel.add(createLabel(xp, valueFont, valueColor));
        statsPanel.add(createLabel("Rank:", labelFont, labelColor));
        statsPanel.add(createLabel(rank, valueFont, valueColor));
        statsPanel.add(createLabel("Win Rate:", labelFont, labelColor));
        statsPanel.add(createLabel(winRate, valueFont, valueColor));
        statsPanel.add(createLabel("Wins:", labelFont, labelColor));
        statsPanel.add(createLabel(wins, valueFont, valueColor));
        statsPanel.add(createLabel("Losses:", labelFont, labelColor));
        statsPanel.add(createLabel(losses, valueFont, valueColor));
        statsPanel.add(createLabel("Avg Points/Match:", labelFont, labelColor));
        statsPanel.add(createLabel(avgPoints, valueFont, valueColor));

        centerPanel.add(statsPanel, BorderLayout.NORTH);

        // Painel de partidas recentes
        JPanel recentPanel = new JPanel(new BorderLayout());
        recentPanel.setOpaque(false);
        JLabel recentLabel = new JLabel("Recent Matches", SwingConstants.LEFT);
        recentLabel.setFont(new Font("Arial", Font.BOLD, 20));
        recentLabel.setForeground(labelColor);
        recentLabel.setBorder(new EmptyBorder(10, 0, 10, 0));
        recentPanel.add(recentLabel, BorderLayout.NORTH);

        String[] columns = {"Opponent", "Result", "Points"};
        JTable recentTable = new JTable(recentMatches, columns);
        recentTable.setFont(new Font("Arial", Font.PLAIN, 16));
        recentTable.setRowHeight(28);
        recentTable.setBackground(new Color(236, 240, 241));
        recentTable.setForeground(labelColor);
        recentTable.getTableHeader().setFont(new Font("Arial", Font.BOLD, 16));
        recentTable.getTableHeader().setBackground(new Color(52, 152, 219));
        recentTable.getTableHeader().setForeground(Color.WHITE);
        recentTable.setEnabled(false);
        JScrollPane scrollPane = new JScrollPane(recentTable);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getViewport().setBackground(new Color(236, 240, 241));
        recentPanel.add(scrollPane, BorderLayout.CENTER);
        centerPanel.add(recentPanel, BorderLayout.CENTER);

        add(centerPanel, BorderLayout.CENTER);

        // Botão de voltar
        JButton backButton = new JButton("Back to Menu");
        backButton.setFont(new Font("Arial", Font.BOLD, 16));
        backButton.setBackground(new Color(46, 204, 113));
        backButton.setForeground(Color.WHITE);
        backButton.setFocusPainted(false);
        backButton.setBorderPainted(false);
        backButton.setPreferredSize(new Dimension(150, 40));
        backButton.setCursor(new Cursor(Cursor.HAND_CURSOR));
        backButton.addActionListener(e -> mainFrame.showMainMenu(username));
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setOpaque(false);
        buttonPanel.setBorder(new EmptyBorder(20, 0, 0, 0));
        buttonPanel.add(backButton);
        add(buttonPanel, BorderLayout.SOUTH);
    }

    private JLabel createLabel(String text, Font font, Color color) {
        JLabel label = new JLabel(text);
        label.setFont(font);
        label.setForeground(color);
        return label;
    }
} 