package com.duelo.client;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.geom.RoundRectangle2D;
import java.awt.geom.AffineTransform;
import javax.swing.Timer;
import javax.swing.border.EmptyBorder;

public class RankingsPanel extends JPanel implements KeyListener {
    private MainFrame mainFrame;
    private String username;
    private JTable rankingsTable;
    private JTable levelsTable;
    private JTabbedPane tabbedPane;
    private static final Color BACKGROUND_COLOR = new Color(44, 62, 80);
    private static final Color PANEL_COLOR = new Color(236, 240, 241);
    private static final Color ACCENT_COLOR = new Color(52, 152, 219);
    private static final Color TEXT_COLOR = new Color(52, 73, 94);
    private static final Color HEADER_COLOR = new Color(41, 128, 185);
    private static final Color BUTTON_COLOR = new Color(46, 204, 113); // Verde do jogo

    public RankingsPanel(MainFrame mainFrame, String username) {
        this.mainFrame = mainFrame;
        this.username = username;
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(800, 600));
        setBackground(BACKGROUND_COLOR);
        setFocusable(true);
        addKeyListener(this);

        // Título com efeito de sombra
        JLabel titleLabel = new JLabel("Player Statistics", SwingConstants.CENTER) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
                
                // Desenha a sombra
                g2d.setColor(new Color(0, 0, 0, 50));
                g2d.setFont(getFont());
                FontMetrics fm = g2d.getFontMetrics();
                int x = (getWidth() - fm.stringWidth(getText())) / 2;
                int y = (getHeight() - fm.getHeight()) / 2 + fm.getAscent() + 2;
                g2d.drawString(getText(), x, y);
                
                // Desenha o texto principal
                g2d.setColor(Color.WHITE);
                g2d.drawString(getText(), x, y - 2);
                
                g2d.dispose();
            }
        };
        titleLabel.setFont(new Font("Arial", Font.BOLD, 36));
        titleLabel.setBorder(new EmptyBorder(30, 0, 10, 0));
        add(titleLabel, BorderLayout.NORTH);

        // Cria o painel de abas
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(new Font("Arial", Font.BOLD, 16));
        tabbedPane.setForeground(TEXT_COLOR);
        tabbedPane.setBackground(PANEL_COLOR);

        // Estiliza as abas
        UIManager.put("TabbedPane.selected", ACCENT_COLOR);
        UIManager.put("TabbedPane.background", PANEL_COLOR);
        UIManager.put("TabbedPane.foreground", TEXT_COLOR);
        UIManager.put("TabbedPane.borderHightlightColor", ACCENT_COLOR);
        UIManager.put("TabbedPane.contentAreaColor", PANEL_COLOR);
        UIManager.put("TabbedPane.contentBorderInsets", new Insets(0, 0, 0, 0));
        UIManager.put("TabbedPane.tabAreaInsets", new Insets(0, 0, 0, 0));
        UIManager.put("TabbedPane.tabInsets", new Insets(10, 20, 10, 20));
        UIManager.put("TabbedPane.selected", ACCENT_COLOR);
        UIManager.put("TabbedPane.unselectedBackground", PANEL_COLOR);
        UIManager.put("TabbedPane.selectedBackground", ACCENT_COLOR);
        UIManager.put("TabbedPane.selectedForeground", Color.WHITE);
        UIManager.put("TabbedPane.unselectedForeground", TEXT_COLOR);
        UIManager.put("TabbedPane.focus", ACCENT_COLOR);
        UIManager.put("TabbedPane.borderColor", ACCENT_COLOR);
        UIManager.put("TabbedPane.tabAreaBackground", PANEL_COLOR);
        UIManager.put("TabbedPane.tabAreaBorder", BorderFactory.createEmptyBorder());
        UIManager.put("TabbedPane.contentBorderInsets", new Insets(0, 0, 0, 0));
        UIManager.put("TabbedPane.tabAreaInsets", new Insets(0, 0, 0, 0));
        UIManager.put("TabbedPane.tabInsets", new Insets(10, 20, 10, 20));

        // Adiciona as abas com painel arredondado como conteúdo
        tabbedPane.addTab("Rankings", createRoundedContentPanel(createRankingsPanel()));
        tabbedPane.addTab("Levels", createRoundedContentPanel(createLevelsPanel()));

        JPanel centerWrapper = new JPanel(new BorderLayout());
        centerWrapper.setOpaque(false);
        centerWrapper.setBorder(new EmptyBorder(10, 20, 0, 20));
        centerWrapper.add(tabbedPane, BorderLayout.CENTER);
        add(centerWrapper, BorderLayout.CENTER);

        // Botão de voltar com estilo personalizado
        JButton backButton = createStyledButton("Back to Menu", BUTTON_COLOR);
        backButton.setMnemonic(KeyEvent.VK_B);
        backButton.addActionListener(e -> mainFrame.showMainMenu(username));
        
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setOpaque(false);
        buttonPanel.setBorder(new EmptyBorder(20, 0, 20, 0));
        buttonPanel.add(backButton);
        add(buttonPanel, BorderLayout.SOUTH);
    }

    private JButton createStyledButton(String text, Color color) {
        class AnimatedButton extends JButton {
            private float scale = 1.0f;
            private boolean isHovered = false;
            private Timer timer;

            public AnimatedButton(String text) {
                super(text);
                timer = new Timer(5, e -> {
                    if (isHovered && scale < 1.1f) {
                        scale = Math.min(1.1f, scale + 0.004f);
                        repaint();
                    } else if (!isHovered && scale > 1.0f) {
                        scale = Math.max(1.0f, scale - 0.004f);
                        repaint();
                    } else {
                        timer.stop();
                    }
                });
            }

            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                
                int w = getWidth();
                int h = getHeight();
                
                // Aplicar transformação de escala
                AffineTransform oldTransform = g2d.getTransform();
                g2d.translate(w / 2, h / 2);
                g2d.scale(scale, scale);
                g2d.translate(-w / 2, -h / 2);
                
                // Desenhar fundo com gradiente
                GradientPaint gp = new GradientPaint(0, 0, color, 0, h, color.darker());
                g2d.setPaint(gp);
                g2d.fillRoundRect(0, 0, w, h, 20, 20);
                
                // Desenhar borda brilhante
                g2d.setColor(new Color(255, 255, 255, 50));
                g2d.setStroke(new BasicStroke(2));
                g2d.drawRoundRect(1, 1, w - 2, h - 2, 20, 20);
                
                // Desenhar texto
                g2d.setTransform(oldTransform);
                g2d.setColor(getForeground());
                FontMetrics fm = g2d.getFontMetrics();
                int x = (w - fm.stringWidth(getText())) / 2;
                int y = (h - fm.getHeight()) / 2 + fm.getAscent();
                g2d.drawString(getText(), x, y);
                
                g2d.dispose();
            }
        }
        
        AnimatedButton button = new AnimatedButton(text);
        button.setFont(new Font("Arial", Font.BOLD, 16));
        button.setBackground(color);
        button.setForeground(Color.WHITE);
        button.setFocusPainted(false);
        button.setBorderPainted(false);
        button.setPreferredSize(new Dimension(150, 40));
        button.setCursor(new Cursor(Cursor.HAND_CURSOR));

        button.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                button.isHovered = true;
                button.timer.start();
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                button.isHovered = false;
                button.timer.start();
            }
        });

        return button;
    }

    private JPanel createRankingsPanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(PANEL_COLOR);
        panel.setBorder(new EmptyBorder(20, 20, 20, 20));

        // Dados hardcoded para rankings
        String[] columnNames = {"Rank", "Player", "Wins", "Losses", "Win Rate"};
        Object[][] data = {
            {"1", "tuai", "15", "3", "83.3%"},
            {"2", "davide", "12", "5", "70.6%"},
            {"3", "player3", "10", "7", "58.8%"},
            {"4", "player4", "8", "9", "47.1%"},
            {"5", "player5", "5", "12", "29.4%"}
        };

        DefaultTableModel model = new DefaultTableModel(data, columnNames) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        rankingsTable = new JTable(model);
        rankingsTable.setFont(new Font("Arial", Font.PLAIN, 14));
        rankingsTable.setBackground(PANEL_COLOR);
        rankingsTable.setForeground(TEXT_COLOR);
        rankingsTable.setGridColor(new Color(189, 195, 199));
        rankingsTable.setSelectionBackground(ACCENT_COLOR);
        rankingsTable.setSelectionForeground(Color.WHITE);
        rankingsTable.getTableHeader().setFont(new Font("Arial", Font.BOLD, 14));
        rankingsTable.getTableHeader().setBackground(HEADER_COLOR);
        rankingsTable.getTableHeader().setForeground(Color.WHITE);
        rankingsTable.setRowHeight(35);
        rankingsTable.setShowGrid(false);
        rankingsTable.setIntercellSpacing(new Dimension(0, 0));

        JScrollPane scrollPane = new JScrollPane(rankingsTable);
        scrollPane.setBackground(PANEL_COLOR);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getViewport().setBackground(PANEL_COLOR);
        panel.add(scrollPane, BorderLayout.CENTER);

        return panel;
    }

    private JPanel createLevelsPanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(PANEL_COLOR);
        panel.setBorder(new EmptyBorder(20, 20, 20, 20));

        // Dados hardcoded para níveis
        String[] columnNames = {"Level", "Player", "XP", "Next Level"};
        Object[][] data = {
            {"10", "tuai", "950/1000", "50 XP"},
            {"8", "davide", "750/800", "50 XP"},
            {"7", "player3", "650/700", "50 XP"},
            {"5", "player4", "450/500", "50 XP"},
            {"3", "player5", "250/300", "50 XP"}
        };

        DefaultTableModel model = new DefaultTableModel(data, columnNames) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        levelsTable = new JTable(model);
        levelsTable.setFont(new Font("Arial", Font.PLAIN, 14));
        levelsTable.setBackground(PANEL_COLOR);
        levelsTable.setForeground(TEXT_COLOR);
        levelsTable.setGridColor(new Color(189, 195, 199));
        levelsTable.setSelectionBackground(ACCENT_COLOR);
        levelsTable.setSelectionForeground(Color.WHITE);
        levelsTable.getTableHeader().setFont(new Font("Arial", Font.BOLD, 14));
        levelsTable.getTableHeader().setBackground(HEADER_COLOR);
        levelsTable.getTableHeader().setForeground(Color.WHITE);
        levelsTable.setRowHeight(35);
        levelsTable.setShowGrid(false);
        levelsTable.setIntercellSpacing(new Dimension(0, 0));

        JScrollPane scrollPane = new JScrollPane(levelsTable);
        scrollPane.setBackground(PANEL_COLOR);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getViewport().setBackground(PANEL_COLOR);
        panel.add(scrollPane, BorderLayout.CENTER);

        return panel;
    }

    // Novo método para painel arredondado do conteúdo da aba
    private JPanel createRoundedContentPanel(JPanel content) {
        JPanel roundedPanel = new JPanel(new BorderLayout()) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g;
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2d.setColor(PANEL_COLOR);
                g2d.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 30, 30));
                g2d.setColor(new Color(189, 195, 199));
                g2d.setStroke(new BasicStroke(2));
                g2d.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 30, 30));
            }
        };
        roundedPanel.setOpaque(false);
        roundedPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
        roundedPanel.add(content, BorderLayout.CENTER);
        return roundedPanel;
    }

    @Override
    public void keyPressed(KeyEvent e) {
        if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
            mainFrame.showMainMenu(username);
        }
    }

    @Override
    public void keyTyped(KeyEvent e) {}

    @Override
    public void keyReleased(KeyEvent e) {}
} 