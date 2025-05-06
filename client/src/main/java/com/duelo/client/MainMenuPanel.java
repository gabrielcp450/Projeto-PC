package com.duelo.client;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.RoundRectangle2D;
import java.awt.geom.AffineTransform;

public class MainMenuPanel extends JPanel {
    private String username;
    private MainFrame mainFrame;
    private JButton startGameButton;
    private JButton rankingsButton;
    private JButton profileButton;
    private JButton logoutButton;
    private JLabel welcomeLabel;

    public MainMenuPanel(MainFrame mainFrame) {
        this.mainFrame = mainFrame;
        
        setLayout(new BorderLayout());
        setOpaque(false);
        setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));

        // Welcome message
        welcomeLabel = new JLabel("Welcome, " + username + "!");
        welcomeLabel.setName("welcomeLabel");
        welcomeLabel.setFont(new Font("Arial", Font.BOLD, 24));
        welcomeLabel.setForeground(new Color(52, 73, 94));
        welcomeLabel.setHorizontalAlignment(SwingConstants.CENTER);
        add(welcomeLabel, BorderLayout.NORTH);

        // Menu buttons
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
        buttonPanel.setOpaque(false);
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(20, 40, 20, 40));
        buttonPanel.add(Box.createHorizontalGlue());

        // Start Game button
        startGameButton = createMenuButton("Start Game", new Color(46, 204, 113));
        startGameButton.setMnemonic(KeyEvent.VK_S); // Alt+S para Start Game
        startGameButton.addActionListener(e -> startGame());

        // Rankings button
        rankingsButton = createMenuButton("Rankings", new Color(52, 152, 219));
        rankingsButton.setMnemonic(KeyEvent.VK_R); // Alt+R para Rankings
        rankingsButton.addActionListener(e -> showRankings());

        // Profile button
        profileButton = createMenuButton("Profile", new Color(155, 89, 182));
        profileButton.setMnemonic(KeyEvent.VK_P); // Alt+P para Profile
        profileButton.addActionListener(e -> showProfile());

        // Logout button
        logoutButton = createMenuButton("Logout", new Color(231, 76, 60));
        logoutButton.setMnemonic(KeyEvent.VK_L); // Alt+L para Logout
        logoutButton.addActionListener(e -> logout());

        // Adiciona KeyListener para Enter nos botões
        KeyStroke enterKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        Action enterAction = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                if (focused == startGameButton) {
                    startGame();
                } else if (focused == rankingsButton) {
                    showRankings();
                } else if (focused == profileButton) {
                    showProfile();
                } else if (focused == logoutButton) {
                    logout();
                }
            }
        };

        // Registra a ação Enter para todos os botões
        startGameButton.getInputMap().put(enterKeyStroke, "enterAction");
        startGameButton.getActionMap().put("enterAction", enterAction);
        
        rankingsButton.getInputMap().put(enterKeyStroke, "enterAction");
        rankingsButton.getActionMap().put("enterAction", enterAction);
        
        profileButton.getInputMap().put(enterKeyStroke, "enterAction");
        profileButton.getActionMap().put("enterAction", enterAction);
        
        logoutButton.getInputMap().put(enterKeyStroke, "enterAction");
        logoutButton.getActionMap().put("enterAction", enterAction);

        buttonPanel.add(Box.createHorizontalGlue());
        buttonPanel.add(startGameButton);
        buttonPanel.add(Box.createHorizontalGlue());
        buttonPanel.add(rankingsButton);
        buttonPanel.add(Box.createHorizontalGlue());
        buttonPanel.add(profileButton);
        buttonPanel.add(Box.createHorizontalGlue());
        buttonPanel.add(logoutButton);
        buttonPanel.add(Box.createHorizontalGlue());

        add(buttonPanel, BorderLayout.CENTER);

        // Adiciona KeyListener para Escape
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    logout();
                }
            }
        });

        setFocusable(true);
    }

    public void setUsername(String username) {
        this.username = username;
        welcomeLabel.setText("Welcome, " + username + "!");
    }

    private void startGame() {
        mainFrame.showGamePanel(username);
    }

    private void showRankings() {
        // TODO: Implementar visualização de rankings
        CustomDialog.showInfoDialog(mainFrame, "Rankings functionality coming soon!");
    }

    private void showProfile() {
        // TODO: Implementar visualização do perfil
        CustomDialog.showInfoDialog(mainFrame, "Profile functionality coming soon!");
    }

    private void logout() {
        mainFrame.showLogin();
    }

    private JButton createMenuButton(String text, Color color) {
        class AnimatedButton extends JButton {
            private float scale = 1.0f;
            private boolean isHovered = false;
            private final Timer timer;
            private float triangleScale = 0.5f; // 0.5 normal, até 1.5 expandido
            private final float TRIANGLE_MAX = 1.5f;
            private final float TRIANGLE_MIN = 0.5f;

            // Tamanhos normal e expandido
            private final Dimension normalSize = new Dimension(120, 320);
            private final Dimension expandedSize = new Dimension(160, 340);

            public AnimatedButton(String text) {
                super(text);
                final float scaleStep = 0.004f;
                final float triangleStep = scaleStep * ((TRIANGLE_MAX - TRIANGLE_MIN) / (1.1f - 1.0f)); // 0.004 * 10 = 0.04
                timer = new Timer(5, e -> {
                    boolean repaintNeeded = false;
                    boolean revalidateNeeded = false;
                    if (isHovered && scale < 1.1f) {
                        scale = Math.min(1.1f, scale + scaleStep);
                        repaintNeeded = true;
                    } else if (!isHovered && scale > 1.0f) {
                        scale = Math.max(1.0f, scale - scaleStep);
                        repaintNeeded = true;
                    }
                    if (isHovered && triangleScale < TRIANGLE_MAX) {
                        triangleScale = Math.min(TRIANGLE_MAX, triangleScale + triangleStep);
                        repaintNeeded = true;
                    } else if (!isHovered && triangleScale > TRIANGLE_MIN) {
                        triangleScale = Math.max(TRIANGLE_MIN, triangleScale - triangleStep);
                        repaintNeeded = true;
                    }
                    // Ajusta o tamanho preferido do botão
                    if (isHovered && !getPreferredSize().equals(expandedSize)) {
                        setPreferredSize(expandedSize);
                        setMaximumSize(expandedSize);
                        setMinimumSize(expandedSize);
                        revalidateNeeded = true;
                    } else if (!isHovered && !getPreferredSize().equals(normalSize)) {
                        setPreferredSize(normalSize);
                        setMaximumSize(normalSize);
                        setMinimumSize(normalSize);
                        revalidateNeeded = true;
                    }
                    if (revalidateNeeded) {
                        Container parent = getParent();
                        if (parent != null) {
                            parent.revalidate();
                        }
                    }
                    if (repaintNeeded || revalidateNeeded) {
                        repaint();
                    } else {
                        ((Timer)e.getSource()).stop();
                    }
                });
                setOpaque(true);
                setPreferredSize(normalSize);
                setMaximumSize(normalSize);
                setMinimumSize(normalSize);
            }

            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                
                int arc = 20;
                int w = getWidth();
                int h = getHeight();
                
                // Aplicar transformação de escala ao contexto gráfico, mantendo o centro
                AffineTransform oldTransform = g2d.getTransform();
                g2d.translate(w / 2, h / 2);
                g2d.scale(scale, scale);
                g2d.translate(-w / 2, -h / 2);
                
                // Desenhar fundo com cantos arredondados
                g2d.setColor(getBackground());
                g2d.fillRoundRect(0, 0, w, h, arc, arc);
                
                // Desenhar triângulo decorativo no canto inferior esquerdo
                int triBase = (int)(w * 0.35f * triangleScale);
                int triHeight = (int)(h * 0.18f * triangleScale);
                int[] xPoints = {0, triBase, 0};
                int[] yPoints = {h, h, h - triHeight};
                Color triangleColor = getBackground().darker();
                g2d.setColor(triangleColor);
                g2d.fillPolygon(xPoints, yPoints, 3);
                
                // Desenhar texto centralizado (sem escala)
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
        button.setFont(new Font("Arial", Font.BOLD, 20));
        button.setBackground(color);
        button.setForeground(Color.WHITE);
        button.setFocusPainted(true); // Mantém o indicador de foco visível
        button.setBorderPainted(false);
        button.setOpaque(true);
        button.setCursor(new Cursor(Cursor.HAND_CURSOR));
        
        // Add hover effect
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

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setColor(Color.WHITE);
        g2d.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 20, 20));
    }
} 