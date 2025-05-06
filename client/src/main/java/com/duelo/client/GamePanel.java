package com.duelo.client;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

public class GamePanel extends JPanel implements KeyListener, MouseListener, MouseMotionListener {
    private static final int PLAYER_SIZE = 40;
    private static final int PROJECTILE_SIZE = 10;
    private static final double PLAYER_SPEED = 5.0;
    private static final double PROJECTILE_SPEED = 10.0;
    private static final int SHOOT_COOLDOWN = 500; // milissegundos entre tiros

    private Rectangle2D.Double player1;
    private Rectangle2D.Double player2;
    private List<Projectile> projectiles;
    private boolean[] keysPressed;
    private long lastShootTime;
    private String username;
    private MainFrame mainFrame;
    private GameManager gameManager;
    private JLabel statusLabel;
    private boolean gameStarted = false;

    private static class Projectile {
        double x, y;
        double dx, dy;
        boolean isMine;

        Projectile(double x, double y, double angle, boolean isMine) {
            this.x = x;
            this.y = y;
            this.dx = Math.cos(angle) * PROJECTILE_SPEED;
            this.dy = Math.sin(angle) * PROJECTILE_SPEED;
            this.isMine = isMine;
        }

        void update() {
            x += dx;
            y += dy;
        }
    }

    public GamePanel(MainFrame mainFrame, String username) {
        this.mainFrame = mainFrame;
        this.username = username;
        this.projectiles = new ArrayList<>();
        this.keysPressed = new boolean[256];
        this.lastShootTime = 0;

        // Configuração do painel
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(800, 600));
        setBackground(Color.BLACK);
        setFocusable(true);
        addKeyListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);

        // Status label
        statusLabel = new JLabel("Waiting for opponent...", SwingConstants.CENTER);
        statusLabel.setFont(new Font("Arial", Font.BOLD, 24));
        statusLabel.setForeground(Color.WHITE);
        add(statusLabel, BorderLayout.NORTH);

        // Inicializa os jogadores em posições opostas
        initializePlayers();

        // Inicia o loop do jogo
        startGameLoop();

        // Conecta ao servidor e entra na fila
        try {
            gameManager = GameManager.getInstance();
            gameManager.setGamePanel(this);
            gameManager.setUsername(username);
            gameManager.enterQueue();
        } catch (Exception e) {
            e.printStackTrace();
            statusLabel.setText("Error connecting to server");
        }
    }

    public void onGameStart() {
        gameStarted = true;
        SwingUtilities.invokeLater(() -> {
            statusLabel.setText("Game Started! Use arrow keys to move and click to shoot!");
            requestFocusInWindow();
        });
    }

    private void initializePlayers() {
        // Jogador 1 (você) - lado esquerdo
        player1 = new Rectangle2D.Double(50, getHeight() / 2 - PLAYER_SIZE / 2, PLAYER_SIZE, PLAYER_SIZE);
        
        // Jogador 2 (oponente) - lado direito
        player2 = new Rectangle2D.Double(getWidth() - 50 - PLAYER_SIZE, getHeight() / 2 - PLAYER_SIZE / 2, PLAYER_SIZE, PLAYER_SIZE);
    }

    private void startGameLoop() {
        Timer timer = new Timer(16, e -> {
            update();
            repaint();
        });
        timer.start();
    }

    private void update() {
        if (!gameStarted) return;

        // Atualiza posição do jogador 1 baseado nas teclas pressionadas
        if (keysPressed[KeyEvent.VK_LEFT]) {
            player1.x -= PLAYER_SPEED;
        }
        if (keysPressed[KeyEvent.VK_RIGHT]) {
            player1.x += PLAYER_SPEED;
        }
        if (keysPressed[KeyEvent.VK_UP]) {
            player1.y -= PLAYER_SPEED;
        }
        if (keysPressed[KeyEvent.VK_DOWN]) {
            player1.y += PLAYER_SPEED;
        }

        // Mantém os jogadores dentro dos limites da tela
        player1.x = Math.max(0, Math.min(getWidth() - PLAYER_SIZE, player1.x));
        player1.y = Math.max(0, Math.min(getHeight() - PLAYER_SIZE, player1.y));
        player2.x = Math.max(0, Math.min(getWidth() - PLAYER_SIZE, player2.x));
        player2.y = Math.max(0, Math.min(getHeight() - PLAYER_SIZE, player2.y));

        // Envia a posição do jogador para o servidor
        if (gameManager != null && gameManager.isInGame()) {
            gameManager.sendPlayerPosition(player1.x, player1.y);
        }

        // Atualiza posição dos projéteis
        for (int i = projectiles.size() - 1; i >= 0; i--) {
            Projectile projectile = projectiles.get(i);
            projectile.update();

            // Verifica colisão com os jogadores
            if (projectile.isMine) {
                if (checkCollision(projectile, player2)) {
                    // Acertou o oponente!
                    projectiles.remove(i);
                    continue;
                }
            } else {
                if (checkCollision(projectile, player1)) {
                    // Foi atingido!
                    projectiles.remove(i);
                    continue;
                }
            }

            // Remove projéteis que saíram da tela
            if (projectile.x < 0 || projectile.x > getWidth() || 
                projectile.y < 0 || projectile.y > getHeight()) {
                projectiles.remove(i);
            }
        }
    }

    private boolean checkCollision(Projectile projectile, Rectangle2D.Double player) {
        return projectile.x >= player.x && 
               projectile.x <= player.x + PLAYER_SIZE &&
               projectile.y >= player.y && 
               projectile.y <= player.y + PLAYER_SIZE;
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Desenha os jogadores
        g2d.setColor(Color.BLUE);
        g2d.fill(player1);
        g2d.setColor(Color.RED);
        g2d.fill(player2);

        // Desenha os projéteis
        for (Projectile projectile : projectiles) {
            g2d.setColor(projectile.isMine ? Color.WHITE : Color.YELLOW);
            g2d.fillOval((int)projectile.x - PROJECTILE_SIZE/2, 
                        (int)projectile.y - PROJECTILE_SIZE/2, 
                        PROJECTILE_SIZE, PROJECTILE_SIZE);
        }
    }

    // KeyListener
    @Override
    public void keyPressed(KeyEvent e) {
        int keyCode = e.getKeyCode();
        if (keyCode >= 0 && keyCode < keysPressed.length) {
            keysPressed[keyCode] = true;
        }
    }

    @Override
    public void keyReleased(KeyEvent e) {
        int keyCode = e.getKeyCode();
        if (keyCode >= 0 && keyCode < keysPressed.length) {
            keysPressed[keyCode] = false;
        }
    }

    @Override
    public void keyTyped(KeyEvent e) {}

    // MouseListener
    @Override
    public void mouseClicked(MouseEvent e) {
        if (!gameStarted) return;

        long currentTime = System.currentTimeMillis();
        if (currentTime - lastShootTime >= SHOOT_COOLDOWN) {
            // Calcula o ângulo do tiro baseado na posição do mouse
            double dx = e.getX() - (player1.x + PLAYER_SIZE / 2);
            double dy = e.getY() - (player1.y + PLAYER_SIZE / 2);
            double angle = Math.atan2(dy, dx);

            // Cria um novo projétil na posição do jogador
            Projectile projectile = new Projectile(
                player1.x + PLAYER_SIZE / 2,
                player1.y + PLAYER_SIZE / 2,
                angle,
                true
            );
            projectiles.add(projectile);

            // Envia o tiro para o servidor
            if (gameManager != null && gameManager.isInGame()) {
                gameManager.sendProjectile(projectile.x, projectile.y, angle);
            }

            lastShootTime = currentTime;
        }
    }

    @Override
    public void mousePressed(MouseEvent e) {}

    @Override
    public void mouseReleased(MouseEvent e) {}

    @Override
    public void mouseEntered(MouseEvent e) {}

    @Override
    public void mouseExited(MouseEvent e) {}

    // MouseMotionListener
    @Override
    public void mouseDragged(MouseEvent e) {}

    @Override
    public void mouseMoved(MouseEvent e) {}

    @Override
    public void removeNotify() {
        super.removeNotify();
        if (gameManager != null) {
            gameManager.disconnect();
        }
    }
} 