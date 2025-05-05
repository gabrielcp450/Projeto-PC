package com.duelo.client;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.geom.RoundRectangle2D;

public class LoginPanel extends JPanel {
    private JTextField usernameField;
    private JPasswordField passwordField;
    private JButton loginButton;
    private JButton registerButton;
    private AuthManager authManager;
    private MainFrame mainFrame;

    public LoginPanel(MainFrame mainFrame) {
        this.mainFrame = mainFrame;
        this.authManager = new AuthManager();
        
        setLayout(new GridBagLayout());
        setOpaque(false);

        // Panel central com fundo branco e cantos arredondados
        JPanel centerPanel = new JPanel(new GridBagLayout()) {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2d = (Graphics2D) g;
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2d.setColor(Color.WHITE);
                g2d.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 20, 20));
            }
        };
        centerPanel.setOpaque(false);
        centerPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
        centerPanel.setPreferredSize(new Dimension(400, 500));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 10, 10, 10);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        // Title
        JLabel titleLabel = new JLabel("DUELO");
        titleLabel.setFont(new Font("Arial", Font.BOLD, 32));
        titleLabel.setForeground(new Color(52, 73, 94));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        centerPanel.add(titleLabel, gbc);

        // Subtitle
        JLabel subtitleLabel = new JLabel("Login to your account");
        subtitleLabel.setFont(new Font("Arial", Font.PLAIN, 14));
        subtitleLabel.setForeground(new Color(127, 140, 141));
        gbc.gridy = 1;
        centerPanel.add(subtitleLabel, gbc);

        // Username
        JLabel usernameLabel = new JLabel("Username");
        usernameLabel.setFont(new Font("Arial", Font.BOLD, 12));
        usernameLabel.setForeground(new Color(52, 73, 94));
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        centerPanel.add(usernameLabel, gbc);

        gbc.gridy = 3;
        usernameField = new JTextField(20);
        styleTextField(usernameField);
        usernameLabel.setLabelFor(usernameField);
        centerPanel.add(usernameField, gbc);

        // Password
        JLabel passwordLabel = new JLabel("Password");
        passwordLabel.setFont(new Font("Arial", Font.BOLD, 12));
        passwordLabel.setForeground(new Color(52, 73, 94));
        gbc.gridy = 4;
        centerPanel.add(passwordLabel, gbc);

        gbc.gridy = 5;
        passwordField = new JPasswordField(20);
        styleTextField(passwordField);
        passwordLabel.setLabelFor(passwordField);
        centerPanel.add(passwordField, gbc);

        // Buttons
        JPanel buttonPanel = new JPanel(new GridLayout(2, 1, 10, 10));
        buttonPanel.setOpaque(false);

        loginButton = new JButton("Login");
        styleButton(loginButton, new Color(46, 204, 113));
        loginButton.setMnemonic(KeyEvent.VK_L); // Alt+L para login
        loginButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                performLogin();
            }
        });

        registerButton = new JButton("Register");
        styleButton(registerButton, new Color(52, 152, 219));
        registerButton.setMnemonic(KeyEvent.VK_R); // Alt+R para register
        registerButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                performRegister();
            }
        });

        // Adiciona KeyListener para Enter nos campos e botões
        KeyStroke enterKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        Action enterAction = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                if (focused == usernameField) {
                    passwordField.requestFocusInWindow();
                } else if (focused == passwordField) {
                    performLogin();
                } else if (focused == loginButton) {
                    performLogin();
                } else if (focused == registerButton) {
                    performRegister();
                }
            }
        };

        // Registra a ação Enter para todos os componentes
        usernameField.getInputMap().put(enterKeyStroke, "enterAction");
        usernameField.getActionMap().put("enterAction", enterAction);
        
        passwordField.getInputMap().put(enterKeyStroke, "enterAction");
        passwordField.getActionMap().put("enterAction", enterAction);
        
        loginButton.getInputMap().put(enterKeyStroke, "enterAction");
        loginButton.getActionMap().put("enterAction", enterAction);
        
        registerButton.getInputMap().put(enterKeyStroke, "enterAction");
        registerButton.getActionMap().put("enterAction", enterAction);

        buttonPanel.add(loginButton);
        buttonPanel.add(registerButton);

        gbc.gridy = 6;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        centerPanel.add(buttonPanel, gbc);

        // Adiciona o painel central ao painel principal
        add(centerPanel);

        // Limpa os campos inicialmente
        clearFields();
    }

    private void performLogin() {
        String username = usernameField.getText();
        String password = new String(passwordField.getPassword());
        
        if (username.isEmpty() || password.isEmpty()) {
            CustomDialog.showErrorDialog(mainFrame, "Please enter both username and password");
            return;
        }

        if (authManager.login(username, password)) {
            CustomDialog.showSuccessDialog(mainFrame, "Login successful!");
            mainFrame.showMainMenuPanel(username);
        } else {
            CustomDialog.showErrorDialog(mainFrame, "Invalid username or password");
        }
    }

    private void performRegister() {
        String username = usernameField.getText();
        String password = new String(passwordField.getPassword());
        
        if (username.isEmpty() || password.isEmpty()) {
            CustomDialog.showErrorDialog(mainFrame, "Please enter both username and password");
            return;
        }

        if (authManager.register(username, password)) {
            CustomDialog.showSuccessDialog(mainFrame, "Account created successfully!");
            // Mostrar diálogo de personalização
            CharacterCustomizationDialog customDialog = new CharacterCustomizationDialog(SwingUtilities.getWindowAncestor(this), username);
            customDialog.setVisible(true);
            
            // Se a personalização foi concluída, fazer login
            if (customDialog.isCustomizationComplete()) {
                if (authManager.login(username, password)) {
                    mainFrame.showMainMenuPanel(username);
                }
            }
        } else {
            CustomDialog.showErrorDialog(mainFrame, "Username already exists or server error");
        }
    }

    public void clearFields() {
        usernameField.setText("");
        passwordField.setText("");
        usernameField.requestFocusInWindow();
    }

    private void styleTextField(JTextField field) {
        field.setFont(new Font("Arial", Font.PLAIN, 14));
        field.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(new Color(189, 195, 199)),
            BorderFactory.createEmptyBorder(5, 10, 5, 10)
        ));
        field.setBackground(Color.WHITE);
    }

    private void styleButton(JButton button, Color color) {
        button.setFont(new Font("Arial", Font.BOLD, 14));
        button.setBackground(color);
        button.setForeground(Color.WHITE);
        button.setFocusPainted(true); // Mantém o indicador de foco visível
        button.setBorderPainted(false);
        button.setOpaque(true);
        button.setCursor(new Cursor(Cursor.HAND_CURSOR));
        button.setPreferredSize(new Dimension(200, 40));
    }
} 