package com.duelo.client;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;

public class CharacterCustomizationDialog extends JDialog {
    private String username;
    private JTextField nameField;
    private JComboBox<String> classComboBox;
    private JComboBox<String> weaponComboBox;
    private boolean customizationComplete = false;

    public CharacterCustomizationDialog(Window owner, String username) {
        super((Frame) SwingUtilities.getWindowAncestor(owner), "Character Customization", true);
        this.username = username;
        
        setLayout(new BorderLayout());
        setResizable(false);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        
        // Painel principal
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
        mainPanel.setBackground(Color.WHITE);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 10, 10, 10);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        // Título
        JLabel titleLabel = new JLabel("Customize Your Character");
        titleLabel.setFont(new Font("Arial", Font.BOLD, 24));
        titleLabel.setForeground(new Color(52, 73, 94));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        mainPanel.add(titleLabel, gbc);
        
        // Nome do personagem
        JLabel nameLabel = new JLabel("Character Name:");
        nameLabel.setFont(new Font("Arial", Font.BOLD, 14));
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(nameLabel, gbc);
        
        nameField = new JTextField(20);
        nameField.setFont(new Font("Arial", Font.PLAIN, 14));
        gbc.gridx = 1;
        mainPanel.add(nameField, gbc);
        
        // Classe
        JLabel classLabel = new JLabel("Class:");
        classLabel.setFont(new Font("Arial", Font.BOLD, 14));
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(classLabel, gbc);
        
        String[] classes = {"Warrior", "Mage", "Archer", "Rogue"};
        classComboBox = new JComboBox<>(classes);
        classComboBox.setFont(new Font("Arial", Font.PLAIN, 14));
        gbc.gridx = 1;
        mainPanel.add(classComboBox, gbc);
        
        // Arma
        JLabel weaponLabel = new JLabel("Weapon:");
        weaponLabel.setFont(new Font("Arial", Font.BOLD, 14));
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(weaponLabel, gbc);
        
        String[] weapons = {"Sword", "Staff", "Bow", "Dagger"};
        weaponComboBox = new JComboBox<>(weapons);
        weaponComboBox.setFont(new Font("Arial", Font.PLAIN, 14));
        gbc.gridx = 1;
        mainPanel.add(weaponComboBox, gbc);
        
        // Botões
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setBackground(Color.WHITE);
        
        JButton confirmButton = new JButton("Confirm");
        confirmButton.setFont(new Font("Arial", Font.BOLD, 14));
        confirmButton.setBackground(new Color(46, 204, 113));
        confirmButton.setForeground(Color.WHITE);
        confirmButton.setMnemonic(KeyEvent.VK_C); // Alt+C para Confirm
        confirmButton.addActionListener(e -> confirmCustomization());
        
        JButton cancelButton = new JButton("Cancel");
        cancelButton.setFont(new Font("Arial", Font.BOLD, 14));
        cancelButton.setBackground(new Color(231, 76, 60));
        cancelButton.setForeground(Color.WHITE);
        cancelButton.setMnemonic(KeyEvent.VK_X); // Alt+X para Cancel
        cancelButton.addActionListener(e -> dispose());
        
        buttonPanel.add(confirmButton);
        buttonPanel.add(cancelButton);
        
        add(mainPanel, BorderLayout.CENTER);
        add(buttonPanel, BorderLayout.SOUTH);
        
        // Configuração do diálogo
        pack();
        setLocationRelativeTo(owner);
        
        // Adiciona KeyListener para Escape
        addKeyListener(new java.awt.event.KeyAdapter() {
            @Override
            public void keyPressed(java.awt.event.KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    dispose();
                }
            }
        });
        setFocusable(true);
    }
    
    private void confirmCustomization() {
        String characterName = nameField.getText().trim();
        if (characterName.isEmpty()) {
            CustomDialog.showErrorDialog(this, "Please enter a character name");
            return;
        }
        
        // TODO: Salvar as informações do personagem no servidor
        customizationComplete = true;
        dispose();
    }
    
    public boolean isCustomizationComplete() {
        return customizationComplete;
    }
    
    public String getCharacterName() {
        return nameField.getText().trim();
    }
    
    public String getCharacterClass() {
        return (String) classComboBox.getSelectedItem();
    }
    
    public String getWeapon() {
        return (String) weaponComboBox.getSelectedItem();
    }
} 