# Duelo Client

Cliente para o jogo Duelo, implementado em Java usando Processing.

## Estrutura do Projeto

- `DueloGame.java` - Classe principal que estende PApplet e gerencia o jogo
- `Menu.java` - Classe base para todos os menus
- `LoginMenu.java` - Menu de login
- `MainMenu.java` - Menu principal
- `RankingsMenu.java` - Menu de rankings
- `ProfileMenu.java` - Menu de perfil
- `GameManager.java` - Gerencia a conexão com o servidor
- `AuthManager.java` - Gerencia autenticação

## Como Executar

1. Certifique-se de ter o Java 11 ou superior instalado
2. Execute o comando Maven:
   ```bash
   mvn clean package
   ```
3. Execute o jar gerado:
   ```bash
   java -jar target/duelo-client-1.0-SNAPSHOT.jar
   ```

## Controles

- WASD - Movimento do jogador
- Mouse - Mirar
- Clique do Mouse - Atirar
- ESC - Voltar ao menu principal

## Features

### Authentication
- User registration with username and password
- User login with credentials
- Account deletion (unregister)
- Server-side authentication
- Persistent user data storage

### Game Features
- Real-time multiplayer gameplay
- Player movement using arrow keys
- Shooting mechanics with mouse aiming
- Projectile collision detection
- Visual distinction between players and projectiles
- Game queue system for matchmaking
- Synchronized game state between players

### Rankings and Levels
- Global player rankings
- Win/loss statistics
- Win rate calculation
- Player levels and XP system
- Progress tracking
- Tabbed interface for rankings and levels

### User Interface
- Single window application with smooth transitions
- Login and registration panel
- Main menu with game options
- In-game interface with player status
- Rankings and levels display
- Keyboard navigation support
- Visual feedback for player actions
- Responsive game controls

## Project Structure

- `src/main/java/com/duelo/client/`
  - `Main.java` - Entry point of the application
  - `MainFrame.java` - Main application window with card layout
  - `LoginPanel.java` - Login and registration interface
  - `MainMenuPanel.java` - Main menu with game options
  - `GamePanel.java` - Game interface and mechanics
  - `RankingsPanel.java` - Rankings and levels display
  - `AuthManager.java` - Handles user authentication
  - `GameManager.java` - Manages game state and server communication
  - `CustomDialog.java` - Custom dialog for character customization

## Game Controls

- **Movement**: Arrow keys (↑, ↓, ←, →)
- **Shooting**: Left mouse click
- **Menu Navigation**: 
  - Tab key to cycle through options
  - Enter to select
  - Escape to go back/logout
  - Alt + underlined letter for quick access

## Game Rules

1. Players are represented by colored squares (blue for you, red for opponent)
2. Use arrow keys to move your character
3. Click to shoot in the direction of your mouse cursor
4. Your projectiles are white, opponent's are yellow
5. Hit the opponent to score points
6. Avoid being hit by opponent's projectiles
7. There's a cooldown of 500ms between shots 