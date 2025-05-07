# Duelo Client

This is the client application for the Duelo game project.

## Requirements

- Java 17 or higher
- Maven

## Building and Running

### Using Shell Scripts
- `crun.sh` - Builds and runs the application
- `run.sh` - Runs the application (must be built first)

### Manual Build and Run
To build the project, run:

```bash
mvn clean package
```

To run the application, execute:

```bash
java -cp target/duelo-client-1.0-SNAPSHOT.jar com.duelo.client.Main
```

## Features

### Authentication & User Management
- User registration and login with credentials
- Account deletion (unregister)
- Server-side authentication
- Persistent user data storage
- Visual feedback for authentication errors

### Main Menu
- Play (enters matchmaking queue)
- Rankings (global leaderboard with scroll)
- Profile (user stats)
- Logout
- Smooth transitions between menu options

### Matchmaking & Queue
- Real-time matchmaking: only starts game when opponent is found
- Animated loading while searching for match
- Cancel search at any time
- Synchronized game start for both players

### Game Features
- Real-time multiplayer gameplay
- Player movement using arrow keys **and** WASD
- Smooth, responsive 2D movement (no deslize)
- Shooting mechanics with mouse aiming
- Projectile collision detection
- Visual distinction between players and projectiles
- **Modifiers (Power-ups):**
  - Aparecem aleatoriamente no campo de jogo
  - Cada tipo tem uma cor distinta
  - Só podem existir até 3 de cada tipo ao mesmo tempo
  - **É possível ter múltiplos modifiers ativos ao mesmo tempo**
  - **Efeitos de modifiers acumulam** (ex: projétil rápido + cooldown rápido)
  - **Modifiers opostos (ex: GREEN/ORANGE, BLUE/RED) cancelam-se mutuamente**
  - **Tipos de modifiers:**
    - **Verde:** Aumenta a velocidade dos teus projéteis
    - **Laranja:** Diminui a velocidade dos teus projéteis
    - **Azul:** Reduz o cooldown entre tiros (podes disparar mais rápido)
    - **Vermelho:** Aumenta o cooldown entre tiros (disparas mais devagar)
  - **Duração:** O efeito dura 5 segundos
  - O efeito é temporário e só afeta o jogador que o apanha
  - Modifiers desaparecem se não forem apanhados em 10 segundos
- Game area with boundaries
- Cooldown of 500ms between shots
- Player and opponent are visually distinct

### Rankings and Levels
- Global player rankings (top 10+)
- Scrollable leaderboard (mouse wheel)
- Fixed header with column meanings
- Alternating row colors for readability
- Top 3 highlighted with different colors
- Win/loss statistics and streaks
- Player levels and XP system
- Progress tracking
- Rankings only update on entry (no spam requests)

### User Interface & Visuals
- Modern, clean, and responsive UI
- Centralized and spaced tables
- Fixed headers and smooth scrolling
- Animated buttons and feedback
- Visual feedback for player actions
- Consistent color scheme and fonts
- Keyboard navigation support
- Error messages and status indicators

### Controls
- **Movement**: Arrow keys (↑, ↓, ←, →) or WASD
- **Shooting**: Left mouse click
- **Menu Navigation**: 
  - Tab key to cycle through options
  - Enter to select
  - Escape to go back/logout
- **Rankings Scroll**: Mouse wheel

## Project Structure

- `src/main/java/com/duelo/client/`
  - `Main.java` - Entry point of the application
  - `Game.java` - Main game logic and UI (Processing)
  - `AuthManager.java` - Handles user authentication
  - `GameManager.java` - Manages game state and server communication
  - `Player.java` - Player logic and rendering
  - `Modifier.java` - Power-up logic
  - `RankingEntry.java` - Data structure for leaderboard
  - ... (other UI and utility classes)

## Game Rules

1. Players are represented by colored circles (blue for you, red for opponent)
2. Use arrow keys or WASD to move your character
3. Click to shoot in the direction of your mouse cursor
4. Your projectiles are white, opponent's are yellow
5. Hit the opponent to score points
6. Avoid being hit by opponent's projectiles
7. **Modifiers:**
   - Podes ter vários modifiers ativos ao mesmo tempo (efeitos acumulam)
   - Modifiers opostos (ex: GREEN/ORANGE, BLUE/RED) cancelam-se
   - Apanhar um modifier ativa o seu efeito temporário (5 segundos)
   - Só afeta o jogador que o apanha
   - Os modifiers podem aumentar/diminuir a velocidade dos projéteis ou o cooldown dos tiros
   - Só podem existir até 3 de cada tipo em campo
   - Modifiers desaparecem se não forem apanhados em 10 segundos
8. There's a cooldown of 500ms between shots
9. Level up by winning games, lose level by losing streaks
10. Check your position in the global rankings!

---

For any issues or suggestions, open an issue or contact the project maintainers. 