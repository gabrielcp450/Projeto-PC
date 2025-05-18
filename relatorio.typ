#import "cover.typ": cover-page

#set document(title: "Duelo - Concurrent Programming Project Report", author: "Davide Santos, Edgar Araújo, Gabriel Paiva, Simão Martins")
#set page(
  margin: (top: 2.5cm, bottom: 2.5cm, left: 3cm, right: 3cm),
  numbering: "1", 
  number-align: center
)
#set text(font: "New Computer Modern", size: 11pt, lang: "en")
#set heading(numbering: "1.")
#set par(justify: true, leading: 0.65em)

// Cover page (no page number)
#let duelo-cover = [
  #set text(font: "New Computer Modern")
  
  #block(
    width: 100%,
    height: 100%,
    fill: white,
    stroke: none,
    [
      #v(2cm)
      
      #align(center)[
        #image("assets/uminho_logo.png", width: 30%)
        
        #v(1cm)
        
        #text(size: 14pt)[Department of Informatics - LCC]
        
        #v(0cm)
        #text(size: 14pt)[Concurrent Programming]
        
        #v(3cm)
        
        #text(size: 28pt, weight: "bold")[Duelo Project Report]
        
        #v(1cm)
        
        #text(size: 16pt, style: "italic")[A Multiplayer 2D Battle Game]
        
        #v(4cm)
        
        #grid(
          columns: (1fr),
          rows: (auto),
          gutter: 1em,
          text(size: 14pt)[
            Davide Santos A102938\
            Edgar Araújo A102946\
            Gabriel Paiva A102507\
            Simão Martins A102877
          ]
        )
        
        #v(2cm)
        
        #text(size: 14pt)[May 2025]
      ]
    ]
  )
]

#duelo-cover
#pagebreak()

// Abstract
#heading(level: 1, numbering: none)[Abstract]
This report details the implementation of Duelo, a multiplayer 2D battle game developed as part of the Concurrent Programming course. The project consists of a Java client with a graphical interface and an Erlang server that manages game state and player interactions. The implementation demonstrates concurrent programming concepts through the use of multiple processes in Erlang for game state management and Java's threading capabilities for client-side operations. The report focuses on the architectural decisions, state management, and communication protocols that enable real-time multiplayer gameplay.

#v(1cm)
#outline()
#pagebreak()

= Client Architecture
== State Machine Design
The client implements a state machine pattern to manage different game screens and states. The architecture follows a hierarchical structure with an abstract State class that defines the interface for all game states. Each concrete state class implements specific behavior for different game phases:

- LoginState: Handles user authentication and account management
- MenuState: Provides the main interface for navigation and game options
- QueueState: Manages the matchmaking process and waiting period
- PlayState: Controls active gameplay and player interactions
- RankingsState: Displays global player rankings
- ResultState: Shows match results and statistics

The state machine implementation ensures clean separation of concerns and modular code organization. Each state encapsulates its own logic and rendering code, making the system maintainable and extensible. State transitions are handled through a centralized manager, ensuring consistent behavior and proper cleanup between states.

== Class Hierarchy
The client's class hierarchy is organized as follows:

```
Game (PApplet)
├── State (Abstract)
│   ├── LoginState
│   ├── MenuState
│   ├── QueueState
│   ├── PlayState
│   ├── RankingsState
│   └── ResultState
├── Entities
│   ├── Player
│   ├── Projectile
│   └── Modifier
└── UI
    ├── Button
    ├── InputField
    └── HUD
```

The class hierarchy reflects the game's architecture, with clear separation between game states, game entities, and user interface components. The Game class serves as the main entry point, extending Processing's PApplet for graphics and event handling.

== State Transitions
The game manages state transitions through a centralized state manager in the Game class. State changes are triggered by various events:

- User actions: button clicks, menu selections
- Server responses: match found, game end
- Game events: timeout, score threshold
- System events: connection loss, error conditions

Each state transition follows a specific protocol to ensure proper resource management and prevent state inconsistencies. The system includes error handling and recovery mechanisms, allowing the game to gracefully handle unexpected situations.

= Fairness and Synchronization
== Server-Side Game Loop
To ensure fair gameplay between clients, the server implements a fixed tick rate system. The game loop operates on a fixed 1ms update interval, providing consistent gameplay across all clients. This approach ensures:

- Synchronized state updates
- Predictable timing for physics calculations
- Fair processing of player inputs
- Consistent gameplay experience

The tick rate system is implemented using Erlang's timer module, which provides precise timing control. Each tick triggers a complete update cycle, including player movement, collision detection, and state broadcasting. The system maintains a buffer of recent game states to handle network latency and ensure smooth gameplay.

== Normalized Coordinates
The game uses normalized coordinates (0.0 to 1.0) for all positions to ensure consistent gameplay across different screen sizes. This approach enables:

- Consistent physics calculations
- Platform-independent gameplay
- Dynamic scaling based on window size
- Consistent hitbox sizes

The coordinate system implementation includes several optimizations for precision and performance. Position values are stored as floating-point numbers, velocity vectors are normalized, and collision detection uses the normalized space for efficient calculations.

= Communication Protocol
== Client to Server Messages
The client communicates with the server using a prefix-based protocol. The protocol includes:

- Authentication commands: account management, login/logout
- Game commands: matchmaking, player input, state queries
- System commands: error reporting, session management

The message protocol is designed for efficiency and reliability, with specific formats and validation rules for each message type. The client implements message queuing and retry mechanisms for reliable delivery.

== Server to Client Messages
The server sends updates to clients using a similar prefix-based protocol. Updates include:

- Authentication responses: operation results, error messages
- Game updates: player positions, projectile movements
- Match events: score updates, power-up spawns
- System events: connection status, match end

The update system implements several optimizations to handle network constraints, including update batching, compression, and priority-based delivery.

== Message Handling
The client processes server messages in a dedicated thread, implementing:

- Asynchronous message handling
- Thread-safe state updates
- Event queuing system
- Error recovery mechanisms

The message handling system ensures that network operations don't block the main game loop, while maintaining thread safety and proper error handling.

= Technical Implementation
== Projectile System
The projectile system implements concurrent movement and collision detection. Key features include:

- Parallel position updates
- Efficient collision detection
- Normalized movement calculations
- Consistent behavior across clients

The system includes optimizations for performance and accuracy, such as vector-based physics and spatial partitioning for collision detection. It also handles various edge cases and network latency issues.

== Modifier System
The modifier system implements concurrent spawning and effect application. The system features:

- Random position spawning
- Weighted type selection
- Effect stacking and cancellation
- State synchronization

The system implements sophisticated effect management, including priority queues and timing systems for consistent behavior across clients.

== State Synchronization
The game implements efficient state synchronization between server and clients. The system includes:

- State buffering for latency handling
- Delta compression for updates
- Timestamp-based ordering
- Batched non-critical updates

The synchronization system uses a circular buffer for efficient memory usage and includes mechanisms for handling network disconnections and reconnections.

= Server State Machine
== Process Structure
The Erlang server implements a state machine using multiple processes. The architecture includes:

- Main server process for client connections
- User state processes for authentication
- Match processes for gameplay
- Supervisor processes for fault tolerance

The process structure is designed for scalability and fault tolerance, with each process having a specific responsibility and communicating through message passing.

== Process Communication
The server uses message passing between processes for communication. The system implements:

- Message queuing and timeout handling
- Pattern matching for routing
- Error handling and retry mechanisms
- Version compatibility checks

The communication system ensures reliable message delivery and proper handling of different client versions.

= Conclusion
The Duelo project successfully demonstrates the implementation of a concurrent multiplayer game using Java and Erlang. Key achievements include:

- Robust client-server architecture
- Efficient real-time communication
- Fair gameplay through fixed tick rate
- Reliable state synchronization

The project showcases the practical application of concurrent programming concepts in game development, providing solutions to common challenges in distributed systems and serving as a reference for similar implementations.
