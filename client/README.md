# Duelo Client

This is the client application for the Duelo game project.

## Requirements

- Java 17 or higher
- Maven

## Building the Project

To build the project, run:

```bash
mvn clean package
```

## Running the Application

To run the application, execute:

```bash
java -cp target/duelo-client-1.0-SNAPSHOT.jar com.duelo.client.Main
```

## Features

- User registration
- User login
- Password protection
- Persistent user data storage

## Project Structure

- `src/main/java/com/duelo/client/`
  - `Main.java` - Entry point of the application
  - `LoginFrame.java` - Login and registration window
  - `AuthManager.java` - Handles user authentication and data persistence 