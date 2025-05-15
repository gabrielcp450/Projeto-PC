#!/bin/bash
# Function to kill child processes
cleanup() {
    echo "Killing child processes..."
    kill 0
}
trap cleanup SIGINT

# Clean and build the project with Maven
mvn clean package 

# Run the clients with native access flag
java -cp target/duelo-client-1.0-SNAPSHOT.jar com.duelo.client.Main edgar 1234 &
java -cp target/duelo-client-1.0-SNAPSHOT.jar com.duelo.client.Main gabriel 1234 

wait
