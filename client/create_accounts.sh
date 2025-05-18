#!/bin/bash

# Number of accounts to create
NUM_ACCOUNTS=$1

# Server address and port
SERVER="localhost"
PORT=13556

# Default password
PASSWORD="1234"

# Loop to create accounts
for ((i=1; i<=NUM_ACCOUNTS; i++))
do
    USERNAME="user$i"
    echo "Creating account: $USERNAME"
    nc  $SERVER $PORT <<EOF &
/c $USERNAME $PASSWORD
EOF
done

wait  
echo "All $NUM_ACCOUNTS accounts have been created successfully."