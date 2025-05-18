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
    echo "Logging in with account: $USERNAME"
    nc  $SERVER $PORT <<EOF &
/l $USERNAME $PASSWORD
/s
EOF
done

wait  
echo "All $NUM_ACCOUNTS accounts have been logged in and command sent successfully."