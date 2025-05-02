#!/bin/bash
nc -N localhost 12345 <<EOF
/c joao 1234
EOF
nc -N localhost 12345 <<EOF
/c edgar 1234
EOF
nc -N localhost 12345 <<EOF
/c gabriel 1234
EOF
nc -N localhost 12345 <<EOF
/c davide 1234
EOF
nc -N localhost 12345 <<EOF
/save
EOF