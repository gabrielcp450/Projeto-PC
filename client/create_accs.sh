#!/bin/bash
nc -N localhost 12346 <<EOF
/c joao 1234
EOF
nc -N localhost 12346 <<EOF
/c edgar 1234
EOF
nc -N localhost 12346 <<EOF
/c gabriel 1234
EOF
nc -N localhost 12346 <<EOF
/c davide 1234
EOF
nc -N localhost 12346 <<EOF
/save
EOF