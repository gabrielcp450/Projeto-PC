#!/bin/bash
nc -N localhost 12546 <<EOF
/c joao 1234
EOF
nc -N localhost 12546 <<EOF
/c edgar 1234
EOF
nc -N localhost 12546 <<EOF
/c gabriel 1234
EOF
nc -N localhost 12546 <<EOF
/c davide 1234
EOF
nc -N localhost 12546 <<EOF
/save
EOF