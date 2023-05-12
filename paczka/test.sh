#!/bin/bash

for file in good/*
do
    echo "Testing $file"
    ./interpreter "$file"
    if [ $? -ne 0 ]; then
        echo "Error"
        exit 1
    fi
    echo "-------------------"
done
