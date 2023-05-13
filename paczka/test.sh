#!/bin/bash

echo "Testing good files"
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

echo "Testing bad files"
echo "Testing type checking errors"
for file in bad/TC-*
do
    echo "Testing $file"
    ./interpreter "$file" > /tmp/err
    if [ $? -ne 0 ]; then
        echo "Error"
        exit 1
    fi
    grep "Type checking failed!" /tmp/err > /dev/null
    if [ $? -ne 0 ]; then
        echo "Error: grep failed"
        exit 1
    fi
    echo "-------------------"
done
