#!/bin/bash
for filename in ../przyklady/*
do
    echo "Test $filename"
    cat "$filename" | ./TestGramatyka
done
