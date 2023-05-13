#!/bin/bash

make_pid=$(ps -A | grep make | grep -Po '(?<=^|^ |^  )[0-9]+')

echo $make_pid

kill -9 $make_pid

fg
