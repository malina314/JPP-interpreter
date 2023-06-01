#!/bin/bash

# for file in ./bad/*.jpp
# do
#   ./interpreter -v "$file" 1>"${file%.jpp}.out" 2>"${file%.jpp}.err"
# done

# for file in ./good/*.jpp
# do
#   ./interpreter "$file" 1>"${file%.jpp}.out" 2>"${file%.jpp}.err"
# done

for file in ./bad/TC-niezadeklarowana-zmienna-0*.jpp
do
  ./interpreter -v "$file" 1>"${file%.jpp}.out" 2>"${file%.jpp}.err"
done

for file in ./good/09-przesłanianie-i-statyczne-wiązanie-0*.jpp
do
  ./interpreter "$file" 1>"${file%.jpp}.out" 2>"${file%.jpp}.err"
done
