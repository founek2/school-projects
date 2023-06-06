#!/bin/bash

RED='\033[0;31m'
NC='\033[0m' # No Color
BLUE='\033[0;34m'

gcc -Wall -pedantic main2.c
positiveCounter=0
for i in `seq -w 0 6`;
#for i in 0;
do
    output=$(./a.out < "tests2/000${i}_in.txt");
    content=$(cat "tests2/000${i}_out.txt");
    if [ "$output" != "$content" ]; then
        echo "${RED}test failed ${i}${NC}";
        echo "${BLUE}expected:${NC}";
        echo "${content}";
        #od -c "tests2/000${i}_out.txt";
        echo "${BLUE}Got:${NC}";

        echo "${output}"
        #echo "$output" > ".secret";
        #od -c ".secret";
    else
        positiveCounter=$(($positiveCounter + 1))
    fi
done
echo ""
echo "Passed test number> ${positiveCounter}"