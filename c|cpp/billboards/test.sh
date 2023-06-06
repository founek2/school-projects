#!/bin/bash

RED='\033[0;31m'
NC='\033[0m' # No Color
BLUE='\033[0;34m'
YELLOW='\033[1;33m'

gcc -Wall -pedantic main.c -fsanitize=address
positiveCounter=0
folder=tests

for inputFile in "${folder}/"*_in.txt;
do
    outputOfRun=$(./a.out < "${inputFile}");
    output=$(cat "${inputFile//in/out}");
    if [ "$outputOfRun" != "$output" ]; then
        echo "${RED}test failed ${inputFile}${NC}";
        echo "${BLUE}expected:${NC}";
        echo "${output}";
        echo "${BLUE}Got:${NC}";
        echo "${outputOfRun}"
        echo "${outputOfRun}" > failedOutput.txt
    else
        positiveCounter=$(($positiveCounter + 1))
    fi
done
echo ""
echo "${YELLOW}Passed test number> ${positiveCounter}${NC}"