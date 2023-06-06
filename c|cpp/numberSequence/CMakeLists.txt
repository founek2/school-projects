cmake_minimum_required(VERSION 3.12)
project(numberSequence C)

set(CMAKE_C_STANDARD 99)

add_executable(numberSequence main.c)

add_executable(karel mainKarel.c)

add_executable(harder main2.c)

add_executable(hyperloob hyperLoop.c)

add_executable(termASCI termArt.c)