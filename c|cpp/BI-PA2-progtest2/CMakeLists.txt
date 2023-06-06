cmake_minimum_required(VERSION 3.12)
project(progtest2)

set(CMAKE_CXX_STANDARD 11)

set(GCC_COVERAGE_COMPILE_FLAGS "-fsanitize-recover=address -Wall -pedantic -Wno-long-long -Wpedantic ")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )

add_executable(progtest2 main.cpp)

add_executable(progtest2.2 main2.cpp)