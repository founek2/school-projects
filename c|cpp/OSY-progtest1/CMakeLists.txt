cmake_minimum_required(VERSION 3.15)
project(OSY_progtest1)

set(CMAKE_CXX_STANDARD 17)


add_executable(OSY_progtest1 common.h progtest_solver.h sample_tester.cpp sample_tester.h solution.cpp)
target_link_libraries(OSY_progtest1 ${CMAKE_SOURCE_DIR}/x86_64-darwin19/libprogtest_solver.a)