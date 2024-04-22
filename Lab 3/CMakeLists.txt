cmake_minimum_required(VERSION 3.13.4)
project(lab3)
set(CMAKE_CXX_STANDARD 20 CACHE STRING "")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(LLVM 15 REQUIRED CONFIG)

add_library(Lab3 SHARED UnitLICM.cpp UnitLoopInfo.cpp UnitSCCP.cpp RegisterPasses.cpp)
target_include_directories(Lab3 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${LLVM_INCLUDE_DIRS})
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(Lab3 PUBLIC "-fno-rtti")
endif()
