cmake_minimum_required(VERSION 3.13.4)
project(lab3)
set(CMAKE_CXX_STANDARD 20 CACHE STRING "")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(LLVM 15 REQUIRED CONFIG)

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
include_directories(${LLVM_INCLUDE_DIRS})

add_library(Lab3 SHARED UnitLICM.cpp UnitLoopInfo.cpp UnitSCCP.cpp RegisterPasses.cpp)

llvm_map_components_to_libnames(llvm_libs irreader transformutils)
target_link_libraries(Lab3 PRIVATE ${llvm_libs})

if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(Lab3 PUBLIC "-fno-rtti")
endif()
