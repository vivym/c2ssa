cmake_minimum_required(VERSION 3.14)
project(C2SSA)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/llvm.cmake)

set (CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_FLAGS "-fno-rtti")

include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

# Now build our tools
add_executable(c2ssa src/main.cpp)

target_link_libraries(c2ssa
  ${LLVM_LIBS}
  ${SYSTEM_LIBS}
)
