cmake_minimum_required(VERSION 3.14)
project(C2SSA)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/llvm.cmake)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_FLAGS "-fno-rtti")

include_directories(
  ${LLVM_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
link_directories(${LLVM_LIBRARY_DIRS})

set(HEADERS
  ${CMAKE_CURRENT_SOURCE_DIR}/include/BackendUtil.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/CBackend.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/CodeGenAction.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/IDMap.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/PrintingPass.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/TopologicalSorter.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/LoopUnrollPass.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/PassManagerBuilder.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/CallGraph.h
)

set(CPPS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/BackendUtil.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/CBackend.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/CodeGenAction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/PrintingPass.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/TopologicalSorter.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/LoopUnrollPass.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/PassManagerBuilder.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/CallGraph.cpp
)

# Now build our tools
add_executable(c2ssa
  ${CPPS}
  ${HEADERS}
)

target_link_libraries(c2ssa
  ${LLVM_LIBS}
  ${SYSTEM_LIBS}
)
