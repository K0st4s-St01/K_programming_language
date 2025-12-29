cmake_minimum_required(VERSION 3.15)

set(CMAKE_BUILD_TYPE Debug)
add_compile_options(-g)
# 🧠 Project definition
project(Winter LANGUAGES C CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# 🧩 Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVM in: ${LLVM_DIR}")
message(STATUS "LLVM include dirs: ${LLVM_INCLUDE_DIRS}")

# Include LLVM and your own headers
include_directories(
    ${LLVM_INCLUDE_DIRS}
    ${PROJECT_SOURCE_DIR}/headers
)

add_definitions(${LLVM_DEFINITIONS})

# 🧩 Source files
file(GLOB_RECURSE SRC_FILES
    ${PROJECT_SOURCE_DIR}/src/*.cpp
)

# 🧱 Executable
add_executable(raven_compiler ${SRC_FILES})

# 🪄 Get correct LLVM link flags dynamically (modern unified library model)
execute_process(
    COMMAND llvm-config --libs core orcjit nativecodegen x86codegen irreader --system-libs
    OUTPUT_VARIABLE LLVM_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
separate_arguments(LLVM_LIBS)

# 🧷 Link everything
target_link_libraries(raven_compiler PRIVATE ${LLVM_LIBS})

# Ensure runtime can find LLVM shared lib (if not in /usr/lib)
execute_process(
    COMMAND llvm-config --libdir
    OUTPUT_VARIABLE LLVM_LIB_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

set_target_properties(raven_compiler PROPERTIES
    BUILD_RPATH "${LLVM_LIB_DIR}"
    INSTALL_RPATH "${LLVM_LIB_DIR}"
)

# 🧹 Group source files logically (for IDEs)
source_group(TREE ${PROJECT_SOURCE_DIR} FILES ${SRC_FILES})