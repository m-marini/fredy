
# Set the minimum version of cmake required to build this project
cmake_minimum_required(VERSION 3.10)

# Thanks to @danger89 and @Ilothar for updating the gist.
# Set the name and the supported language of the project
project(fredy C CXX)

# Use the package PkgConfig to detect GTK+ headers/library files
find_package(PkgConfig REQUIRED)

pkg_check_modules(GTK REQUIRED gtkmm-3.0)

add_executable(fredy Fredy.cpp Inference.cpp Fuzzy.cpp)

# Add other flags to the compiler
#target_compile_definitions(fredy PRIVATE ${GTKMM_CFLAGS_OTHER})
target_compile_options(fredy PRIVATE ${GTKMM_CFLAGS_OTHER})

# Setup CMake to use GTK+, tell the compiler where to look for headers
# and to the linker where to look for libraries
target_include_directories(fredy PRIVATE ${GTKMM_INCLUDE_DIRS} include /usr/local/include)

target_link_libraries(fredy PRIVATE ${GTKMM_LIBRARIES})

