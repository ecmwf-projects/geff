cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

if(POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
endif()

project(geff VERSION 5.0.0 HOMEPAGE_URL "https://git.ecmwf.int/projects/CEMSF/repos/geff" LANGUAGES Fortran)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


find_package(eccodes REQUIRED HINTS eccodes_DIR)

if (NOT TARGET eccodes_f90)
    find_path(eccodes_MOD_PATH "eccodes.mod" PATHS eccodes_DIR ENV eccodes_DIR PATH_SUFFIXES "include")
    message(STATUS "ecCodes Fortran module path: ${eccodes_MOD_PATH}")
    if (NOT eccodes_MOD_PATH)
        message(FATAL_ERROR "ecCodes Fortran module path (containing eccodes.mod) not found")
    endif()
endif()

add_subdirectory(src)


include(CTest)

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
    add_subdirectory(tests)
endif()


set(CPACK_PACKAGE_CONTACT "ECMWF")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Global ECMWF Fire Forecasting (GEFF) model")
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_SOURCE_DIR}/LICENSE)

set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_SOURCE_IGNORE_FILES ".git/;.gitignore;.vscode;CMakeLists.txt.user*;${CPACK_SOURCE_IGNORE_FILES}")

include(CPack)

