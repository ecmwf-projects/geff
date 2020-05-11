cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

if(POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
endif()
 
project(geff VERSION 4.0.1 HOMEPAGE_URL "https://git.ecmwf.int/projects/CEMSF/repos/geff" LANGUAGES Fortran)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


if (NOT TARGET eccodes_f90)
    message(STATUS "eccodes Fortran library: attempting to find package")
    find_package(eccodes REQUIRED HINTS eccodes_DIR)
    if (NOT TARGET eccodes_f90)
        message(FATAL_ERROR "eccodes Fortran library (eccodes_f90) not found")
    endif()
endif()


add_subdirectory(src)


include(CTest)
add_subdirectory(tests)


set(CPACK_PACKAGE_CONTACT "ECMWF")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Global ECMWF Fire Forecasting (GEFF) model")
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_SOURCE_DIR}/LICENSE)

set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_SOURCE_IGNORE_FILES "/.git/;.gitignore;.vscode;CMakeLists.txt.user*;/data/*;${CPACK_SOURCE_IGNORE_FILES}")

include(CPack)

