cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

if(POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
endif()

file(READ "VERSION" VERSION)
project(geff VERSION ${VERSION} LANGUAGES Fortran HOMEPAGE_URL "https://git.ecmwf.int/projects/CEMSF/repos/geff")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)


# build
if(NOT TARGET eccodes_f90)
    message(STATUS "ecCodes Fortran library: attempting to find package")
    find_package(eccodes REQUIRED HINTS eccodes_ROOT $ENV{eccodes_ROOT} eccodes_DIR $ENV{eccodes_DIR})
    if(NOT TARGET eccodes_f90)
        message(FATAL_ERROR "ecCodes Fortran library (eccodes_f90) not found")
    endif()
endif()

if(HAVE_GEFF_INTERPOLATION AND NOT TARGET atlas_f)
    message(STATUS "Atlas Fortran library: attempting to find package")
    find_package(atlas REQUIRED HINTS atlas_ROOT $ENV{atlas_ROOT} atlas_DIR $ENV{atlas_DIR})
    if(NOT TARGET atlas_f)
        message(FATAL_ERROR "Atlas Fortran library (atlas_f) not found, interpolation not available")
    endif()
endif()

add_subdirectory(src)


# test
include(CTest)
add_subdirectory(tests)


# package
set(CPACK_PACKAGE_CONTACT "ECMWF")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Global ECMWF Fire Forecasting")
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_SOURCE_DIR}/LICENSE)
set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_SOURCE_IGNORE_FILES "/.git/;.gitignore;.vscode;CMakeLists.txt.user*;/data/*;${CPACK_SOURCE_IGNORE_FILES}")
include(CPack)

