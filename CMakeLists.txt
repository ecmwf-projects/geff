# (C) Copyright 1996- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

project(geff VERSION 4.2.2 HOMEPAGE_URL "https://github.com/ecmwf-projects/geff" LANGUAGES Fortran)

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

