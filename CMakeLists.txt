cmake_minimum_required(VERSION 3.0)

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_SOURCE_DIR}/cmake)

project(geff VERSION 42.0.0 HOMEPAGE_URL "https://git.ecmwf.int/projects/CEMSF/repos/geff" LANGUAGES Fortran)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


find_package(NetCDF COMPONENTS Fortran)
message(STATUS "Package NetCDF: ${NetCDF_FOUND}")

find_package(ecCodes COMPONENTS Fortran)
message(STATUS "Package ecCodes: ${eccodes_FOUND}")

if (NOT NetCDF_FOUND AND NOT eccodes_FOUND)
    message(FATAL_ERROR "GEFF requires ecCodes either/or NetCDF")
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

