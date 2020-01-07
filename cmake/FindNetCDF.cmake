# - Find NetCDF
# Find the native NetCDF includes and library
#
#  NETCDF_INCLUDES    - where to find netcdf.h, etc
#  NETCDF_LIBRARIES   - Link these libraries when using NetCDF
#  NETCDF_FOUND       - True if NetCDF found including required interfaces (see below)
#
# You can require certain interfaces via COMPONENTS: C, CXX, Fortran
#
# The following are not for general use and are included in
# NETCDF_LIBRARIES if the corresponding option above is set.
#
#  NETCDF_LIBRARIES_C    - Just the C interface
#  NETCDF_LIBRARIES_CXX  - C++ interface, if available
#  NETCDF_LIBRARIES_F77  - Fortran 77 interface, if available
#  NETCDF_LIBRARIES_F90  - Fortran 90 interface, if available
#
# Normal usage would be:
#  find_package (NetCDF COMPONENTS CXX REQUIRED)
#  target_link_libraries (myTarget PRIVATE netcdf::netcdf)

find_path (NETCDF_INCLUDES netcdf.h HINTS NETCDF_DIR ENV NETCDF_DIR PATH_SUFFIXES "" "include")
if (NETCDF_INCLUDES)
    set (NETCDF_INCLUDE_DIRS "${NETCDF_INCLUDES}")
endif()

find_library (NETCDF_LIBRARIES_C NAMES netcdf HINTS NETCDF_DIR ENV NETCDF_DIR PATH_SUFFIXES "" "lib")
mark_as_advanced (NETCDF_LIBRARIES_C)

set (NetCDF_has_interfaces "YES") # will be set to NO if we're missing any interfaces
set (NetCDF_libs "${NETCDF_LIBRARIES_C}")

get_filename_component (NetCDF_lib_dirs "${NETCDF_LIBRARIES_C}" PATH)

macro (NetCDF_check_interface lang header libs)
    find_path (NETCDF_INCLUDES_${lang} NAMES ${header}
      HINTS "${NETCDF_INCLUDES}" NO_DEFAULT_PATH)
    find_library (NETCDF_LIBRARIES_${lang} NAMES ${libs}
      HINTS "${NetCDF_lib_dirs}" NO_DEFAULT_PATH)
    mark_as_advanced (NETCDF_INCLUDES_${lang} NETCDF_LIBRARIES_${lang})
    if (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
      list (INSERT NetCDF_libs 0 ${NETCDF_LIBRARIES_${lang}}) # prepend so that -lnetcdf is last
    else (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
      set (NetCDF_has_interfaces "NO")
      message (STATUS "Failed to find NetCDF interface for ${lang}")
    endif (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
endmacro (NetCDF_check_interface)

if (NetCDF_FIND_COMPONENTS)
  foreach (component IN LISTS NetCDF_FIND_COMPONENTS)
    if (component STREQUAL "Fortran")
      NetCDF_check_interface (F77 netcdf.inc netcdff)
      NetCDF_check_interface (F90 netcdf.mod netcdff)
    elseif (component STREQUAL "C")
      NetCDF_check_interface (C netcdf.h netcdff)
    elseif (component STREQUAL "CXX")
      NetCDF_check_interface (CXX netcdf netcdf_c++4)
    endif()
  endforeach()
else (NetCDF_FIND_COMPONENTS)
  NetCDF_check_interface (C netcdf.h netcdff)
endif (NetCDF_FIND_COMPONENTS)

list (REMOVE_DUPLICATES NetCDF_libs)
set (NETCDF_LIBRARIES "${NetCDF_libs}" CACHE STRING "All NetCDF libraries required for interface level")

# handle the QUIETLY and REQUIRED arguments and set NETCDF_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (NetCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDES NetCDF_has_interfaces)

if (NetCDF_FOUND AND NOT TARGET netcdf::netcdf)
    add_library(netcdf::netcdf INTERFACE IMPORTED)
    set_target_properties(netcdf::netcdf PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${NETCDF_INCLUDES}" INTERFACE_LINK_LIBRARIES "${NETCDF_LIBRARIES}")
endif()

mark_as_advanced (NETCDF_LIBRARIES NETCDF_INCLUDES)

