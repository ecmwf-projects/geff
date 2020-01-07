# - Find ecCodes
# Find the native eccodes includes and library
#
#  eccodes_INCLUDES    - where to find eccodes.h, etc
#  eccodes_LIBRARIES   - Link these libraries when using ecCodes
#  eccodes_FOUND       - True if ecCodes found including required interfaces (see below)
#
# You can require certain interfaces via COMPONENTS: C, Fortran
#
# Normal usage would be:
#  find_package (eccodes COMPONENTS C REQUIRED)
#  target_link_libraries (myTarget PRIVATE eccodes::eccodes)

find_path (eccodes_INCLUDES eccodes.h HINTS eccodes_DIR ECCODES_DIR ENV eccodes_DIR PATH_SUFFIXES "" "include" DOC "ecCodes includes")

macro (eccodes_check_interface lang header libs)
    find_path (eccodes_INCLUDES_${lang} NAMES ${header} HINTS "${eccodes_INCLUDES}" DOC "ecCodes includes (${lang} interface)" NO_DEFAULT_PATH)
    find_library (eccodes_LIBRARIES_${lang} NAMES ${libs} HINTS eccodes_DIR ECCODES_DIR ENV eccodes_DIR PATH_SUFFIXES "" "lib" DOC "ecCodes libraries (${lang} interface)" NO_DEFAULT_PATH)
    mark_as_advanced (eccodes_LIBRARIES_${lang} eccodes_INCLUDES_${lang})

    if (eccodes_INCLUDES_${lang} AND eccodes_LIBRARIES_${lang})
        list (INSERT eccodes_libs 0 ${eccodes_LIBRARIES_${lang}}) # prepend so that -leccodes is last
    else ()
        set (eccodes_has_interfaces "NO")
        message (STATUS "Failed to find ecCodes interface for ${lang}")
    endif ()
endmacro (eccodes_check_interface)

set (eccodes_has_interfaces "YES") # set to NO if we're missing any interfaces
if (ecCodes_FIND_COMPONENTS)
    foreach (component IN LISTS ecCodes_FIND_COMPONENTS)
        if (component STREQUAL "Fortran")
            eccodes_check_interface (F90 eccodes.mod eccodes_f90)
        elseif (component STREQUAL "C")
            eccodes_check_interface (C eccodes.h eccodes)
        endif()
    endforeach()
else()
    eccodes_check_interface (C eccodes.h eccodes)
endif()

list (REMOVE_DUPLICATES eccodes_libs)
set (eccodes_LIBRARIES "${eccodes_libs}" CACHE PATH "ecCodes libraries")

# handle the QUIETLY and REQUIRED arguments and set eccodes_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (eccodes DEFAULT_MSG eccodes_LIBRARIES eccodes_INCLUDES eccodes_has_interfaces)

if (eccodes_FOUND AND NOT TARGET eccodes::eccodes)
    add_library(eccodes::eccodes INTERFACE IMPORTED)
    set_target_properties(eccodes::eccodes PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${eccodes_INCLUDES}" INTERFACE_LINK_LIBRARIES "${eccodes_LIBRARIES}")
endif()

mark_as_advanced (eccodes_LIBRARIES eccodes_INCLUDES)

