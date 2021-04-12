# (C) Copyright 1996- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

if(BUILD_TESTING)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

    file(GLOB _files RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "*.f90")
    foreach(_file ${_files})
        get_filename_component(_basename "${_file}" NAME)
        get_filename_component(_name "${_basename}" NAME_WE)

        add_executable("${_name}" "${_file}")
        add_test(NAME "${_name}" COMMAND "${_name}")
        if(_name MATCHES "-fail$")
            set_tests_properties("${_name}" PROPERTIES WILL_FAIL TRUE)
        endif()
    endforeach()

    execute_process(COMMAND "${CMAKE_COMMAND}" -E create_symlink "${CMAKE_CURRENT_SOURCE_DIR}/data" "${CMAKE_CURRENT_BINARY_DIR}/data")
    execute_process(COMMAND "${CMAKE_COMMAND}" -E create_symlink "${CMAKE_CURRENT_SOURCE_DIR}/geff.namelist" "${CMAKE_CURRENT_BINARY_DIR}/geff.namelist")
    add_test(NAME geff-tool COMMAND geff)
endif()

