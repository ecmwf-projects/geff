
if (BUILD_TESTING)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

    file(GLOB _files RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "*.f90")
    foreach(_file ${_files})
        get_filename_component(_basename "${_file}" NAME)
        get_filename_component(_name "${_basename}" NAME_WE)
        set(_name "${PROJECT_NAME}_${_name}")

        add_executable("${_name}" "${_file}")
        add_test(NAME "${_name}" COMMAND "${_name}")
        if (_name MATCHES "-fail$")
            set_tests_properties("${_name}" PROPERTIES WILL_FAIL TRUE)
        endif()

    endforeach()
endif()

