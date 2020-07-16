
configure_file(mo_version.f90.in mo_version.f90 @ONLY)

add_library(geff SHARED
    mo_constants.f90
    mo_control.f90
    mo_fire.f90
    mo_fuelmodel.f90
    mo_fwi.f90
    mo_interpolation.F90
    mo_io_eccodes.f90
    mo_mark5.f90
    mo_nfdrs.f90
    mo_statistics.f90
    mo_utilities.f90
    mo_vegstage.f90
    ${CMAKE_CURRENT_BINARY_DIR}/mo_version.f90)

target_link_libraries(geff PUBLIC eccodes_f90)
if (NOT TARGET eccodes_f90)
    target_include_directories(geff PUBLIC ${eccodes_MOD_PATH})
endif()

if(HAVE_GEFF_INTERPOLATION)
    target_link_libraries(geff PUBLIC atlas_f)
    set_source_files_properties(mo_interpolation.F90 PROPERTIES COMPILE_DEFINITIONS HAVE_GEFF_INTERPOLATION)
    target_sources(geff PRIVATE mo_interpolation_atlas.f90)
endif()

install(TARGETS geff RUNTIME DESTINATION "${CMAKE_INSTALL_PREFIX}/lib")

foreach(_t geff-tool geff-statistics geff-daymax)
    add_executable(${_t} ${_t}.f90)
    target_link_libraries(${_t} geff)
endforeach()

set_target_properties(geff-tool geff-statistics geff-daymax PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
install(TARGETS geff-tool geff-statistics geff-daymax RUNTIME DESTINATION "${CMAKE_INSTALL_PREFIX}/bin")

