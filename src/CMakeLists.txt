
configure_file(mo_version.f90.in mo_version.f90 @ONLY)

add_library(geff
    mo_geff.f90
    mo_constants.f90
    mo_control.f90
    mo_fire.f90
    mo_fuelmodel.f90
    mo_fwi.f90
    mo_io_eccodes.f90
    mo_mark5.f90
    mo_nfdrs.f90
    mo_utilities.f90
    mo_vegstage.f90
    ${CMAKE_CURRENT_BINARY_DIR}/mo_version.f90)

target_link_libraries(geff PUBLIC eccodes_f90)
if (HAVE_GEFF_INTERPOLATION)
    target_link_libraries(geff PUBLIC atlas_f)
    target_sources(geff PRIVATE mo_interpolation.f90)
else()
    target_sources(geff PRIVATE mo_no_interpolation.f90)
endif()


add_executable(geff-tool geff.f90)
target_link_libraries(geff-tool PUBLIC geff)
set_target_properties(geff-tool PROPERTIES OUTPUT_NAME geff)


add_executable(interpol interpol.f90)
target_link_libraries(interpol PUBLIC geff)

