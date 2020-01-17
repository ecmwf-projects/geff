
configure_file (mo_version.f90.in mo_version.f90 @ONLY)

add_executable (geff
    geff.f90
    mo_constants.f90
    mo_control.f90
    mo_fire.f90
    mo_fuelmodel.f90
    mo_fwi.f90
    mo_mark5.f90
    mo_ncdf_tools.f90
    mo_nfdrs.f90
    mo_vegstage.f90
    ${CMAKE_CURRENT_BINARY_DIR}/mo_version.f90)

target_link_libraries (geff PRIVATE netcdf::netcdf)
if (TARGET eccodes::eccodes)
    target_link_libraries (geff PRIVATE eccodes::eccodes)
    target_sources (geff PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/mo_grib_tools.f90")
    set_source_files_properties (mo_inout.F90 PROPERTIES COMPILE_DEFINITIONS ECCODES_FOUND)
endif()

set_target_properties (geff PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

