
add_executable (geff
    dump_restart.f90
    geff.f90
    getdata.f90
    initialize.f90
    mo_constants.f90
    mo_control.f90
    mo_fire.f90
    mo_fuelmodel.f90
    mo_fwi.f90
    mo_mark5.f90
    mo_namelist.f90
    mo_ncdf_tools.f90
    mo_nfdrs.f90
    mo_vegstage.f90
    open_input.f90
    open_output.f90
    set_grid.f90
    setdown.f90
    setup.f90)

target_link_libraries (geff PRIVATE netcdf::netcdf)
if (TARGET eccodes::eccodes)
    target_link_libraries (geff PRIVATE eccodes::eccodes)
endif()

set_target_properties (geff PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

