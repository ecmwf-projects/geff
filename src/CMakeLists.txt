# (C) Copyright 1996- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

configure_file(mo_version.f90.in mo_version.f90 @ONLY)

add_executable(geff
    geff.f90
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

target_link_libraries(geff PRIVATE eccodes_f90)
set_target_properties(geff PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

install(TARGETS geff RUNTIME DESTINATION "${CMAKE_INSTALL_PREFIX}/bin")

