! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief GEFF interpolation
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_interpolationx

    USE atlas_module

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: interpolation


CONTAINS


    SUBROUTINE interpolation(method, gridNameA, gridNameB, valuesA, valuesB)
        CHARACTER(LEN=20), INTENT(IN) :: method
        CHARACTER(LEN=20), INTENT(IN) :: gridNameA
        CHARACTER(LEN=20), INTENT(IN) :: gridNameB
        REAL, ALLOCATABLE, INTENT(IN) :: valuesA(:)

        REAL, ALLOCATABLE, INTENT(INOUT) :: valuesB(:)

        TYPE(atlas_Grid)          :: gridA
        TYPE(atlas_Grid)          :: gridB
        TYPE(atlas_functionspace) :: fsA
        TYPE(atlas_functionspace) :: fsB
        TYPE(atlas_Field)         :: fieldA
        TYPE(atlas_Field)         :: fieldB
        TYPE(atlas_Config)        :: config
        TYPE(atlas_Interpolation) :: interpol
        TYPE(atlas_Trace)         :: trace

        CALL atlas_library%initialise()
        trace = atlas_Trace("mo_interpolation", __LINE__, "Here")

        ! Setup interpolation
        gridA = atlas_Grid(gridNameA)
        gridB = atlas_Grid(gridNameB)

        config = atlas_Config()
        CALL config%set("type", method)
        interpol = atlas_Interpolation(config, gridA, gridB)

        ! Create function spaces for each mesh
        fsA = interpol%source()
        fsB = interpol%target()

        ! Create fields and initialise source field
        fieldA  = fsA%create_field(name="A", KIND=atlas_real(atlas_kind_real64))
        fieldB  = fsB%create_field(name="B", KIND=atlas_real(atlas_kind_real64))
print *, fieldA%size()
        CALL initialise_field_hill(fsA, fieldA)

        ! Interpolate
        CALL interpol%execute(fieldA, fieldB)

        ! cleanup
        CALL config%final()
        CALL interpol%final()
        CALL fieldA%final()
        CALL fieldB%final()
        CALL fsA%final()
        CALL fsB%final()
        CALL gridA%final()
        CALL gridB%final()

        CALL trace%final()
        CALL atlas_library%finalise()
    ENDSUBROUTINE


    SUBROUTINE initialise_field_hill(funcspace, field)
        TYPE(atlas_functionspace), INTENT(IN) :: funcspace
        TYPE(atlas_Field), INTENT(INOUT) :: field
        REAL(atlas_kind_real64), PARAMETER :: M_PI = 3.14159265358979323846
        REAL(atlas_kind_real64), PARAMETER :: deg2rad = M_PI/180._8
        TYPE(atlas_mesh_Nodes) :: nodes
        TYPE(atlas_Field) :: field_lonlat
        REAL(atlas_kind_real64), POINTER :: value(:), lonlat(:, :)
        INTEGER :: jnode, nb_nodes
        REAL(atlas_kind_real64) :: lon, lat, c2, c_lon, c_lat, c_rad, dist, s1, s2

        !c_lat = 0. * M_PI
        !c_lon = 1. * M_PI
        !c_rad = 2. * M_PI / 9.
        !nodes = funcspace%nodes()
        !field_lonlat = nodes%lonlat()
        !CALL field_lonlat%data(lonlat)
        !CALL field%data(value)
        !nb_nodes = nodes%size()
        !do jnode=1, nb_nodes
        !    lon = deg2rad * lonlat(1, jnode)
        !    lat = deg2rad * lonlat(2, jnode)
        !    c2  = cos(lat)
        !    s1  = sin( (lon-c_lon)/2. )
        !    s2  = sin( (lat-c_lat)/2. )
        !    dist = 2. * sqrt( c2*s1*c2*s1 + s2*s2 )
        !    if( dist < c_rad ) then
        !        value(jnode) = 1. + cos(M_PI*dist/c_rad)
        !    else
        !        value(jnode) = 0
        !    endif
        !enddo
    ENDSUBROUTINE

ENDMODULE
