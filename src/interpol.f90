! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief GEFF runtime. Code structure:
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
PROGRAM interpol

    USE atlas_module
    USE mo_io_eccodes

    IMPLICIT NONE

    CHARACTER*256 ARG
    INTEGER I
    INTEGER IARGC

    REAL, ALLOCATABLE :: valuesA(:)

    ALLOCATE(valuesA(5248))   ! O32
    !ALLOCATE(valuesA(18688))  ! O64


    DO I = 1, IARGC()
        CALL GETARG(I,ARG)
        PRINT *, 'ARG='//ARG
    ENDDO



contains



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
    !CALL initialise_field_hill(fsA, fieldA)

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













ENDPROGRAM
