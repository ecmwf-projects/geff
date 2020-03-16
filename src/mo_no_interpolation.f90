! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief GEFF interpolation (dummy)
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_interpolation

    USE mo_utilities

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: interpolation


CONTAINS


    SUBROUTINE interpolation(gridNameA, aValues, gridNameB, bValues)
        CHARACTER(LEN=20), INTENT(IN) :: gridNameA
        CHARACTER(LEN=20), INTENT(IN) :: gridNameB
        REAL, ALLOCATABLE, INTENT(IN) :: aValues(:)
        REAL, ALLOCATABLE, INTENT(INOUT) :: bValues(:)

        CALL assert_dbg(.FALSE., "Interpolation not supported")
    ENDSUBROUTINE

END MODULE mo_interpolation
