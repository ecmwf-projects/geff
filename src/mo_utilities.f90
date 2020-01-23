! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Module for meteorological constant fields and fire variables
!> @brief Tunable constants for model
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_utilities

    IMPLICIT NONE

    PROCEDURE(), POINTER :: assert => assert_dbg

CONTAINS

    SUBROUTINE timer(message, c, t1, t2)
        CHARACTER(LEN=*) :: message
        INTEGER :: c
        REAL :: t1, t2

        CALL cpu_time(t2)
        PRINT *, 'Check point ', c, message, 1000. * (t2 - t1)

        t1 = t2
        c = c + 1
    END SUBROUTINE

    LOGICAL FUNCTION isnan(R)
        REAL, INTENT(IN) :: R
        isnan = R .NE. R
    END FUNCTION

    SUBROUTINE assert_prd(condition, message)
        LOGICAL, INTENT(IN) :: condition
        CHARACTER(LEN=*), INTENT(IN) :: message
        ! Do nothing
    END SUBROUTINE

    SUBROUTINE assert_dbg(condition, message)
        LOGICAL, INTENT(IN) :: condition
        CHARACTER(LEN=*), INTENT(IN) :: message
        IF (.NOT. condition) THEN
            PRINT *, '%ERROR: assertion failed: '//message
            STOP 1
        ENDIF
    END SUBROUTINE

END MODULE

