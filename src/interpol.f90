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

    USE mo_interpolation
    USE eccodes

    IMPLICIT NONE

    CHARACTER*256 ARG
    INTEGER I
    INTEGER IARGC


    DO I = 1, IARGC()
        CALL GETARG(I,ARG)
        PRINT *, 'ARG='//ARG
    ENDDO

ENDPROGRAM
