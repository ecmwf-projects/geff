! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Control parameters
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_control

    IMPLICIT NONE

    CHARACTER(LEN=200) :: output_file=''
    CHARACTER(LEN=200) :: constant_file=''
    CHARACTER(LEN=200) :: init_file=''
    CHARACTER(LEN=50)  :: now=''

    CHARACTER(LEN=*), PARAMETER :: namelst='geff.namelist'

    INTEGER :: inidate
    INTEGER :: initime
    INTEGER :: dt
    INTEGER :: restart_day

    CHARACTER(LEN=200) :: rainfile
    CHARACTER(LEN=200) :: rainclimfile
    CHARACTER(LEN=200) :: tempfile
    CHARACTER(LEN=200) :: maxtempfile
    CHARACTER(LEN=200) :: mintempfile
    CHARACTER(LEN=200) :: rhfile
    CHARACTER(LEN=200) :: maxrhfile
    CHARACTER(LEN=200) :: minrhfile
    CHARACTER(LEN=200) :: snowfile
    CHARACTER(LEN=200) :: ccfile
    CHARACTER(LEN=200) :: wspeedfile
    CHARACTER(LEN=200) :: dpfile
    CHARACTER(LEN=200) :: vsfile

    CHARACTER(LEN=200) :: lsmfile
    CHARACTER(LEN=200) :: cvfile
    CHARACTER(LEN=200) :: slopefile
    CHARACTER(LEN=200) :: crfile
    CHARACTER(LEN=200) :: fmfile

    REAL :: rfillvalue=-9999.0 ! missing value
    INTEGER :: ifillvalue=-9999 ! missing value
    LOGICAL :: lwritten_results

END MODULE mo_control
