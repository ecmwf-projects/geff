! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Control parameters
!> @author Di Giuseppe, F., ECMWF
MODULE mo_control

  USE mo_constants

  IMPLICIT NONE
!
! these will be user definable soon
!
!
! run control
!
! define parameters in namelist

  CHARACTER (len = 200) :: output_file
  CHARACTER (len = 50)  :: now
  CHARACTER (len = 100) :: init_file='none'

  CHARACTER(len=*), PARAMETER :: namelst='geff.namelist'

  INTEGER :: inidate
  INTEGER :: initime
  INTEGER :: dt ! dt is in hours
  ! move derived parameters to setup.f90
  INTEGER :: restart_day !number of timestep before dumping a restart file


  ! input file indices

  CHARACTER (LEN=2) :: crainvar="tp"
  CHARACTER (LEN=6) :: crainclimvar="tpclim"
  CHARACTER (LEN=2) :: ctempvar="t2"
  CHARACTER (LEN=5) :: cmaxtempvar="maxt2"
  CHARACTER (LEN=5) :: cmintempvar="mint2"
  CHARACTER (LEN=2) :: crhvar="rh"
  CHARACTER (LEN=5) :: cmaxrhvar="maxrh"
  CHARACTER (LEN=5) :: cminrhvar="minrh"
  CHARACTER (LEN=6) :: cwspeedvar="wspeed"
  CHARACTER (LEN=2) :: cccvar="cc"
  CHARACTER (LEN=4) :: csnowvar="snow"
  CHARACTER (LEN=3) :: cdpvar="dp" ! duration of precipitation in the previous 24 hours(hr)
  CHARACTER (LEN=9) :: cvsvar="veg_stage" !
  CHARACTER (LEN=3) :: clsmvar="lsm"
  CHARACTER (LEN=3) :: ccvvar="cv"
  CHARACTER (LEN=5) :: cslopevar="slope"
  CHARACTER (LEN=13) :: ccrvar="climate_class" ! climate regions taken from the dataset  Koeppen_CRU_GPCCVASClimO
  CHARACTER (LEN=10) :: cfmvar="fuel_model" !

  !============================================================

  ! climate data file
  CHARACTER(len=200) :: rainfile
  CHARACTER(len=200) :: rainclimfile
  CHARACTER(len=200) :: tempfile
  CHARACTER(len=200) :: maxtempfile
  CHARACTER(len=200) :: mintempfile
  CHARACTER(len=200) :: rhfile
  CHARACTER(len=200) :: maxrhfile
  CHARACTER(len=200) :: minrhfile
  CHARACTER(len=200) :: snowfile
  CHARACTER(len=200) :: ccfile
  CHARACTER(len=200) :: wspeedfile
  CHARACTER(len=200) :: dpfile
  CHARACTER(len=200) :: vsfile

  CHARACTER(len=200) :: lsmfile
  CHARACTER(len=200) :: cvfile
  CHARACTER(len=200) :: slopefile
  CHARACTER(len=200) :: crfile
  CHARACTER(len=200) :: fmfile

! index of point to dump quick timeseries diagnostics
  INTEGER :: nxdg=369
  INTEGER :: nydg=229
  REAL :: rfillvalue=-9999.0 ! missing value
  INTEGER :: ifillvalue=-9999 ! missing value

END MODULE mo_control
