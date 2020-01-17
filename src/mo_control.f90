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

  INTEGER :: inidate, initime
  INTEGER :: dt ! dt is in hours
  ! move derived parameters to setup.f90
  INTEGER :: restart_day !number of timestep before dumping a restart file

  LOGICAL :: lgrib=.false.


! netcdf and input file indices

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
!  CHARACTER (LEN=3) :: clalvar="lal" !
  CHARACTER (LEN=3) :: clsmvar="lsm"
  CHARACTER (LEN=3) :: ccvvar="cv"
  CHARACTER (LEN=5) :: cslopevar="slope"
  CHARACTER (LEN=13) :: ccrvar="climate_class" ! climate regions taken from the dataset  Koeppen_CRU_GPCCVASClimO
  CHARACTER (LEN=10) :: cfmvar="fuel_model" !


  INTEGER :: ncid_temp, ncid_maxtemp,ncid_mintemp,&
       ncid_minrh, ncid_maxrh, ncid_rh, &
       ncid_rain, ncid_cc, ncid_wspeed, &
       ncid_snow, ncid_dp, ncid_vs,     &
       ncid_rainclim,         & !ncid_lal,
       ncid_lsm,     &
       ncid_cv,ncid_slope,   &
       ncid_cr, ncid_fm, infile    ! file id
  INTEGER :: varid_temp, varid_maxtemp, varid_mintemp, &
       varid_rh, varid_maxrh, varid_minrh, &
       varid_rain, varid_cc,varid_wspeed,  &
       varid_snow,varid_dp,varid_vs,       &
       varid_rainclim,           & !varid_lal,
       varid_lsm, varid_cv, varid_slope,  &
       varid_cr,varid_fm ! var  id
  LOGICAL :: ltemp_latreverse=.false. ! reverse the lats?
  LOGICAL :: lmaxtemp_latreverse=.false.
  LOGICAL :: lmintemp_latreverse=.false. ! reverse the lats?
  LOGICAL :: lrh_latreverse=.false.
  LOGICAL :: lmaxrh_latreverse=.false. ! reverse the lats?
  LOGICAL :: lminrh_latreverse=.false.
  LOGICAL :: lrain_latreverse=.false.
  LOGICAL :: lrainclim_latreverse=.false.
  LOGICAL :: lwspeed_latreverse=.false.
  LOGICAL :: lsnow_latreverse=.false.
  LOGICAL :: lcc_latreverse=.false.
  LOGICAL :: ldp_latreverse=.false.
  LOGICAL :: lvs_latreverse=.false.
!  LOGICAL :: llal_latreverse=.false.
  LOGICAL :: llsm_latreverse=.false.
  LOGICAL :: lcv_latreverse=.false.
  LOGICAL :: lslope_latreverse=.false.
  LOGICAL :: lcr_latreverse=.false.
  LOGICAL :: lfm_latreverse=.false.

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
!  CHARACTER(len=200) :: lalfile

  CHARACTER(len=200) :: lsmfile
  CHARACTER(len=200) :: cvfile
  CHARACTER(len=200) :: slopefile
  CHARACTER(len=200) :: crfile
  CHARACTER(len=200) :: fmfile


  CHARACTER (len=31) :: time_units ! e.g. "days since 1990-11-25 00:00 UTC" this is a variable fields depends on date1
  CHARACTER (len = *), PARAMETER :: lat_name = "lat"
  CHARACTER (len = *), PARAMETER :: lon_name = "lon"
  CHARACTER (len = *), PARAMETER :: time_name = "time"
  CHARACTER (len = *), PARAMETER :: units = "units"
  CHARACTER (len = *), PARAMETER :: lat_units = "degrees_north"
  CHARACTER (len = *), PARAMETER :: lon_units = "degrees_east"

! control of output file
  LOGICAL :: lnc_rain=.true.
  LOGICAL :: lnc_rainclim=.true.
  LOGICAL :: lnc_temp=.true.
  LOGICAL :: lnc_maxtemp=.true.
  LOGICAL :: lnc_mintemp=.true.
  LOGICAL :: lnc_rh=.true.
  LOGICAL :: lnc_maxrh=.true.
  LOGICAL :: lnc_minrh=.true.
  LOGICAL :: lnc_snow=.true.
  LOGICAL :: lnc_wspeed=.true.
  LOGICAL :: lnc_cc=.true.
  LOGICAL :: lnc_dp=.true.
  LOGICAL :: lnc_vs=.true.
!  LOGICAL :: lnc_lal=.true.
  LOGICAL :: lnc_lsm=.true.
  LOGICAL :: lnc_cv=.true.
  LOGICAL :: lnc_slope=.true.
  LOGICAL :: lnc_cr=.true.
  LOGICAL :: lnc_fm=.true.

  !fire variables

  LOGICAL :: lnc_nfdrs=.true.
  LOGICAL :: lnc_mark5=.true.
  LOGICAL :: lnc_fwi=.true.

! index of point to dump quick timeseries diagnostics
  INTEGER :: nxdg=369,nydg=229
  REAL :: rfillvalue=-9999.0 ! missing value
  INTEGER :: ifillvalue=-9999 ! missing value

END MODULE mo_control
