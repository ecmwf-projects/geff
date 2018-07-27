SUBROUTINE getdata(iday)
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! getdata subroutine to read in climate and weather regions slice
!
!
!---------------------------------------------------------
  USE netcdf
  USE mo_control
  USE mo_constants
  USE mo_fire
  USE mo_ncdf_tools

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iday

  !---------------------------------------------------------------------------
  !  meteorology variables read in from analysis, obs or forecast, or constant
  !---------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! gridded input - assumes the input files have the correct timestamps
  !                 i.e. just reads the file blindly - the lat/lon info is 
  !                 important though...
  !------------------------------------------------------------------------
 
 
  CALL check(NF90_GET_VAR(ncid_rain,   VarId_rain, rrain, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_temp, VarId_temp, rtemp, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_maxtemp, VarId_maxtemp, rmaxtemp, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_mintemp, VarId_mintemp, rmintemp, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_rh, VarId_rh, rrh, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_maxrh, VarId_maxrh, rmaxrh, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_minrh, VarId_minrh, rminrh, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_cc, VarId_cc, rcc, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_wspeed, VarId_wspeed, rwspeed, start=(/1,1,iday/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_snow, VarId_snow, rsnow, start=(/1,1,iday/),count=(/nlon,nlat,1/))) 
  CALL check(NF90_GET_VAR(ncid_dp, VarId_dp, rdp, start=(/1,1,iday/),count=(/nlon,nlat,1/))) 
  CALL check(NF90_GET_VAR(ncid_vs, VarId_vs, ivs, start=(/1,1,iday/),count=(/nlon,nlat,1/))) 
! CALL check(NF90_GET_VAR(ncid_lal, VarId_lal, ilal, start=(/1,1,iday/),count=(/nlon,nlat,1/))) 

  IF (lrain_latreverse)        rrain=rrain(:,nlat:1:-1)
  IF (ltemp_latreverse)        rtemp=rtemp(:,nlat:1:-1)
  IF (lmaxtemp_latreverse)     rmaxtemp=rmaxtemp(:,nlat:1:-1)
  IF (lmintemp_latreverse)     rmintemp=rmintemp(:,nlat:1:-1)
  IF (lrh_latreverse)          rrh=rrh(:,nlat:1:-1)
  IF (lmaxrh_latreverse)       rmaxrh=rmaxrh(:,nlat:1:-1)
  IF (lminrh_latreverse)       rminrh=rminrh(:,nlat:1:-1)
  IF (lcc_latreverse)          rcc=rcc(:,nlat:1:-1)
  IF (lwspeed_latreverse)      rwspeed=rwspeed(:,nlat:1:-1)
  IF (lsnow_latreverse)        rsnow=rsnow(:,nlat:1:-1)
  IF (ldp_latreverse)          rdp=rdp(:,nlat:1:-1)
  IF (lvs_latreverse)          ivs=ivs(:,nlat:1:-1)

 


!  IF (llal_latreverse)         ilal=ilal(:,nlat:1:-1)
END SUBROUTINE getdata

