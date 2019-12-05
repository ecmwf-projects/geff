SUBROUTINE setdown

!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!  setdown subroutine to close shop and tidy up...
!
!---------------------------------------------------------
!

  USE netcdf
  USE mo_control
  USE mo_constants
  USE mo_fire
  USE mo_ncdf_tools
  USE mo_nfdrs
  USE mo_mark5 
  USE mo_fwi
  IMPLICIT NONE

 ! INTEGER :: latvarid, lonvarid, timevarid, unlimdimid
 ! INTEGER :: nlatcheck, nloncheck, ndaycheck
 ! INTEGER :: i,j
 ! REAL, DIMENSION(nlon,nlat)::  meanrbndryt
  !---
  ! 1. close down the netcdf input and output files
  !---


  CALL check(NF90_CLOSE(ncidout))
  CALL check(NF90_CLOSE(ncid_rain))
  CALL check(NF90_CLOSE(ncid_rainclim))
  CALL check(NF90_CLOSE(ncid_temp))
  CALL check(NF90_CLOSE(ncid_maxtemp))
  CALL check(NF90_CLOSE(ncid_mintemp))
  CALL check(NF90_CLOSE(ncid_rh))
  CALL check(NF90_CLOSE(ncid_maxrh))
  CALL check(NF90_CLOSE(ncid_minrh))
  CALL check(NF90_CLOSE(ncid_cc))
  CALL check(NF90_CLOSE(ncid_wspeed))
  CALL check(NF90_CLOSE(ncid_snow))
  CALL check(NF90_CLOSE(ncid_dp))
  CALL check(NF90_CLOSE(ncid_vs))
!  CALL check(NF90_CLOSE(ncid_lal))
  CALL check(NF90_CLOSE(ncid_cv))
  CALL check(NF90_CLOSE(ncid_cr))
  CALL check(NF90_CLOSE(ncid_fm))
  CALL check(NF90_CLOSE(ncid_slope))

  print *,'input/output closed '

 

  RETURN
END SUBROUTINE setdown
