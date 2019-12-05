SUBROUTINE set_grid
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!set the calculation grid by reading the land sea mask
!
!---------------------------------------------------------

  USE netcdf
  USE mo_control
  USE mo_constants
  USE mo_fire
  USE mo_namelist
  USE mo_ncdf_tools

  IMPLICIT NONE

  ! local variables
  INTEGER :: istatus,i
  
  ! netcdf vars - only needed locally
  INTEGER :: nlatcheck, nloncheck
  REAL, ALLOCATABLE :: latscheck(:)
!---------------------------------------------------------------------------
!  meteorological  variables read in from analysis, obs or forecast
!---------------------------------------------------------------------------
 
   !------------
    !Land sea mask
    !------------

    print *,'opening Land sea-mask file ',input//lsmfile
    CALL check(NF90_OPEN(path=input//lsmfile,mode=nf90_nowrite,ncid=ncid_lsm))

    ! dimensions are taken from the LSM
    CALL check(NF90_INQ_DIMID(ncid_lsm, "lat", LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_lsm, "lon", LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LonDimID, len = nloncheck))
   
    ! here we check that the rh and temperature files have the same number of  as the rain file
    nlon=nloncheck
    nlat=nlatcheck 

    WRITE(iounit,*) '*** data dimensions  *** ',nlon,nlat
    
    ALLOCATE(lons(nlon))
    ALLOCATE(lats(nlat))

    CALL check(NF90_GET_VAR(ncid_lsm,LatDimID,lats ))
    CALL check(NF90_GET_VAR(ncid_lsm,LonDimID,lons ))
    WRITE(iounit,*) '*** grid points   *** ',lats(:),lons(:)
 

END SUBROUTINE set_grid
