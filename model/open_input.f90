SUBROUTINE open_input
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! opens the input  files 
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
  INTEGER :: nlatcheck, nloncheck, ndaycheck
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
    CALL check(NF90_INQ_DIMID(ncid_lsm, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_lsm, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LonDimID, len = nloncheck))
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, rainfile
      STOP 1
    ENDIF  
    ! here we check that the rherature file has the same number of days as the rain file
     ! data for 
    CALL check(NF90_INQ_VARID(ncid_lsm, clsmvar, VarId_lsm))
    CALL check(NF90_GET_VAR(ncid_lsm, VarId_lsm, rlsm ))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_lsm, VarId_lsm, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
   
    ! IF (latscheck(nlat)<latscheck(1)) llsm_latreverse=.true. 
   ! IF (llsm_latreverse) rlsm=rlsm(:,nlat:1:-1)

    !---------
    ! rainfall
    !---------
    print *,'opening rainfall ',input//rainfile
    CALL check(NF90_OPEN(path=input//rainfile,mode=nf90_nowrite,ncid=ncid_rain))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_rain, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rain, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_rain, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rain, LonDimID, len = nloncheck))
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, rainfile
      STOP 1
    ENDIF

    !----------------------------
    ! read in number of timesteps
    !----------------------------
    CALL check(NF90_INQ_DIMID(ncid_rain, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(ncid_rain, timeDimID, len = nday))

    ! id for rainfall 
    CALL check(NF90_INQ_VARID(ncid_rain, crainvar, VarId_rain))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_rain, VarId_rain, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ALLOCATE(latscheck(nlat))

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_rain,LatDimID,latscheck ))
    ! ! IF (latscheck(nlat)<latscheck(1)) lrain_latreverse=.true. 


    !------------
    ! temperature
    !------------
    print *,'opening temperature ',input//tempfile
    CALL check(NF90_OPEN(path=input//tempfile,mode=nf90_nowrite,ncid=ncid_temp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_temp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_temp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_temp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_temp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_temp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_temp, timeDimID, len = ndaycheck))

    ! here we check that the temperature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck ) THEN 
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, tempfile
      STOP 1
    ENDIF

    ! id for temperature
    CALL check(NF90_INQ_VARID(ncid_temp, ctempvar, VarId_temp))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_temp, VarId_temp, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_temp,LatDimID,latscheck ))
 !   ! IF (latscheck(nlat)<latscheck(1)) ltemp_latreverse=.true.
    

    
    !------------
    ! temperature maximum
    !------------
    print *,'opening max temperature ',input//maxtempfile
    CALL check(NF90_OPEN(path=input//maxtempfile,mode=nf90_nowrite,ncid=ncid_maxtemp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_maxtemp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxtemp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_maxtemp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxtemp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_maxtemp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxtemp, timeDimID, len = ndaycheck))

    ! here we check that the temperature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck) THEN 
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, maxtempfile
      STOP 1
    ENDIF

    ! ids for maximum temperature
    CALL check(NF90_INQ_VARID(ncid_maxtemp, cmaxtempvar, VarId_maxtemp))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_maxtemp, VarId_maxtemp, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_maxtemp,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lmaxtemp_latreverse=.true.  


    !------------
    ! temperature minimum
    !------------
    print *,'opening minimum temperature ',input//mintempfile
    CALL check(NF90_OPEN(path=input//mintempfile,mode=nf90_nowrite,ncid=ncid_mintemp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_mintemp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_mintemp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_mintemp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_mintemp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_mintemp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_mintemp, timeDimID, len = ndaycheck))

    ! here we check that the temperature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck)  THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, mintempfile
      STOP 1
    ENDIF

    ! ids for minimum temperature
    CALL check(NF90_INQ_VARID(ncid_mintemp, cmintempvar, VarId_mintemp))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_mintemp, VarId_mintemp, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_mintemp,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lmintemp_latreverse=.true.  

    !------------
    ! relative humidity
    !------------
    print *,'opening relative humidity ',input//rhfile
    CALL check(NF90_OPEN(path=input//rhfile,mode=nf90_nowrite,ncid=ncid_rh))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_rh, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rh, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_rh, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rh, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_rh, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rh, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, rhfile
      STOP 1
    ENDIF

    ! id  for rh
    CALL check(NF90_INQ_VARID(ncid_rh, crhvar, VarId_rh))
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_rh, VarId_rh, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_rh,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lrh_latreverse=.true. 

    !------------
    ! max relative humidity
    !------------
    print *,'opening relative humidity ',input//maxrhfile
    CALL check(NF90_OPEN(path=input//maxrhfile,mode=nf90_nowrite,ncid=ncid_maxrh))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_maxrh, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxrh, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_maxrh, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxrh, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_maxrh, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxrh, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, maxrhfile
      STOP 1
    ENDIF

    ! data for rh
    CALL check(NF90_INQ_VARID(ncid_maxrh, cmaxrhvar, VarId_maxrh))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_maxrh, VarId_maxrh, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_maxrh,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lmaxrh_latreverse=.true. 

    !------------
    ! min relative humidity
    !------------
    print *,'opening min relative humidity ',input//minrhfile
    CALL check(NF90_OPEN(path=input//minrhfile,mode=nf90_nowrite,ncid=ncid_minrh))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_minrh, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_minrh, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_minrh, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_minrh, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_minrh, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_minrh, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, minrhfile
      STOP 1
    ENDIF

    ! data for 
    CALL check(NF90_INQ_VARID(ncid_minrh, cminrhvar, VarId_minrh))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_minrh, VarId_minrh, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_minrh,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lminrh_latreverse=.true.  !rrh=rrh(:,nlat:1:-1,:)

    !------------
    ! Cloud cover 
    !------------
    print *,'opening cloud cover file ',input//ccfile
    CALL check(NF90_OPEN(path=input//ccfile,mode=nf90_nowrite,ncid=ncid_cc))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_cc, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cc, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_cc, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cc, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_cc, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cc, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, ccfile
      STOP 1
    ENDIF

    ! data for 
    CALL check(NF90_INQ_VARID(ncid_cc, cccvar, VarId_cc))
  
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_cc, VarId_cc, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_cc,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lcc_latreverse=.true. 

    !------------
    ! Wind speed 
    !------------
    print *,'opening wind speed file ',input//wspeedfile
    CALL check(NF90_OPEN(path=input//wspeedfile,mode=nf90_nowrite,ncid=ncid_wspeed))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_wspeed, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_wspeed, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_wspeed, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_wspeed, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_wspeed, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_wspeed, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, wspeedfile
      STOP 1
    ENDIF

    ! id  for weend speed 
    CALL check(NF90_INQ_VARID(ncid_wspeed, cwspeedvar, VarId_wspeed))
  
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_wspeed, VarId_wspeed, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_wspeed,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lwspeed_latreverse=.true.  
    !------------
    ! snow cover
    !------------
    print *,'opening snowcover file ',input//snowfile
    CALL check(NF90_OPEN(path=input//snowfile,mode=nf90_nowrite,ncid=ncid_snow))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_snow, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_snow, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_snow, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_snow, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_snow, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_snow, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck)  THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, snowfile
      STOP 1
    ENDIF

    ! data for 
    CALL check(NF90_INQ_VARID(ncid_snow, csnowvar, VarId_snow))
    
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_snow, VarId_snow, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_snow,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lsnow_latreverse=.true.  
    !------------
    ! rain duration 
    !------------
    print *,'opening rain duration (DP) file ',input//dpfile
    CALL check(NF90_OPEN(path=input//dpfile,mode=nf90_nowrite,ncid=ncid_dp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_dp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_dp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_dp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_dp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_dp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_dp, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck)  THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, dpfile
      STOP 1
    ENDIF

    ! data for 
    CALL check(NF90_INQ_VARID(ncid_dp, cdpvar, VarId_dp))
    
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_dp, VarId_dp, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_dp,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) ldp_latreverse=.true.  
 
    !------------
    ! vegetation stages summary file 
    !------------
    print *,'opening Vegetation stage  file (vegstage) file ',input//vsfile
    CALL check(NF90_OPEN(path=input//vsfile,mode=nf90_nowrite,ncid=ncid_vs))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_vs, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_vs, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_vs, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_vs, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_vs, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_vs, timeDimID, len = ndaycheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   nday.ne.ndaycheck)  THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, vsfile
      STOP 1
    ENDIF

    ! data for 
    CALL check(NF90_INQ_VARID(ncid_vs, cvsvar, VarId_vs))
    
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_vs, VarId_vs, "_FillValue", ivar_FillValue)
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_vs,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lvs_latreverse=.true.  


!    !------------
!    ! lightning activity file 
!    !------------
!    print *,'opening Lightning activity   file ',input//lalfile
!    CALL check(NF90_OPEN(path=input//lalfile,mode=nf90_nowrite,ncid=ncid_lal))
!
!    ! dimensions to make sure input file is correct size.
!    CALL check(NF90_INQ_DIMID(ncid_lal, lat_name, LatDimID))
!    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lal, LatDimID, len = nlatcheck))
!    CALL check(NF90_INQ_DIMID(ncid_lal, lon_name, LonDimID))
!    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lal, LonDimID, len = nloncheck))
!    CALL check(NF90_INQ_DIMID(ncid_lal, time_name, timeDimID))
!    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lal, timeDimID, len = ndaycheck))
!
!    ! here we check that the rherature file has the same number of days as the rain file
!    IF (nlon.ne.nloncheck .or. &
!    &   nlat.ne.nlatcheck .or. &
!    &   nday.ne.ndaycheck)  THEN
!      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,nday,ndaycheck, lalfile
!      STOP 1
!    ENDIF
!
!    ! data for 
!    CALL check(NF90_INQ_VARID(ncid_lal, clalvar, VarId_lal))
!    
!    ! if attribute not there then default to fillvalue
!    istatus=NF90_GET_ATT(ncid_lal, VarId_lal, "_FillValue", ivar_FillValue)
!    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue
!
!    ! check the order of latitude and reverse if necessary
!    CALL check(NF90_GET_VAR(ncid_lal,LatDimID,latscheck ))
!    ! IF (latscheck(nlat)<latscheck(1)) llal_latreverse=.true.  
!   
!   
!    !------------
    ! constant fields no time dependency 
    !------------
   !---------
    ! climate rainfall
    !---------
    print *,'opening  climate rainfall ',input//rainclimfile
    CALL check(NF90_OPEN(path=input//rainclimfile,mode=nf90_nowrite,ncid=ncid_rainclim))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_rainclim, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rainclim, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_rainclim, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rainclim, LonDimID, len = nloncheck))
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, rainclimfile
      STOP 1
    ENDIF
   ! data for 
    CALL check(NF90_INQ_VARID(ncid_rainclim, crainclimvar, VarId_rainclim))
    CALL check(NF90_GET_VAR(ncid_rainclim, VarId_rainclim, rrainclim ))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_rainclim, VarId_rainclim, "_FillValue", rvar_FillValue)
    IF (istatus /= nf90_noerr) rvar_FillValue=rfillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_rainclim,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lrainclim_latreverse=.true. 
    IF (llsm_latreverse) rrainclim=rrainclim(:,nlat:1:-1)
      
  
   
    !------------
    ! climate regions
    !------------
    print *,'opening climatic regions file ',input//crfile
    CALL check(NF90_OPEN(path=input//crfile,mode=nf90_nowrite,ncid=ncid_cr))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_cr, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cr, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_cr, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cr, LonDimID, len = nloncheck))
 
    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN 
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, crfile
      STOP 1
    ENDIF

    ! data for climate regions
    
    CALL check(NF90_INQ_VARID(ncid_cr, ccrvar, VarId_cr))
    CALL check(NF90_GET_VAR(ncid_cr, VarId_cr, icr ))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_cr, VarId_cr, "_FillValue", ivar_FillValue)
    print*,"ivar_FillValue",ivar_FillValue
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_cr,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lcr_latreverse=.true.  
    IF (lcr_latreverse) icr=icr(:,nlat:1:-1)

    !------------
    ! fuel-model map     
    !------------
    print *,'opening fuel-model maps ',input//fmfile
    CALL check(NF90_OPEN(path=input//fmfile,mode=nf90_nowrite,ncid=ncid_fm))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_fm, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_fm, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_fm, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_fm, LonDimID, len = nloncheck))
 
    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN 
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, fmfile
      STOP 1
    ENDIF

    ! data for climate regions
    
    CALL check(NF90_INQ_VARID(ncid_fm, cfmvar, VarId_fm))
    CALL check(NF90_GET_VAR(ncid_fm, VarId_fm, ifm ))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_fm, VarId_fm, "_FillValue", ivar_FillValue)
    print*,"ivar_FillValue",ivar_FillValue
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_fm,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lfm_latreverse=.true.  
    IF (lfm_latreverse) ifm=ifm(:,nlat:1:-1)
  
    !------------
    ! Vegetation cover (high +low)
    !------------
    print *,'opening  vegetation cover  ',input//cvfile
    CALL check(NF90_OPEN(path=input//cvfile,mode=nf90_nowrite,ncid=ncid_cv))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_cv, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cv, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_cv, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cv, LonDimID, len = nloncheck))
 
    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN 
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, cvfile
      STOP 1
    ENDIF

    ! data id
    CALL check(NF90_INQ_VARID(ncid_cv, ccvvar, VarId_cv))
    CALL check(NF90_GET_VAR(ncid_cv, VarId_cv, rcv ))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_cv, VarId_cv, "_FillValue", ivar_FillValue)
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_cv,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lcv_latreverse=.true.  
    IF (lcv_latreverse) rcv=rcv(:,nlat:1:-1)


 
    !------------
    !Slope due to subgridscale orography
    !------------
    print *,'opening slope of subgridscale orography  ',input//slopefile
    CALL check(NF90_OPEN(path=input//slopefile,mode=nf90_nowrite,ncid=ncid_slope))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_slope, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_slope, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_slope, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_slope, LonDimID, len = nloncheck))
 
    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN 
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, slopefile
      STOP 1
    ENDIF

    ! data id
    CALL check(NF90_INQ_VARID(ncid_slope, cslopevar, VarId_slope))
    CALL check(NF90_GET_VAR(ncid_slope, VarId_slope, islope ))
 
    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_slope, VarId_slope, "_FillValue", ivar_FillValue)
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_slope,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lslope_latreverse=.true.  
    IF (lslope_latreverse) islope=islope(:,nlat:1:-1)


    DEALLOCATE(latscheck)
    ! allocate the fields for dates (has to be after above, since nday read from rainfile
    ALLOCATE(ndate(nday)) 
    nrun=nday/dt !
    ndaydiag=1
    DO i=1,nday    
      ndate(i)=i-1
    ENDDO
    nstep=nday/dt ! length of run in timesteps


END SUBROUTINE open_input
