! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief NetCDF I/O
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_ncdf_tools

  USE netcdf
  USE mo_nfdrs
  USE mo_mark5
  USE mo_fwi

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: ncdf_getdata
  PUBLIC :: ncdf_initialize
  PUBLIC :: ncdf_open_input
  PUBLIC :: ncdf_open_output
  PUBLIC :: ncdf_set_grid
  PUBLIC :: ncdf_setdown
  PUBLIC :: ncdf_write_constant_fields
  PUBLIC :: ncdf_write_restart
  PUBLIC :: ncdf_write_results

CONTAINS

SUBROUTINE check(status)

  INTEGER, INTENT(in) :: status

  IF (status /= nf90_noerr) THEN
    PRINT *,TRIM(NF90_STRERROR(status))
    STOP 1
  END IF
END SUBROUTINE check


SUBROUTINE define_output(ncidx,ivarid,name,title,iunits,type,ndiag,idarray)
  INTEGER, INTENT(in) :: ncidx,idarray(:) ! flexible vector to set coordinate dimensions
  INTEGER, INTENT(inout) :: ivarid(2), ndiag
  CHARACTER (len=*), INTENT(IN) :: name, title, iunits,type
  SELECT CASE (type)

  CASE ("FLOAT")
     CALL check(NF90_DEF_VAR(ncidx, name, nf90_FLOAT, idarray , iVarID(1)))
     CALL check(NF90_PUT_ATT(ncidx, iVarID(1), "title", title) )
     CALL check(NF90_PUT_ATT(ncidx, iVarID(1), "units", iunits) )
     CALL check(NF90_PUT_ATT(ncidx, iVarID(1), "_FillValue", rfillvalue) )
     ndiag=ndiag+1   ! increase the diagnostic counter
     iVarid(2)=ndiag
  CASE ("INTEGER")
     CALL check(NF90_DEF_VAR(ncidx, name, nf90_INT, idarray , iVarID(1)))
     CALL check(NF90_PUT_ATT(ncidx, iVarID(1), "title", title) )
     CALL check(NF90_PUT_ATT(ncidx, iVarID(1), "units", iunits) )
     CALL check(NF90_PUT_ATT(ncidx, iVarID(1), "_FillValue", ifillvalue) )
     ndiag=ndiag+1   ! increase the diagnostic counter
     iVarid(2)=ndiag
  END SELECT


END SUBROUTINE define_output

SUBROUTINE ncdf_write_restart
  INTEGER :: latvarid, lonvarid, timevarid, unlimdimid
  INTEGER :: i,j
  REAL, DIMENSION(nlon,nlat)::  meanrbndryt
  INTEGER, PARAMETER :: ncd_ps=2

  INTEGER :: ncd_mc1(ncd_ps)
  INTEGER :: ncd_mc10(ncd_ps)
  INTEGER :: ncd_mc100(ncd_ps)
  INTEGER :: ncd_mc1000(ncd_ps)
  INTEGER :: ncd_x1000(ncd_ps)
  INTEGER :: ncd_rbndryt(ncd_ps)
  INTEGER :: ncd_mcherb(ncd_ps)
  INTEGER :: ncd_mcwood(ncd_ps)
  INTEGER :: ncd_fwi_fwi(ncd_ps)
  INTEGER :: ncd_fwi_ffmc(ncd_ps)
  INTEGER :: ncd_fwi_dmc(ncd_ps)
  INTEGER :: ncd_fwi_dc(ncd_ps)
  INTEGER :: ncd_mark5_kb(ncd_ps)
  INTEGER :: ncd_mark5_tsr(ncd_ps)



  PRINT *,'- now dump restart'

  ! dump restart file:

  CALL check(NF90_CREATE(path = input//"restart_geff.nc", cmode=or(nf90_clobber,nf90_64bit_offset), ncid = ncidrest))


! define the dimensions
  CALL check(NF90_DEF_DIM(ncidrest, lon_name, nlon, LonDimID))


  CALL check(NF90_DEF_DIM(ncidrest, lat_name, nlat, LatDimID))

  ! Define the coordinate variables. They will hold the coordinate
  ! information, that is, the latitudes and longitudes. A varid is
 ! returned for each.
  CALL check(nf90_def_var(ncidrest, lon_name, NF90_REAL, londimid, lonvarid) )
  CALL check(nf90_def_var(ncidrest, lat_name, NF90_REAL, latdimid, latvarid) )

  ! Assign units attributes to coordinate var data. This attaches a
  ! text attribute to each of the coordinate variables, containing the
  ! units.
  CALL check( nf90_put_att(ncidrest, latvarid, units, lat_units) )
  CALL check( nf90_put_att(ncidrest, lonvarid, units, lon_units) )

  ! define global attributes
  CALL check( NF90_PUT_ATT(ncidrest, NF90_GLOBAL, "rundate", now))
  CALL check( NF90_PUT_ATT(ncidrest, NF90_GLOBAL, "geff", version))
  CALL check( NF90_PUT_ATT(ncidrest, NF90_GLOBAL, "comments","geff  restart file"))
  CALL check( NF90_PUT_ATT(ncidrest, NF90_GLOBAL, "refdate", time_units))
  CALL check( NF90_PUT_ATT(ncidrest, NF90_GLOBAL, "nday after starting date",restart_day))

  !---------------------------------------------
  ! define the output structures
  ! routine also sets up the diagnostics indices
  ! reuse the nc 2 byte arrays for indices
  !---------------------------------------------
  !


 ndiag2d=0 ! dummy variable

   CALL define_output(ncidrest,ncd_mc1,"mc1" , &
        & "Moisture Content 1h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )


   CALL define_output(ncidrest,ncd_mc10,"mc10" , &
        & "Moisture Content 10h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_output(ncidrest,ncd_mc100,"mc100" , &
        & "Moisture Content 100h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_output(ncidrest,ncd_mc1000,"mc1000" , &
        & "Moisture Content 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_output(ncidrest,ncd_rbndryt,"rbndryt" , &
        & "Mean of moisture Content 1000h- previous 6 days","frac", "FLOAT",ndiag2d, (/LonDimId, LatDimID /) )


   CALL define_output(ncidrest,ncd_x1000,"x1000" , &
        & "Moisture Content correction of herbaceous 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_output(ncidrest,ncd_mcherb,"mcherb" , &
        & "Moisture Content herbaceous plant","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_output(ncidrest,ncd_mcwood,"mcwood" , &
        & "Moisture Content shrub(wood) ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )



  CALL define_output(ncidrest,ncd_fwi_ffmc,"ffmc", &
      & "Fine Fuel Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
  CALL define_output(ncidrest,ncd_fwi_dmc,"dmc", &
      & "Duff Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
  CALL define_output(ncidrest,ncd_fwi_dc,"dc", &
      & "Drought Code","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )
  CALL define_output(ncidrest,ncd_mark5_kb,"kbdi" , &
         & "keetch-Byram drought index","inch", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )


   CALL define_output(ncidrest,ncd_mark5_tsr,"mark5_timesincerain", &
        & "Time since rain","number", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )

  CALL check(NF90_ENDDEF(ncidrest))




  PRINT *,'writing vars'
  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  CALL check( nf90_put_var(ncidrest, latvarid, lats) )
  CALL check( nf90_put_var(ncidrest, lonvarid, lons) )

  PRINT *,"NFDRS"
 PRINT *,"======"

  PRINT *,"mc1"
  CALL check( nf90_put_var(ncidrest, ncd_mc1(1)   , mc%r1hr ))
  PRINT *,"mc10"
  CALL check( nf90_put_var(ncidrest, ncd_mc10(1)  , mc%r10hr ))
  PRINT *,"mc100"
  CALL check( nf90_put_var(ncidrest, ncd_mc100(1) , mc%r100hr  ))
  PRINT *,"mc1000"
  CALL check( nf90_put_var(ncidrest, ncd_mc1000(1) , mc%r1000hr  ))
  PRINT *,"mc-herb"
  CALL check( nf90_put_var(ncidrest, ncd_mcherb(1)  , mc%rherb  ))
  PRINT *,"mc-wood"
  CALL check( nf90_put_var(ncidrest, ncd_mcwood(1)  , mc%rwood  ))
  PRINT *,"x1000"
  CALL check( nf90_put_var(ncidrest, ncd_x1000(1)  , mc%rx1000  ))
  PRINT *,"meanrbndryt"
  CALL check( nf90_put_var(ncidrest, ncd_rbndryt(1)  , mc%rbndryt ))

  PRINT *,"MARK-5"
  PRINT *,"======"
  PRINT *,"kbdi"
  CALL check( nf90_put_var(ncidrest, ncd_mark5_kb(1)  , mark5_fuel%kb_drought_index  ))
  PRINT *,"timesincerain"
  CALL check( nf90_put_var(ncidrest, ncd_mark5_tsr(1)  , mark5_fuel%timesincerain  ))

  PRINT *,"FWI"
  PRINT *,"======"
  PRINT *,"ffmc"
  CALL check( nf90_put_var(ncidrest, ncd_fwi_ffmc(1), fwi_risk%ffmc ))
  PRINT *,"dmc"
  CALL check( nf90_put_var(ncidrest, ncd_fwi_dmc(1), fwi_risk%dmc ))
  PRINT *,"dc"
  CALL check( nf90_put_var(ncidrest, ncd_fwi_dc(1), fwi_risk%dc ))
  CALL check( nf90_close(ncidrest))

  RETURN
END SUBROUTINE ncdf_write_restart


SUBROUTINE ncdf_open_input
  INTEGER :: istatus,i

  ! netcdf vars - only needed locally
  INTEGER :: nlatcheck, nloncheck, ntimestepcheck
  REAL, ALLOCATABLE :: latscheck(:)
!---------------------------------------------------------------------------
!  meteorological  variables read in from analysis, obs or forecast
!---------------------------------------------------------------------------

   !------------
    !Land sea mask
    !------------

    PRINT *,'opening Land sea-mask file ',input//lsmfile
    CALL check(NF90_OPEN(path=input//lsmfile,mode=nf90_nowrite,ncid=ncid_lsm))

    ! dimensions are taken from the LSM
    CALL check(NF90_INQ_DIMID(ncid_lsm, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_lsm, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LonDimID, len = nloncheck))
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, rainfile
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
    PRINT *,'opening rainfall ',input//rainfile
    CALL check(NF90_OPEN(path=input//rainfile,mode=nf90_nowrite,ncid=ncid_rain))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_rain, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rain, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_rain, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rain, LonDimID, len = nloncheck))
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, rainfile
      STOP 1
    ENDIF

    !----------------------------
    ! read in number of timesteps
    !----------------------------
    CALL check(NF90_INQ_DIMID(ncid_rain, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(ncid_rain, timeDimID, len = ntimestep))

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
    PRINT *,'opening temperature ',input//tempfile
    CALL check(NF90_OPEN(path=input//tempfile,mode=nf90_nowrite,ncid=ncid_temp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_temp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_temp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_temp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_temp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_temp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_temp, timeDimID, len = ntimestepcheck))

    ! here we check that the temperature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck ) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, tempfile
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
    PRINT *,'opening max temperature ',input//maxtempfile
    CALL check(NF90_OPEN(path=input//maxtempfile,mode=nf90_nowrite,ncid=ncid_maxtemp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_maxtemp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxtemp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_maxtemp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxtemp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_maxtemp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxtemp, timeDimID, len = ntimestepcheck))

    ! here we check that the temperature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, maxtempfile
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
    PRINT *,'opening minimum temperature ',input//mintempfile
    CALL check(NF90_OPEN(path=input//mintempfile,mode=nf90_nowrite,ncid=ncid_mintemp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_mintemp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_mintemp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_mintemp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_mintemp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_mintemp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_mintemp, timeDimID, len = ntimestepcheck))

    ! here we check that the temperature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck)  THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, mintempfile
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
    PRINT *,'opening relative humidity ',input//rhfile
    CALL check(NF90_OPEN(path=input//rhfile,mode=nf90_nowrite,ncid=ncid_rh))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_rh, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rh, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_rh, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rh, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_rh, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rh, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, rhfile
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
    PRINT *,'opening relative humidity ',input//maxrhfile
    CALL check(NF90_OPEN(path=input//maxrhfile,mode=nf90_nowrite,ncid=ncid_maxrh))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_maxrh, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxrh, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_maxrh, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxrh, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_maxrh, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_maxrh, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, maxrhfile
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
    PRINT *,'opening min relative humidity ',input//minrhfile
    CALL check(NF90_OPEN(path=input//minrhfile,mode=nf90_nowrite,ncid=ncid_minrh))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_minrh, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_minrh, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_minrh, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_minrh, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_minrh, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_minrh, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, minrhfile
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
    PRINT *,'opening cloud cover file ',input//ccfile
    CALL check(NF90_OPEN(path=input//ccfile,mode=nf90_nowrite,ncid=ncid_cc))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_cc, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cc, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_cc, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cc, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_cc, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cc, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, ccfile
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
    PRINT *,'opening wind speed file ',input//wspeedfile
    CALL check(NF90_OPEN(path=input//wspeedfile,mode=nf90_nowrite,ncid=ncid_wspeed))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_wspeed, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_wspeed, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_wspeed, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_wspeed, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_wspeed, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_wspeed, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, wspeedfile
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
    PRINT *,'opening snowcover file ',input//snowfile
    CALL check(NF90_OPEN(path=input//snowfile,mode=nf90_nowrite,ncid=ncid_snow))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_snow, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_snow, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_snow, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_snow, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_snow, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_snow, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck)  THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, snowfile
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
    PRINT *,'opening rain duration (DP) file ',input//dpfile
    CALL check(NF90_OPEN(path=input//dpfile,mode=nf90_nowrite,ncid=ncid_dp))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_dp, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_dp, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_dp, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_dp, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_dp, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_dp, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck)  THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, dpfile
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
    PRINT *,'opening Vegetation stage  file (vegstage) file ',input//vsfile
    CALL check(NF90_OPEN(path=input//vsfile,mode=nf90_nowrite,ncid=ncid_vs))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_vs, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_vs, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_vs, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_vs, LonDimID, len = nloncheck))
    CALL check(NF90_INQ_DIMID(ncid_vs, time_name, timeDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_vs, timeDimID, len = ntimestepcheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck .or. &
    &   ntimestep.ne.ntimestepcheck)  THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, vsfile
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
!    PRINT *,'opening Lightning activity   file ',input//lalfile
!    CALL check(NF90_OPEN(path=input//lalfile,mode=nf90_nowrite,ncid=ncid_lal))
!
!    ! dimensions to make sure input file is correct size.
!    CALL check(NF90_INQ_DIMID(ncid_lal, lat_name, LatDimID))
!    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lal, LatDimID, len = nlatcheck))
!    CALL check(NF90_INQ_DIMID(ncid_lal, lon_name, LonDimID))
!    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lal, LonDimID, len = nloncheck))
!    CALL check(NF90_INQ_DIMID(ncid_lal, time_name, timeDimID))
!    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lal, timeDimID, len = ntimestepcheck))
!
!    ! here we check that the rherature file has the same number of days as the rain file
!    IF (nlon.ne.nloncheck .or. &
!    &   nlat.ne.nlatcheck .or. &
!    &   ntimestep.ne.ntimestepcheck)  THEN
!      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,ntimestep,ntimestepcheck, lalfile
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
    PRINT *,'opening  climate rainfall ',input//rainclimfile
    CALL check(NF90_OPEN(path=input//rainclimfile,mode=nf90_nowrite,ncid=ncid_rainclim))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_rainclim, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rainclim, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_rainclim, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_rainclim, LonDimID, len = nloncheck))
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, rainclimfile
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
    PRINT *,'opening climatic regions file ',input//crfile
    CALL check(NF90_OPEN(path=input//crfile,mode=nf90_nowrite,ncid=ncid_cr))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_cr, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cr, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_cr, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cr, LonDimID, len = nloncheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, crfile
      STOP 1
    ENDIF

    ! data for climate regions

    CALL check(NF90_INQ_VARID(ncid_cr, ccrvar, VarId_cr))
    CALL check(NF90_GET_VAR(ncid_cr, VarId_cr, icr ))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_cr, VarId_cr, "_FillValue", ivar_FillValue)
    PRINT *,"ivar_FillValue",ivar_FillValue
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_cr,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lcr_latreverse=.true.
    IF (lcr_latreverse) icr=icr(:,nlat:1:-1)

    !------------
    ! fuel-model map
    !------------
    PRINT *,'opening fuel-model maps ',input//fmfile
    CALL check(NF90_OPEN(path=input//fmfile,mode=nf90_nowrite,ncid=ncid_fm))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_fm, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_fm, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_fm, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_fm, LonDimID, len = nloncheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, fmfile
      STOP 1
    ENDIF

    ! data for climate regions

    CALL check(NF90_INQ_VARID(ncid_fm, cfmvar, VarId_fm))
    CALL check(NF90_GET_VAR(ncid_fm, VarId_fm, ifm ))

    ! if attribute not there then default to fillvalue
    istatus=NF90_GET_ATT(ncid_fm, VarId_fm, "_FillValue", ivar_FillValue)
    PRINT *,"ivar_FillValue",ivar_FillValue
    IF (istatus /= nf90_noerr) ivar_FillValue=ifillvalue

    ! check the order of latitude and reverse if necessary
    CALL check(NF90_GET_VAR(ncid_fm,LatDimID,latscheck ))
    ! IF (latscheck(nlat)<latscheck(1)) lfm_latreverse=.true.
    IF (lfm_latreverse) ifm=ifm(:,nlat:1:-1)

    !------------
    ! Vegetation cover (high +low)
    !------------
    PRINT *,'opening  vegetation cover  ',input//cvfile
    CALL check(NF90_OPEN(path=input//cvfile,mode=nf90_nowrite,ncid=ncid_cv))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_cv, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cv, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_cv, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_cv, LonDimID, len = nloncheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, cvfile
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
    PRINT *,'opening slope of subgridscale orography  ',input//slopefile
    CALL check(NF90_OPEN(path=input//slopefile,mode=nf90_nowrite,ncid=ncid_slope))

    ! dimensions to make sure input file is correct size.
    CALL check(NF90_INQ_DIMID(ncid_slope, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_slope, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_slope, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_slope, LonDimID, len = nloncheck))

    ! here we check that the rherature file has the same number of days as the rain file
    IF (nlon.ne.nloncheck .or. &
    &   nlat.ne.nlatcheck ) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck, slopefile
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
    ! allocate the fields for dates (has to be after above, since ntimestep read from rainfile
    ALLOCATE(nhours(ntimestep))
    ndaydiag=1
    DO i=1,ntimestep
      nhours(i)=(i-1)*dt
    ENDDO
END SUBROUTINE ncdf_open_input


SUBROUTINE ncdf_open_output

  ! netcdf vars - only needed locally
  INTEGER :: latvarid, lonvarid, timevarid

  ! output file details
  CHARACTER (len = 8)  :: str_date
  CHARACTER (len = 4)  :: str_time
! need to make this more flexible for 1,3 or 4 dimensions
!  REAL, ALLOCATABLE :: zdummy2d(:,:),zdummy3d(:,:,:),zdummy4d(:,:,:,:),time(:)

!
  !---------------------------
  ! OUTPUT NETCDF FILE
  !---------------------------
  WRITE(str_date,'(I8)') inidate
  WRITE(str_time,'(I4)') initime
  time_units="hours since "//str_date(1:4)//"-"//str_date(5:6)//"-"//str_date(7:8)//" "//str_time(1:2)//":"//str_time(3:4)//" UTC"
!===========================================================

  CALL check(NF90_CREATE(path = output_file, cmode=NF90_HDF5, ncid = ncidout))

! define the dimensions
  CALL check(NF90_DEF_DIM(ncidout, lon_name, nlon, LonDimID))
  CALL check(NF90_DEF_DIM(ncidout, lat_name, nlat, LatDimID))
  CALL check(NF90_DEF_DIM(ncidout, time_name, nf90_unlimited, timeDimID))

  ! Define the coordinate variables. They will hold the coordinate
  ! information, that is, the latitudes and longitudes. A varid is
  ! returned for each.
  CALL check(nf90_def_var(ncidout, lon_name, NF90_REAL, londimid, lonvarid) )
  CALL check(nf90_def_var(ncidout, lat_name, NF90_REAL, latdimid, latvarid) )
  CALL check(nf90_def_var(ncidout, time_name, NF90_REAL, timedimid, timevarid) )

  ! Assign units attributes to coordinate var data. This attaches a
  ! text attribute to each of the coordinate variables, containing the
  ! units.
  CALL check( nf90_put_att(ncidout, latvarid, units, lat_units) )
  CALL check( nf90_put_att(ncidout, lonvarid, units, lon_units) )
  CALL check( nf90_put_att(ncidout, timevarid, units, time_units) )

  ! define global attributes
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "Reference date", now))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "ECMWF fire model", version))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "License", "Copernicus"))


  !---------------------------------------------
  ! *** define the 3d structures
  ! routine also sets up the diagnostics indices
  !---------------------------------------------
  !                             index      short name title       units
  ndiag2d=0


  IF(lnc_rain) &
       & CALL define_output(ncidout,ncvar_rain,"tp",&
       "Rainfall","mm/day", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )


  IF(lnc_temp) &
       & CALL define_output(ncidout,ncvar_temp,"t2",&
       &"Temperature","degrees K","FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  IF(lnc_maxtemp) &
       & CALL define_output(ncidout,ncvar_maxtemp,"maxt2" , &
       & "Maximum daily temperature","degrees K", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  IF(lnc_mintemp) &
       & CALL define_output(ncidout,ncvar_mintemp,"mint2" , &
       & "Minimum daily temperature","degrees K", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  IF(lnc_rh) &
       & CALL define_output(ncidout,ncvar_rh,"rh" , &
       & "Relative Humidity","%",  "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  IF(lnc_maxrh) &
       & CALL define_output(ncidout,ncvar_maxrh,"maxrh" , &
       & "Maximum daily Relative Humidity","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_minrh) &
       & CALL define_output(ncidout,ncvar_minrh,"minrh" , &
       & "Minimum daily Relative Humidity","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_cc) &
       & CALL define_output(ncidout,ncvar_cc,"cc" , &
       & "Cloud Cover ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_snow) &
       & CALL define_output(ncidout,ncvar_snow,"snow" , &
       & "Ground Snow ","mask","FLOAT", ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  IF(lnc_dp) &
       & CALL define_output(ncidout,ncvar_dp,"dp" , &
       & "Duration of precipitation in the prev 24hr","hr", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_vs) &
       & CALL define_output(ncidout,ncvar_vs,"vs" , &
       & "Vegetation stage 1-pregreen,2-green-up,3-green,4-transition,5-cured/frozen",&
       &"--", "INTEGER",ndiag2d,(/LonDimId, LatDimID, timeDimID /) )

! IF(lnc_lal) &
!       & CALL define_output(ncidout,ncvar_lal,"lal" , &
!       & "Lightning Activity Level","--", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
!

  IF(lnc_wspeed) &
       & CALL define_output(ncidout,ncvar_wspeed,"wspeed" , &
       & "Wind Speed ","m/s", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

!constant fields


  IF(lnc_lsm) &
       & CALL define_output(ncidout,ncvar_lsm,"lsm" , &
       & "Land-sea mask","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
 IF(lnc_cv) &
       & CALL define_output(ncidout,ncvar_cv,"cv" , &
       & "Fractional coverage for vegetation","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
IF(lnc_slope) &
       & CALL define_output(ncidout,ncvar_slope,"slope" , &
       & "Slope of subgridscale orography","rad", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
IF(lnc_cr) &
       & CALL define_output(ncidout,ncvar_cr,"cr" , &
       & "Climatic Region","num", "INTEGER",ndiag2d, (/ LonDimId, LatDimID /) )
IF(lnc_fm) &
       & CALL define_output(ncidout,ncvar_fm,"fm" , &
       & "Fuel Model","num", "INTEGER",ndiag2d, (/ LonDimId, LatDimID /) )
 IF(lnc_rainclim) &
       & CALL define_output(ncidout,ncvar_rainclim,"tpclim", &
       &"Climate Rainfall","mm in a year","FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )


! fire variables


IF(lnc_nfdrs) THEN
   CALL define_output(ncidout,ncvar_mc1,"nfdrs_mc1" , &
        & "Moisture Content 1h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_mc10,"nfdrs_mc10" , &
        & "Moisture Content 10h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_mc100,"nfdrs_mc100" , &
        & "Moisture Content 100h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_mc1000,"nfdrs_mc1000" , &
        & "Moisture Content 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_x1000,"nfdrs_x1000" , &
        & "Moisture Content correction of herbaceous 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_mcherb,"nfdrs_mcherb" , &
        & "Moisture Content herbaceous plant","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_mcwood,"nfdrs_mcwood" , &
        & "Moisture Content shrub(wood) ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_ros,"nfdrs_ros" , &
      & "Rate of Spread "," ft /min", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_sc,"sc" , &
      & "Spread component ","ft/min", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_erc,"erc" , &
      & "Energy Release Component","25Btu/ft^2", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_output(ncidout,ncvar_bi,"bi" , &
      & "Burning Index","10ft", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_ic,"ic" , &
      & "Ignition Probability","%", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_mcoi,"nfdrs_mcoi", &
      & "Human-caused Fire Occurrence Index","%", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_loi,"nfdrs_loi", &
      & "Lightning-caused fire occurrence index","1/10^6*acres", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fli,"nfdrs_fli", &
      & "Fire-Load index","--", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
END IF

IF (lnc_mark5) THEN
  CALL define_output(ncidout,ncvar_mark5_kb,"kbdi" , &
      & "keetch-Byram drought index","inch", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )


  CALL define_output(ncidout,ncvar_mark5_df,"df", &
      & "Mark5 drought factor","--", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  CALL define_output(ncidout,ncvar_mark5_mc,"mark5_moist", &
      & "Mark5 moisture content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_mark5_w,"mark5_weight", &
      & "Mark5 fuel weight","tonnes/acres", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
 CALL define_output(ncidout,ncvar_mark5_ros0,"mark5_ros0", &
      & "Mark5 Rate of Spread","km/hr", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

 CALL define_output(ncidout,ncvar_mark5_ros,"mark5_ros", &
      & "Mark5 Rate of Spread for slope theta","km/hr", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

 CALL define_output(ncidout,ncvar_mark5_height,"mark5_height", &
      & "Mark5 flame height","m", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

 CALL define_output(ncidout,ncvar_mark5_heightd,"mark5_heightd", &
      & "Mark5 distance at which flame is perceivable","km", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

 CALL define_output(ncidout,ncvar_mark5_fdi,"fdi", &
      & "Mark5 fire danger index","--", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
END IF


IF (lnc_fwi) THEN
  CALL define_output(ncidout,ncvar_fwi_fwi,"fwi" , &
      & "Fire Weather Index","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_ffmc,"ffmc", &
      & "Fine Fuel Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_dmc,"dmc", &
      & "Duff Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_dc,"dc", &
      & "Drought Code","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_isi,"isi", &
      & "Initial Spread Index","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_bui,"bui", &
      & "Build-up Index","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_dsr,"dsr", &
      & "Daily Severity Rating","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_output(ncidout,ncvar_fwi_danger_risk,"danger_risk", &
      & "Danger Risk Class","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

END IF


  ! End define mode.
  CALL check(NF90_ENDDEF(ncidout))

  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  CALL check( nf90_put_var(ncidout, latvarid, lats) )
  CALL check( nf90_put_var(ncidout, lonvarid, lons) )
  CALL check( nf90_put_var(ncidout, timevarid, nhours) )


  !------------------
  ! Set up the arrays
  !------------------
  ! at end of list ndiag2d contains the number of diagnostics... so we can allocate the diags array

  ALLOCATE(rdiag2d(nlon,nlat,ndiag2d))
  rdiag2d=0.0

  RETURN
END SUBROUTINE ncdf_open_output


SUBROUTINE ncdf_set_grid

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

    PRINT *,'opening Land sea-mask file ',input//lsmfile
    CALL check(NF90_OPEN(path=input//lsmfile,mode=nf90_nowrite,ncid=ncid_lsm))

    ! dimensions are taken from the LSM
    CALL check(NF90_INQ_DIMID(ncid_lsm, "lat", LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LatDimID, len = nlatcheck))
    CALL check(NF90_INQ_DIMID(ncid_lsm, "lon", LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid_lsm, LonDimID, len = nloncheck))

    ! here we check that the rh and temperature files have the same number of  as the rain file
    nlon=nloncheck
    nlat=nlatcheck

    PRINT *, '*** data dimensions  *** ',nlon,nlat

    ALLOCATE(lons(nlon))
    ALLOCATE(lats(nlat))

    CALL check(NF90_GET_VAR(ncid_lsm,LatDimID,lats ))
    CALL check(NF90_GET_VAR(ncid_lsm,LonDimID,lons ))
    PRINT *, '*** grid points   *** ',lats(:),lons(:)


END SUBROUTINE ncdf_set_grid


SUBROUTINE ncdf_initialize

  !local
  INTEGER i, j, ncid, ivarid,  nlatcheck, nloncheck
  REAL, DIMENSION(nlon,nlat)::  meanrbndryt


  ! initialization options
  PRINT*,"Initialization type: ", TRIM(init_file(1:4))
  SELECT CASE (TRIM(init_file(1:4)))

  !
  ! restart file, exact initialization
  !
  CASE('rest')

    PRINT *, 'opening RESTART condition file '//input//init_file
    CALL check(NF90_OPEN(path=input//init_file,mode=nf90_nowrite,ncid=ncid))

    CALL check(NF90_INQ_DIMID(ncid, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid, LatDimID, len = nlatcheck))

    CALL check(NF90_INQ_DIMID(ncid, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid, LonDimID, len = nloncheck))

    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      PRINT *, '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,'init_file'
      STOP 1
    ENDIF

    !
    ! . read data for initialization
    !
    !
    CALL check(NF90_INQ_VARID(ncid, "mc100", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId,  mc(:,:)%r100hr))
    CALL check(NF90_INQ_VARID(ncid, "mc1000", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId,  mc(:,:)%r1000hr))
    CALL check(NF90_INQ_VARID(ncid, "x1000", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, mc(:,:)%rx1000))

    CALL check(NF90_INQ_VARID(ncid, "rbndryt", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, mc(:,:)%rbndryt))


    CALL check(NF90_INQ_VARID(ncid, "kbdi", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, mark5_fuel(:,:)%kb_drought_index))
    CALL check(NF90_INQ_VARID(ncid, "mark5_timesincerain", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, mark5_fuel(:,:)%timesincerain))

    CALL check(NF90_INQ_VARID(ncid, "ffmc", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, fwi_risk(:,:)%ffmc))
    CALL check(NF90_INQ_VARID(ncid, "dmc", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, fwi_risk(:,:)%dmc))
    CALL check(NF90_INQ_VARID(ncid, "dc", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, fwi_risk(:,:)%dc))



    ! close the initialization file
    CALL check(NF90_CLOSE(ncid))
    PRINT *,'initial conditions read ok'

   CASE DEFAULT  ! idealized fixed initial conditions
      PRINT *,'modelling initializing from artificial conditions'
   ! Here the loop is necessary !
   ! Dead fuel




      DO j=1,nlon
         DO i=1,nlat
            IF (rlsm(j,i) .GT. 0.00001 )   THEN
               mc(j,i)%r100hr=5.0+(5.0*icr(j,i))
               mc(j,i)%r1000hr=10+(5.0*icr(j,i))
               mc(j,i)%rbndryt=10+(5.0*icr(j,i))
               mc(j,i)%rx1000=10+(5.0*icr(j,i))


            mark5_fuel(j,i)%kb_drought_index=0 ! saturation of the soil
            mark5_fuel(j,i)%timesincerain=0

!For the FWI moisture code values (FFMC=85, DMC=6, DC=15) provide a
!reasonable set of conditions for post-snowmelt springtime conditions
!in eastern/central Canada, the Northern U.S., and Alaska; physically
!these spring start-up values represent about 3 days of drying from
!complete moisture saturation of the fuel layer. In areas or years
!with particularly dry winters (or parts of the world without
!significant snow cover) these start-up values for FFMC and DMC may
!still be appropriate as these two elements respond relatively quickly
!to changes in the weather. The DC component however, because of its
!very long response time, can take considerable time to adjust to
!unrealistic initial values and some effort to estimate over-winter
!value of the DC may be necessary. Users can look again to Lawson and
!Armitage (2008) for a more detailed description of code calculation
!startup issues and the over-winter adjustment process.
            fwi_risk(j,i)%ffmc=85
            fwi_risk(j,i)%dmc=6
            fwi_risk(j,i)%dc=15


         END IF
      ENDDO
   ENDDO

END SELECT
  PRINT *,'Initialization completed'

   RETURN
END SUBROUTINE ncdf_initialize


SUBROUTINE ncdf_setdown

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

  PRINT *,'input/output closed '



  RETURN
END SUBROUTINE ncdf_setdown


SUBROUTINE ncdf_getdata(istep)
  INTEGER, INTENT(IN) :: istep

  !---------------------------------------------------------------------------
  !  meteorology variables read in from analysis, obs or forecast, or constant
  !---------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! gridded input - assumes the input files have the correct timestamps
  !                 i.e. just reads the file blindly - the lat/lon info is
  !                 important though...
  !------------------------------------------------------------------------


  CALL check(NF90_GET_VAR(ncid_rain,   VarId_rain, rrain, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_temp, VarId_temp, rtemp, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_maxtemp, VarId_maxtemp, rmaxtemp, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_mintemp, VarId_mintemp, rmintemp, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_rh, VarId_rh, rrh, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_maxrh, VarId_maxrh, rmaxrh, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_minrh, VarId_minrh, rminrh, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_cc, VarId_cc, rcc, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_wspeed, VarId_wspeed, rwspeed, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_snow, VarId_snow, rsnow, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_dp, VarId_dp, rdp, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
  CALL check(NF90_GET_VAR(ncid_vs, VarId_vs, ivs, start=(/1,1,istep/),count=(/nlon,nlat,1/)))
! CALL check(NF90_GET_VAR(ncid_lal, VarId_lal, ilal, start=(/1,1,istep/),count=(/nlon,nlat,1/)))

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
END SUBROUTINE ncdf_getdata


SUBROUTINE ncdf_write_results(istep)
    integer, intent(in) :: istep

IF (lnc_rain) CALL check( nf90_put_var(ncidout, ncvar_rain(1), rrain, start=(/ 1, 1, istep /) ))
IF (lnc_temp) CALL check( nf90_put_var(ncidout, ncvar_temp(1), rtemp, start=(/ 1, 1, istep /) ))
IF (lnc_maxtemp) CALL check( nf90_put_var(ncidout, ncvar_maxtemp(1), rmaxtemp, start=(/ 1, 1, istep /) ))
IF (lnc_mintemp) CALL check( nf90_put_var(ncidout, ncvar_mintemp(1), rmintemp, start=(/ 1, 1, istep /) ))
IF (lnc_rh) CALL check( nf90_put_var(ncidout, ncvar_rh(1), rrh, start=(/ 1, 1, istep /) ))
IF (lnc_maxrh) CALL check( nf90_put_var(ncidout, ncvar_maxrh(1), rmaxrh, start=(/ 1, 1, istep /) ))
IF (lnc_minrh) CALL check( nf90_put_var(ncidout, ncvar_minrh(1), rminrh, start=(/ 1, 1, istep /) ))
IF (lnc_cc) CALL check( nf90_put_var(ncidout, ncvar_cc(1), rcc, start=(/ 1, 1, istep /) ))
IF (lnc_snow) CALL check( nf90_put_var(ncidout, ncvar_snow(1), rsnow, start=(/ 1, 1, istep /) ))
IF (lnc_wspeed) CALL check( nf90_put_var(ncidout, ncvar_wspeed(1), rwspeed, start=(/ 1, 1, istep /) ))
IF (lnc_dp) CALL check( nf90_put_var(ncidout, ncvar_dp(1), rdp, start=(/ 1, 1, istep /) ))
IF (lnc_vs   ) CALL check(nf90_put_var(ncidout, ncvar_vs (1) ,ivs,start =(/1 ,1,istep/)))
!IF (lnc_lal   ) CALL check(nf90_put_var(ncidout, ncvar_lal(1) ,ilal,start =(/1 ,1,istep/)))

IF (lnc_nfdrs) THEN

      CALL check( nf90_put_var(ncidout, ncvar_mc1(1),  mc%r1hr, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mc10(1), mc%r10hr , start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mc100(1),mc%r100hr , start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mc1000(1),mc%r1000hr , start=(/ 1, 1, istep /) ))

      CALL check( nf90_put_var(ncidout, ncvar_x1000(1),  mc%rx1000, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mcherb(1), mc%rherb, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mcwood(1), mc%rwood , start=(/ 1, 1, istep /) ))

      CALL check( nf90_put_var(ncidout, ncvar_ros(1),  fire_prop%ros, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_sc(1) ,  fire_prop%sc , start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_erc(1),  fire_prop%erc, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_bi(1),   fire_prop%bi, start=(/ 1, 1, istep /) ))

      CALL check( nf90_put_var(ncidout, ncvar_ic(1),   fire_prob%ic, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mcoi(1), fire_prob%mcoi , start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_loi(1),  fire_prob%loi, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fli(1),  fire_prob%fli, start=(/ 1, 1, istep /) ))

ENDIF

IF (lnc_mark5) THEN

      CALL check( nf90_put_var(ncidout, ncvar_mark5_kb(1),  mark5_fuel%kb_drought_index, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_df(1),  mark5_fuel%drought_factor , start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_mc(1),  mark5_fuel%moist, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_w(1),   mark5_fuel%weight, start=(/ 1, 1, istep /) ))

      CALL check( nf90_put_var(ncidout, ncvar_mark5_ros0(1),    mark5_prop%ros_theta0,   start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_ros(1),     mark5_prop%ros_theta,    start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_height(1),  mark5_prop%flame_height, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_heightd(1), mark5_prop%flame_distance, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_fdi(1),     mark5_prob%fire_danger_index, start=(/ 1, 1, istep /) ))

   END IF

IF (lnc_fwi) THEN

      CALL check( nf90_put_var(ncidout, ncvar_fwi_fwi(1),  fwi_risk%fwi, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_ffmc(1), fwi_risk%ffmc , start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_dmc(1),  fwi_risk%dmc, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_dc(1),   fwi_risk%dc, start=(/ 1, 1, istep /) ))

      CALL check( nf90_put_var(ncidout, ncvar_fwi_isi(1),     fwi_risk%isi,   start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_bui(1),     fwi_risk%bui,    start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_dsr(1),     fwi_risk%dsr, start=(/ 1, 1, istep /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_danger_risk(1), fwi_risk%danger_risk, start=(/ 1, 1, istep /) ))


   END IF
END SUBROUTINE ncdf_write_results


SUBROUTINE ncdf_write_constant_fields
IF(lnc_lsm)            CALL check( nf90_put_var(ncidout, ncvar_lsm(1),   rlsm , start=(/ 1, 1 /)))
IF(lnc_cr)             CALL check( nf90_put_var(ncidout, ncvar_cr(1) ,   icr  , start=(/ 1, 1 /)))
IF(lnc_fm)             CALL check( nf90_put_var(ncidout, ncvar_fm(1) ,   ifm  , start=(/ 1, 1 /)))
IF(lnc_cv)             CALL check( nf90_put_var(ncidout, ncvar_cv (1),   rcv  , start=(/ 1, 1 /)))
IF(lnc_slope)          CALL check( nf90_put_var(ncidout, ncvar_slope(1), islope, start=(/ 1, 1 /)))
IF(lnc_rainclim)       CALL check( nf90_put_var(ncidout, ncvar_rainclim(1), rrainclim, start=(/ 1, 1 /)))
END SUBROUTINE ncdf_write_constant_fields


END MODULE mo_ncdf_tools

