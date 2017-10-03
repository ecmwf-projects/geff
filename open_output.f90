SUBROUTINE open_output
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! opens the output files 
!
!---------------------------------------------------------


  USE netcdf
  USE mo_control
  USE mo_constants
  USE mo_fire
  USE mo_namelist
  USE mo_ncdf_tools
  USE mo_nfdrs
  USE mo_mark5
  USE mo_fwi
  IMPLICIT NONE

  ! local variables
  CHARACTER :: filestr1*3, filestr2*3, strdt*2

  ! netcdf vars - only needed locally
  INTEGER :: latvarid, lonvarid, timevarid

  ! output file details
  CHARACTER (len = 8)  :: str_date

! need to make this more flexible for 1,3 or 4 dimensions
!  REAL, ALLOCATABLE :: zdummy2d(:,:),zdummy3d(:,:,:),zdummy4d(:,:,:,:),time(:)

! --- output files --- 
  WRITE (strdt,'(I2.2)') INT(dt*24)
 
! 
  !---------------------------
  ! OUTPUT NETCDF FILE
  !---------------------------
  WRITE(str_date,'(I8)') date1
  time_units="days since "//str_date(1:4)//"-"//str_date(5:6)//"-"//str_date(7:8)//" 00:00 UTC"
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
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "date", now))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "ecfire", version))



  !---------------------------------------------
  ! *** define the 3d structures
  ! routine also sets up the diagnostics indices
  !---------------------------------------------
  !                             index      short name title       units
  ndiag2d=0

 
  IF(lnc_rain) &
       & CALL define_ncdf_output(ncvar_rain,"tp","Rainfall","mm/day", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  
 
  IF(lnc_temp) &
       & CALL define_ncdf_output(ncvar_temp,"t2","Temperature","degrees K","FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  
  IF(lnc_maxtemp) &
       & CALL define_ncdf_output(ncvar_maxtemp,"maxt2" , &
       & "Maximum daily temperature","degrees K", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  
  IF(lnc_mintemp) &
       & CALL define_ncdf_output(ncvar_mintemp,"mint2" , &
       & "Minimum daily temperature","degrees K", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  
  IF(lnc_rh) &
       & CALL define_ncdf_output(ncvar_rh,"rh" , &
       & "Relative Humidity","%",  "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  
  IF(lnc_maxrh) &
       & CALL define_ncdf_output(ncvar_maxrh,"maxrh" , &
       & "Maximum daily Relative Humidity","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_minrh) &
       & CALL define_ncdf_output(ncvar_minrh,"minrh" , &
       & "Minimum daily Relative Humidity","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_cc) &
       & CALL define_ncdf_output(ncvar_cc,"cc" , &
       & "Cloud Cover ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_snow) &
       & CALL define_ncdf_output(ncvar_snow,"snow" , &
       & "Ground Snow ","mask","FLOAT", ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

  IF(lnc_dp) &
       & CALL define_ncdf_output(ncvar_dp,"dp" , &
       & "Duration of precipitation in the prev 24hr","hr", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  IF(lnc_vs) &
       & CALL define_ncdf_output(ncvar_vs,"vs" , &
       & "Vegetation stage 1-pregreen,2-green-up,3-green,4-transition,5-cured/frozen",&
       &"--", "INTEGER",ndiag2d,(/LonDimId, LatDimID, timeDimID /) )

! IF(lnc_lal) &
!       & CALL define_ncdf_output(ncvar_lal,"lal" , &
!       & "Lightning Activity Level","--", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
!

  IF(lnc_wspeed) &
       & CALL define_ncdf_output(ncvar_wspeed,"wspeed" , &
       & "Wind Speed ","m/s", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

!constant fields 


  IF(lnc_lsm) &
       & CALL define_ncdf_output(ncvar_lsm,"lsm" , &
       & "Land-sea mask","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
 IF(lnc_cv) &
       & CALL define_ncdf_output(ncvar_cv,"cv" , &
       & "Fractional coverage for vegetation","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
IF(lnc_slope) &
       & CALL define_ncdf_output(ncvar_slope,"slope" , &
       & "Slope of subgridscale orography","rad", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
IF(lnc_cr) &
       & CALL define_ncdf_output(ncvar_cr,"cr" , &
       & "Climatic Region","num", "INTEGER",ndiag2d, (/ LonDimId, LatDimID /) )
IF(lnc_fm) &
       & CALL define_ncdf_output(ncvar_fm,"fm" , &
       & "Fuel Model","num", "INTEGER",ndiag2d, (/ LonDimId, LatDimID /) )
 IF(lnc_rainclim) &
       & CALL define_ncdf_output(ncvar_rainclim,"tpclim", &
       &"Climate Rainfall","mm in a year","FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )
  

! fire variables 


IF(lnc_nfdrs) THEN
   CALL define_ncdf_output(ncvar_mc1,"nfdrs_mc1" , &
        & "Moisture Content 1h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_mc10,"nfdrs_mc10" , &
        & "Moisture Content 10h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_mc100,"nfdrs_mc100" , &
        & "Moisture Content 100h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_mc1000,"nfdrs_mc1000" , &
        & "Moisture Content 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_x1000,"nfdrs_x1000" , &
        & "Moisture Content correction of herbaceous 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_mcherb,"nfdrs_mcherb" , &
        & "Moisture Content herbaceous plant","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_mcwood,"nfdrs_mcwood" , &
        & "Moisture Content shrub(wood) ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_ros,"nros" , &
      & "Rate of Spread "," ft /min", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_sc,"nsc" , &
      & "Spread component ","ft/min", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_erc,"nerc" , &
      & "Energy Release Component","25Btu/ft^2", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
   CALL define_ncdf_output(ncvar_bi,"nbi" , &
      & "Burning Index","10ft", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_ic,"nic" , &
      & "Ignition Probability","%", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_mcoi,"nfdrs_mcoi", &
      & "Human-caused Fire Occurrence Index","%", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_loi,"nfdrs_loi", &
      & "Lightning-caused fire occurrence index","1/10^6*acres", "INTEGER",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fli,"nfdrs_fli", &
      & "Fire-Load index","--", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
END IF

IF (lnc_mark5) THEN
  CALL define_ncdf_output(ncvar_mark5_kb,"kbdi" , &
      & "keetch-Byram drought index","inch", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_mark5_df,"mdf", &
      & "Mark5 drought factor","--", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_mark5_mc,"mark5_moist", &
      & "Mark5 moisture content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_mark5_w,"mark5_weight", &
      & "Mark5 fuel weight","tonnes/acres", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
 CALL define_ncdf_output(ncvar_mark5_ros0,"mark5_ros0", &
      & "Mark5 Rate of Spread","km/hr", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
 CALL define_ncdf_output(ncvar_mark5_ros,"maros", &
      & "Mark5 Rate of Spread for slope theta","km/hr", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
 CALL define_ncdf_output(ncvar_mark5_height,"mark5_height", &
      & "Mark5 flame height","m", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
 CALL define_ncdf_output(ncvar_mark5_heightd,"mark5_heightd", &
      & "Mark5 distance at which flame is perceivable","km", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
 CALL define_ncdf_output(ncvar_mark5_fdi,"mfdi", &
      & "Mark5 fire danger index","--", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
END IF


IF (lnc_fwi) THEN
  CALL define_ncdf_output(ncvar_fwi_fwi,"ffwi" , &
      & "Fire Weather Index","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_ffmc,"ffmc", &
      & "Fine Fuel Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_dmc,"fdmc", &
      & "Duff Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_dc,"fdc", &
      & "Drought Code","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_isi,"fisi", &
      & "Initial Spread Index","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_bui,"fbui", &
      & "Build-up Index","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_dsr,"fdsr", &
      & "Daily Severity Rating","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )
  CALL define_ncdf_output(ncvar_fwi_danger_risk,"fdri", &
      & "Danger Risk Class","-", "FLOAT",ndiag2d, (/ LonDimId, LatDimID, timeDimID /) )

END IF


  ! End define mode.
  CALL check(NF90_ENDDEF(ncidout))

  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  CALL check( nf90_put_var(ncidout, latvarid, lats) )
  CALL check( nf90_put_var(ncidout, lonvarid, lons) )
  CALL check( nf90_put_var(ncidout, timevarid, ndate) )
! write constant fields 
  IF(lnc_lsm)            CALL check( nf90_put_var(ncidout, ncvar_lsm(1),   rlsm , start=(/ 1, 1 /)))
  IF(lnc_cr)             CALL check( nf90_put_var(ncidout, ncvar_cr(1) ,   icr  , start=(/ 1, 1 /)))
  IF(lnc_fm)             CALL check( nf90_put_var(ncidout, ncvar_fm(1) ,   ifm  , start=(/ 1, 1 /)))
  IF(lnc_cv)             CALL check( nf90_put_var(ncidout, ncvar_cv (1),   rcv  , start=(/ 1, 1 /)))
  IF(lnc_slope)          CALL check( nf90_put_var(ncidout, ncvar_slope(1), islope, start=(/ 1, 1 /)))
  IF(lnc_rainclim)       CALL check( nf90_put_var(ncidout, ncvar_rainclim(1), rrainclim, start=(/ 1, 1 /)))
  !------------------
  ! Set up the arrays
  !------------------
  ! at end of list ndiag2d contains the number of diagnostics... so we can allocate the diags array
 
  ALLOCATE(rdiag2d(nlon,nlat,ndiag2d))
  rdiag2d=0.0

  RETURN
END SUBROUTINE open_output
