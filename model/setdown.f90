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

  INTEGER :: latvarid, lonvarid, timevarid, unlimdimid
  INTEGER :: nlatcheck, nloncheck, ndaycheck
  INTEGER :: i,j
  REAL, DIMENSION(nlon,nlat)::  meanrbndryt
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

  print *,'input/output closed - now dump restart'

  !---
  ! 2. dump a restart file
  !---

  ! dump restart file:

  CALL check(NF90_CREATE(path = input//"restart_ecfire.nc", cmode=or(nf90_clobber,nf90_64bit_offset), ncid = ncidout))


! define the dimensions
  CALL check(NF90_DEF_DIM(ncidout, lon_name, nlon, LonDimID))
  CALL check(NF90_DEF_DIM(ncidout, lat_name, nlat, LatDimID))
 
  ! Define the coordinate variables. They will hold the coordinate
  ! information, that is, the latitudes and longitudes. A varid is
 ! returned for each.
  CALL check(nf90_def_var(ncidout, lon_name, NF90_REAL, londimid, lonvarid) )
  CALL check(nf90_def_var(ncidout, lat_name, NF90_REAL, latdimid, latvarid) )

  ! Assign units attributes to coordinate var data. This attaches a
  ! text attribute to each of the coordinate variables, containing the
  ! units.
  CALL check( nf90_put_att(ncidout, latvarid, units, lat_units) )
  CALL check( nf90_put_att(ncidout, lonvarid, units, lon_units) )

  ! define global attributes
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "rundate", now))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "geff", version))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "comments","geff  restart file"))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "refdate", time_units))
  CALL check( NF90_PUT_ATT(ncidout, NF90_GLOBAL, "nday",nday))

  !---------------------------------------------
  ! define the output structures
  ! routine also sets up the diagnostics indices
  ! reuse the nc 2 byte arrays for indices
  !---------------------------------------------
  ! 
  ndiag2d=0 ! dummy variable
   CALL define_ncdf_output(ncvar_mc1,"mc1" , &
        & "Moisture Content 1h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )
   CALL define_ncdf_output(ncvar_mc10,"mc10" , &
        & "Moisture Content 10h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncvar_mc100,"mc100" , &
        & "Moisture Content 100h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncvar_mc1000,"mc1000" , &
        & "Moisture Content 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncvar_rbndryt,"rbndryt" , &
        & "Mean of moisture Content 1000h- previous 6 days","frac", "FLOAT",ndiag2d, (/LonDimId, LatDimID /) )
 

   CALL define_ncdf_output(ncvar_x1000,"x1000" , &
        & "Moisture Content correction of herbaceous 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncvar_mcherb,"mcherb" , &
        & "Moisture Content herbaceous plant","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncvar_mcwood,"mcwood" , &
        & "Moisture Content shrub(wood) ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )

   CALL define_ncdf_output(ncvar_mark5_kb,"kbdi" , &
        & "keetch-Byram drought index","inch", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncvar_mark5_tsr,"mark5_timesincerain", &
        & "Time since rain","number", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )


  CALL define_ncdf_output(ncvar_fwi_ffmc,"ffmc", &
      & "Fine Fuel Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
  CALL define_ncdf_output(ncvar_fwi_dmc,"dmc", &
      & "Duff Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
  CALL define_ncdf_output(ncvar_fwi_dc,"dc", &
      & "Drought Code","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )

  
  CALL check(NF90_ENDDEF(ncidout))


  

  print *,'writing vars'
  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  CALL check( nf90_put_var(ncidout, latvarid, lats) )
  CALL check( nf90_put_var(ncidout, lonvarid, lons) )
  
  print*,"NFDRS" 
 print*,"======"

  print*,"mc1"
  CALL check( nf90_put_var(ncidout, ncvar_mc1(1)   , mc%r1hr ))
  print*,"mc10"
  CALL check( nf90_put_var(ncidout, ncvar_mc10(1)  , mc%r10hr ))
  print*,"mc100"
  CALL check( nf90_put_var(ncidout, ncvar_mc100(1) , mc%r100hr  ))
  print*,"mc1000"
  CALL check( nf90_put_var(ncidout, ncvar_mc1000(1) , mc%r1000hr  ))
  print*,"mc-herb"
  CALL check( nf90_put_var(ncidout, ncvar_mcherb(1)  , mc%rherb  ))
  print*,"mc-wood"
  CALL check( nf90_put_var(ncidout, ncvar_mcwood(1)  , mc%rwood  ))
  print*,"x1000"
  CALL check( nf90_put_var(ncidout, ncvar_x1000(1)  , mc%rx1000  ))
  print*,"meanrbndryt"
  CALL check( nf90_put_var(ncidout, ncvar_rbndryt(1)  , mc%rbndryt ))
  
  print*,"MARK-5"
  print*,"======"
  print*,"kbdi"
  CALL check( nf90_put_var(ncidout, ncvar_mark5_kb(1)  , mark5_fuel%kb_drought_index  ))
  print*,"timesincerain"
  CALL check( nf90_put_var(ncidout, ncvar_mark5_tsr(1)  , mark5_fuel%timesincerain  ))
 
  print*,"FWI"
  print*,"======"
  print*,"ffmc"
  CALL check( nf90_put_var(ncidout, ncvar_fwi_ffmc(1), fwi_risk%ffmc ))
  print*,"dmc"
  CALL check( nf90_put_var(ncidout, ncvar_fwi_dmc(1), fwi_risk%dmc ))
  print*,"dc"
  CALL check( nf90_put_var(ncidout, ncvar_fwi_dc(1), fwi_risk%dc ))
  CALL check( nf90_close(ncidout))

  RETURN
END SUBROUTINE setdown
