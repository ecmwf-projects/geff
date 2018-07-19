SUBROUTINE dump_restart

!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!  dump a restart file for a follow on simulation
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
  
  print *,'- now dump restart'

  ! dump restart file:

  CALL check(NF90_CREATE(path = input//"restart_ecfire.nc", cmode=or(nf90_clobber,nf90_64bit_offset), ncid = ncidrest))


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
  CALL check( NF90_PUT_ATT(ncidrest, NF90_GLOBAL, "nday",nday))

  !---------------------------------------------
  ! define the output structures
  ! routine also sets up the diagnostics indices
  ! reuse the nc 2 byte arrays for indices
  !---------------------------------------------
  ! 
 

 ndiag2d=0 ! dummy variable

   CALL define_ncdf_output(ncidrest,ncvar_mc1,"mc1" , &
        & "Moisture Content 1h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )


   CALL define_ncdf_output(ncidrest,ncvar_mc10,"mc10" , &
        & "Moisture Content 10h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncidrest,ncvar_mc100,"mc100" , &
        & "Moisture Content 100h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncidrest,ncvar_mc1000,"mc1000" , &
        & "Moisture Content 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncidrest,ncvar_rbndryt,"rbndryt" , &
        & "Mean of moisture Content 1000h- previous 6 days","frac", "FLOAT",ndiag2d, (/LonDimId, LatDimID /) )


   CALL define_ncdf_output(ncidrest,ncvar_x1000,"x1000" , &
        & "Moisture Content correction of herbaceous 1000h- ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncidrest,ncvar_mcherb,"mcherb" , &
        & "Moisture Content herbaceous plant","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
   CALL define_ncdf_output(ncidrest,ncvar_mcwood,"mcwood" , &
        & "Moisture Content shrub(wood) ","frac", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )

 

  CALL define_ncdf_output(ncidrest,ncvar_fwi_ffmc,"ffmc", &
      & "Fine Fuel Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
  CALL define_ncdf_output(ncidrest,ncvar_fwi_dmc,"dmc", &
      & "Duff Moisture Content","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
  CALL define_ncdf_output(ncidrest,ncvar_fwi_dc,"dc", &
      & "Drought Code","%", "FLOAT",ndiag2d, (/ LonDimId, LatDimID/) )
print*,"ciao"
   CALL define_ncdf_output(ncidrest,ncvar_mark5_kb,"kbdi" , &
         & "keetch-Byram drought index","inch", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )


   CALL define_ncdf_output(ncidrest,ncvar_mark5_tsr,"mark5_timesincerain", &
        & "Time since rain","number", "FLOAT",ndiag2d, (/ LonDimId, LatDimID /) )
 
  CALL check(NF90_ENDDEF(ncidrest))


  

  print *,'writing vars'
  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  CALL check( nf90_put_var(ncidrest, latvarid, lats) )
  CALL check( nf90_put_var(ncidrest, lonvarid, lons) )
  
  print*,"NFDRS" 
 print*,"======"

  print*,"mc1"
  CALL check( nf90_put_var(ncidrest, ncvar_mc1(1)   , mc%r1hr ))
  print*,"mc10"
  CALL check( nf90_put_var(ncidrest, ncvar_mc10(1)  , mc%r10hr ))
  print*,"mc100"
  CALL check( nf90_put_var(ncidrest, ncvar_mc100(1) , mc%r100hr  ))
  print*,"mc1000"
  CALL check( nf90_put_var(ncidrest, ncvar_mc1000(1) , mc%r1000hr  ))
  print*,"mc-herb"
  CALL check( nf90_put_var(ncidrest, ncvar_mcherb(1)  , mc%rherb  ))
  print*,"mc-wood"
  CALL check( nf90_put_var(ncidrest, ncvar_mcwood(1)  , mc%rwood  ))
  print*,"x1000"
  CALL check( nf90_put_var(ncidrest, ncvar_x1000(1)  , mc%rx1000  ))
  print*,"meanrbndryt"
  CALL check( nf90_put_var(ncidrest, ncvar_rbndryt(1)  , mc%rbndryt ))
  
  print*,"MARK-5"
  print*,"======"
  print*,"kbdi"
  CALL check( nf90_put_var(ncidrest, ncvar_mark5_kb(1)  , mark5_fuel%kb_drought_index  ))
  print*,"timesincerain"
  CALL check( nf90_put_var(ncidrest, ncvar_mark5_tsr(1)  , mark5_fuel%timesincerain  ))
 
  print*,"FWI"
  print*,"======"
  print*,"ffmc"
  CALL check( nf90_put_var(ncidrest, ncvar_fwi_ffmc(1), fwi_risk%ffmc ))
  print*,"dmc"
  CALL check( nf90_put_var(ncidrest, ncvar_fwi_dmc(1), fwi_risk%dmc ))
  print*,"dc"
  CALL check( nf90_put_var(ncidrest, ncvar_fwi_dc(1), fwi_risk%dc ))
  CALL check( nf90_close(ncidrest))

  RETURN
END SUBROUTINE dump_restart
