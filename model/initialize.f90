SUBROUTINE initialize

!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!initialize arrays 
!
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


  !local
  INTEGER i, j, ncid, ivarid,  nlatcheck, nloncheck
  REAL, DIMENSION(nlon,nlat)::  meanrbndryt
 
 
  ! initialization options 
  PRINT*,TRIM(init_file(1:4))
  SELECT CASE (TRIM(init_file(1:4)))

  ! 
  ! restart file, exact initialization
  !
  CASE('rest') 

    print *, 'opening RESTART condition file '//input//init_file
    CALL check(NF90_OPEN(path=input//init_file,mode=nf90_nowrite,ncid=ncid))

    CALL check(NF90_INQ_DIMID(ncid, lat_name, LatDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid, LatDimID, len = nlatcheck))

    CALL check(NF90_INQ_DIMID(ncid, lon_name, LonDimID))
    CALL check(NF90_INQUIRE_DIMENSION(Ncid, LonDimID, len = nloncheck))
   
    IF (nlon.ne.nloncheck .or. nlat.ne.nlatcheck) THEN
      WRITE(iounit,*) '*** data dimensions input error *** ',nlon,nloncheck,nlat,nlatcheck,'init_file'
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
    CALL check(NF90_INQ_VARID(ncid, "fdmc", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, fwi_risk(:,:)%dmc))
    CALL check(NF90_INQ_VARID(ncid, "fdc", iVarId))
    CALL check(NF90_GET_VAR(ncid, iVarId, fwi_risk(:,:)%dc))



    ! close the initialization file
    CALL check(NF90_CLOSE(ncid))
    print *,'initial conditions read ok'

   CASE DEFAULT  ! idealized fixed initial conditions 
      print *,'modelling initializing from artificial conditions'
   ! Here the loop is necessary !
   ! Dead fuel 


                                                            

      DO j=1,nlon
         DO i=1,nlat
            IF (rlsm(j,i) .GT. 0.0 .AND. icr(j,i) .GT. 0.0 )   THEN 
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
 

   RETURN
END SUBROUTINE initialize
