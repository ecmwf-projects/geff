SUBROUTINE setup
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! setup subroutine to read in forcing data and setup output files
!
!---------------------------------------------------------
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
  INTEGER :: ix,iy,i,j,idate

 

  !-------------------------- 
  !
  ! get basic run info
  !

 CALL read_namelists
 CALL ncdf_set_grid

  !
  ! allocate arrays
  !
 ! meteo
  ALLOCATE(rrain(nlon,nlat))
  ALLOCATE(rrainclim(nlon,nlat))
  ALLOCATE(rtemp(nlon,nlat))
  ALLOCATE(rmaxtemp(nlon,nlat))
  ALLOCATE(rmintemp(nlon,nlat))
  ALLOCATE(rrh(nlon,nlat))
  ALLOCATE(rmaxrh(nlon,nlat))
  ALLOCATE(rminrh(nlon,nlat))
  ALLOCATE(rcc(nlon,nlat)) 
  ALLOCATE(rwspeed(nlon,nlat))
  ALLOCATE(rsnow(nlon,nlat))
  ALLOCATE(rdp(nlon,nlat))
  ALLOCATE(ivs(nlon,nlat))
!  ALLOCATE(ilal(nlon,nlat))
!constant

  ALLOCATE(rlsm(nlon,nlat))
  ALLOCATE(rcv(nlon,nlat))
  ALLOCATE(icr(nlon,nlat))
  ALLOCATE(ifm(nlon,nlat))
  ALLOCATE(islope(nlon,nlat))
  ! types/structures

 ! NFDRS
 ALLOCATE(mc(nlon,nlat))
 ALLOCATE(fire_prop(nlon,nlat))
 ALLOCATE(fire_prob(nlon,nlat)) 
 
!  mc(:,:)%r1hr=rfillvalue
!  mc(:,:)%r10hr=rfillvalue
!  mc(:,:)%r100hr=rfillvalue
!  mc(:,:)%r1000hr=rfillvalue  
!  mc(:,:)%rherb=rfillvalue
!  mc(:,:)%rwood=rfillvalue
!  mc(:,:)%rx1000=rfillvalue
  

 fire_prop(:,:)%ros=rfillvalue
 fire_prop(:,:)%sc=ifillvalue
 fire_prop(:,:)%erc=ifillvalue
 fire_prop(:,:)%bi=ifillvalue

 fire_prob(:,:)%ic=ifillvalue
 fire_prob(:,:)%mcoi=ifillvalue
 fire_prob(:,:)%loi=ifillvalue
 fire_prob(:,:)%fli=rfillvalue
 

 !MARK-5
 ALLOCATE(mark5_fuel(nlon,nlat))
 ALLOCATE(mark5_prop(nlon,nlat))
 ALLOCATE(mark5_prob(nlon,nlat))

 mark5_fuel(:,:)%moist=rfillvalue
 mark5_fuel(:,:)%weight=rfillvalue
 mark5_fuel(:,:)%curing=rfillvalue
 mark5_fuel(:,:)%kb_drought_index=rfillvalue
 mark5_fuel(:,:)%drought_factor=rfillvalue
 mark5_fuel(:,:)%timesincerain=rfillvalue

 mark5_prop(:,:)%ros_theta0=rfillvalue
 mark5_prop(:,:)%ros_theta=rfillvalue
 mark5_prop(:,:)%flame_height=rfillvalue
 mark5_prop(:,:)%flame_distance=rfillvalue
 
 mark5_prob(:,:)%fire_danger_index=rfillvalue

 ! FWI

 ALLOCATE(fwi_risk(nlon,nlat))

fwi_risk(:,:)%fwi=rfillvalue
fwi_risk(:,:)%ffmc=rfillvalue
fwi_risk(:,:)%dmc=rfillvalue
fwi_risk(:,:)%dc=rfillvalue
fwi_risk(:,:)%isi=rfillvalue
fwi_risk(:,:)%bui=rfillvalue
fwi_risk(:,:)%dsr=rfillvalue
fwi_risk(:,:)%danger_risk=rfillvalue


  CALL ncdf_open_input
  PRINT*, "Input files: OPENED" 
  CALL ncdf_initialize

  CALL ncdf_open_output
  PRINT*, "Output file: CREATED " 


END SUBROUTINE setup


