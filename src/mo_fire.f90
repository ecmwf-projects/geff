MODULE mo_fire
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! module for meteo constant fields and specific fire variables 
!
!---------------------------------------------------------
  USE mo_constants
  USE mo_control

  IMPLICIT NONE

  ! spatial dimensions
  INTEGER  :: nlat,nlon
 
  ! time dimensions 
  INTEGER :: ntimestep        ! total integration time as read from input files 
 
  ! region data
  REAL :: lon,dlon,lat,dlat
  REAL, ALLOCATABLE :: lons(:), lats(:) ! lon lat values
  
  ! meteorological variables

  INTEGER, ALLOCATABLE  :: nhours(:)      ! nhours(ntimestep)         !          integer date
  REAL, ALLOCATABLE :: rtemp (:,:)       ! rtemp(nx,ny)      ! (K)      T2m     as a function of  space for one slice at time 
  REAL, ALLOCATABLE :: rmaxtemp (:,:)    ! rtempmax(nx,ny)   ! (K)      T2m max as a function of  space for one slice at time
  REAL, ALLOCATABLE :: rmintemp (:,:)    ! rtempmin(nx,ny)   ! (K)      T2m min as a function of  space for one slice at time
  REAL, ALLOCATABLE :: rrh(:,:)          ! rrh(nx,ny)        ! (frac)   RH as a function of space for one slice at time 
  REAL, ALLOCATABLE :: rminrh(:,:)       ! rrhmin(nx,ny)     ! (frac)   RH min as a function of space for one slice at time 
  REAL, ALLOCATABLE :: rmaxrh(:,:)       ! rrhmax(nx,ny)     ! (frac)   RH max as a function of space for one slice at time 
  REAL, ALLOCATABLE :: rrain(:,:)        ! rrain(nx,ny)      ! (mm/day) precip as a function of space for one slice at time 
  REAL, ALLOCATABLE :: rrainclim(:,:)    ! rrainclim(nx,ny)  ! (mm/day) climatic precip as a function of space for one slice at time 
 
  REAL, ALLOCATABLE :: rcc(:,:)          ! rcc(nx,ny)        ! (frac)   Cloud cover as a function of space for one slice at time 
  REAL, ALLOCATABLE :: rwspeed(:,:)      ! rwspeed(nx,ny)    ! (m/s)    Wind speed  a function of space for one slice at time 
  REAL, ALLOCATABLE :: rsnow(:,:)        ! rsnow(nx,ny)      ! (mask)   Snow mask a function of space for one slice at time 
  REAL, ALLOCATABLE :: rdp(:,:)          ! rdp (nx,ny)       ! (hr)     Duration of precipitation in the previous 24 hours
  INTEGER, ALLOCATABLE :: ivs(:,:)       ! ivs(nx,ny)        ! (num)    index which identifies the vegetation stage 
!  INTEGER, ALLOCATABLE :: ilal(:,:)       ! ilal(nx,ny)        ! (num)    index which identifies the lightning activity (1-6) 

!constant fields 

  REAL,    ALLOCATABLE :: rlsm(:,:)        ! rlsm(nx,ny)      ! (mask)  Land=1 sea=0   0.5=coast mask
  REAL,    ALLOCATABLE :: rcv(:,:)        !  rcv(nx,ny)      ! (type)  fraction/cover of high + low vegetation (IFS)
  INTEGER,    ALLOCATABLE :: islope(:,:)   ! slope(nx,ny)     ! (num)   slope class 
  INTEGER, ALLOCATABLE :: icr(:,:)         ! icr(nx,ny)       ! (type)  climate region (Koeppler WMO)
  INTEGER, ALLOCATABLE :: ifm(:,:)         ! ifm(nx,ny)       ! (type)  fuel-model maps (JRC)
  
 

 
  REAL    :: rvar_fillvalue !! missing values in datasets
  INTEGER :: ivar_fillvalue !! missing values in datasets
  REAL, ALLOCATABLE :: rdiag2d(:,:,:) ! nlon, nlat, ndiag2d 
  INTEGER :: ndiag2d=0 ! number of diagnostics (defined in setup)




! --------------------------
! output diagnostics control
! --------------------------
  INTEGER :: ndaydiag ! diagnostics every n days
  INTEGER :: ncidout  ! ncdf file id for output
  INTEGER :: ncidrest  ! ncdf file id for restart

  INTEGER :: LonDimID, LatDimID, timeDimId

! ncids and indices for 2d output fields
  INTEGER, PARAMETER :: ncvar_ps=2
  INTEGER :: ncvar_rain(ncvar_ps)       
  INTEGER :: ncvar_rainclim(ncvar_ps)       
  INTEGER :: ncvar_temp(ncvar_ps)       
  INTEGER :: ncvar_maxtemp(ncvar_ps)
  INTEGER :: ncvar_mintemp(ncvar_ps)
  INTEGER :: ncvar_rh(ncvar_ps)
  INTEGER :: ncvar_maxrh(ncvar_ps)
  INTEGER :: ncvar_minrh(ncvar_ps)
  INTEGER :: ncvar_wspeed(ncvar_ps)
  INTEGER :: ncvar_cc(ncvar_ps)
  INTEGER :: ncvar_snow(ncvar_ps)
  INTEGER :: ncvar_dp(ncvar_ps)
  INTEGER :: ncvar_vs(ncvar_ps)
!  INTEGER :: ncvar_lal(ncvar_ps)

  INTEGER :: ncvar_lsm(ncvar_ps)
  INTEGER :: ncvar_cv(ncvar_ps) 
  INTEGER :: ncvar_slope(ncvar_ps) 
  INTEGER :: ncvar_cr(ncvar_ps)
  INTEGER :: ncvar_fm(ncvar_ps)
! prognostic

  INTEGER :: ncvar_mc1(ncvar_ps)
  INTEGER :: ncvar_mc10(ncvar_ps)
  INTEGER :: ncvar_mc100(ncvar_ps)
  INTEGER :: ncvar_mc1000(ncvar_ps)
  INTEGER :: ncvar_x1000(ncvar_ps)
  INTEGER :: ncvar_mcherb(ncvar_ps)
  INTEGER :: ncvar_mcwood(ncvar_ps)
  INTEGER :: ncvar_rbndryt(ncvar_ps)

  INTEGER :: ncvar_ros(ncvar_ps)
  INTEGER :: ncvar_sc(ncvar_ps)
  INTEGER :: ncvar_erc(ncvar_ps)
  INTEGER :: ncvar_bi(ncvar_ps)
  INTEGER :: ncvar_ic(ncvar_ps)
  INTEGER :: ncvar_mcoi(ncvar_ps)
  INTEGER :: ncvar_loi(ncvar_ps)
  INTEGER :: ncvar_fli(ncvar_ps)
  
!mark 5

 INTEGER :: ncvar_mark5_kb(ncvar_ps)
 INTEGER :: ncvar_mark5_df(ncvar_ps)
 INTEGER :: ncvar_mark5_mc(ncvar_ps)
 INTEGER :: ncvar_mark5_w(ncvar_ps)
 INTEGER :: ncvar_mark5_ros0(ncvar_ps)
 INTEGER :: ncvar_mark5_ros(ncvar_ps)
 INTEGER :: ncvar_mark5_height(ncvar_ps)
 INTEGER :: ncvar_mark5_heightd(ncvar_ps)
 INTEGER :: ncvar_mark5_fdi(ncvar_ps)
 INTEGER :: ncvar_mark5_tsr(ncvar_ps)

!FWI 

 INTEGER :: ncvar_fwi_fwi(ncvar_ps)
 INTEGER :: ncvar_fwi_ffmc(ncvar_ps)
 INTEGER :: ncvar_fwi_dmc(ncvar_ps)
 INTEGER :: ncvar_fwi_dc(ncvar_ps)
 INTEGER :: ncvar_fwi_isi(ncvar_ps)
 INTEGER :: ncvar_fwi_bui(ncvar_ps)
 INTEGER :: ncvar_fwi_dsr(ncvar_ps)
 INTEGER :: ncvar_fwi_danger_risk(ncvar_ps)
END MODULE mo_fire
