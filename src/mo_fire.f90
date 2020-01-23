! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Meteorological and fire fields/variables
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_fire

    IMPLICIT NONE

    INTEGER :: ntimestep  !< total integration time
    INTEGER :: npoints    !< number of points

    REAL, PARAMETER :: stmspd=30         !< [mile h^-1] translational speed of a storm
    REAL, PARAMETER :: pnorm1=0.00232    !< needed in ignition component
    REAL, PARAMETER :: pnorm2=0.99767    !< needed in ignition component
    REAL, PARAMETER :: pnorm3=0.0000185  !< needed in ignition component

    ! fields/variables defined in time (SIZE() = ntimestep)
    INTEGER, ALLOCATABLE :: nhours(:)

    ! fields/variables defined in space (SIZE() = npoints)
    REAL, ALLOCATABLE :: lons(:)         !< [degree] longitude
    REAL, ALLOCATABLE :: lats(:)         !< [degree] latitude
    REAL, ALLOCATABLE :: rtemp(:)        !< [K] temperature
    REAL, ALLOCATABLE :: rmaxtemp(:)     !< [K] temperature maximum in the last 24h
    REAL, ALLOCATABLE :: rmintemp(:)     !< [K] temperature minimum in the last 24h
    REAL, ALLOCATABLE :: rrh(:)          !< [fraction] relative humidity
    REAL, ALLOCATABLE :: rminrh(:)       !< [fraction] relative humidity minimum in the last 24h
    REAL, ALLOCATABLE :: rmaxrh(:)       !< [fraction] relative humidity maximum in the last 24h
    REAL, ALLOCATABLE :: rrain(:)        !< [mm day^-1] rainfall/accumulated precipitation
    REAL, ALLOCATABLE :: rrainclim(:)    !< [mm day^-1] climatic rainfall/accumulated precipitation
    REAL, ALLOCATABLE :: rcc(:)          !< [fraction] cloud cover
    REAL, ALLOCATABLE :: rwspeed(:)      !< [m s^-1] wind speed
    REAL, ALLOCATABLE :: rsnow(:)        !< [?] ground snow/snow cover
    REAL, ALLOCATABLE :: rdp(:)          !< [h] duration of precipitation in the last 24h
    REAL, ALLOCATABLE :: rlsm(:)         !< [fraction] land-sea mask (1=land, 0=sea, 0.5=coast)
    REAL, ALLOCATABLE :: rcv(:)          !< [index] fractional cover of vegetation (high + low)
    INTEGER, ALLOCATABLE :: ivs(:)       !< [index] vegetation stage (1=cured, 2=pre-green, 3=green, 4=transition, 5=cured/frozen)
    INTEGER, ALLOCATABLE :: islope(:)    !< [index] slope class
    INTEGER, ALLOCATABLE :: icr(:)       !< [index] climatic region (Koeppler WMO, 1=arid, 2=semi-arid, 3=humid, 4=wet, 5=arctic)
    INTEGER, ALLOCATABLE :: ifm(:)       !< [index] fuel model (JRC)

    ! [index] slope class
    REAL, PARAMETER, DIMENSION(1:5) :: rslpfct=(/0.267,0.533,1.068,2.134,4.273/)

    REAL, PARAMETER, DIMENSION(0:3) :: rweathercorrection_temp=(/25.,19.,12.,5./)
    REAL, PARAMETER, DIMENSION(0:3) :: rweathercorrection_rh=(/0.75,0.83,0.92,1.0/)

    ! [index] climatic region
    REAL, PARAMETER, DIMENSION(1:5) :: rherbga=(/-70.0,-100.0,-135.0,-185.0,-250.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: rherbgb=(/12.8,14.0,15.5,17.4,20.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: annta=(/-150.5,-187.7,-245.2,-305.2,-400.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: anntb=(/18.4,19.6,22.0,24.3,50.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: perta=(/11.2,-10.3,-42.7,-93.5,-100.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: pertb=(/7.4,8.3,9.8,12.2,15.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: pregrn=(/50.,60.,70.,80.,100./)
    REAL, PARAMETER, DIMENSION(1:5) :: woodga=(/12.5,-5.0,-22.5,-45.0,-60.0/)
    REAL, PARAMETER, DIMENSION(1:5) :: woodgb=(/7.5,8.2,8.9,9.8,10.0/)

    ! lightning
    REAL, PARAMETER, DIMENSION(1:6) :: cgrate=(/0.0,12.5,25.0,50.0,100.0,-9999.0/)  !< [strike min^-1] lightning discarge rate
    REAL, PARAMETER, DIMENSION(1:6) :: stmdia=(/0.0,3.0,4.0,5.0,7.0,-9999.0/)       !< [mile] width of corrodor effected
    REAL, PARAMETER, DIMENSION(1:6) :: totwid=(/0.0,7.0,8.0,9.0,11.0,-9999.0/)

END MODULE mo_fire
