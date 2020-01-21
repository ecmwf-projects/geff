! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Module for meteorological constant fields and fire variables
!> @author Di Giuseppe, F., ECMWF
MODULE mo_fire

    USE mo_control

    IMPLICIT NONE

    ! time dimensions
    INTEGER :: ntimestep        ! total integration time as read from input files
    INTEGER :: npoints

    ! region data
    REAL :: lon,dlon,lat,dlat
    REAL, ALLOCATABLE :: lons(:), lats(:) ! lon lat values

    ! meteorological variables
    INTEGER, ALLOCATABLE  :: nhours(:)   ! nhours(ntimestep)   !          integer date
    REAL, ALLOCATABLE :: rtemp(:)        ! rtemp(npoints)      ! (K)      T2m     as a function of  space for one slice at time
    REAL, ALLOCATABLE :: rmaxtemp(:)     ! rtempmax(npoints)   ! (K)      T2m max as a function of  space for one slice at time
    REAL, ALLOCATABLE :: rmintemp(:)     ! rtempmin(npoints)   ! (K)      T2m min as a function of  space for one slice at time
    REAL, ALLOCATABLE :: rrh(:)          ! rrh(npoints)        ! (frac)   RH as a function of space for one slice at time
    REAL, ALLOCATABLE :: rminrh(:)       ! rrhmin(npoints)     ! (frac)   RH min as a function of space for one slice at time
    REAL, ALLOCATABLE :: rmaxrh(:)       ! rrhmax(npoints)     ! (frac)   RH max as a function of space for one slice at time
    REAL, ALLOCATABLE :: rrain(:)        ! rrain(npoints)      ! (mm/day) precip as a function of space for one slice at time
    REAL, ALLOCATABLE :: rrainclim(:)    ! rrainclim(npoints)  ! (mm/day) climatic precip as a function of space for one slice at time

    REAL, ALLOCATABLE :: rcc(:)          ! rcc(npoints)        ! (frac)   Cloud cover as a function of space for one slice at time
    REAL, ALLOCATABLE :: rwspeed(:)      ! rwspeed(npoints)    ! (m/s)    Wind speed  a function of space for one slice at time
    REAL, ALLOCATABLE :: rsnow(:)        ! rsnow(npoints)      ! (mask)   Snow mask a function of space for one slice at time
    REAL, ALLOCATABLE :: rdp(:)          ! rdp (npoints)       ! (hr)     Duration of precipitation in the previous 24 hours
    INTEGER, ALLOCATABLE :: ivs(:)       ! ivs(npoints)        ! (num)    index which identifies the vegetation stage

    ! constant fields
    REAL,    ALLOCATABLE :: rlsm(:)      ! rlsm(npoints)      ! (mask)  Land=1 sea=0   0.5=coast mask
    REAL,    ALLOCATABLE :: rcv(:)       ! rcv(npoints)      ! (type)  fraction/cover of high + low vegetation (IFS)
    INTEGER, ALLOCATABLE :: islope(:)    ! slope(npoints)     ! (num)   slope class
    INTEGER, ALLOCATABLE :: icr(:)       ! icr(npoints)       ! (type)  climate region (Koeppler WMO)
    INTEGER, ALLOCATABLE :: ifm(:)       ! ifm(npoints)       ! (type)  fuel-model maps (JRC)

    REAL    :: rvar_fillvalue !! missing values in datasets
    INTEGER :: ivar_fillvalue !! missing values in datasets
    REAL, ALLOCATABLE :: rdiag2d(:, :) ! npoints, ndiag2d
    INTEGER :: ndiag2d=0 ! number of diagnostics (defined in setup)

    ! output diagnostics control
    INTEGER :: ndaydiag ! diagnostics every n days
    INTEGER :: ncidout  ! ncdf file id for output
    INTEGER :: ncidrest  ! ncdf file id for restart

    INTEGER :: LonDimID, LatDimID, timeDimId
END MODULE mo_fire
