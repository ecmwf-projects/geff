! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Module for meteorological constant fields and fire variables
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_fire

    IMPLICIT NONE

    INTEGER :: ntimestep          ! total integration time as read from input files
    INTEGER :: npoints            ! number of points
    REAL, ALLOCATABLE :: lons(:)  ! longitude values
    REAL, ALLOCATABLE :: lats(:)  ! latitude values

    ! meteorological fields
    INTEGER, ALLOCATABLE :: nhours(:)    ! nhours(ntimestep)   !             integer date
    REAL, ALLOCATABLE :: rtemp(:)        ! rtemp(npoints)      ! (K)         T2m
    REAL, ALLOCATABLE :: rmaxtemp(:)     ! rtempmax(npoints)   ! (K)         T2m max
    REAL, ALLOCATABLE :: rmintemp(:)     ! rtempmin(npoints)   ! (K)         T2m min
    REAL, ALLOCATABLE :: rrh(:)          ! rrh(npoints)        ! (fraction)  RH
    REAL, ALLOCATABLE :: rminrh(:)       ! rrhmin(npoints)     ! (fraction)  RH min
    REAL, ALLOCATABLE :: rmaxrh(:)       ! rrhmax(npoints)     ! (fraction)  RH max
    REAL, ALLOCATABLE :: rrain(:)        ! rrain(npoints)      ! (mm/day)    precip
    REAL, ALLOCATABLE :: rrainclim(:)    ! rrainclim(npoints)  ! (mm/day)    climatic precip

    REAL, ALLOCATABLE :: rcc(:)          ! rcc(npoints)        ! (fraction)  Cloud cover
    REAL, ALLOCATABLE :: rwspeed(:)      ! rwspeed(npoints)    ! (m/s)       Wind speed
    REAL, ALLOCATABLE :: rsnow(:)        ! rsnow(npoints)      ! (mask)      Snow mask
    REAL, ALLOCATABLE :: rdp(:)          ! rdp (npoints)       ! (hr)        Duration of precipitation in the previous 24 hours
    INTEGER, ALLOCATABLE :: ivs(:)       ! ivs(npoints)        ! (num)       index which identifies the vegetation stage

    ! constant fields
    REAL,    ALLOCATABLE :: rlsm(:)      ! rlsm(npoints)       ! (mask)      Land=1 sea=0 0.5=coast mask
    REAL,    ALLOCATABLE :: rcv(:)       ! rcv(npoints)        ! (type)      fraction/cover of high + low vegetation (IFS)
    INTEGER, ALLOCATABLE :: islope(:)    ! slope(npoints)      ! (num)       slope class
    INTEGER, ALLOCATABLE :: icr(:)       ! icr(npoints)        ! (type)      climate region (Koeppler WMO)
    INTEGER, ALLOCATABLE :: ifm(:)       ! ifm(npoints)        ! (type)      fuel-model maps (JRC)

    REAL    :: rvar_fillvalue            ! missing values in datasets
    INTEGER :: ivar_fillvalue            ! missing values in datasets
    REAL, ALLOCATABLE :: rdiag2d(:, :)   ! npoints, ndiag2d

END MODULE mo_fire
