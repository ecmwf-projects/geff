! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Tunable constants for model
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_constants

    IMPLICIT NONE

    REAL, PARAMETER :: reps=1.0e-8             !< small positive threshold number
    REAL, PARAMETER :: r0CtoK=273.15           !< freezing point [K]
    REAL, PARAMETER :: tokmhr=3.6              !< conversion from [m s^-1] to [km h^-1]
    REAL, PARAMETER :: rtopoundsft2=0.0459137  !< conversion from [ton acre-1] to [lb ft^-2]
    REAL, PARAMETER :: std=0.0555              !< part of dead fuel made of inert material
    REAL, PARAMETER :: stl=0.0555              !< part of live fuel made of inert material
    REAL, PARAMETER :: rhod=32                 !< particle density for dead fuels in [lb ft^-3]
    REAL, PARAMETER :: rhol=32                 !< particle density for live fuels in [lb ft^-3]
    REAL, PARAMETER :: sd=0.01                 !< part of dead fuel made up of silica free, non-combustible material
    REAL, PARAMETER :: sl=0.01                 !< part of live fuel made up of silica free, non-combustible material
    
    INTEGER, PARAMETER :: ndayinyear=365

    REAL, PARAMETER :: rfillvalue=-9999.0 ! missing value
    INTEGER, PARAMETER :: ifillvalue=-9999 ! missing value

END MODULE

