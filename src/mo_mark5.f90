! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Variables needed for the mark5 calculation
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_mark5

    USE mo_constants

    IMPLICIT NONE

    ! intermediate (fuel) variables
    TYPE mark5_fuel_type
        REAL :: moist             = rfillvalue  !< fuel moisture content
        REAL :: weight            = rfillvalue  !< fuel weight
        REAL :: curing            = rfillvalue  !< degree of curing
        REAL :: kb_drought_index  = rfillvalue  !< Keetch-Byram drought index (0 no drought - 203 severe drought)
        REAL :: drought_factor    = rfillvalue  !< drought factor
        REAL :: timesincerain     = rfillvalue
    END TYPE

    ! Mark 5 (fuel) status
    TYPE mark5_prop_type
        REAL :: ros_theta0        = rfillvalue  !< spread component
        REAL :: ros_theta         = rfillvalue  !< spread rate
        REAL :: flame_height      = rfillvalue
        REAL :: flame_distance    = rfillvalue  !< distance at which flame can be seen
    END TYPE

    TYPE mark5_prob_type
        REAL :: fire_danger_index = rfillvalue
    END TYPE

    TYPE(mark5_fuel_type), ALLOCATABLE :: mark5_fuel(:)
    TYPE(mark5_prob_type), ALLOCATABLE :: mark5_prob(:)
    TYPE(mark5_prop_type), ALLOCATABLE :: mark5_prop(:)

END MODULE mo_mark5
