! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Variables needed for the mark5 calculation
!> @author Di Giuseppe, F., ECMWF
MODULE mo_mark5

  IMPLICIT NONE

! intermediate (fuel) variables:

  TYPE mark5_fuel_type
    REAL :: moist             !< fuel moisture content
    REAL :: weight            !< fuel weight
    REAL :: curing            !< degree of curing
    REAL :: kb_drought_index  !< Keetch-Byram drought index (0 no drought - 203 severe drought)
    REAL :: drought_factor    !< drought factor
    REAL :: timesincerain
 END type mark5_fuel_type

  TYPE(mark5_fuel_type)       ,  ALLOCATABLE :: mark5_fuel(:,:)

! Mark 5 fuel status:

  TYPE mark5_prop_type
     REAL :: ros_theta0,ros_theta ! spread component and rate of spread
     REAL :: flame_height
     REAL :: flame_distance ! distance at which flame can be seen

  END type mark5_prop_type

  TYPE mark5_prob_type
     REAL :: fire_danger_index    ! fire danger index
  END type mark5_prob_type

  TYPE(mark5_prob_type)       ,  ALLOCATABLE :: mark5_prob(:,:)
  TYPE(mark5_prop_type)       ,  ALLOCATABLE :: mark5_prop(:,:)


END MODULE mo_mark5

