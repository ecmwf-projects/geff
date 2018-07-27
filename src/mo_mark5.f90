MODULE mo_mark5
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! This module contains all the prognostic and diagnostic vaiables and specific  subroutines
!  needed for the calculation of the Mark-5 index
!
!---------------------------------------------------------
  USE mo_constants
  USE mo_control
  USE mo_fire

  IMPLICIT NONE

! variables needed for the mark5 calculation 

! intermediate (fuel) variables: 
 
  TYPE mark5_fuel_type
 
    REAL :: moist             ! fuel moisture content
    REAL :: weight            !  fuel weight
    REAL :: curing            ! degree of curing
    REAL :: kb_drought_index  ! Keetch-Byram drought index (0 no drought - 203 severe drought)
    REAL :: drought_factor    ! drought factor 
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

