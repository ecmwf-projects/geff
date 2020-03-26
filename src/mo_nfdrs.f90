! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Prognostic/diagnostic variables for National Fire Danger Rating System
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_nfdrs

    USE mo_constants
    USE mo_utilities

    IMPLICIT NONE

    ! prognostic
    ! NOTE: first size members are the same as fuelweight_type
    TYPE mc_type
        REAL :: r1hr    = rfillvalue
        REAL :: r10hr   = rfillvalue
        REAL :: r100hr  = rfillvalue
        REAL :: r1000hr = rfillvalue
        REAL :: rherb   = rfillvalue
        REAL :: rwood   = rfillvalue
        REAL :: rx1000  = rfillvalue
        REAL :: rbndryt = rfillvalue
    END TYPE

    ! diagnostic
    TYPE fire_prop_type
        REAL :: ros     = rfillvalue  !< rate of spread
        INTEGER :: sc   = ifillvalue  !< spread component
        INTEGER :: erc  = ifillvalue  !< energy release component
        INTEGER :: bi   = ifillvalue  !< burning index
    END TYPE

    TYPE fire_prob_type
        INTEGER :: ic   = ifillvalue  !< ignition probability
        INTEGER :: mcoi = ifillvalue  !< human caused fire occurrence
        INTEGER :: loi  = ifillvalue  !< lightining coused occurrence index
        REAL :: fli     = rfillvalue  !< fire load index
    END TYPE

    TYPE(mc_type), ALLOCATABLE :: mc(:)
    TYPE(fire_prop_type), ALLOCATABLE :: fire_prop(:)
    TYPE(fire_prob_type), ALLOCATABLE :: fire_prob(:)

CONTAINS

SUBROUTINE emc(temp,rh,remc)
  REAL, INTENT(IN)    :: temp, rh
  REAL, INTENT(OUT) :: remc

  INTEGER :: i,j
  REAL :: tempfh

  !
  !   ! note that in the formula below temperature  has to be in Fahrenheit
  !   the relative humidy in %
  !
  !

  IF ( rh .lt. 10) THEN
     remc=0.03229+0.281073*rh-0.000578*tempfh*rh
  ELSE IF ( rh .ge. 10 .and.  rh .lt. 50) THEN
     remc=2.22749+0.160107*rh-0.014784*tempfh
  ELSE IF ( rh .ge. 50) THEN
     remc=21.0606+0.005565*rh**2-0.00035*rh*tempfh-0.483199*rh
  ENDIF
END SUBROUTINE emc

SUBROUTINE kwet(mc1000,increment,rkwet)

  REAL, INTENT(IN)    :: mc1000,increment
  REAL, INTENT(OUT) :: rkwet

 ! the kwet factor just depends on the slow evolving moisture content
  !

 IF (mc1000 .gt. 25 )THEN
    rkwet=1.0
 ELSE IF (mc1000 .gt. 10 .and. mc1000 .le. 25  )THEN
    rkwet=(0.0333*mc1000 +0.1675)
 ELSE IF (mc1000 .le. 10  )THEN
    rkwet=0.5
 END IF
 !overwrite the above if the increment in moisture content is negative

 IF (increment  .LE.  0) rkwet=1.0
END SUBROUTINE kwet


SUBROUTINE ktmp(tmin,tmax,rktmp)

  REAL, INTENT(IN)    :: tmin,tmax
  REAL, INTENT(OUT)   :: rktmp

  !local
  !
  REAL :: tmaxfh,tminfh
 tminfh=kelvin_to_fahrenheit(tmin)
 tmaxfh=kelvin_to_fahrenheit(tmax)

 rktmp=1.0

 IF ( ((tminfh+tmaxfh)*0.5) <= 50 ) rktmp=0.6
END SUBROUTINE ktmp


END MODULE mo_nfdrs
