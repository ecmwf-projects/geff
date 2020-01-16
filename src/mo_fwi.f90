! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Prognostic/diagnostic variables and calculation of the Canadian FWI
!> index
!> @author Di Giuseppe, F., ECMWF
MODULE mo_fwi

  IMPLICIT NONE

! variables needed for the FWI  calculation

! intermediate (fuel) variables:

  TYPE fwi_risk_type
    REAL :: fwi              ! fire weather index
    REAL :: ffmc             ! fine fuel moisture content
    REAL :: dmc              ! Duff moisture content
    REAL :: dc               ! Drought code
    REAL :: isi              ! initial spread index
    REAL :: bui              ! Built-up index
    REAL :: dsr              ! Daily Severity Rating
    REAL :: danger_risk      ! fwi subdivided into classes
 END type fwi_risk_type

  TYPE(fwi_risk_type)       ,  ALLOCATABLE :: fwi_risk(:,:)

CONTAINS
!=================================


  SUBROUTINE DryingFactor(lat, mm, df)
    IMPLICIT NONE
    REAL,    INTENT(IN)   :: lat
    INTEGER, INTENT(IN)   :: mm
    REAL,    INTENT (OUT) :: df
    REAL, DIMENSION(1:12)     , PARAMETER ::  LfN = (/-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6/)
    REAL, DIMENSION(1:12)     , PARAMETER ::  LfS = (/6.4, 5.0, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8/)
! local
    REAL:: dl

 ! original formulation

    IF (lat .GE. 15) THEN
       df = LfN(mm)
       !  Use Equatorial numbers between -10 and 10
    ELSE IF (lat .LT. 10 .AND. lat .GE. -10) THEN
       df= 1.39
    ELSE IF (lat .LT. -15) THEN
       df = LfS(mm)
    ENDIF
!
!Alternative  global implementation
!Australian fire weather as represented by the
!McArthur Forest Fire Danger Index and the
!Canadian Forest Fire Weather Index
!Andrew J. Dowdy, Graham A. Mills, Klara Finkele and William de Groot
!CAWCR Technical Report No. 10
!June 2009
! equation A25
!
!    CALL DayLength(lat,mm,dl)

!    df = max (1.43*dl-4.25, -1.6)

 END SUBROUTINE  DryingFactor


   SUBROUTINE  DayLength (lat, mm, dl)
      IMPLICIT NONE

    !   '''Approximates the length of the day given month and latitude'''
     REAL, INTENT(IN):: lat
     INTEGER, INTENT(IN) :: mm
     REAL, INTENT (OUT) ::dl
 ! Original implementation from Van Wegner (1989)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength46N = (/6.5,7.5,9.0,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7.0,6.0/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength20N = (/7.9,8.4,8.9,9.5,9.9,10.2,10.1,9.7,9.1,8.6,8.1,7.8/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength20S = (/10.1,9.6,9.1,8.5,8.1,7.8,7.9,8.3,8.9,9.4,9.9,10.2/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength40S = (/11.5,10.5,9.2,7.9,6.8,6.2,6.5,7.4,8.7,10.0,11.2,11.8/)
   ! EFFIS /GWIS implementation
   ! Reference:  Updated source code for calculating fire danger indices in the Canadian FWI System - Wang, Anderson and Suddaby – 2015.
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength_ge_30N = (/6.5,7.5,9.0,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7.0,6.0/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength_ge_10N_lt_30N = (/7.9,8.4,8.9,9.5,9.9,10.2,10.1,9.7,9.1,8.6,8.1,7.8/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength_ge_10S_lt_10N = (/9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength_ge_30S_lt_10S = (/10.1,9.6,9.1,8.5,8.1,7.8,7.9,8.3,8.9,9.4,9.9,10.2/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength_lt_30S = (/11.5,10.5,9.2,7.9,6.8,6.2,6.5,7.4,8.7,10.0,11.2,11.8/)

! Original implementation (Van wegner 1989)
!   IF (lat .LE. 90.0 .AND. lat .GT. 33.0) THEN
!      dl=DayLength46N(mm)
!   ELSE IF (lat .LE. 33.0 .AND. lat .GT. 0.0) THEN
!      dl=DayLength20N(mm)
!   ELSE IF (lat .LE. 0.0 .AND. lat .GT. -30.0) THEN
!      dl=DayLength20S(mm)
!   ELSE IF (lat .LE. -30.0 .AND. lat .GT. -90.0) THEN
!      dl=DayLength40S(mm)
!   ENDIF

!  EFFIS/GWIS implementation

  IF (lat .LE. 90.0 .AND. lat .GE. 30.0) THEN
      dl=DayLength_ge_30N(mm)
   ELSE IF (lat .LT. 30.0 .AND. lat .GE. 10.0) THEN
      dl=DayLength_ge_10N_lt_30N(mm)
   ELSE IF (lat .LT. 10 .AND. lat .GE. -10.0) THEN
      dl=DayLength_ge_10S_lt_10N(mm)
   ELSE IF (lat .LT. -10 .AND. lat .GE. -30.0) THEN
      dl=DayLength_ge_30S_lt_10S(mm)
   ELSE IF (lat .LT. -30 ) THEN
      dl=DayLength_lt_30S(mm)
 END IF

 END SUBROUTINE  DayLength
END MODULE mo_fwi

