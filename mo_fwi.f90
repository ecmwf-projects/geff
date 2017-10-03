MODULE mo_fwi
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
! This module contains all the prognostic and diagnostic vaiables and specific  subroutines
!  needed for the calculation of the canadian FWI  index
!
!---------------------------------------------------------
  USE mo_constants
  USE mo_control
  USE mo_fire

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

!  original formulation 
!
!    IF (lat .GE. 15) THEN 
!       df = LfN(mm)
!       !  Use Equatorial numbers 
!    ELSE IF (lat .LT. 15 .AND. lat .GE. -15) THEN
!       df= 1.39
!    ELSE IF (lat .LT. -15) THEN
!       df = LfS(mm)
!    ENDIF
!
! New global implementation 
!Australian fire weather as represented by the 
!McArthur Forest Fire Danger Index and the 
!Canadian Forest Fire Weather Index 
!Andrew J. Dowdy, Graham A. Mills, Klara Finkele and William de Groot 
!CAWCR Technical Report No. 10 
!June 2009 
! equation A25
!

    CALL DayLength(lat,mm,dl)

    df = max (1.43*dl-4.25, -1.6)

 END SUBROUTINE  DryingFactor


   SUBROUTINE  DayLength (lat, mm, dl)
      IMPLICIT NONE

    !   '''Approximates the length of the day given month and latitude'''
     REAL, INTENT(IN):: lat
     INTEGER, INTENT(IN) :: mm
     REAL, INTENT (OUT) ::dl
 
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength46N = (/6.5,7.5,9.0,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7.0,6.0/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength20N = (/7.9,8.4,8.9,9.5,9.9,10.2,10.1,9.7,9.1,8.6,8.1,7.8/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength20S = (/10.1,9.6,9.1,8.5,8.1,7.8,7.9,8.3,8.9,9.4,9.9,10.2/)
   REAL, DIMENSION(1:12)     , PARAMETER ::  DayLength40S = (/11.5,10.5,9.2,7.9,6.8,6.2,6.5,7.4,8.7,10.0,11.2,11.8/)

   IF (lat .LE. 90.0 .AND. lat .GT. 33.0) THEN
      dl=DayLength46N(mm)
   ELSE IF (lat .LE. 33.0 .AND. lat .GT. 0.0) THEN 
      dl=DayLength20N(mm)
   ELSE IF (lat .LE. 0.0 .AND. lat .GT. -30.0) THEN 
      dl=DayLength20S(mm)
   ELSE IF (lat .LE. -30.0 .AND. lat .GT. -90.0) THEN 
      dl=DayLength40S(mm)
   ENDIF

 END SUBROUTINE  DayLength
END MODULE mo_fwi

