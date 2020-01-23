! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Structures for vegetation stage
!> @author Di Giuseppe, F., ECMWF
MODULE mo_vegstage

  USE mo_constants

  IMPLICIT NONE

  TYPE vegstage_type

     INTEGER :: cured,green_up, green,transition

  END type vegstage_type



CONTAINS

  SUBROUTINE define_vegstage( ivegind, veg_stage  )
 !-------------------------------------------------------------------------------
! Description:
!   Creates a structure with fuel-specific information. The definition of the
!   fuel characteristic is controlled by the  ifueltype parameter
!-------------------------------------------------------------------------------
! Declarations:
!===============================================================================


  IMPLICIT NONE
    TYPE (vegstage_type)        , INTENT (OUT)        ::       &
         veg_stage             !fuel model type depends on classification of vegetation of pixel

    INTEGER,                        INTENT (IN)         ::       &
         ivegind
    !local variables

     INTEGER:: istage,vegstage


    !initialise veg_stage

    veg_stage%cured=0
    veg_stage%green_up=0
    veg_stage%green=0
    veg_stage%transition=0
    istage=1
    DO WHILE (istage <=4 )
       IF ( ivegind .GE. (istage-1)*ndayinyear .and. ivegind .LT. istage*ndayinyear) THEN
          vegstage=istage
       END IF
       istage=istage+1
    END DO

    SELECT CASE (vegstage)
    CASE (1)
       veg_stage%cured=ivegind
    CASE(2)
        veg_stage%green_up=(ivegind-ndayinyear)
     CASE(3)
        veg_stage%green=ivegind-2*ndayinyear
     CASE(4)
        veg_stage%transition=ivegind-3*ndayinyear

    END SELECT
    RETURN

  END SUBROUTINE define_vegstage


END MODULE mo_vegstage

