! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Structures for the fuel model
!> @author Di Giuseppe, F., ECMWF
MODULE mo_fuelmodel

  USE mo_control

  IMPLICIT NONE


  TYPE fuel_weight
     REAL :: r1hr,r10hr,r100hr,r1000hr
     REAL :: rherb,rwood

  END type fuel_weight

 TYPE surf2volume

     REAL :: r1hr,r10hr,r100hr,r1000hr
     REAL :: rherb,rwood

  END type surf2volume




TYPE fuelmodel_type

  CHARACTER (LEN=1)  :: &
      cfuelmodel_id           ! fuel model id  (A-U)
  CHARACTER (LEN=50)  :: &
      cfuelmodel_description  ! fuel model description
  CHARACTER (LEN=50)  :: &
     herb_type                ! herbaceaous type (perennial, annual)



  REAL :: &
      rdepth                 ,& !depth
      rmxd                   ,& !moisture of extinsion
      rhd                    ,& !heat of combustion (dead)
      rhl                    ,& !heat of combustion (live)
      rscm                   ,& ! spread component value
      rwndfc                    !wind reduction factor


  TYPE(fuel_weight):: weight
  TYPE(surf2volume):: s2v



 END TYPE fuelmodel_type



CONTAINS

  SUBROUTINE define_fuelmodel( ifueltype, fuelmodel )
 !-------------------------------------------------------------------------------
! Description:
!   Creates a structure with fuel-specific information. The definition of the
!   fuel characteristic is controlled by the  ifueltype parameter
!-------------------------------------------------------------------------------
! Declarations:
!===============================================================================

    IMPLICIT NONE

    TYPE (fuelmodel_type)        , INTENT (OUT)        ::       &
         fuelmodel             !fuel model type depends on classification of vegetation of pixel

    INTEGER,                        INTENT (IN)         ::       &
       ifueltype

  ! initialization of the pointer values

    fuelmodel%cfuelmodel_id="0"
    fuelmodel%cfuelmodel_description="none"
    fuelmodel%herb_type="perennial" ! otherwise expressly specified annual
    fuelmodel%rdepth= rfillvalue
    fuelmodel%rmxd=rfillvalue     ! (%)
    fuelmodel%rhd=rfillvalue   ! watts/kg
    fuelmodel%rhl=rfillvalue   ! watts/kg
    fuelmodel%rscm=rfillvalue
    fuelmodel%rwndfc=rfillvalue

    fuelmodel%weight%r1hr    =rfillvalue
    fuelmodel%weight%r10hr   =rfillvalue
    fuelmodel%weight%r100hr  =rfillvalue
    fuelmodel%weight%r1000hr =rfillvalue
    fuelmodel%weight%rherb   =rfillvalue
    fuelmodel%weight%rwood   =rfillvalue

    fuelmodel%s2v%r1hr    =rfillvalue
    fuelmodel%s2v%r10hr   =rfillvalue
    fuelmodel%s2v%r100hr  =rfillvalue
    fuelmodel%s2v%r1000hr =rfillvalue
    fuelmodel%s2v%rherb   =rfillvalue
    fuelmodel%s2v%rwood   =rfillvalue

    SELECT CASE (ifueltype)
       CASE(1)
          fuelmodel%cfuelmodel_id ="A"
          fuelmodel%cfuelmodel_description="Western Grasses(annual)"
          fuelmodel%herb_type="annual"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =0.2
          fuelmodel%weight%r10hr     =0.00
          fuelmodel%weight%r100hr    =0.00
          fuelmodel%weight%r1000hr   =0.00
          fuelmodel%weight%rwood     =0.0
          fuelmodel%weight%rherb     =0.3
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =3000
          fuelmodel%s2v%r10hr       =0
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =0
          fuelmodel%s2v%rherb       =3000
          !Fuel depth (ft)
          fuelmodel%rdepth=0.8
          !Extinsion (%)
          fuelmodel%rmxd=15
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=300
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.6
    CASE(2)
          fuelmodel%cfuelmodel_id ="B"
          fuelmodel%cfuelmodel_description="Chaparral"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =3.5
          fuelmodel%weight%r10hr     =4
          fuelmodel%weight%r100hr    =0.5
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rherb     =0.0
          fuelmodel%weight%rwood     =11.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =700
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1250  !weight in 1/ meter
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=4.5
          !Extinsion (%)
          fuelmodel%rmxd=15
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=9500
          fuelmodel%rhl=9500
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=58
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.5
  CASE(3)
          fuelmodel%cfuelmodel_id ="C"
          fuelmodel%cfuelmodel_description="Pine Grass Savannah"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =0.4
          fuelmodel%weight%r10hr     =1
          fuelmodel%weight%r100hr    =0.0
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.8
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =2000
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2500
          !Fuel depth (ft)
          fuelmodel%rdepth=0.75
          !Extinsion (%)
          fuelmodel%rmxd=20
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=32
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
  CASE(4)
          fuelmodel%cfuelmodel_id ="D"
          fuelmodel%cfuelmodel_description="Southern Rough"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =2.0
          fuelmodel%weight%r10hr     =1
          fuelmodel%weight%r100hr    =0.0
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =3.0
          fuelmodel%weight%rherb      =0.75
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1250
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =1500
          !Fuel depth (ft)
          fuelmodel%rdepth=2.0
          !Extinsion (%)
          fuelmodel%rmxd=30
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=9000
          fuelmodel%rhl=9000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=25
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
 CASE(5)
          fuelmodel%cfuelmodel_id ="E"
          fuelmodel%cfuelmodel_description="Hardwood Litter"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.5
          fuelmodel%weight%r10hr     =1
          fuelmodel%weight%r100hr    =0.0
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =3.0
          fuelmodel%weight%rherb      =0.75
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1250
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=0.4
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=25
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
  CASE(6)
          fuelmodel%cfuelmodel_id ="F"
          fuelmodel%cfuelmodel_description="Intermediate Brush"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =2.5
          fuelmodel%weight%r10hr     =2
          fuelmodel%weight%r100hr    =2.5
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =9.0
          fuelmodel%weight%rherb      =0.0
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =700
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1250
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=4.5
          !Extinsion (%)
          fuelmodel%rmxd=15
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=9500
          fuelmodel%rhl=9500
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=24
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.5
  CASE(7)
          fuelmodel%cfuelmodel_id ="G"
          fuelmodel%cfuelmodel_description="Short Needle, Heavy Dead"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =2.5
          fuelmodel%weight%r10hr     =2
          fuelmodel%weight%r100hr    =5.0
          fuelmodel%weight%r1000hr   =12
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =2000
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=1
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=30
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
  CASE(8)
          fuelmodel%cfuelmodel_id ="H"
          fuelmodel%cfuelmodel_description="Short Needle, Normal Dead"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.5
          fuelmodel%weight%r10hr     =1
          fuelmodel%weight%r100hr    =2.0
          fuelmodel%weight%r1000hr   =2
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =2000
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=0.3
          !Extinsion (%)
          fuelmodel%rmxd=20
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=8
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
  CASE(9)
          fuelmodel%cfuelmodel_id ="I"
          fuelmodel%cfuelmodel_description="Heavy Slash"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =12
          fuelmodel%weight%r10hr     =12
          fuelmodel%weight%r100hr    =10
          fuelmodel%weight%r1000hr   =10
          fuelmodel%weight%rwood      =0
          fuelmodel%weight%rherb      =0
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =0
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=2
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=65
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.5

 CASE(10)
          fuelmodel%cfuelmodel_id ="J"
          fuelmodel%cfuelmodel_description="Intermediate Slash"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =7
          fuelmodel%weight%r10hr     =7
          fuelmodel%weight%r100hr    =6
          fuelmodel%weight%r1000hr   =5.5
          fuelmodel%weight%rwood      =0
          fuelmodel%weight%rherb      =0
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =0
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=1.3
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=44
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.5
CASE(11)
          fuelmodel%cfuelmodel_id ="K"
          fuelmodel%cfuelmodel_description="Light Slash"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =2.5
          fuelmodel%weight%r10hr     =2.5
          fuelmodel%weight%r100hr    =2
          fuelmodel%weight%r1000hr   =2.5
          fuelmodel%weight%rwood      =0
          fuelmodel%weight%rherb      =0
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =0
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=0.6
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=23
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.5
CASE(12)
          fuelmodel%cfuelmodel_id ="L"
          fuelmodel%cfuelmodel_description="Western Perennial Grass"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =0.25
          fuelmodel%weight%r10hr     =1.5
          fuelmodel%weight%r100hr    =0
          fuelmodel%weight%r1000hr   =0
          fuelmodel%weight%rwood      =0
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =2000
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =0
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=1.0
          !Extinsion (%)
          fuelmodel%rmxd=15
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=178
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.6
CASE(13)
          fuelmodel%cfuelmodel_id ="N"
          fuelmodel%cfuelmodel_description="Sawgrass"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.5
          fuelmodel%weight%r10hr     =3.0
          fuelmodel%weight%r100hr    =0
          fuelmodel%weight%r1000hr   =0
          fuelmodel%weight%rwood      =2.0
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1600
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=3
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8700
          fuelmodel%rhl=8700
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=167
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.6
CASE(14)
          fuelmodel%cfuelmodel_id ="O"
          fuelmodel%cfuelmodel_description="High Poossin"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =2.0
          fuelmodel%weight%r10hr     =1.0
          fuelmodel%weight%r100hr    =3
          fuelmodel%weight%r1000hr   =2
          fuelmodel%weight%rwood      =7.0
          fuelmodel%weight%rherb      =0.0
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=4
          !Extinsion (%)
          fuelmodel%rmxd=30
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=9000
          fuelmodel%rhl=9000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=99
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.5
CASE(15)
          fuelmodel%cfuelmodel_id ="P"
          fuelmodel%cfuelmodel_description="Southern Pine"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.0
          fuelmodel%weight%r10hr     =2.5
          fuelmodel%weight%r100hr    =0.5
          fuelmodel%weight%r1000hr   =0
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.0
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1750
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =0
          !Fuel depth (ft)
          fuelmodel%rdepth=0.4
          !Extinsion (%)
          fuelmodel%rmxd=30
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=14
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
CASE(16)
          fuelmodel%cfuelmodel_id ="Q"
          fuelmodel%cfuelmodel_description="Alaska Black Spruce"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =2.0
          fuelmodel%weight%r10hr     =0.5
          fuelmodel%weight%r100hr    =2
          fuelmodel%weight%r1000hr   =1
          fuelmodel%weight%rwood      =4
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =1200
          fuelmodel%s2v%rherb       =1500
          !Fuel depth (ft)
          fuelmodel%rdepth=3
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=59
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
CASE(17)
          fuelmodel%cfuelmodel_id ="R"
          fuelmodel%cfuelmodel_description="Hardwood Litter, summer"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =0.5
          fuelmodel%weight%r10hr     =0.5
          fuelmodel%weight%r100hr    =0.5
          fuelmodel%weight%r1000hr   =0
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=0.25
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=6
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4
CASE(18)
          fuelmodel%cfuelmodel_id ="S"
          fuelmodel%cfuelmodel_description="Tundra"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =0.5
          fuelmodel%weight%r10hr     =0.5
          fuelmodel%weight%r100hr    =0.5
          fuelmodel%weight%r1000hr   =0.5
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =8
          fuelmodel%s2v%rwood       =1200
          fuelmodel%s2v%rherb       =1500
          !Fuel depth (ft)
          fuelmodel%rdepth=0.4
          !Extinsion (%)
          fuelmodel%rmxd=25
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=17
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.6
CASE(19)
          fuelmodel%cfuelmodel_id ="T"
          fuelmodel%cfuelmodel_description="Sagebrush-Grass"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.0
          fuelmodel%weight%r10hr     =1.5
          fuelmodel%weight%r100hr    =0
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =2.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =2500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=1.25
          !Extinsion (%)
          fuelmodel%rmxd=15
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=73
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.6
CASE(20)
          fuelmodel%cfuelmodel_id ="U"
          fuelmodel%cfuelmodel_description="Western Pines"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.5
          fuelmodel%weight%r10hr     =0
          fuelmodel%weight%r100hr    =1
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =0.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =1750
          fuelmodel%s2v%r10hr       =0
          fuelmodel%s2v%r100hr      =30
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=0.5
          !Extinsion (%)
          fuelmodel%rmxd=20
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=16
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.4

        CASE DEFAULT

          fuelmodel%cfuelmodel_id ="T"
          fuelmodel%cfuelmodel_description="Sagebrush-Grass"
          !fuel loads (tons/acre)
          fuelmodel%weight%r1hr      =1.0
          fuelmodel%weight%r10hr     =1.5
          fuelmodel%weight%r100hr    =0
          fuelmodel%weight%r1000hr   =0.0
          fuelmodel%weight%rwood      =2.5
          fuelmodel%weight%rherb      =0.5
          !Surface to volume ratio (ft-1)
          fuelmodel%s2v%r1hr        =2500
          fuelmodel%s2v%r10hr       =109
          fuelmodel%s2v%r100hr      =0
          fuelmodel%s2v%r1000hr     =0
          fuelmodel%s2v%rwood       =1500
          fuelmodel%s2v%rherb       =2000
          !Fuel depth (ft)
          fuelmodel%rdepth=1.25
          !Extinsion (%)
          fuelmodel%rmxd=15
          ! Dead and live fuel heat of combustion (Btu/lb)
          fuelmodel%rhd=8000
          fuelmodel%rhl=8000
          !Assigned spread component value when all
          ! ignitions become reportable fires
          fuelmodel%rscm=73
          !  Wind reduction factor (from 20-foot to midflame height)
          fuelmodel%rwndfc=0.6


       END SELECT



       RETURN

!CASE(21)
!   fuelmodel%cfuelmodel_id ="U"
!
!CASE(22)
!CASE(23)
!CASE(24)
!CASE(25)
!CASE(26)
!
!21  Water        Water
!22  Barren       Barren
!23  Marsh        Marsh
!24  Ice          Snow and Ice
!25  Urban        Urban
!26  Agriculture  Agriculture
!

     END SUBROUTINE define_fuelmodel

   END MODULE mo_fuelmodel

