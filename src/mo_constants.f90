! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Tunable constants for model
!> @author Di Giuseppe, F., ECMWF
MODULE mo_constants

  IMPLICIT NONE

  INTEGER, PARAMETER :: nweather=4
  INTEGER, PARAMETER :: nclima=5 ! 1 arid 2 semiarid 3 humid 3 wet 5 artic
  INTEGER, PARAMETER :: nslope=5
  INTEGER, PARAMETER :: nlightclass=6
  REAL, PARAMETER    :: rtopoundsft2=0.0459137 ! conversion factor from tons/acre to pounds/ft^2
  REAL, PARAMETER    :: std=0.0555,stl=0.0555  ! part of fuel made of inert material
  REAL, PARAMETER    :: rhol=32,rhod=32        ! particle density for the dead and live fuels in lb/ft^3
  REAL, PARAMETER    :: sd=0.01,sl=0.01        ! part of fuel made up of silica free, non-combustible material

  REAL, DIMENSION(1:nslope)     , PARAMETER :: rslpfct=(/0.267,0.533,1.068,2.134,4.273/)
  REAL, DIMENSION(0:nweather-1) , PARAMETER :: rweathercorrection_temp=(/25.,19.,12.,5./)
  REAL, DIMENSION(0:nweather-1) , PARAMETER :: rweathercorrection_rh=(/0.75,0.83,0.92,1.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: rherbga=(/-70.0,-100.0,-135.0,-185.0,-250.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: rherbgb=(/12.8,14.0,15.5,17.4,20.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: annta=(/-150.5,-187.7,-245.2,-305.2,-400.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: anntb=(/18.4,19.6,22.0,24.3,50.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: perta=(/11.2,-10.3,-42.7,-93.5,-100.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: pertb=(/7.4,8.3,9.8,12.2,15.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: pregrn=(/50.,60.,70.,80.,100./)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: woodga=(/12.5,-5.0,-22.5,-45.0,-60.0/)
  REAL, DIMENSION(1:nclima)     , PARAMETER :: woodgb=(/7.5,8.2,8.9,9.8,10.0/)
  REAL, DIMENSION(1:nlightclass), PARAMETER :: cgrate=(/0.0,12.5,25.0,50.0,100.0,-9999.0/) !lightning discarge rate in strikes/min
  REAL, DIMENSION(1:nlightclass), PARAMETER :: stmdia=(/0.0,3.0,4.0,5.0,7.0,-9999.0/)!width (miles) of corrodor effected
  REAL, DIMENSION(1:nlightclass), PARAMETER :: totwid=(/0.0,7.0,8.0,9.0,11.0,-9999.0/)!
  REAL,                           PARAMETER :: stmspd=30 !mph translational speed of a storm
  REAL,                           PARAMETER :: pnorm1=0.00232,pnorm2=0.99767,pnorm3=0.0000185 ! needed in ignition component


!============================================================================

! math constants
  REAL, PARAMETER :: rpi=ACOS(-1.0)     ! pi
  REAL, PARAMETER :: reps=1.0e-8        ! small positive threshold number
  REAL, PARAMETER :: r0CtoK=273.15      ! freezing point in Kelvin
  REAL, PARAMETER :: tokmhr=3.6        ! from ms-1 to kmhr-1  for wind conversion
  INTEGER,  PARAMETER :: ndayinyear=365

END MODULE mo_constants
