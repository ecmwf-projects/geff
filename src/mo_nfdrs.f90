! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Prognostic/diagnostic variables and calculation of the National Fire
!> Danger Rating System
!> @author Di Giuseppe, F., ECMWF
MODULE mo_nfdrs

  USE mo_fire

  IMPLICIT NONE

! variables needed for the nfdrs calculation

! prognostic:

  TYPE mc_type

    REAL :: r1hr,r10hr,r100hr,r1000hr
    REAL :: rherb,rwood
    REAL :: rx1000
    REAL :: rbndryt

  END type mc_type

  TYPE(mc_type)       ,  ALLOCATABLE :: mc(:)

!diagnostic:

  TYPE fire_prop_type
     REAL :: ros ! spread component and rate of spread
     INTEGER :: sc
     INTEGER :: erc,bi ! energy release component,burning index

  END type fire_prop_type

  TYPE fire_prob_type
     INTEGER :: ic     !ignition probability
     INTEGER :: mcoi   ! human caused fire occurrence
     INTEGER :: loi    ! lightining coused occurrence index
     REAL :: fli    ! fire load index
  END type fire_prob_type

  TYPE(fire_prop_type)       ,  ALLOCATABLE :: fire_prop(:)
  TYPE(fire_prob_type)       ,  ALLOCATABLE :: fire_prob(:)


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


  RETURN
END SUBROUTINE emc


INTEGER FUNCTION  juldate(date)
 ! From gregorian date to julian date (do not consider partial day )

  INTEGER, INTENT(IN) :: date

  INTEGER, PARAMETER :: igreg=15+31*(10+12*1582)
  INTEGER:: yyyy,mm,dd
  INTEGER:: jy,jm,jd,ja

  yyyy=date/10000
  mm=(date-yyyy*10000)/100
  dd=date-(yyyy*10000+mm*100)

  IF (mm.gt.2) then
     jy=yyyy
     jm=mm+1
  ELSE
     jy=yyyy-1
     jm=mm+13
  ENDIF
  juldate=int(365.25*jy)+int(30.6001*jm)+dd+1720995
  IF (dd+31*(mm+12*yyyy).GE.igreg) THEN
     ja=int(0.01*jy)
     juldate=juldate+2-ja+int(0.25*ja)
  ENDIF
  RETURN
END FUNCTION juldate


INTEGER FUNCTION  day_of_year(date)
 ! From gregorian date to julian date (do not consider partial day )

  INTEGER, INTENT(IN) :: date

  INTEGER:: yyyy,mm,dd
  INTEGER:: k
  LOGICAL:: LEAP

  yyyy=date/10000
  mm=(date-yyyy*10000)/100
  dd=date-(yyyy*10000+mm*100)

  LEAP = .FALSE.
  IF (MOD(yyyy,4) .EQ. 0) LEAP = .TRUE.
  IF (MOD(yyyy,100) .EQ. 0) LEAP = .FALSE.
  IF (MOD(yyyy,400) .EQ. 0) LEAP = .TRUE.

  IF (LEAP) THEN
     K = 1
  ELSE
     K = 2
  END IF

  day_of_year = ((275*mm)/9) - K*((mm+9)/12) + dd - 30

  RETURN
END FUNCTION day_of_year


INTEGER FUNCTION  gregdate(julian)
! from julian day to gregorian dates as yyyymmdd

  INTEGER, INTENT(IN)  :: julian
  INTEGER :: y,d,m,jul

  jul = julian - 1721119
  y = (4 * jul - 1) / 146097
  jul = 4 * jul - 1 - 146097 * y
  d = jul / 4
  jul = (4 * d + 3) / 1461
  d = 4 * d + 3 - 1461 * jul
  d = (d + 4) / 4
  m = (5 * d - 3) / 153
  d = 5 * d - 3 - 153 * m
  d = (d + 5) / 5
  y = 100 * y + jul
  if ( m < 10 )then
     m = m + 3
  else
     m = m - 9
     y = y + 1
  end if

  gregdate =y*10000+m*100+d
  RETURN
END FUNCTION gregdate


SUBROUTINE ADD_DAY (date,time,hh,newdate)

   INTEGER, INTENT(IN)  :: date,time,hh
   INTEGER, INTENT(OUT) :: newdate

   ! local

   INTEGER:: jul,nday

   nday=INT((hh+time)/24.)


   jul=juldate(date)
   jul=jul+nday
   newdate=gregdate(jul)

  RETURN
END SUBROUTINE ADD_DAY


SUBROUTINE CAL_DAYLIT (lat,date,daylit)

  REAL, INTENT(IN)    :: lat   ! latitude in degrees
  INTEGER,INTENT(IN)  :: date ! yyyymmdd to be transformed in julian day 1-365
  REAL, INTENT(OUT)   :: daylit

! local variables


  INTEGER:: jdate !julian day 1-365
  REAL:: zphi,decl, ratio

! duration of daylight depends on  the latitude and the julian day
! I have introduced a corrcetion for  the 'Arctic Circle' bug.  In previous revisions,
! this module would cause a DOMAIN math error for the ACOS function.
! this would occur for certain winter dates and latitudes above the
! arctic circle; i.e. days when there is no sunlight.  To fix this
! problem, the intermediate variable RATIO was created.  It is set to
! the argument of the ACOS function, checked for the proper domain,
! and then passed to the ACOS function.  In other words, the formula
! now sets DAYLIT (hours of daylight) to zero for days/latitudes outside
! the domain of ACOS.

  jdate=day_of_year(date)
  zphi=lat*0.01745 !duration of daylight
  decl=0.41008*SIN((jdate-82.)*0.01745)


  ratio=tan(zphi)*tan(decl)
  if ((ratio.lt.1.0).and.(ratio.gt.-1.0)) then
     daylit=24*(1.-ACOS(ratio)/rpi)
  else
     daylit=0
  endif
  !if (daylit .gt. 14) then
  !   PRINT *,"decl",decl,lat,zphi,rpi,daylit
  !endif
END SUBROUTINE CAL_DAYLIT


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
   RETURN
END SUBROUTINE kwet


SUBROUTINE ktmp(tmin,tmax,rktmp)

  REAL, INTENT(IN)    :: tmin,tmax
  REAL, INTENT(OUT)   :: rktmp

  !local
  !
  REAL :: tmaxfh,tminfh
 tminfh=(9./5.*tmin)-459.69 !conversion in Fahrenheit
 tmaxfh=(9./5.*tmax)-459.69

 rktmp=1.0

 IF ( ((tminfh+tmaxfh)*0.5) <= 50 ) rktmp=0.6

  RETURN
END SUBROUTINE ktmp


END MODULE mo_nfdrs

