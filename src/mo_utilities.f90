! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Module for meteorological constant fields and fire variables
!> @brief Tunable constants for model
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_utilities

    IMPLICIT NONE

    PROCEDURE(), POINTER :: assert => assert_dbg

CONTAINS

    SUBROUTINE timer(message, c, t1, t2)
        CHARACTER(LEN=*) :: message
        INTEGER :: c
        REAL :: t1, t2

        CALL cpu_time(t2)
        PRINT *, 'Check point ', c, message, 1000. * (t2 - t1)

        t1 = t2
        c = c + 1
    END SUBROUTINE

    LOGICAL FUNCTION isnan(R)
        REAL, INTENT(IN) :: R
        isnan = R .NE. R
    END FUNCTION

    SUBROUTINE assert_prd(condition, message)
        LOGICAL, INTENT(IN) :: condition
        CHARACTER(LEN=*), INTENT(IN) :: message
        ! Do nothing
    END SUBROUTINE

    SUBROUTINE assert_dbg(condition, message)
        LOGICAL, INTENT(IN) :: condition
        CHARACTER(LEN=*), INTENT(IN) :: message
        IF (.NOT. condition) THEN
            PRINT *, '%ERROR: assertion failed: '//message
            STOP 1
        ENDIF
    END SUBROUTINE

    !> @brief Convert from Gregorian date to Julian day (not considering partial days)
    INTEGER FUNCTION gregorian_to_julian(date)
        INTEGER, INTENT(IN) :: date
        INTEGER, PARAMETER :: igreg = 15+31*(10+12*1582)
        INTEGER :: yyyy, mm, dd, jy, jm, jd, ja

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

        gregorian_to_julian = int(365.25*jy)+int(30.6001*jm)+dd+1720995
        IF (dd+31*(mm+12*yyyy).GE.igreg) THEN
            ja=int(0.01*jy)
            gregorian_to_julian = gregorian_to_julian + 2 - ja + int(0.25*ja)
        ENDIF
    END FUNCTION

    INTEGER FUNCTION day_of_year(date)
        INTEGER, INTENT(IN) :: date
        INTEGER :: yyyy,mm,dd, k
        LOGICAL :: LEAP

        yyyy=date/10000
        mm=(date-yyyy*10000)/100
        dd=date-(yyyy*10000+mm*100)

        LEAP = (MOD(yyyy,4) .EQ. 0)
        IF (MOD(yyyy,100) .EQ. 0) LEAP = .FALSE.
        IF (MOD(yyyy,400) .EQ. 0) LEAP = .TRUE.

        IF (LEAP) THEN
            K = 1
        ELSE
            K = 2
        ENDIF

        day_of_year = ((275*mm)/9) - K*((mm+9)/12) + dd - 30
    END FUNCTION

    !> @brief Convert from Julian day to Gregorian date [yyyymmdd]
    INTEGER FUNCTION julian_to_gregorian(julian)
        INTEGER, INTENT(IN) :: julian
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

        IF (m < 10) THEN
            m = m + 3
        ELSE
            m = m - 9
            y = y + 1
        ENDIF

        julian_to_gregorian = y*10000+m*100+d
    END FUNCTION

    INTEGER FUNCTION add_day(date, time, hh)
        INTEGER, INTENT(IN)  :: date, time, hh
        INTEGER :: jul

        jul = gregorian_to_julian(date) + INT((hh+time)/24.)
        add_day = julian_to_gregorian(jul)
    END FUNCTION

    !> @brief Daylight [h]
    REAL FUNCTION daylight(lat, date)
        REAL, INTENT(IN) :: lat   ! latitude [degree]
        INTEGER, INTENT(IN) :: date ! yyyymmdd to be transformed in julian day 1-365

        INTEGER :: jdate  ! julian day 1-365
        REAL :: zphi, decl, ratio

        ! Duration of daylight depends on the latitude and the Julian day
        ! I have introduced a correction for the 'Arctic Circle' bug. In previous revisions,
        ! this module would cause a DOMAIN math error for the ACOS function.
        ! This would occur for certain winter dates and latitudes above the
        ! arctic circle; i.e. days when there is no sunlight. To fix this
        ! problem, the intermediate variable RATIO was created. It is set to
        ! the argument of the ACOS function, checked for the proper domain,
        ! and then passed to the ACOS function. In other words, the formula
        ! now sets DAYLIGHT (hours of daylight) to zero for days/latitudes outside
        ! the domain of ACOS.

        jdate=day_of_year(date)
        zphi=lat*0.01745 !duration of daylight
        decl=0.41008*SIN((jdate-82.)*0.01745)
        ratio=tan(zphi)*tan(decl)

        IF ((ratio.lt.1.0).and.(ratio.gt.-1.0)) THEN
            daylight = 24*(1.-ACOS(ratio)/ACOS(-1.0))
            RETURN
        ENDIF
        daylight = 0
    END FUNCTION

END MODULE

