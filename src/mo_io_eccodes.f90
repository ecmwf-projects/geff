! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief ecCodes GRIB I/O
!> @author Maciel, P., ECMWF
MODULE mo_io_eccodes

    USE eccodes

    USE mo_constants
    USE mo_control
    USE mo_fire
    USE mo_fwi
    USE mo_mark5
    USE mo_nfdrs

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: io_getdata
    PUBLIC :: io_initialize
    PUBLIC :: io_open_input
    PUBLIC :: io_open_output
    PUBLIC :: io_set_grid
    PUBLIC :: io_setdown
    PUBLIC :: io_write_constant_fields
    PUBLIC :: io_write_restart
    PUBLIC :: io_write_results

    PROCEDURE(), POINTER :: assert => assert_dbg

    ! GRIB paramIds
    INTEGER, PARAMETER :: rain_pid    = 212001
    INTEGER, PARAMETER :: temp_pid    = 212002
    INTEGER, PARAMETER :: maxtemp_pid = 212003
    INTEGER, PARAMETER :: mintemp_pid = 212004
    INTEGER, PARAMETER :: rh_pid      = 212005
    INTEGER, PARAMETER :: maxrh_pid   = 212006
    INTEGER, PARAMETER :: minrh_pid   = 212007
    INTEGER, PARAMETER :: cc_pid      = 212008
    INTEGER, PARAMETER :: snow_pid    = 212009
    INTEGER, PARAMETER :: wspeed_pid  = 212010
    INTEGER, PARAMETER :: dp_pid      = 212011
    INTEGER, PARAMETER :: vs_pid      = 212012

    INTEGER, PARAMETER :: mc_r1hr_pid        = 212013
    INTEGER, PARAMETER :: mc_r10hr_pid       = 212014
    INTEGER, PARAMETER :: mc_r100hr_pid      = 212015
    INTEGER, PARAMETER :: mc_r1000hr_pid     = 212016
    INTEGER, PARAMETER :: mc_rx1000_pid      = 212017
    INTEGER, PARAMETER :: mc_rherb_pid       = 212018
    INTEGER, PARAMETER :: mc_rwood_pid       = 212019

    INTEGER, PARAMETER :: fire_prop_ros_pid  = 212020
    INTEGER, PARAMETER :: fire_prop_sc_pid   = 212021
    INTEGER, PARAMETER :: fire_prop_erc_pid  = 212022
    INTEGER, PARAMETER :: fire_prop_bi_pid   = 212023

    INTEGER, PARAMETER :: fire_prob_ic_pid   = 212024
    INTEGER, PARAMETER :: fire_prob_mcoi_pid = 212025
    INTEGER, PARAMETER :: fire_prob_loi_pid  = 212026
    INTEGER, PARAMETER :: fire_prob_fli_pid  = 212027

    INTEGER, PARAMETER :: mark5_fuel_kb_drought_index_pid  = 212028
    INTEGER, PARAMETER :: mark5_fuel_drought_factor_pid    = 212029
    INTEGER, PARAMETER :: mark5_fuel_moist_pid             = 212030
    INTEGER, PARAMETER :: mark5_fuel_weight_pid            = 212031

    INTEGER, PARAMETER :: mark5_prop_ros_theta0_pid        = 212032
    INTEGER, PARAMETER :: mark5_prop_ros_theta_pid         = 212033
    INTEGER, PARAMETER :: mark5_prop_flame_height_pid      = 212034
    INTEGER, PARAMETER :: mark5_prop_flame_distance_pid    = 212035
    INTEGER, PARAMETER :: mark5_prob_fire_danger_index_pid = 212036

    INTEGER, PARAMETER :: fwi_risk_fwi_pid         = 212037
    INTEGER, PARAMETER :: fwi_risk_ffmc_pid        = 212038
    INTEGER, PARAMETER :: fwi_risk_dmc_pid         = 212039
    INTEGER, PARAMETER :: fwi_risk_dc_pid          = 212040

    INTEGER, PARAMETER :: fwi_risk_isi_pid         = 212041
    INTEGER, PARAMETER :: fwi_risk_bui_pid         = 212042
    INTEGER, PARAMETER :: fwi_risk_dsr_pid         = 212043
    INTEGER, PARAMETER :: fwi_risk_danger_risk_pid = 212044

    TYPE :: GribField
       INTEGER :: fd
       INTEGER :: handle
       INTEGER :: paramId
       CHARACTER(LEN=20) :: shortName
       CHARACTER(LEN=200) :: name
       INTEGER :: Npoints
       INTEGER :: count
    CONTAINS
       PROCEDURE, PUBLIC :: open_as_input => gribfield_open_as_input
       PROCEDURE, PUBLIC :: open_as_output => gribfield_open_as_output
       PROCEDURE, PUBLIC :: open_as_restart => gribfield_open_as_restart
       PROCEDURE, PUBLIC :: next => gribfield_next
       PROCEDURE, PUBLIC :: close => gribfield_close
       PROCEDURE, PUBLIC :: coordinates => gribfield_coordinates
       PROCEDURE, PUBLIC :: values => gribfield_values
       PROCEDURE, PUBLIC :: values_as_integer => gribfield_values_as_integer
       PROCEDURE, PUBLIC :: header => gribfield_header
       PROCEDURE, PUBLIC :: write_other_field => gribfield_write_other_field
       PROCEDURE, PUBLIC :: same_geometry => gribfield_same_geometry
    END TYPE

    ! missing value indicator (NetCDF's rfillValue, rpopdensity<0 checks imply negative)
    REAL, PARAMETER :: missingValue = -1.e-20

    TYPE(GribField), TARGET :: input(18)

    TYPE(GribField), POINTER :: grib_lsm      => input(1)
    TYPE(GribField), POINTER :: grib_rain     => input(2)
    TYPE(GribField), POINTER :: grib_temp     => input(3)
    TYPE(GribField), POINTER :: grib_maxtemp  => input(4)
    TYPE(GribField), POINTER :: grib_mintemp  => input(5)
    TYPE(GribField), POINTER :: grib_rh       => input(6)
    TYPE(GribField), POINTER :: grib_maxrh    => input(7)
    TYPE(GribField), POINTER :: grib_minrh    => input(8)
    TYPE(GribField), POINTER :: grib_cc       => input(9)
    TYPE(GribField), POINTER :: grib_wspeed   => input(10)
    TYPE(GribField), POINTER :: grib_snow     => input(11)
    TYPE(GribField), POINTER :: grib_dp       => input(12)
    TYPE(GribField), POINTER :: grib_vs       => input(13)

    TYPE(GribField), POINTER :: grib_cr       => input(14)
    TYPE(GribField), POINTER :: grib_fm       => input(15)
    TYPE(GribField), POINTER :: grib_slope    => input(16)
    TYPE(GribField), POINTER :: grib_cv       => input(17)
    TYPE(GribField), POINTER :: grib_rainclim => input(18)


CONTAINS


    SUBROUTINE io_getdata(istep)
        INTEGER, INTENT(IN) :: istep
        INTEGER :: i
        CHARACTER(LEN=2) :: str_i

        CALL assert(grib_rain%next(), 'grib_rain%next()')
        CALL assert(grib_temp%next(), 'grib_temp%next()')
        CALL assert(grib_maxtemp%next(), 'grib_maxtemp%next()')
        CALL assert(grib_mintemp%next(), 'grib_mintemp%next()')
        CALL assert(grib_rh%next(), 'grib_rh%next()')
        CALL assert(grib_maxrh%next(), 'grib_maxrh%next()')
        CALL assert(grib_minrh%next(), 'grib_minrh%next()')
        CALL assert(grib_cc%next(), 'grib_cc%next()')
        CALL assert(grib_wspeed%next(), 'grib_wspeed%next()')
        CALL assert(grib_snow%next(), 'grib_snow%next()')
        CALL assert(grib_dp%next(), 'grib_dp%next()')
        CALL assert(grib_vs%next(), 'grib_vs%next()')

        CALL grib_rain%values(rrain)
        CALL grib_temp%values(rtemp)
        CALL grib_mintemp%values(rmintemp)
        CALL grib_maxtemp%values(rmaxtemp)
        CALL grib_rh%values(rrh)
        CALL grib_minrh%values(rminrh)
        CALL grib_maxrh%values(rmaxrh)
        CALL grib_cc%values(rcc)
        CALL grib_wspeed%values(rwspeed)
        CALL grib_snow%values(rsnow)
        CALL grib_dp%values(rdp)
        CALL grib_vs%values_as_integer(ivs)
    END SUBROUTINE


    SUBROUTINE io_initialize
        INTEGER :: i

        IF (TRIM(init_file(1:4)) == 'rest') THEN
            PRINT *, "Initialization type: exact initialization from '" // init_file // "'"
            CALL assert(.FALSE., "NOTIMP")
            RETURN
        ENDIF

        PRINT *,'Initialization type: artificial conditions'

        ! Here the loop is necessary
        ! Dead fuel
        DO i = 1, npoints
            IF (rlsm(i) .GT. 0.00001 )   THEN
                mc(i)%r100hr  =  5. + (5. * icr(i))
                mc(i)%r1000hr = 10. + (5. * icr(i))
                mc(i)%rbndryt = 10. + (5. * icr(i))
                mc(i)%rx1000  = 10. + (5. * icr(i))

                mark5_fuel(i)%kb_drought_index = 0. ! saturation of the soil
                mark5_fuel(i)%timesincerain = 0.

                ! For the FWI moisture code values (FFMC=85, DMC=6, DC=15) provide a
                ! reasonable set of conditions for post-snowmelt springtime conditions
                ! in eastern/central Canada, the Northern U.S., and Alaska; physically
                ! these spring start-up values represent about 3 days of drying from
                ! complete moisture saturation of the fuel layer. In areas or years
                ! with particularly dry winters (or parts of the world without
                ! significant snow cover) these start-up values for FFMC and DMC may
                ! still be appropriate as these two elements respond relatively quickly
                ! to changes in the weather. The DC component however, because of its
                ! very long response time, can take considerable time to adjust to
                ! unrealistic initial values and some effort to estimate over-winter
                ! value of the DC may be necessary. Users can look again to Lawson and
                ! Armitage (2008) for a more detailed description of code calculation
                ! startup issues and the over-winter adjustment process.
                fwi_risk(i)%ffmc = 85.
                fwi_risk(i)%dmc  =  6.
                fwi_risk(i)%dc   = 15.
            END IF
        ENDDO
    END SUBROUTINE


    SUBROUTINE io_open_input
        PRINT *, "pass"
    END SUBROUTINE


    SUBROUTINE io_open_output
        INTEGER, ALLOCATABLE :: emptyInteger(:)
        CHARACTER(LEN=1), ALLOCATABLE :: emptyCharacter(:)
        INTEGER :: i

        ! Open output file
        ncidout = 0
        CALL codes_open_file(ncidout, output_file, 'w')
        CALL assert(ncidout /= 0, 'codes_open_file (w): '//output_file)

        ! set the arrays/3d structures and diagnostics indices
        ndiag2d  = 50  ! FIXME: hardcoded!
        ndaydiag =  1  ! FIXME: hardcoded!

        CALL assert(.NOT. ALLOCATED(rdiag2d), '.NOT. ALLOCATED(nhours)')
        CALL assert(input(1)%Npoints > 0, 'Npoints > 0')
        ALLOCATE(rdiag2d(npoints, ndiag2d))
        rdiag2d = 0.

        ! set dates/times
        CALL assert(.NOT. ALLOCATED(nhours), '.NOT. ALLOCATED(nhours)')
        ALLOCATE(nhours(ntimestep))
        DO i = 1, ntimestep
            nhours(i) = (i - 1) * dt
        ENDDO
    END SUBROUTINE


    SUBROUTINE io_set_grid
        INTEGER :: i

        CALL grib_lsm%open_as_input(lsmfile ,'Land-sea mask', [clsmvar], [172])
        CALL assert(grib_lsm%Npoints > 0)
        npoints = grib_lsm%Npoints

        PRINT *, '*** data dimensions  *** ', npoints
        CALL assert(.NOT. ALLOCATED(lons) .AND. .NOT. ALLOCATED(lats))
        ALLOCATE(lats(npoints))
        ALLOCATE(lons(npoints))

        CALL assert(grib_lsm%next(), 'grib_lsm%next()')
        CALL grib_lsm%coordinates(lats, lons)

        CALL grib_temp%open_as_input(tempfile, 'Temperature', [ctempvar], [167])
        CALL grib_mintemp%open_as_input(maxtempfile, 'Maximum daily temperature', [ctempvar], [167])
        CALL grib_maxtemp%open_as_input(mintempfile, 'Minimum daily temperature', [ctempvar], [167])
        CALL grib_rh%open_as_input(rhfile ,'Relative humidity', [crhvar], [168])
        CALL grib_maxrh%open_as_input(maxrhfile ,'Maximum daily relative humidity', [crhvar], [168])
        CALL grib_minrh%open_as_input(minrhfile ,'Minimum daily relative humidity', [crhvar], [168])
        CALL grib_rain%open_as_input(rainfile ,'Rainfall', [crainvar], [228])
        CALL grib_cc%open_as_input(ccfile ,'Cloud cover', [cccvar], [164])
        CALL grib_wspeed%open_as_input(wspeedfile ,'Wind speed', [cwspeedvar], [165, 166])
        CALL grib_snow%open_as_input(snowfile ,'Ground snow', [csnowvar], [141])
        CALL grib_dp%open_as_input(dpfile ,'Duration of precipitation in the previous 24h', [cdpvar], [228])
        CALL grib_vs%open_as_input(vsfile ,'Vegetation stage', [cvsvar], [66])
        CALL grib_cr%open_as_input(crfile ,'Climate region', [ccrvar], [0])
        CALL grib_fm%open_as_input(fmfile ,'Fuel model', [cfmvar], [0])
        CALL grib_slope%open_as_input(slopefile ,'Slope of sub-gridscale orography', [cslopevar], [163])
        CALL grib_cv%open_as_input(cvfile ,'Fractional coverage for vegetation (high + low)', [ccvvar], [28])
        CALL grib_rainclim%open_as_input(rainclimfile ,'Climate rainfall', [crainclimvar], [228])

        ntimestep = MAXVAL(input(:)%count)
        CALL assert(input(1)%count == 1 .OR.input(1)%count == ntimestep)

        DO i = 2, SIZE(input)
            CALL assert(input(i)%same_geometry(input(1)))
            CALL assert(input(i)%count == 1 .OR.input(i)%count == ntimestep)
        END DO
    END SUBROUTINE


    SUBROUTINE io_setdown
        CALL assert(.FALSE., "NOTIMP")
    END SUBROUTINE


    SUBROUTINE io_write_constant_fields
        CALL assert(.FALSE., "NOTIMP")
    END SUBROUTINE


    SUBROUTINE io_write_restart
        CALL assert(.FALSE., "NOTIMP")
    END SUBROUTINE


    SUBROUTINE io_write_results(istep)
        INTEGER, INTENT(IN) :: istep
        REAL, ALLOCATABLE :: tmp(:)

        CALL assert(ncidout /= 0, 'Output file open: '//output_file)

        CALL write_field(rain_pid, rrain)
        CALL write_field(temp_pid, rtemp)
        CALL write_field(maxtemp_pid, rmaxtemp)
        CALL write_field(mintemp_pid, rmintemp)
        CALL write_field(rh_pid, rrh)
        CALL write_field(maxrh_pid, rmaxrh)
        CALL write_field(minrh_pid, rminrh)
        CALL write_field(cc_pid, rcc)
        CALL write_field(snow_pid, rsnow)
        CALL write_field(wspeed_pid, rwspeed)
        CALL write_field(dp_pid, rdp)
        CALL write_field_from_integer(vs_pid, ivs)

        CALL write_field(mc_r1hr_pid, mc(:)%r1hr)
        CALL write_field(mc_r10hr_pid, mc(:)%r10hr)
        CALL write_field(mc_r100hr_pid, mc(:)%r100hr)
        CALL write_field(mc_r1000hr_pid, mc(:)%r1000hr)

        CALL write_field(mc_rx1000_pid, mc(:)%rx1000)
        CALL write_field(mc_rherb_pid, mc(:)%rherb)
        CALL write_field(mc_rwood_pid, mc(:)%rwood)

        CALL write_field(fire_prop_ros_pid, fire_prop(:)%ros)
        CALL write_field_from_integer(fire_prop_sc_pid, fire_prop(:)%sc)
        CALL write_field_from_integer(fire_prop_erc_pid, fire_prop(:)%erc)
        CALL write_field_from_integer(fire_prop_bi_pid, fire_prop(:)%bi)

        CALL write_field_from_integer(fire_prob_ic_pid, fire_prob(:)%ic)
        CALL write_field_from_integer(fire_prob_mcoi_pid, fire_prob(:)%mcoi)
        CALL write_field_from_integer(fire_prob_loi_pid, fire_prob(:)%loi)
        CALL write_field(fire_prob_fli_pid, fire_prob(:)%fli)

        CALL write_field(mark5_fuel_kb_drought_index_pid, mark5_fuel(:)%kb_drought_index)
        CALL write_field(mark5_fuel_drought_factor_pid, mark5_fuel(:)%drought_factor)
        CALL write_field(mark5_fuel_moist_pid, mark5_fuel(:)%moist)
        CALL write_field(mark5_fuel_weight_pid, mark5_fuel(:)%weight)

        CALL write_field(mark5_prop_ros_theta0_pid, mark5_prop(:)%ros_theta0)
        CALL write_field(mark5_prop_ros_theta_pid, mark5_prop(:)%ros_theta)
        CALL write_field(mark5_prop_flame_height_pid, mark5_prop(:)%flame_height)
        CALL write_field(mark5_prop_flame_distance_pid, mark5_prop(:)%flame_distance)
        CALL write_field(mark5_prob_fire_danger_index_pid, mark5_prob(:)%fire_danger_index)

        CALL write_field(fwi_risk_fwi_pid, fwi_risk(:)%fwi)
        CALL write_field(fwi_risk_ffmc_pid, fwi_risk(:)%ffmc)
        CALL write_field(fwi_risk_dmc_pid, fwi_risk(:)%dmc)
        CALL write_field(fwi_risk_dc_pid,  fwi_risk(:)%dc)

        CALL write_field(fwi_risk_isi_pid, fwi_risk(:)%isi)
        CALL write_field(fwi_risk_bui_pid, fwi_risk(:)%bui)
        CALL write_field(fwi_risk_dsr_pid, fwi_risk(:)%dsr)
        CALL write_field(fwi_risk_danger_risk_pid, fwi_risk(:)%danger_risk)

        IF (ALLOCATED(tmp)) THEN
            DEALLOCATE(tmp)
        ENDIF

    CONTAINS

        SUBROUTINE write_field(paramid, values)
            INTEGER, INTENT(IN) :: paramid
            REAL, INTENT(IN) :: values(:)
            ! Note: grib_rain field defines metadata (aside from paramId)
            CALL grib_rain%write_other_field(ncidout, paramid, values)
        END SUBROUTINE

        SUBROUTINE write_field_from_integer(paramid, values)
            INTEGER, INTENT(IN) :: paramid
            INTEGER, INTENT(IN) :: values(:)
            CALL assert(SIZE(values) == npoints)
            IF (.NOT. ALLOCATED(tmp)) THEN
                ALLOCATE(tmp(npoints))
            ENDIF
            tmp(:) = values(:)
            CALL write_field(paramid, tmp)
        END SUBROUTINE

    END SUBROUTINE

    FUNCTION EmptyGribField() RESULT(g)
      TYPE(GribField) :: g
      g%fd = 0
      g%handle = 0
      g%paramId = 0
      g%shortName = ''
      g%name = ''
      g%Npoints = 0
      g%count = 0
    END FUNCTION

    SUBROUTINE gribfield_coordinates(this, latitudes, longitudes)
        CLASS(GribField), INTENT(IN) :: this
        REAL, ALLOCATABLE, INTENT(INOUT) :: latitudes(:)
        REAL, ALLOCATABLE, INTENT(INOUT) :: longitudes(:)
        REAL, ALLOCATABLE :: tmp(:)

        CALL assert(ALLOCATED(latitudes) .AND. SIZE(latitudes) == this%Npoints, 'latitudes allocation')
        CALL assert(ALLOCATED(longitudes) .AND. SIZE(longitudes) == this%Npoints, 'longitudes allocation')
        latitudes = 0
        longitudes = 0

        ALLOCATE(tmp(this%Npoints))
        CALL codes_grib_get_data(this%handle, latitudes, longitudes, tmp)
        DEALLOCATE(tmp)
    END SUBROUTINE

    SUBROUTINE gribfield_values(this, values)
        CLASS(GribField), INTENT(IN) :: this
        REAL, ALLOCATABLE, INTENT(INOUT) :: values(:)

        ! Note: this routine reorders the input; when the data structures are one-dimensional
        ! arrays, it can be read directly which is much more efficient
        REAL, ALLOCATABLE :: tmp(:)
        INTEGER :: i, n

        CALL assert(ALLOCATED(values), 'gribfield_values values allocated')
        CALL assert(SIZE(values) == this%Npoints, 'gribfield_values values size mismatch)')

        CALL codes_get_size(this%handle, "values", n)
        CALL assert(SIZE(values) == n, 'gribfield_values codes_get_size("values") mismatch')

        CALL codes_get(this%handle, "values", values)
    END SUBROUTINE

    SUBROUTINE gribfield_values_as_integer(this, values)
        CLASS(GribField), INTENT(IN) :: this
        INTEGER, ALLOCATABLE, INTENT(INOUT) :: values(:)
        INTEGER :: n

        ! Note: this routine reorders the input; when the data structures are one-dimensional
        ! arrays, it can be read directly which is much more efficient
        REAL, ALLOCATABLE :: tmp(:)

        CALL assert(ALLOCATED(values), 'gribfield_values values allocated')
        CALL assert(SIZE(values) == this%Npoints, 'gribfield_values values size mismatch)')

        CALL codes_get_size(this%handle, "values", n)
        CALL assert(SIZE(values) == n, 'gribfield_values codes_get_size("values") mismatch')

        ALLOCATE(tmp(this%Npoints))
        CALL codes_get(this%handle, "values", tmp)
        values(:) = tmp(:)
        DEALLOCATE(tmp)
    END SUBROUTINE

    SUBROUTINE gribfield_header(this)
        CLASS(GribField), INTENT(INOUT) :: this
        CALL assert(this%handle /= 0, 'this%handle')

        CALL codes_get(this%handle, 'shortName', this%shortName)
        CALL codes_get(this%handle, 'name', this%name)
        CALL codes_get(this%handle, 'paramId', this%paramid)
        ! CALL assert(this%paramId > 0, 'paramId > 0')

        CALL codes_get(this%handle, 'numberOfDataPoints', this%Npoints)
        CALL assert(this%Npoints > 0, 'numberOfDataPoints > 0')
    END SUBROUTINE

   FUNCTION gribfield_next(this) RESULT(r)
      CLASS(GribField), INTENT(INOUT) :: this
      INTEGER :: iret
      LOGICAL :: r
      IF (this%handle /= 0) CALL codes_release(this%handle)
      CALL codes_grib_new_from_file(this%fd, this%handle, iret)
      r = iret /= CODES_END_OF_FILE
      CALL assert(.NOT. r .OR. iret == 0 .AND. this%handle /= 0, 'codes_grib_new_from_file')
   END FUNCTION

   SUBROUTINE gribfield_close(this)
      CLASS(GribField), INTENT(INOUT) :: this
      IF (this%handle /= 0) CALL codes_release(this%handle)
      this%handle = 0
      CALL codes_close_file(this%fd)
      this%fd = 0
   END SUBROUTINE

   SUBROUTINE gribfield_open_as_input(this, file, var, names, pids)
      CLASS(GribField), INTENT(INOUT) :: this

      CHARACTER(LEN=*), INTENT(IN) :: file
      CHARACTER(LEN=*), INTENT(IN) :: var
      CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: names
      INTEGER, DIMENSION(:), INTENT(IN) :: pids

      CHARACTER(LEN=20) :: shortName
      INTEGER :: i, Npoints
      LOGICAL :: found

      ! open file and read messages to the end
      CALL codes_open_file(this%fd, file, 'r')
      CALL assert(this%next(), 'file "'//TRIM(file)//'": '//TRIM(var)//' GRIB not found')

      ! first GRIB message: get header (variable name/id and geometry), then
      ! next GRIB messages: confirm numberOfDataPoints, increment count
      CALL this%header()

      found = SIZE(names) .EQ. 0 .AND. SIZE(pids) .EQ. 0
      IF (.NOT. found) THEN
          DO i = 1, SIZE(names)
              found = this%shortName .EQ. names(i)
              IF (found) EXIT
          END DO
      END IF
      IF (.NOT. found) THEN
          DO i = 1, SIZE(pids)
              found = this%paramId .EQ. pids(i)
              IF (found) EXIT
          END DO
      END IF
      CALL assert(found, 'file "'//TRIM(file)//'": '//TRIM(var)//' name not found')
      PRINT *, 'Found '//TRIM(var)//' variable called: ', TRIM(this%shortName), ' with paramId=', this%paramId

      this%count = 1
      DO WHILE (this%next())
          this%count = this%count + 1
          CALL codes_get(this%handle, 'paramId', i)
          CALL codes_get(this%handle, 'numberOfDataPoints', Npoints)
          CALL assert(Npoints == this%Npoints .AND. i == this%paramId,&
                      'Input fields should have the same paramId and geometry (numberOfDataPoints)')
      END DO

      ! end reached, re-open the file to read messages one-by-one
      CALL this%close()
      CALL codes_open_file(this%fd, file, 'r')
      CALL assert(this%fd /= 0, 'file "'//TRIM(file)//'": GRIB codes_open_file (r)')
   END SUBROUTINE

   SUBROUTINE gribfield_open_as_output(this, file)
      CLASS(GribField), INTENT(INOUT) :: this
      CHARACTER(LEN=*), INTENT(IN) :: file

      this%fd = 0
      this%handle = 0
      CALL assert(LEN(file) > 0, 'file "'//TRIM(file)//'": invalid file name')
      CALL codes_open_file(this%fd, file, 'a')
      CALL assert(this%fd /= 0, 'file "'//TRIM(file)//'": GRIB codes_open_file (a)')
   END SUBROUTINE

   SUBROUTINE gribfield_open_as_restart(this, file)
      CLASS(GribField), INTENT(INOUT) :: this
      CHARACTER(LEN=*), INTENT(IN) :: file

      this%fd = 0
      this%handle = 0
      CALL assert(LEN(file) > 0, 'file "'//TRIM(file)//'": invalid file name')
      CALL codes_open_file(this%fd, file, 'r')
      CALL assert(this%fd /= 0, 'file "'//TRIM(file)//'": GRIB codes_open_file (r)')
   END SUBROUTINE

   SUBROUTINE gribfield_write_other_field(this, fd, paramid, values)
      CLASS(GribField), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: fd, paramid
      REAL, INTENT(IN) :: values(:)
      INTEGER :: handle

      CALL assert(fd /= 0 .AND. paramid > 0, 'gribfield_write_other_field: requirements')
      CALL assert(this%Npoints == SIZE(values), 'gribfield_write_other_field: values size')

      handle = 0
      CALL codes_clone(this%handle, handle)
      CALL assert(handle /= 0, 'gribfield_write_other_field: codes_clone')

      CALL codes_set(handle, 'paramId', paramid)
      CALL codes_set(handle, 'missingValue', missingValue)
      IF (ANY(values == missingValue)) CALL codes_set(handle, 'bitmapPresent', 1)
      CALL codes_set(handle, 'values', PACK(values, MASK=.TRUE.))

      CALL codes_write(handle, fd)
      CALL codes_release(handle)
   END SUBROUTINE

   FUNCTION gribfield_same_geometry(this, other) RESULT(yes)
      CLASS(GribField), INTENT(INOUT) :: this, other
      LOGICAL :: yes
      yes = this%Npoints == other%Npoints
      yes = yes .AND. (this%count == 1 .OR. other%count == 1 .OR. this%count == other%count)
      IF (.not. yes) THEN
          PRINT *, "%ERROR: fields don't have the same geometry: ",&
          char(10), "(paramId=", this%paramId, ", numberOfDataPoints=", this%Npoints, ", count=", this%count,")",&
          char(10), "(paramId=", other%paramId, ", numberOfDataPoints=", other%Npoints, ", count=", other%count,")"
          STOP 1
      END IF
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
      END IF
   END SUBROUTINE

END MODULE

