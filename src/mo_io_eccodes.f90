! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief ecCodes GRIB I/O
!> @author Di Giuseppe, F., ECMWF
!> @author Maciel, P., ECMWF
MODULE mo_io_eccodes

    use eccodes, only: codes_clone, codes_close_file, codes_get, codes_get_size, codes_grib_get_data, codes_grib_new_from_file, &
                       codes_open_file, codes_release, codes_set, codes_write, CODES_END_OF_FILE

    USE mo_control
    USE mo_fire
    USE mo_fwi
    USE mo_mark5
    USE mo_nfdrs
    USE mo_utilities

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: io_getdata
    PUBLIC :: io_initialize
    PUBLIC :: io_write_restart
    PUBLIC :: io_write_results

    ! GRIB paramIds
    INTEGER, PARAMETER :: ilsm_pids(1)      = [172]
    INTEGER, PARAMETER :: irainclim_pids(1) = [212036]
    INTEGER, PARAMETER :: irain_pids(1)     = [228]
    INTEGER, PARAMETER :: itemp_pids(1)     = [167]
    INTEGER, PARAMETER :: imaxtemp_pids(1)  = [212032]
    INTEGER, PARAMETER :: imintemp_pids(1)  = [212033]
    INTEGER, PARAMETER :: irh_pids(1)       = [168]
    INTEGER, PARAMETER :: imaxrh_pids(1)    = [212034]
    INTEGER, PARAMETER :: iminrh_pids(1)    = [212035]
    INTEGER, PARAMETER :: icc_pids(1)       = [164]
    INTEGER, PARAMETER :: isnow_pids(2)     = [141, 3066]
    INTEGER, PARAMETER :: iwspeed_pids(2)   = [165, 166]
    INTEGER, PARAMETER :: idp_pids(1)       = [212031]
    INTEGER, PARAMETER :: ivs_pids(1)       = [212030]
    INTEGER, PARAMETER :: icr_pids(1)       = [212028]
    INTEGER, PARAMETER :: ifm_pids(1)       = [212029]
    INTEGER, PARAMETER :: islope_pids(1)    = [163]
    INTEGER, PARAMETER :: icv_pids(1)       = [28]

    INTEGER, PARAMETER :: imc_r1hr_pids(1)        = [212001]
    INTEGER, PARAMETER :: imc_r10hr_pids(1)       = [212002]
    INTEGER, PARAMETER :: imc_r100hr_pids(1)      = [212003]
    INTEGER, PARAMETER :: imc_r1000hr_pids(1)     = [212004]
    INTEGER, PARAMETER :: imc_rx1000_pids(1)      = [212005]
    INTEGER, PARAMETER :: imc_rherb_pids(1)       = [212006]
    INTEGER, PARAMETER :: imc_rwood_pids(1)       = [212007]
    INTEGER, PARAMETER :: imc_rbndryt_pids(1)     = [212008]

    INTEGER, PARAMETER :: ifire_prop_ros_pids(1)  = [212009]
    INTEGER, PARAMETER :: ifire_prop_sc_pids(1)   = [260561]
    INTEGER, PARAMETER :: ifire_prop_erc_pids(1)  = [260564]
    INTEGER, PARAMETER :: ifire_prop_bi_pids(1)   = [260562]

    INTEGER, PARAMETER :: ifire_prob_ic_pids(1)   = [260563]
    INTEGER, PARAMETER :: ifire_prob_mcoi_pids(1) = [212014]
    INTEGER, PARAMETER :: ifire_prob_loi_pids(1)  = [212015]
    INTEGER, PARAMETER :: ifire_prob_fli_pids(1)  = [212016]

    INTEGER, PARAMETER :: imark5_fuel_kb_drought_index_pids(1)  = [260557]
    INTEGER, PARAMETER :: imark5_fuel_drought_factor_pids(1)    = [260558]
    INTEGER, PARAMETER :: imark5_fuel_moist_pids(1)             = [212019]
    INTEGER, PARAMETER :: imark5_fuel_weight_pids(1)            = [212020]
    INTEGER, PARAMETER :: imark5_fuel_timesincerain_pids(1)     = [212021]

    INTEGER, PARAMETER :: imark5_prop_ros_theta0_pids(1)        = [212022]
    INTEGER, PARAMETER :: imark5_prop_ros_theta_pids(1)         = [260559]
    INTEGER, PARAMETER :: imark5_prop_flame_height_pids(1)      = [212024]
    INTEGER, PARAMETER :: imark5_prop_flame_distance_pids(1)    = [212025]
    INTEGER, PARAMETER :: imark5_prob_fire_danger_index_pids(1) = [260560]

    INTEGER, PARAMETER :: ifwi_risk_fwi_pids(1)         = [260540]
    INTEGER, PARAMETER :: ifwi_risk_ffmc_pids(1)        = [260541]
    INTEGER, PARAMETER :: ifwi_risk_dmc_pids(1)         = [260542]
    INTEGER, PARAMETER :: ifwi_risk_dc_pids(1)          = [260543]

    INTEGER, PARAMETER :: ifwi_risk_isi_pids(1)         = [260544]
    INTEGER, PARAMETER :: ifwi_risk_bui_pids(1)         = [260545]
    INTEGER, PARAMETER :: ifwi_risk_dsr_pids(1)         = [260546]
    INTEGER, PARAMETER :: ifwi_risk_danger_risk_pids(1) = [212027]

    type, abstract :: io_t
    contains
        procedure(io_input), public, deferred, nopass :: input
        procedure(io_output), public, deferred, nopass :: output
    endtype

    interface
        subroutine io_input(path, data)
            character(len=*), intent(in) :: path
            real(kind=8), dimension(:, :), allocatable, intent(out) :: data
        endsubroutine

        subroutine io_output(path, data)
            character(len=*), intent(in) :: path
            real(kind=8), dimension(:), intent(in) :: data
        endsubroutine
    endinterface

    type, public, extends(io_t) :: eccodes_t
    contains
        procedure, nopass :: input => eccodes_input
        procedure, nopass :: output => eccodes_output
    endtype

    TYPE :: GribField
        INTEGER :: fd = 0
        INTEGER :: handle = 0
        INTEGER :: paramId = 0
        CHARACTER(LEN=20) :: shortName = ''
        CHARACTER(LEN=200) :: name = ''
        INTEGER :: npoints = 0
        INTEGER :: count = 0
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
        PROCEDURE, PUBLIC :: same_geometry => gribfield_same_geometry
    END TYPE

    TYPE(GribField) :: reference  !internal to eccodes_t (a very simplified interface)
    REAL, PARAMETER :: missingValue = rfillvalue !FIXME: -1.e20

    TYPE(GribField), TARGET :: input(18)

    TYPE(GribField), POINTER :: grib_lsm => input(1)
    TYPE(GribField), POINTER :: grib_rain => input(2)
    TYPE(GribField), POINTER :: grib_temp => input(3)
    TYPE(GribField), POINTER :: grib_maxtemp => input(4)
    TYPE(GribField), POINTER :: grib_mintemp => input(5)
    TYPE(GribField), POINTER :: grib_rh => input(6)
    TYPE(GribField), POINTER :: grib_maxrh => input(7)
    TYPE(GribField), POINTER :: grib_minrh => input(8)
    TYPE(GribField), POINTER :: grib_cc => input(9)
    TYPE(GribField), POINTER :: grib_wspeed => input(10)
    TYPE(GribField), POINTER :: grib_snow => input(11)
    TYPE(GribField), POINTER :: grib_dp => input(12)
    TYPE(GribField), POINTER :: grib_vs => input(13)
    TYPE(GribField), POINTER :: grib_cr => input(14)
    TYPE(GribField), POINTER :: grib_fm => input(15)
    TYPE(GribField), POINTER :: grib_slope => input(16)
    TYPE(GribField), POINTER :: grib_cv => input(17)
    TYPE(GribField), POINTER :: grib_rainclim => input(18)

CONTAINS

    SUBROUTINE io_getdata(istep)
        INTEGER, INTENT(IN) :: istep
        REAL, ALLOCATABLE :: tmp(:)

        CALL assert(npoints > 0)
        IF (istep == 1) RETURN

        IF (grib_rain%count > 1) CALL next_values('io_getdata: rain', grib_rain, grib_rain%paramId, rrain)
        IF (grib_temp%count > 1) CALL next_values('io_getdata: temp', grib_temp, grib_temp%paramId, rtemp)
        IF (grib_maxtemp%count > 1) CALL next_values('io_getdata: maxtemp', grib_maxtemp, grib_maxtemp%paramId, rmaxtemp)
        IF (grib_mintemp%count > 1) CALL next_values('io_getdata: mintemp', grib_mintemp, grib_mintemp%paramId, rmintemp)
        IF (grib_rh%count > 1) CALL next_values('io_getdata: rh', grib_rh, grib_rh%paramId, rrh)
        IF (grib_maxrh%count > 1) CALL next_values('io_getdata: maxrh', grib_maxrh, grib_maxrh%paramId, rmaxrh)
        IF (grib_minrh%count > 1) CALL next_values('io_getdata: minrh', grib_minrh, grib_minrh%paramId, rminrh)
        IF (grib_cc%count > 1) CALL next_values('io_getdata: cc', grib_cc, grib_cc%paramId, rcc)
        IF (grib_wspeed%count > 1) CALL next_values('io_getdata: wspeed', grib_wspeed, grib_wspeed%paramId, rwspeed)
        IF (grib_snow%count > 1) CALL next_values('io_getdata: snow', grib_snow, grib_snow%paramId, rsnow)
        IF (grib_dp%count > 1) CALL next_values('io_getdata: dp', grib_dp, grib_dp%paramId, rdp)

        IF (grib_vs%count > 1) THEN
            ALLOCATE (tmp(npoints))
            CALL next_values('io_getdata: vs', grib_vs, grib_vs%paramId, tmp)
            ivs = tmp
            DEALLOCATE (tmp)
        ENDIF
    END SUBROUTINE

    SUBROUTINE next_values(message, grib, paramId, values)
        CHARACTER(LEN=*), INTENT(IN) :: message
        TYPE(GribField), INTENT(INOUT) :: grib
        INTEGER, INTENT(IN) :: paramId
        REAL, INTENT(INOUT), ALLOCATABLE :: values(:)
        CALL assert(SIZE(values) == npoints, message//' SIZE(values) == npoints')

        CALL assert(grib%next(), message//' grib%next()')
        CALL grib%header()

        CALL assert(grib%npoints == npoints, message//' grib%npoints == npoints')
        CALL assert(grib%paramId == paramId, message//' grib%paramId == paramId')

        CALL grib%values(values)
    END SUBROUTINE

    SUBROUTINE io_initialize
        TYPE(GribField) :: restart
        INTEGER :: i
        REAL, ALLOCATABLE :: tmp(:)

        ! use land-sea mask to define geometry
        CALL grib_lsm%open_as_input(lsmfile, 'land-sea mask', ilsm_pids)

        npoints = grib_lsm%npoints
        PRINT *, 'Number of points: ', npoints

        CALL assert(npoints > 0)
        ALLOCATE (lats(npoints))
        ALLOCATE (lons(npoints))

        CALL assert(grib_lsm%next(), 'io_initialize: grib_lsm%next()')
        CALL grib_lsm%coordinates(lats, lons)

        CALL grib_temp%open_as_input(tempfile, 'temperature', itemp_pids)
        CALL grib_maxtemp%open_as_input(maxtempfile, 'maximum daily temperature', imaxtemp_pids)
        CALL grib_mintemp%open_as_input(mintempfile, 'minimum daily temperature', imintemp_pids)
        CALL grib_rh%open_as_input(rhfile, 'relative humidity', irh_pids)
        CALL grib_maxrh%open_as_input(maxrhfile, 'maximum daily relative humidity', imaxrh_pids)
        CALL grib_minrh%open_as_input(minrhfile, 'minimum daily relative humidity', iminrh_pids)
        CALL grib_rain%open_as_input(rainfile, 'rainfall', irain_pids)
        CALL grib_cc%open_as_input(ccfile, 'cloud cover', icc_pids)
        CALL grib_wspeed%open_as_input(wspeedfile, 'wind speed', iwspeed_pids)
        CALL grib_snow%open_as_input(snowfile, 'ground snow', isnow_pids)
        CALL grib_dp%open_as_input(dpfile, 'duration of precipitation in the previous 24h', idp_pids)
        CALL grib_vs%open_as_input(vsfile, 'vegetation stage', ivs_pids)
        CALL grib_cr%open_as_input(crfile, 'climate region', icr_pids)
        CALL grib_fm%open_as_input(fmfile, 'fuel model', ifm_pids)
        CALL grib_slope%open_as_input(slopefile, 'slope of sub-gridscale orography', islope_pids)
        CALL grib_cv%open_as_input(cvfile, 'fractional coverage for vegetation (high + low)', icv_pids)
        CALL grib_rainclim%open_as_input(rainclimfile, 'climate rainfall', irainclim_pids)

        ! fields/variables defined in time (SIZE() = ntimestep)
        ntimestep = MAXVAL(input(:)%count)
        PRINT *, 'Number of time steps: ', ntimestep

        CALL assert(ntimestep > 0, "io_initialize: ntimestep > 0")
        ALLOCATE (nhours(ntimestep))
        DO i = 1, ntimestep
            nhours(i) = (i - 1)*dt
        ENDDO

        ! fields/variables defined in space (SIZE() = npoints)
        CALL assert(npoints > 0, "io_initialize: npoints > 0")
        ALLOCATE (tmp(npoints))

        DO i = 1, SIZE(input)
            IF (i > 1) CALL assert(input(i)%same_geometry(input(1)))
            CALL assert(input(i)%count == 1 .OR. input(i)%count == ntimestep)
        ENDDO

        ALLOCATE (rlsm(npoints))
        ! CALL assert(grib_lsm%next())  ! already called (above)
        CALL grib_lsm%values(rlsm)

        ALLOCATE (rrain(npoints))
        CALL next_values('io_initialize: grib_rain', grib_rain, grib_rain%paramId, rrain)

        ALLOCATE (rtemp(npoints))
        CALL next_values('io_initialize: grib_temp', grib_temp, grib_temp%paramId, rtemp)

        ALLOCATE (rmaxtemp(npoints))
        CALL next_values('io_initialize: grib_maxtemp', grib_maxtemp, grib_maxtemp%paramId, rmaxtemp)

        ALLOCATE (rmintemp(npoints))
        CALL next_values('io_initialize: grib_mintemp', grib_mintemp, grib_mintemp%paramId, rmintemp)

        ALLOCATE (rrh(npoints))
        CALL next_values('io_initialize: grib_rh', grib_rh, grib_rh%paramId, rrh)

        ALLOCATE (rmaxrh(npoints))
        CALL next_values('io_initialize: grib_maxrh', grib_maxrh, grib_maxrh%paramId, rmaxrh)

        ALLOCATE (rminrh(npoints))
        CALL next_values('io_initialize: grib_minrh', grib_minrh, grib_minrh%paramId, rminrh)

        ALLOCATE (rcc(npoints))
        CALL next_values('io_initialize: grib_cc', grib_cc, grib_cc%paramId, rcc)

        ALLOCATE (rwspeed(npoints))
        CALL next_values('io_initialize: grib_wspeed', grib_wspeed, grib_wspeed%paramId, rwspeed)

        ALLOCATE (rsnow(npoints))
        CALL next_values('io_initialize: grib_snow', grib_snow, grib_snow%paramId, rsnow)

        ALLOCATE (rdp(npoints))
        CALL next_values('io_initialize: grib_dp', grib_dp, grib_dp%paramId, rdp)

        ALLOCATE (rcv(npoints))
        CALL next_values('io_initialize: grib_cv', grib_cv, grib_cv%paramId, rcv)

        ALLOCATE (rrainclim(npoints))
        CALL next_values('io_initialize: grib_rainclim', grib_rainclim, grib_rainclim%paramId, rrainclim)

        ALLOCATE (ivs(npoints))
        CALL next_values('io_initialize: grib_vs', grib_vs, grib_vs%paramId, tmp)
        ivs = tmp

        ALLOCATE (icr(npoints))
        CALL next_values('io_initialize: grib_cr', grib_cr, grib_cr%paramId, tmp)
        icr = tmp

        ALLOCATE (ifm(npoints))
        CALL next_values('io_initialize: grib_fm', grib_fm, grib_fm%paramId, tmp)
        ifm = tmp

        ALLOCATE (islope(npoints))
        CALL next_values('io_initialize: grib_slope', grib_slope, grib_slope%paramId, tmp)
        islope = tmp

        ALLOCATE (mc(npoints))
        ALLOCATE (fire_prop(npoints))
        ALLOCATE (fire_prob(npoints))
        ALLOCATE (mark5_fuel(npoints))
        ALLOCATE (mark5_prop(npoints))
        ALLOCATE (mark5_prob(npoints))
        ALLOCATE (fwi_risk(npoints))

        IF (LEN(TRIM(restart_file)) > 0) THEN
            PRINT *, "Initialization type: exact initialization from '"//TRIM(restart_file)//"'"

            CALL restart%open_as_restart(restart_file)

            CALL next_values('restart mc%r1hr', restart, imc_r1hr_pids(1), tmp)
            mc(:)%r1hr = tmp

            CALL next_values('restart mc%r10hr', restart, imc_r10hr_pids(1), tmp)
            mc(:)%r10hr = tmp

            CALL next_values('restart mc%r100hr', restart, imc_r100hr_pids(1), tmp)
            mc(:)%r100hr = tmp

            CALL next_values('restart mc%r1000hr', restart, imc_r1000hr_pids(1), tmp)
            mc(:)%r1000hr = tmp

            CALL next_values('restart mc%rherb', restart, imc_rherb_pids(1), tmp)
            mc(:)%rherb = tmp

            CALL next_values('restart mc%rwood', restart, imc_rwood_pids(1), tmp)
            mc(:)%rwood = tmp

            CALL next_values('restart mc%rx1000', restart, imc_rx1000_pids(1), tmp)
            mc(:)%rx1000 = tmp

            CALL next_values('restart mc%rbndryt', restart, imc_rbndryt_pids(1), tmp)
            mc(:)%rbndryt = tmp

            CALL next_values('restart mark5_fuel%kb_drought_index', restart, imark5_fuel_kb_drought_index_pids(1), tmp)
            mark5_fuel(:)%kb_drought_index = tmp

            CALL next_values('restart mark5_fuel%timesincerain', restart, imark5_fuel_timesincerain_pids(1), tmp)
            mark5_fuel(:)%timesincerain = tmp

            CALL next_values('restart fwi_risk%ffmc', restart, ifwi_risk_ffmc_pids(1), tmp)
            fwi_risk(:)%ffmc = tmp

            CALL next_values('restart fwi_risk%dmc', restart, ifwi_risk_dmc_pids(1), tmp)
            fwi_risk(:)%dmc = tmp

            CALL next_values('restart fwi_risk%dc', restart, ifwi_risk_dc_pids(1), tmp)
            fwi_risk(:)%dc = tmp

            CALL restart%close ()
        ELSE
            PRINT *, 'Initialization type: artificial conditions'

            ! Here the loop is necessary
            ! Dead fuel
            DO i = 1, npoints
                IF (rlsm(i) .GT. 0.0001) THEN

                    IF (1 <= icr(i) .AND. icr(i) <= 5) THEN
                        mc(i)%r100hr = 5.+(5.*icr(i))
                        mc(i)%r1000hr = 10.+(5.*icr(i))
                        mc(i)%rbndryt = 10.+(5.*icr(i))
                        mc(i)%rx1000 = 10.+(5.*icr(i))
                    ENDIF

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
                    fwi_risk(i)%dmc = 6.
                    fwi_risk(i)%dc = 15.
                ENDIF
            ENDDO
        ENDIF

        DEALLOCATE (tmp)
    END SUBROUTINE

    SUBROUTINE io_write_restart
        INTEGER :: fd

        ! Open restart file
        fd = 0
        CALL assert(LEN(TRIM(output_restart)) > 0, 'output_restart not empty')
        CALL codes_open_file(fd, output_restart, 'w')
        CALL assert(fd /= 0, 'codes_open_file (w): '//TRIM(output_restart))

        CALL write_field(fd, imc_r1hr_pids(1), mc(:)%r1hr)
        CALL write_field(fd, imc_r10hr_pids(1), mc(:)%r10hr)
        CALL write_field(fd, imc_r100hr_pids(1), mc(:)%r100hr)
        CALL write_field(fd, imc_r1000hr_pids(1), mc(:)%r1000hr)
        CALL write_field(fd, imc_rherb_pids(1), mc(:)%rherb)
        CALL write_field(fd, imc_rwood_pids(1), mc(:)%rwood)
        CALL write_field(fd, imc_rx1000_pids(1), mc(:)%rx1000)
        CALL write_field(fd, imc_rbndryt_pids(1), mc(:)%rbndryt)

        CALL write_field(fd, imark5_fuel_kb_drought_index_pids(1), mark5_fuel(:)%kb_drought_index)
        CALL write_field(fd, imark5_fuel_timesincerain_pids(1), mark5_fuel(:)%timesincerain)

        CALL write_field(fd, ifwi_risk_ffmc_pids(1), fwi_risk(:)%ffmc)
        CALL write_field(fd, ifwi_risk_dmc_pids(1), fwi_risk(:)%dmc)
        CALL write_field(fd, ifwi_risk_dc_pids(1), fwi_risk(:)%dc)

        CALL codes_close_file(fd)
    END SUBROUTINE

    SUBROUTINE io_write_results(istep)
        INTEGER, INTENT(IN) :: istep  ! NOTE: ignored
        INTEGER :: fd
        CHARACTER, SAVE :: cmode = 'w'

        ! Open results file
        fd = 0
        CALL assert(LEN(TRIM(output_file)) > 0, 'output_file not empty')
        CALL codes_open_file(fd, output_file, cmode)
        CALL assert(fd /= 0, 'codes_open_file ('//cmode//'): '//TRIM(output_file))
        cmode = 'a'

        CALL write_field(fd, irain_pids(1), rrain)
        CALL write_field(fd, itemp_pids(1), rtemp)
        CALL write_field(fd, imaxtemp_pids(1), rmaxtemp)
        CALL write_field(fd, imintemp_pids(1), rmintemp)
        CALL write_field(fd, irh_pids(1), rrh)
        CALL write_field(fd, imaxrh_pids(1), rmaxrh)
        CALL write_field(fd, iminrh_pids(1), rminrh)
        CALL write_field(fd, icc_pids(1), rcc)
        CALL write_field(fd, isnow_pids(1), rsnow)
        CALL write_field(fd, iwspeed_pids(1), rwspeed)
        CALL write_field(fd, idp_pids(1), rdp)
        CALL write_field_from_integer(fd, ivs_pids(1), ivs)

        CALL write_field(fd, imc_r1hr_pids(1), mc(:)%r1hr)
        CALL write_field(fd, imc_r10hr_pids(1), mc(:)%r10hr)
        CALL write_field(fd, imc_r100hr_pids(1), mc(:)%r100hr)
        CALL write_field(fd, imc_r1000hr_pids(1), mc(:)%r1000hr)
        CALL write_field(fd, imc_rx1000_pids(1), mc(:)%rx1000)
        CALL write_field(fd, imc_rherb_pids(1), mc(:)%rherb)
        CALL write_field(fd, imc_rwood_pids(1), mc(:)%rwood)

        CALL write_field(fd, ifire_prop_ros_pids(1), fire_prop(:)%ros)
        CALL write_field_from_integer(fd, ifire_prop_sc_pids(1), fire_prop(:)%sc)
        CALL write_field_from_integer(fd, ifire_prop_erc_pids(1), fire_prop(:)%erc)
        CALL write_field_from_integer(fd, ifire_prop_bi_pids(1), fire_prop(:)%bi)

        CALL write_field_from_integer(fd, ifire_prob_ic_pids(1), fire_prob(:)%ic)
        CALL write_field_from_integer(fd, ifire_prob_mcoi_pids(1), fire_prob(:)%mcoi)
        CALL write_field_from_integer(fd, ifire_prob_loi_pids(1), fire_prob(:)%loi)
        CALL write_field(fd, ifire_prob_fli_pids(1), fire_prob(:)%fli)

        CALL write_field(fd, imark5_fuel_kb_drought_index_pids(1), mark5_fuel(:)%kb_drought_index)
        CALL write_field(fd, imark5_fuel_drought_factor_pids(1), mark5_fuel(:)%drought_factor)
        CALL write_field(fd, imark5_fuel_moist_pids(1), mark5_fuel(:)%moist)
        CALL write_field(fd, imark5_fuel_weight_pids(1), mark5_fuel(:)%weight)

        CALL write_field(fd, imark5_prop_ros_theta0_pids(1), mark5_prop(:)%ros_theta0)
        CALL write_field(fd, imark5_prop_ros_theta_pids(1), mark5_prop(:)%ros_theta)
        CALL write_field(fd, imark5_prop_flame_height_pids(1), mark5_prop(:)%flame_height)
        CALL write_field(fd, imark5_prop_flame_distance_pids(1), mark5_prop(:)%flame_distance)
        CALL write_field(fd, imark5_prob_fire_danger_index_pids(1), mark5_prob(:)%fire_danger_index)

        CALL write_field(fd, ifwi_risk_fwi_pids(1), fwi_risk(:)%fwi)
        CALL write_field(fd, ifwi_risk_ffmc_pids(1), fwi_risk(:)%ffmc)
        CALL write_field(fd, ifwi_risk_dmc_pids(1), fwi_risk(:)%dmc)
        CALL write_field(fd, ifwi_risk_dc_pids(1), fwi_risk(:)%dc)

        CALL write_field(fd, ifwi_risk_isi_pids(1), fwi_risk(:)%isi)
        CALL write_field(fd, ifwi_risk_bui_pids(1), fwi_risk(:)%bui)
        CALL write_field(fd, ifwi_risk_dsr_pids(1), fwi_risk(:)%dsr)
        CALL write_field(fd, ifwi_risk_danger_risk_pids(1), fwi_risk(:)%danger_risk)

        IF (output_constant) THEN
            CALL write_field(fd, ilsm_pids(1), rlsm)
            CALL write_field(fd, icv_pids(1), rcv)
            CALL write_field(fd, irainclim_pids(1), rrainclim)
            CALL write_field_from_integer(fd, icr_pids(1), icr)
            CALL write_field_from_integer(fd, ifm_pids(1), ifm)
            CALL write_field_from_integer(fd, islope_pids(1), islope)
        ENDIF

        CALL codes_close_file(fd)
    END SUBROUTINE

    subroutine eccodes_input(path, data)
        implicit none

        character(len=*), intent(in) :: path
        real(kind=8), dimension(:, :), allocatable, intent(out) :: data

        integer :: i, paramId, npoints
        real(kind=4), dimension(:), allocatable :: tmp  ! ecCodes I/O does not support real(kind=8)?

        type(GribField) :: grib

        ! open file and read messages to the end
        ! first message: set paramId/numberOfDataPoints, then
        ! next messages: count messages, confirm paramId/numberOfDataPoints
        call codes_open_file(grib%fd, path, 'r')
        call assert(grib%next(), 'file "'//trim(path)//' GRIB not found')

        call grib%header()
        paramId = grib%paramId
        npoints = grib%npoints

        if (reference%handle == 0) then
            call codes_clone(grib%handle, reference%handle)
            call assert(reference%handle /= 0)
            call reference%header()
        endif

        grib%count = 1
        do while (grib%next())
            grib%count = grib%count + 1
            call grib%header()
            call assert(paramId == grib%paramId .AND. npoints == grib%npoints, &
                        'Fields should have the same paramId and numberOfDataPoints')
        enddo

        print *, "input: '"//trim(path)//"', paramId: ", grib%paramId, &
            ", npoints: ", grib%npoints, ", count: ", grib%count

        ! end reached, re-open the file to read messages one-by-one
        call grib%close ()
        call codes_open_file(grib%fd, path, 'r')

        allocate (data(npoints, grib%count))
        allocate (tmp(npoints))
        do i = 1, grib%count
            call assert(grib%next(), "file '"//trim(path)//"' GRIB not found")
            call grib%values(tmp)
            data(:, i) = tmp(:)
        enddo
        deallocate (tmp)
    end subroutine

    subroutine eccodes_output(path, data)
        implicit none

        character(len=*), intent(in) :: path
        real(kind=8), dimension(:), intent(in) :: data

        real(kind=4), dimension(:), allocatable :: tmp  ! ecCodes I/O does not support real(kind=8)?
        integer :: handle, fd, bitmapPresent

        call assert(reference%handle /= 0, "output: reference%handle /= 0")

        ! clone reference handle and set custom metadata/data before writing
        call assert(reference%handle /= 0)
        call assert(reference%npoints == size(data), 'output: reference%npoints == size(data)')

        handle = 0
        call codes_clone(reference%handle, handle)
        call assert(handle /= 0, 'output: codes_clone(reference)')

        bitmapPresent = 0
        if (any(data == missingValue)) bitmapPresent = 1
        call codes_set(handle, 'bitmapPresent', bitmapPresent)
        call codes_set(handle, 'missingValue', missingValue)

        allocate (tmp(size(data)))
        tmp = data
        call codes_set(handle, 'values', tmp)
        deallocate (tmp)

        print *, "writing output file '"//trim(path)//"'"
        fd = 0
        call codes_open_file(fd, path, 'w')
        call assert(fd /= 0, "codes_open_file(w): '"//trim(path)//"'")
        call codes_write(handle, fd)
        call codes_close_file(fd)

        call codes_release(handle)
    end subroutine

    SUBROUTINE gribfield_coordinates(this, latitudes, longitudes)
        CLASS(GribField), INTENT(IN) :: this
        REAL, ALLOCATABLE, INTENT(INOUT) :: latitudes(:)
        REAL, ALLOCATABLE, INTENT(INOUT) :: longitudes(:)
        REAL, ALLOCATABLE :: tmp(:)

        CALL assert(ALLOCATED(latitudes) .AND. SIZE(latitudes) == this%npoints, 'latitudes allocation')
        CALL assert(ALLOCATED(longitudes) .AND. SIZE(longitudes) == this%npoints, 'longitudes allocation')
        latitudes = 0
        longitudes = 0

        ALLOCATE (tmp(this%npoints))
        CALL codes_grib_get_data(this%handle, latitudes, longitudes, tmp)
        DEALLOCATE (tmp)
    END SUBROUTINE

    SUBROUTINE gribfield_values(this, values)
        CLASS(GribField), INTENT(IN) :: this
        REAL, ALLOCATABLE, INTENT(INOUT) :: values(:)
        INTEGER :: n, bitmapPresent

        CALL assert(ALLOCATED(values), 'gribfield_values values allocated')
        CALL assert(SIZE(values) == this%npoints, 'gribfield_values: values size mismatch)')

        CALL codes_get_size(this%handle, "values", n)
        CALL assert(SIZE(values) == n, 'gribfield_values: codes_get_size("values") mismatch')

        ! ensure all missing values use the same indicator ('set' before 'get')
        CALL codes_get(this%handle, "bitmapPresent", bitmapPresent)
        IF (bitmapPresent /= 0) CALL codes_set(this%handle, "missingValue", missingValue)

        CALL codes_get(this%handle, "values", values)
    END SUBROUTINE

    SUBROUTINE gribfield_values_as_integer(this, values)
        CLASS(GribField), INTENT(IN) :: this
        INTEGER, ALLOCATABLE, INTENT(INOUT) :: values(:)
        REAL, ALLOCATABLE :: rvalues(:)
        CALL assert(ALLOCATED(values), 'gribfield_values_as_integer: values allocated')

        ALLOCATE (rvalues(SIZE(values)))
        CALL gribfield_values(this, rvalues)

        values = rvalues
        DEALLOCATE (rvalues)
    END SUBROUTINE

    SUBROUTINE gribfield_header(this)
        CLASS(GribField), INTENT(INOUT) :: this
        CALL assert(this%handle /= 0, 'this%handle')

        CALL codes_get(this%handle, 'shortName', this%shortName)
        CALL codes_get(this%handle, 'name', this%name)
        CALL codes_get(this%handle, 'paramId', this%paramid)
        ! CALL assert(this%paramId > 0, 'paramId > 0')

        CALL codes_get(this%handle, 'numberOfDataPoints', this%npoints)
        CALL assert(this%npoints > 0, 'numberOfDataPoints > 0')
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

    SUBROUTINE gribfield_open_as_input(this, file, var, pids)
        CLASS(GribField), INTENT(INOUT) :: this

        CHARACTER(LEN=*), INTENT(IN) :: file
        CHARACTER(LEN=*), INTENT(IN) :: var
        INTEGER, DIMENSION(:), INTENT(IN) :: pids

        INTEGER :: i, n
        LOGICAL :: found

        ! open file and read messages to the end
        CALL codes_open_file(this%fd, file, 'r')
        CALL assert(this%next(), 'file "'//TRIM(file)//'": '//TRIM(var)//' GRIB not found')

        ! first GRIB message: get header (variable name/id and geometry), then
        ! next GRIB messages: confirm numberOfDataPoints, increment count
        CALL this%header()

        found = .FALSE.
        DO i = 1, SIZE(pids)
            found = this%paramId .EQ. pids(i)
            IF (found) EXIT
        ENDDO
        CALL assert(found, 'file "'//TRIM(file)//'": '//TRIM(var)//' field not found')
        PRINT *, 'Found '//TRIM(var)//' field with paramId=', this%paramId

        this%count = 1
        DO WHILE (this%next())
            this%count = this%count + 1
            CALL codes_get(this%handle, 'paramId', i)
            CALL codes_get(this%handle, 'numberOfDataPoints', n)
            CALL assert(n == this%npoints .AND. i == this%paramId, &
                        'Input fields should have the same paramId and geometry (numberOfDataPoints)')
        ENDDO

        ! end reached, re-open the file to read messages one-by-one
        CALL this%close ()
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

    SUBROUTINE write_field(fd, paramid, values)
        INTEGER, INTENT(IN) :: fd, paramid
        REAL, INTENT(IN) :: values(:)
        INTEGER :: handle, i, bitmapPresent, edition
        TYPE(GribField), POINTER, SAVE :: ref => input(11)

        ! reference defines metadata (date/time/step/..., aside from paramId)
        IF (.NOT. (ref%count > 1)) THEN
            DO i = 2, SIZE(input)
                IF (input(i)%count > 1) THEN
                    ref => input(i)
                    EXIT
                ENDIF
            ENDDO
        ENDIF
        CALL assert(ref%handle /= 0)

        CALL assert(fd /= 0 .AND. paramid > 0, 'write_field: requirements')
        CALL assert(ref%npoints == SIZE(values), 'write_field: values size')

        handle = 0
        CALL codes_clone(ref%handle, handle)
        CALL assert(handle /= 0, 'write_field: codes_clone')

        ! FIXME fix GRIB2-only fields from GRIB1
        IF (paramid > 260000) THEN
            CALL codes_get(handle, 'edition', edition)
            IF (edition == 1) CALL codes_set(handle, 'edition', 2)
        ENDIF

        CALL codes_set(handle, 'paramId', paramid)
        CALL codes_set(handle, 'missingValue', missingValue)

        bitmapPresent = 0
        IF (ANY(values == missingValue)) bitmapPresent = 1
        CALL codes_set(handle, 'bitmapPresent', bitmapPresent)

        CALL codes_set(handle, 'values', values)

        CALL codes_write(handle, fd)
        CALL codes_release(handle)
    END SUBROUTINE

    SUBROUTINE write_field_from_integer(fd, paramid, values)
        INTEGER, INTENT(IN) :: fd, paramid
        INTEGER, INTENT(IN) :: values(:)
        REAL, ALLOCATABLE :: tmp(:)

        ALLOCATE (tmp(SIZE(values)))
        tmp = values
        CALL write_field(fd, paramid, tmp)
        DEALLOCATE (tmp)
    END SUBROUTINE

    FUNCTION gribfield_same_geometry(this, other) RESULT(yes)
        CLASS(GribField), INTENT(INOUT) :: this, other
        LOGICAL :: yes
        yes = this%npoints == other%npoints
        yes = yes .AND. (this%count == 1 .OR. other%count == 1 .OR. this%count == other%count)
        IF (.not. yes) THEN
            PRINT *, "%ERROR: fields don't have the same geometry: ", &
                char(10), "(paramId=", this%paramId, ", numberOfDataPoints=", this%npoints, ", count=", this%count, ")", &
                char(10), "(paramId=", other%paramId, ", numberOfDataPoints=", other%npoints, ", count=", other%count, ")"
            STOP 1
        ENDIF
    END FUNCTION

END MODULE
