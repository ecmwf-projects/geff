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

    USE mo_control
    USE mo_fire
    USE mo_fwi
    USE mo_mark5
    USE mo_nfdrs

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: io_getdata
    PUBLIC :: io_initialize
    PUBLIC :: io_set_grid
    PUBLIC :: io_write_constant_fields
    PUBLIC :: io_write_restart
    PUBLIC :: io_write_results

    PROCEDURE(), POINTER :: assert => assert_dbg

    ! GRIB shortNames
    CHARACTER(LEN=2) :: crain_vars(1)     = ['tp']
    CHARACTER(LEN=6) :: crainclim_vars(1) = ['tpclim']
    CHARACTER(LEN=2) :: ctemp_vars(1)     = ['t2']
    CHARACTER(LEN=2) :: crh_vars(1)       = ['rh']
    CHARACTER(LEN=6) :: cwspeed_vars(1)   = ['wspeed']
    CHARACTER(LEN=2) :: ccc_vars(1)       = ['cc']
    CHARACTER(LEN=4) :: csnow_vars(1)     = ['snow']
    CHARACTER(LEN=3) :: cdp_vars(1)       = ['dp']
    CHARACTER(LEN=9) :: cvs_vars(1)       = ['veg_stage']
    CHARACTER(LEN=3) :: clsm_vars(1)      = ['lsm']
    CHARACTER(LEN=3) :: ccv_vars(1)       = ['cv']
    CHARACTER(LEN=5) :: cslope_vars(1)    = ['slope']
    CHARACTER(LEN=13) :: ccr_vars(1)      = ['climate_class'] ! climate regions taken from the dataset Koeppen_CRU_GPCCVASClimO
    CHARACTER(LEN=10) :: cfm_vars(1)      = ['fuel_model']

    ! GRIB paramIds
    INTEGER, PARAMETER :: ilsm_pids(1)      = [172]
    INTEGER, PARAMETER :: irainclim_pids(1) = [228]
    INTEGER, PARAMETER :: irain_pids(1)     = [228]
    INTEGER, PARAMETER :: itemp_pids(1)     = [167]
    INTEGER, PARAMETER :: imaxtemp_pids(1)  = [167]
    INTEGER, PARAMETER :: imintemp_pids(1)  = [167]
    INTEGER, PARAMETER :: irh_pids(1)       = [168]
    INTEGER, PARAMETER :: imaxrh_pids(1)    = [168]
    INTEGER, PARAMETER :: iminrh_pids(1)    = [168]
    INTEGER, PARAMETER :: icc_pids(1)       = [164]
    INTEGER, PARAMETER :: isnow_pids(1)     = [141]
    INTEGER, PARAMETER :: iwspeed_pids(2)   = [165, 166]
    INTEGER, PARAMETER :: idp_pids(1)       = [228]
    INTEGER, PARAMETER :: ivs_pids(1)       = [66]
    INTEGER, PARAMETER :: icr_pids(1)       = [0]
    INTEGER, PARAMETER :: ifm_pids(1)       = [0]
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
    INTEGER, PARAMETER :: ifire_prop_sc_pids(1)   = [212010]
    INTEGER, PARAMETER :: ifire_prop_erc_pids(1)  = [212011]
    INTEGER, PARAMETER :: ifire_prop_bi_pids(1)   = [212012]

    INTEGER, PARAMETER :: ifire_prob_ic_pids(1)   = [212013]
    INTEGER, PARAMETER :: ifire_prob_mcoi_pids(1) = [212014]
    INTEGER, PARAMETER :: ifire_prob_loi_pids(1)  = [212015]
    INTEGER, PARAMETER :: ifire_prob_fli_pids(1)  = [212016]

    INTEGER, PARAMETER :: imark5_fuel_kb_drought_index_pids(1)  = [212017]
    INTEGER, PARAMETER :: imark5_fuel_drought_factor_pids(1)    = [212018]
    INTEGER, PARAMETER :: imark5_fuel_moist_pids(1)             = [212019]
    INTEGER, PARAMETER :: imark5_fuel_weight_pids(1)            = [212020]
    INTEGER, PARAMETER :: imark5_fuel_timesincerain_pids(1)     = [212021]

    INTEGER, PARAMETER :: imark5_prop_ros_theta0_pids(1)        = [212022]
    INTEGER, PARAMETER :: imark5_prop_ros_theta_pids(1)         = [212023]
    INTEGER, PARAMETER :: imark5_prop_flame_height_pids(1)      = [212024]
    INTEGER, PARAMETER :: imark5_prop_flame_distance_pids(1)    = [212025]
    INTEGER, PARAMETER :: imark5_prob_fire_danger_index_pids(1) = [212026]

    INTEGER, PARAMETER :: ifwi_risk_fwi_pids(1)         = [212027]
    INTEGER, PARAMETER :: ifwi_risk_ffmc_pids(1)        = [212028]
    INTEGER, PARAMETER :: ifwi_risk_dmc_pids(1)         = [212029]
    INTEGER, PARAMETER :: ifwi_risk_dc_pids(1)          = [212030]

    INTEGER, PARAMETER :: ifwi_risk_isi_pids(1)         = [212031]
    INTEGER, PARAMETER :: ifwi_risk_bui_pids(1)         = [212032]
    INTEGER, PARAMETER :: ifwi_risk_dsr_pids(1)         = [212033]
    INTEGER, PARAMETER :: ifwi_risk_danger_risk_pids(1) = [212034]

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
       PROCEDURE, PUBLIC :: write_other_field_as_integer => gribfield_write_other_field_as_integer
       PROCEDURE, PUBLIC :: same_geometry => gribfield_same_geometry
    END TYPE

    ! missing value indicator (NetCDF's rfillValue, rpopdensity<0 checks imply negative)
    REAL, PARAMETER :: missingValue = -1.e-20

    TYPE(GribField), TARGET :: input(18)
    TYPE(GribField), POINTER :: ref           => input(1)

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
        TYPE(GribField) :: restart
        INTEGER :: i
        REAL, ALLOCATABLE :: tmp(:)

        IF (TRIM(init_file(1:4)) == 'rest') THEN
            PRINT *, "Initialization type: exact initialization from '" // init_file // "'"

            CALL assert(npoints > 0, "npoints > 0")
            ALLOCATE(tmp(npoints))
            CALL restart%open_as_restart(init_file)

            CALL restart_next_values('Restart (mc(:)%r1hr)', imc_r1hr_pids(1), tmp)
            mc(:)%r1hr = tmp(:)

            CALL restart_next_values('Restart (mc(:)%r10hr)', imc_r10hr_pids(1), tmp)
            mc(:)%r10hr = tmp(:)

            CALL restart_next_values('Restart (mc(:)%r100hr)', imc_r100hr_pids(1), tmp)
            mc(:)%r100hr = tmp(:)

            CALL restart_next_values('Restart (mc(:)%r1000hr)', imc_r1000hr_pids(1), tmp)
            mc(:)%r1000hr = tmp(:)

            CALL restart_next_values('Restart (mc(:)%rherb)', imc_rherb_pids(1), tmp)
            mc(:)%rherb = tmp(:)

            CALL restart_next_values('Restart (mc(:)%rwood)', imc_rwood_pids(1), tmp)
            mc(:)%rwood = tmp(:)

            CALL restart_next_values('Restart (mc(:)%rx1000)', imc_rx1000_pids(1), tmp)
            mc(:)%rx1000 = tmp(:)

            CALL restart_next_values('Restart (mc(:)%rbndryt)', imc_rbndryt_pids(1), tmp)
            mc(:)%rbndryt = tmp(:)

            CALL restart_next_values('Restart (mark5_fuel(:)%kb_drought_index)', imark5_fuel_kb_drought_index_pids(1), tmp)
            mark5_fuel(:)%kb_drought_index = tmp(:)

            CALL restart_next_values('Restart (mark5_fuel(:)%timesincerain)', imark5_fuel_timesincerain_pids(1), tmp)
            mark5_fuel(:)%timesincerain = tmp(:)

            CALL restart_next_values('Restart (fwi_risk(:)%ffmc)', ifwi_risk_ffmc_pids(1), tmp)
            fwi_risk(:)%ffmc = tmp(:)

            CALL restart_next_values('Restart (fwi_risk(:)%dmc)', ifwi_risk_dmc_pids(1), tmp)
            fwi_risk(:)%dmc = tmp(:)

            CALL restart_next_values('Restart (fwi_risk(:)%dc)', ifwi_risk_dc_pids(1), tmp)
            fwi_risk(:)%dc = tmp(:)

            CALL restart%close()
            DEALLOCATE(tmp)
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

        IF (ALLOCATED(tmp)) THEN
            DEALLOCATE(tmp)
        ENDIF

        CONTAINS

        SUBROUTINE restart_next_values(message, paramId, values)
            CHARACTER(LEN=*), INTENT(IN) :: message
            INTEGER, INTENT(IN) :: paramId
            REAL, ALLOCATABLE :: values(:)

            IF (.NOT. ALLOCATED(values)) THEN
                ALLOCATE(values(npoints))
            ENDIF
            CALL assert(ref%Npoints == SIZE(values))

            CALL assert(restart%next(), message//': restart%next()')
            CALL restart%header()

            CALL assert(restart%Npoints == ref%Npoints, message//': restart%Npoints == ref%Npoints')
            CALL assert(restart%paramId == paramId, message//': restart%paramId == paramId')

            CALL restart%values(values)
        END SUBROUTINE
    END SUBROUTINE


    SUBROUTINE io_set_grid
        INTEGER :: i

        CALL grib_lsm%open_as_input(lsmfile ,'Land-sea mask', clsm_vars, ilsm_pids)
        CALL assert(grib_lsm%Npoints > 0)
        npoints = grib_lsm%Npoints

        PRINT *, '*** data dimensions  *** ', npoints
        CALL assert(.NOT. ALLOCATED(lons) .AND. .NOT. ALLOCATED(lats))
        ALLOCATE(lats(npoints))
        ALLOCATE(lons(npoints))

        CALL assert(grib_lsm%next(), 'grib_lsm%next()')
        CALL grib_lsm%coordinates(lats, lons)

        CALL grib_temp%open_as_input(tempfile, 'temperature', ctemp_vars, itemp_pids)
        CALL grib_mintemp%open_as_input(maxtempfile, 'maximum daily temperature', ctemp_vars, imaxtemp_pids)
        CALL grib_maxtemp%open_as_input(mintempfile, 'minimum daily temperature', ctemp_vars, imintemp_pids)
        CALL grib_rh%open_as_input(rhfile ,'relative humidity', crh_vars, irh_pids)
        CALL grib_maxrh%open_as_input(maxrhfile ,'maximum daily relative humidity', crh_vars, imaxrh_pids)
        CALL grib_minrh%open_as_input(minrhfile ,'minimum daily relative humidity', crh_vars, iminrh_pids)
        CALL grib_rain%open_as_input(rainfile ,'rainfall', crain_vars, irain_pids)
        CALL grib_cc%open_as_input(ccfile ,'cloud cover', ccc_vars, icc_pids)
        CALL grib_wspeed%open_as_input(wspeedfile ,'wind speed', cwspeed_vars, iwspeed_pids)
        CALL grib_snow%open_as_input(snowfile ,'ground snow', csnow_vars, isnow_pids)
        CALL grib_dp%open_as_input(dpfile ,'duration of precipitation in the previous 24h', cdp_vars, idp_pids)
        CALL grib_vs%open_as_input(vsfile ,'vegetation stage', cvs_vars, ivs_pids)
        CALL grib_cr%open_as_input(crfile ,'climate region', ccr_vars, icr_pids)
        CALL grib_fm%open_as_input(fmfile ,'fuel model', cfm_vars, ifm_pids)
        CALL grib_slope%open_as_input(slopefile ,'slope of sub-gridscale orography', cslope_vars, islope_pids)
        CALL grib_cv%open_as_input(cvfile ,'fractional coverage for vegetation (high + low)', ccv_vars, icv_pids)
        CALL grib_rainclim%open_as_input(rainclimfile ,'climate rainfall', crainclim_vars, irainclim_pids)

        ntimestep = MAXVAL(input(:)%count)
        CALL assert(input(1)%count == 1 .OR. input(1)%count == ntimestep)

        DO i = 2, SIZE(input)
            CALL assert(input(i)%same_geometry(input(1)))
            CALL assert(input(i)%count == 1 .OR. input(i)%count == ntimestep)
        END DO
    END SUBROUTINE


    SUBROUTINE io_write_constant_fields
        INTEGER :: fd, handle

        CALL assert(ref%handle /= 0)
        IF (LEN(TRIM(constant_file)) == 0 .OR. TRIM(constant_file) == 'none') RETURN
        CALL codes_open_file(fd, constant_file, 'w')

        CALL ref%write_other_field(fd, ilsm_pids(1), rlsm)
        CALL ref%write_other_field(fd, icv_pids(1), rcv)
        CALL ref%write_other_field(fd, irainclim_pids(1), rrainclim)

        CALL ref%write_other_field_as_integer(fd, icr_pids(1), icr)
        CALL ref%write_other_field_as_integer(fd, ifm_pids(1), ifm)
        CALL ref%write_other_field_as_integer(fd, islope_pids(1), islope)

        CALL codes_close_file(fd)
        fd = 0
    END SUBROUTINE


    SUBROUTINE io_write_restart
        INTEGER :: fd, handle,j

        CALL assert(ref%handle /= 0)
        IF (LEN(TRIM(init_file)) == 0 .OR. TRIM(init_file) == 'none') RETURN
        CALL codes_open_file(fd, init_file, 'w')

        CALL ref%write_other_field(fd, imc_r1hr_pids(1), mc(:)%r1hr)
        CALL ref%write_other_field(fd, imc_r10hr_pids(1), mc(:)%r10hr)
        CALL ref%write_other_field(fd, imc_r100hr_pids(1), mc(:)%r100hr)
        CALL ref%write_other_field(fd, imc_r1000hr_pids(1), mc(:)%r1000hr)
        CALL ref%write_other_field(fd, imc_rherb_pids(1), mc(:)%rherb)
        CALL ref%write_other_field(fd, imc_rwood_pids(1), mc(:)%rwood)
        CALL ref%write_other_field(fd, imc_rx1000_pids(1), mc(:)%rx1000)
        CALL ref%write_other_field(fd, imc_rbndryt_pids(1), mc(:)%rbndryt)

        CALL ref%write_other_field(fd, imark5_fuel_kb_drought_index_pids(1), mark5_fuel(:)%kb_drought_index)
        CALL ref%write_other_field(fd, imark5_fuel_timesincerain_pids(1), mark5_fuel(:)%timesincerain)

        CALL ref%write_other_field(fd, ifwi_risk_ffmc_pids(1), fwi_risk(:)%ffmc)
        CALL ref%write_other_field(fd, ifwi_risk_dmc_pids(1), fwi_risk(:)%dmc)
        CALL ref%write_other_field(fd, ifwi_risk_dc_pids(1), fwi_risk(:)%dc)

        CALL codes_close_file(fd)
        fd = 0
    END SUBROUTINE


    SUBROUTINE io_write_results(istep)
        INTEGER, INTENT(IN) :: istep
        INTEGER :: fd
        REAL, ALLOCATABLE :: tmp(:)

        ! Open output file
        fd = 0
        CALL codes_open_file(fd, output_file, 'w')
        CALL assert(fd /= 0, 'codes_open_file (w): '//output_file)

        CALL write_field(irain_pids(1), rrain)
        CALL write_field(itemp_pids(1), rtemp)
        CALL write_field(imaxtemp_pids(1), rmaxtemp)
        CALL write_field(imintemp_pids(1), rmintemp)
        CALL write_field(irh_pids(1), rrh)
        CALL write_field(imaxrh_pids(1), rmaxrh)
        CALL write_field(iminrh_pids(1), rminrh)
        CALL write_field(icc_pids(1), rcc)
        CALL write_field(isnow_pids(1), rsnow)
        CALL write_field(iwspeed_pids(1), rwspeed)
        CALL write_field(idp_pids(1), rdp)
        CALL write_field_from_integer(ivs_pids(1), ivs)

        CALL write_field(imc_r1hr_pids(1), mc(:)%r1hr)
        CALL write_field(imc_r10hr_pids(1), mc(:)%r10hr)
        CALL write_field(imc_r100hr_pids(1), mc(:)%r100hr)
        CALL write_field(imc_r1000hr_pids(1), mc(:)%r1000hr)
        CALL write_field(imc_rx1000_pids(1), mc(:)%rx1000)
        CALL write_field(imc_rherb_pids(1), mc(:)%rherb)
        CALL write_field(imc_rwood_pids(1), mc(:)%rwood)

        CALL write_field(ifire_prop_ros_pids(1), fire_prop(:)%ros)
        CALL write_field_from_integer(ifire_prop_sc_pids(1), fire_prop(:)%sc)
        CALL write_field_from_integer(ifire_prop_erc_pids(1), fire_prop(:)%erc)
        CALL write_field_from_integer(ifire_prop_bi_pids(1), fire_prop(:)%bi)

        CALL write_field_from_integer(ifire_prob_ic_pids(1), fire_prob(:)%ic)
        CALL write_field_from_integer(ifire_prob_mcoi_pids(1), fire_prob(:)%mcoi)
        CALL write_field_from_integer(ifire_prob_loi_pids(1), fire_prob(:)%loi)
        CALL write_field(ifire_prob_fli_pids(1), fire_prob(:)%fli)

        CALL write_field(imark5_fuel_kb_drought_index_pids(1), mark5_fuel(:)%kb_drought_index)
        CALL write_field(imark5_fuel_drought_factor_pids(1), mark5_fuel(:)%drought_factor)
        CALL write_field(imark5_fuel_moist_pids(1), mark5_fuel(:)%moist)
        CALL write_field(imark5_fuel_weight_pids(1), mark5_fuel(:)%weight)

        CALL write_field(imark5_prop_ros_theta0_pids(1), mark5_prop(:)%ros_theta0)
        CALL write_field(imark5_prop_ros_theta_pids(1), mark5_prop(:)%ros_theta)
        CALL write_field(imark5_prop_flame_height_pids(1), mark5_prop(:)%flame_height)
        CALL write_field(imark5_prop_flame_distance_pids(1), mark5_prop(:)%flame_distance)
        CALL write_field(imark5_prob_fire_danger_index_pids(1), mark5_prob(:)%fire_danger_index)

        CALL write_field(ifwi_risk_fwi_pids(1), fwi_risk(:)%fwi)
        CALL write_field(ifwi_risk_ffmc_pids(1), fwi_risk(:)%ffmc)
        CALL write_field(ifwi_risk_dmc_pids(1), fwi_risk(:)%dmc)
        CALL write_field(ifwi_risk_dc_pids(1), fwi_risk(:)%dc)

        CALL write_field(ifwi_risk_isi_pids(1), fwi_risk(:)%isi)
        CALL write_field(ifwi_risk_bui_pids(1), fwi_risk(:)%bui)
        CALL write_field(ifwi_risk_dsr_pids(1), fwi_risk(:)%dsr)
        CALL write_field(ifwi_risk_danger_risk_pids(1), fwi_risk(:)%danger_risk)

        IF (ALLOCATED(tmp)) THEN
            DEALLOCATE(tmp)
        ENDIF

    CONTAINS

        SUBROUTINE write_field(paramid, values)
            INTEGER, INTENT(IN) :: paramid
            REAL, INTENT(IN) :: values(:)
            ! Note: reference field defines metadata (aside from paramId)
            CALL ref%write_other_field(fd, paramid, values)
        END SUBROUTINE

        SUBROUTINE write_field_from_integer(paramid, values)
            INTEGER, INTENT(IN) :: paramid
            INTEGER, INTENT(IN) :: values(:)
            CALL assert(SIZE(values) == npoints)
            IF (.NOT. ALLOCATED(tmp)) THEN
                ALLOCATE(tmp(npoints))
            ENDIF
            tmp = values
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
      INTEGER :: i, n
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
          CALL codes_get(this%handle, 'numberOfDataPoints', n)
          CALL assert(n == this%Npoints .AND. i == this%paramId,&
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

    SUBROUTINE gribfield_write_other_field_as_integer(this, fd, paramid, values)
       CLASS(GribField), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: fd, paramid
       INTEGER, INTENT(IN) :: values(:)
       REAL, ALLOCATABLE :: tmp(:)

       ALLOCATE(tmp(SIZE(values)))
       tmp = values
       CALL gribfield_write_other_field(this, fd, paramid, tmp)
       DEALLOCATE(tmp)
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

