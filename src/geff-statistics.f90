! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief GEFF statistics runtime
program main

    use eccodes_m
    use mo_statistics

    implicit none

    real(kind=8), dimension(:, :), allocatable :: fc, cl
    real(kind=8), dimension(:), allocatable :: efi, sot, anomaly, rank, canomaly, crank

    integer :: nfc, ncl, npoints
    type(eccodes_t) :: io

    call io%input('fc.grib', fc)
    call io%input('cl.grib', cl)

    nfc = size(fc, 2)
    ncl = size(cl, 2)
    npoints = size(fc, 1)

    allocate (efi(npoints))
    allocate (sot(npoints))
    allocate (anomaly(npoints))
    allocate (rank(npoints))
    allocate (canomaly(npoints))
    allocate (crank(npoints))

    call assert(0 == extreme_forecast_index(ncl, nfc, npoints, cl, fc, &
                                            efi, sot=sot, anomaly=anomaly, canomaly=canomaly, rank=rank, crank=crank))

    call io%output('efi.grib', efi)
    call io%output('sot.grib', sot)
    call io%output('anomaly.grib', anomaly)
    call io%output('rank.grib', rank)
    call io%output('canomaly.grib', canomaly)
    call io%output('crank.grib', crank)

end program
