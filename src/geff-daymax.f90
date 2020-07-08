! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief GEFF max of the day
program main

    use eccodes_m
    use mo_statistics

    implicit none

    real(kind=8), dimension(:, :), allocatable :: var
    real(kind=8), dimension(:), allocatable :: maxvar, minvar, meanvar, timemax

    integer :: nt, npoints
    type(eccodes_t) :: io

    call io%input('dvar.grib', var)
   
    nt = size(var, 2)
    npoints = size(var, 1)
     allocate (maxvar(npoints))
    allocate (minvar(npoints))
    allocate (meanvar(npoints))
    allocate (timemax(npoints))
   
    call assert(0 == compute_maxfwi(nt, npoints,var, -9999.D0, &
                                         maxvar=maxvar, minvar=minvar, meanvar=meanvar, timemax=timemax))

    call io%output('maxvar.grib', maxvar)
    call io%output('minvar.grib', minvar)
    call io%output('meanvar.grib', meanvar)
    call io%output('timemax.grib', timemax)
   
end program
