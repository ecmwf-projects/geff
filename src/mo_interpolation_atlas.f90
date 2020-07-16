! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Interpolation implementation using Atlas
!> @author Maciel, P., ECMWF
module mo_interpolation_atlas

    use fckit_module
    use atlas_module

    use mo_interpolation

    implicit none
    private

    type, public, extends(interpolation_t) :: atlas_interpolation_t
    contains
        procedure, nopass :: interpolate => atlas_interpolate
    end type

contains

    subroutine atlas_interpolate(path, data)
        implicit none

        character(len=*), intent(in) :: path
        real(kind=8), dimension(:, :), allocatable, intent(out) :: data

        !integer :: i, paramId, npoints
        !real(kind=4), dimension(:), allocatable :: tmp  ! ecCodes I/O does not support real(kind=8)?

        !type(GribField) :: grib

        call fckit_log%info("MeMeMe!")

    end subroutine
end module
