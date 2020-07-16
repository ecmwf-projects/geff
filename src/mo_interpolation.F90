! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Interpolation interface module
!> @author Maciel, P., ECMWF
module mo_interpolation

    use mo_utilities, only: assert

    implicit none
    private

    type, public, abstract :: interpolation_t
    contains
        procedure(generic_interpolate), public, deferred, nopass :: interpolate
    endtype

    interface
        subroutine generic_interpolate(path, data)
            character(len=*), intent(in) :: path
            real(kind=8), dimension(:, :), allocatable, intent(out) :: data
        end subroutine
    end interface

    type, public, extends(interpolation_t) :: no_interpolation_t
    contains
        procedure, nopass :: interpolate => no_interpolation
    endtype

contains

    subroutine no_interpolation(path, data)
        implicit none

        character(len=*), intent(in) :: path
        real(kind=8), dimension(:, :), allocatable, intent(out) :: data

        call assert(.false., 'no interpolation setup')

    end subroutine
end module
