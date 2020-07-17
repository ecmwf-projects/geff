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
        procedure(generic_can_interpolate), public, deferred, nopass :: can_interpolate
    endtype

    interface
        subroutine generic_interpolate(method, field)
            character(len=*), intent(in) :: method
            real, allocatable, intent(inout) :: field(:)
        end subroutine
        function generic_can_interpolate()
            logical :: generic_can_interpolate
        end function
    end interface

    type, public, extends(interpolation_t) :: no_interpolation_t
    contains
        procedure, nopass :: interpolate => no_interpolation_interpolate
        procedure, nopass :: can_interpolate => no_interpolation_can_interpolate
    endtype

    type(no_interpolation_t), public, target :: no_interpol

contains

    subroutine no_interpolation_interpolate(method, field)
        implicit none
        character(len=*), intent(in) :: method
        real, allocatable, intent(inout) :: field(:)

        call assert(.false., 'interpolate: no interpolation setup')
    end subroutine

    function no_interpolation_can_interpolate()
        implicit none
        logical :: no_interpolation_can_interpolate
        no_interpolation_can_interpolate = .false.
    end function
end module
