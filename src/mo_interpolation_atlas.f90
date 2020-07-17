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
        procedure, nopass :: can_interpolate => atlas_can_interpolate
    end type

    type(atlas_interpolation_t), public, target :: atlas_interpol

contains

    subroutine atlas_interpolate(method, gridA, gridB, fieldA, fieldB)
        implicit none
        character(len=*), intent(in) :: method, gridA, gridB
        real, intent(in) :: fieldA(:)
        real, intent(inout) :: fieldB(:)

        call fckit_log%info("MeMeMe!")
    end subroutine

    function atlas_can_interpolate()
        implicit none
        logical :: atlas_can_interpolate
        atlas_can_interpolate = .true.
    end function
end module

