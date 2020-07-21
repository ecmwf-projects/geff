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
    use mo_utilities, only: assert

    implicit none
    private

    type, public, extends(interpolation_t) :: atlas_interpolation_t
    contains
        procedure, nopass :: coordinates => atlas_coordinates
        procedure, nopass :: interpolate => atlas_interpolate
        procedure, nopass :: can_interpolate => atlas_can_interpolate
    end type

    type(atlas_interpolation_t), public, target :: atlas_interpol

contains

    subroutine atlas_coordinates(grid, lat, lon)
        implicit none
        character(len=*), intent(in) :: grid
        real, intent(inout) :: lat(:), lon(:)

        type(atlas_StructuredGrid) :: g
        integer :: i, j, k, npts
        real :: l(2)

        call fckit_log%debug("GEFF coordinates...")

        g = atlas_StructuredGrid(grid)

        npts = g%size()
        call assert(size(lat) == npts, 'atlas_coordinates: size(lat) == npts')
        call assert(size(lon) == npts, 'atlas_coordinates: size(lon) == npts')

        k = 0
        do j = 1, g%ny()
            do i = 1, g%nx(j)
                k = k + 1
                call assert(k <= npts, 'atlas_coordinates: k <= npts')
                l = g%lonlat(i, j)
                lon(k) = l(1)
                lat(k) = l(2)
            enddo
        enddo
        call assert(k == npts, 'atlas_coordinates: k == npts')

        call assert(maxval(lat) <= 90, 'atlas_coordinates: maxval(lat) <= 90')
        call assert(minval(lat) >= -90, 'atlas_coordinates: minval(lat) >= -90')
        call assert(maxval(lon) - minval(lon) <= 360, 'atlas_coordinates: maxval(lon) - minval(lon) <= 360')

        call fckit_log%debug("GEFF coordinates.")
    end subroutine

    subroutine atlas_interpolate(method, gridA, gridB, fieldA, fieldB)
        implicit none
        character(len=*), intent(in) :: method, gridA, gridB
        real, intent(in) :: fieldA(:)
        real, intent(inout) :: fieldB(:)

        type(atlas_functionspace_NodeColumns) :: fsA, fsB
        type(atlas_Config) :: cfg1, cfg2
        type(atlas_Field) :: fA, fB
        type(atlas_StructuredGrid) :: A, B
        type(atlas_Interpolation) :: interpol
        type(atlas_Mesh) :: meshA, meshB
        type(atlas_MeshGenerator) :: gen

        call fckit_log%debug("GEFF interpolate...")

        call fckit_log%debug("GEFF atlas_MeshGenerator")
        cfg1 = atlas_Config()
        call cfg1%set("type", "structured")
        call cfg1%set("3d", .true.)
        gen = atlas_MeshGenerator(cfg1)

        call fckit_log%debug("GEFF atlas_functionspace_NodeColumns(meshA("//TRIM(gridA)//"))")
        A = atlas_StructuredGrid(gridA)
        meshA = gen%generate(A)
        fsA = atlas_functionspace_NodeColumns(meshA)

        call fckit_log%debug("GEFF atlas_functionspace_NodeColumns(meshB("//TRIM(gridB)//"))")
        B = atlas_StructuredGrid(gridB)
        meshB = gen%generate(B)
        fsB = atlas_functionspace_NodeColumns(meshB)

        call fckit_log%debug("GEFF atlas_Interpolation("//TRIM(method)//", fsA, fsB)")
        cfg2 = atlas_Config()
        call cfg2%set("type", method)
        interpol = atlas_Interpolation(cfg2, fsA, fsB)

        fA = atlas_Field(fieldA)
        fB = atlas_Field(fieldB)
        call interpol%execute(fA, fB)
        call interpol%final()  ! is this necessary?

        call fckit_log%debug("GEFF interpolate.")
    end subroutine

    function atlas_can_interpolate()
        implicit none
        logical :: atlas_can_interpolate
        atlas_can_interpolate = .true.
    end function
end module

