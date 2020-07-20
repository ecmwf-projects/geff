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

        type(atlas_functionspace_NodeColumns) :: fsA, fsB
        type(atlas_Config) :: cfg1, cfg2
        type(atlas_Field) :: fA, fB
        type(atlas_StructuredGrid) :: A, B
        type(atlas_Interpolation) :: interpol
        type(atlas_Mesh) :: meshA, meshB
        type(atlas_MeshGenerator) :: gen

        call fckit_log%info("interpolate...")

        call fckit_log%info("atlas_MeshGenerator")
        cfg1 = atlas_Config()
        call cfg1%set("type", "structured")
        call cfg1%set("3d", .true.)
        gen = atlas_MeshGenerator(cfg1)

        call fckit_log%info("atlas_functionspace_NodeColumns(meshA("//TRIM(gridA)//"))")
        A = atlas_StructuredGrid(gridA)
        meshA = gen%generate(A)
        fsA = atlas_functionspace_NodeColumns(meshA)

        call fckit_log%info("atlas_functionspace_NodeColumns(meshB("//TRIM(gridB)//"))")
        B = atlas_StructuredGrid(gridB)
        meshB = gen%generate(B)
        fsB = atlas_functionspace_NodeColumns(meshB)

        call fckit_log%info("atlas_Interpolation("//TRIM(method)//", fsA, fsB)")
        cfg2 = atlas_Config()
        call cfg2%set("type", method)
        interpol = atlas_Interpolation(cfg2, fsA, fsB)

        fA = atlas_Field(fieldA)
        fB = atlas_Field(fieldB)
        call interpol%execute(fA, fB)
        call interpol%final()  ! is this necessary?

        call fckit_log%info("interpolate.")
    end subroutine

    function atlas_can_interpolate()
        implicit none
        logical :: atlas_can_interpolate
        atlas_can_interpolate = .true.
    end function
end module

