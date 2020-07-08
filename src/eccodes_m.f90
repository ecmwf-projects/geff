! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief ecCodes I/O
module eccodes_m

    use eccodes, only: codes_clone, codes_close_file, codes_get, codes_get_size, codes_grib_new_from_file, codes_open_file, &
                       codes_release, codes_set, codes_write, CODES_END_OF_FILE
    use mo_utilities, only: assert

    implicit none
    private

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

    type :: GribField
        integer :: fd = 0
        integer :: handle = 0
        integer :: paramId = 0
        integer :: npoints = 0
        integer :: count = 0
    contains
        procedure :: next => gribfield_next
        procedure :: close => gribfield_close
        procedure :: values => gribfield_values
        procedure :: header => gribfield_header
    endtype

    type(GribField) :: reference
    real, parameter :: missingValue = -9999.0

contains

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
            call reference%close()
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
        call grib%close()
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

    subroutine gribfield_values(this, values)
        class(GribField), intent(in) :: this
        real, allocatable, intent(inout) :: values(:)
        integer :: n, bitmapPresent

        call assert(allocated(values), 'gribfield_values values allocated')
        call assert(size(values) == this%npoints, 'gribfield_values: values size mismatch)')

        call codes_get_size(this%handle, "values", n)
        call assert(size(values) == n, 'gribfield_values: codes_get_size("values") mismatch')

        ! ensure all missing values use the same indicator ('set' before 'get')
        call codes_get(this%handle, "bitmapPresent", bitmapPresent)
        if (bitmapPresent /= 0) call codes_set(this%handle, "missingValue", missingValue)

        call codes_get(this%handle, "values", values)
    end subroutine

    subroutine gribfield_header(this)
        class(GribField), intent(inout) :: this
        call assert(this%handle /= 0, 'this%handle')

        call codes_get(this%handle, 'paramId', this%paramid)
        call codes_get(this%handle, 'numberOfDataPoints', this%npoints)
        call assert(this%npoints > 0, 'numberOfDataPoints > 0')
    end subroutine

    function gribfield_next(this) result(r)
        class(GribField), intent(inout) :: this
        integer :: iret
        logical :: r
        if (this%handle /= 0) call codes_release(this%handle)
        call codes_grib_new_from_file(this%fd, this%handle, iret)
        r = iret /= CODES_END_OF_FILE
        call assert(.NOT. r .OR. iret == 0 .AND. this%handle /= 0, 'codes_grib_new_from_file')
    end function

    subroutine gribfield_close(this)
        class(GribField), intent(inout) :: this
        if (this%handle /= 0) call codes_release(this%handle)
        this%handle = 0
        if (this%fd /= 0) call codes_close_file(this%fd)
        this%fd = 0
    end subroutine

end module

