! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief I/O abstraction module
!> @author Maciel, P., ECMWF
MODULE mo_io

   USE mo_io_netcdf
   USE mo_io_eccodes
   USE mo_control

   IMPLICIT NONE

CONTAINS

    SUBROUTINE io_set_grid
      IF (lgrib) THEN
          CALL eccodes_set_grid
          RETURN
      ENDIF
      CALL netcdf_set_grid
    END SUBROUTINE

    SUBROUTINE io_open_input
      IF (lgrib) THEN
          CALL eccodes_open_input
          RETURN
      ENDIF
      CALL netcdf_open_input
    END SUBROUTINE

    SUBROUTINE io_initialize
      IF (lgrib) THEN
          CALL eccodes_initialize
          RETURN
      ENDIF
      CALL netcdf_initialize
    END SUBROUTINE

    SUBROUTINE io_open_output
      IF (lgrib) THEN
          CALL eccodes_open_output
          RETURN
      ENDIF
      CALL netcdf_open_output
    END SUBROUTINE

    SUBROUTINE io_getdata(istep)
      INTEGER, INTENT(IN) :: istep
      IF (lgrib) THEN
          CALL eccodes_getdata(istep)
          RETURN
      ENDIF
      CALL netcdf_getdata(istep)
    END SUBROUTINE

    SUBROUTINE io_write_results(istep)
      INTEGER, INTENT(IN) :: istep
      IF (lgrib) THEN
          CALL eccodes_write_results(istep)
          RETURN
      ENDIF
      CALL netcdf_write_results(istep)
    END SUBROUTINE

    SUBROUTINE io_write_restart
      IF (lgrib) THEN
          CALL eccodes_write_restart
          RETURN
      ENDIF
      CALL netcdf_write_restart
    END SUBROUTINE

    SUBROUTINE io_write_constant_fields
      IF (lgrib) THEN
          CALL eccodes_write_constant_fields
          RETURN
      ENDIF
      CALL netcdf_write_constant_fields
    END SUBROUTINE

    SUBROUTINE io_setdown
      IF (lgrib) THEN
          CALL eccodes_setdown
          RETURN
      ENDIF
      CALL netcdf_setdown
    END SUBROUTINE

END MODULE

!     module factory_pattern

!     type CFactory
!         private
!             character(len=20) :: factory_type               !! Descriptive name for database
!             class(Connection), pointer :: connection_type   !! Which type of database ?
!         contains                                            !! Note 'class' not 'type' !
!             procedure :: init                               !! Constructor
!             procedure :: create_connection                  !! Connect to database
!             procedure :: final                              !! Destructor
!     end type

!     type, abstract :: Connection
!         contains
!         procedure(generic_desc), deferred, pass(self) :: description
!     end type Connection


!     abstract interface
!         subroutine io_generic_desc(self)
!             import :: Connection
!             class(Connection), intent(in) :: self
!         end subroutine io_generic_desc
!     end interface


!     !! An Oracle connection
!     type, extends(Connection) :: OracleConnection
!         contains
!             procedure, pass(self) :: description => oracle_desc
!     end type OracleConnection


!     !! A MySQL connection
!     type, extends(Connection) :: MySQLConnection
!         contains
!             procedure, pass(self) :: description => mysql_desc
!     end type MySQLConnection

!     contains

!     subroutine io_oracle_desc(self)
!         class(OracleConnection), intent(in) :: self
!         write(*,'(A)') "You are now connected with Oracle"
!     end subroutine io_oracle_desc

!     subroutine io_mysql_desc(self)
!         class(MySQLConnection), intent(in) :: self
!         write(*,'(A)')  "You are now connected with MySQL"
!     end subroutine io_mysql_desc

!     subroutine io_init(self, string)
!         class(CFactory), intent(inout) :: self
!         character(len=*), intent(in) :: string
!         self%factory_type = trim(string)
!         self%connection_type => null()            !! pointer is nullified
!     end subroutine io_init

!     subroutine io_final(self)
!         class(CFactory), intent(inout) :: self
!         deallocate(self%connection_type)          !! Free the memory
!         nullify(self%connection_type)
!     end subroutine io_final

!     function create_connection(self)  result(ptr)
!         class(CFactory) :: self
!         class(Connection), pointer :: ptr

!         if(self%factory_type == "Oracle") then
!             if(associated(self%connection_type))   deallocate(self%connection_type)
!             allocate(OracleConnection :: self%connection_type)
!             ptr => self%connection_type
!         elseif(self%factory_type == "MySQL") then
!             if(associated(self%connection_type))   deallocate(self%connection_type)
!             allocate(MySQLConnection :: self%connection_type)
!             ptr => self%connection_type
!         end if
!     end function create_connection

!     end module

!    program main

!    use factory_pattern
!    implicit none
!    
!        type(CFactory) :: factory
!        class(Connection), pointer :: db_connect => null()
!    
!        call factory%init("Oracle")
!        db_connect => factory%create_connection()   !! Create Oracle DB
!        call db_connect%description()
!    
!        !! The same factory can be used to create different connections
!        call factory%init("MySQL")                  !! Create MySQL DB
!    
!        !! 'connect' is a 'class' pointer. So can be used for either Oracle or MySQL
!        db_connect => factory%create_connection()
!        call db_connect%description()
!    
!        call factory%final()        ! Destroy the object
!    
!    end program main

