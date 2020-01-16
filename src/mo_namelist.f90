! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


!> @brief Namelists for input
!> @author Di Giuseppe, F., ECMWF
MODULE mo_namelist

  USE mo_fire

  IMPLICIT NONE

! program control namelist
  NAMELIST /control/output_file,rundir,inidate,initime,dt,init_file,lstick,restart_day,now,version

! climate data namelist
  NAMELIST /climate/tempfile,maxtempfile,mintempfile,rhfile,maxrhfile,minrhfile,rainfile,ccfile,wspeedfile,snowfile,dpfile,vsfile

! constant data  namelist
  NAMELIST /constdata/rainclimfile,lsmfile,crfile,fmfile,cvfile,slopefile

CONTAINS
!-----------------------------------------------
  SUBROUTINE read_namelists

!-------------------------------
! 1. NAMELISTS
!-------------------------------
  PRINT *,'file ',input//'geff.namelist'
  OPEN(8,file=input//'geff.namelist',status='OLD')
  READ(8,nml=control)
  READ(8,nml=climate)
  READ(8,nml=constdata)
  CLOSE(8) ! namelist file

!-------------------------------
! 2. OUTPUT NAMELIST DATA
!-------------------------------

! ------
! output
! ------
  PRINT *, '-------------- GEFF '//version//' ---------------'
  PRINT *, 'run date ',now
  PRINT *,


  PRINT *,' timestep (hours): ',dt
  PRINT *,

  RETURN
END SUBROUTINE read_namelists

END MODULE mo_namelist

