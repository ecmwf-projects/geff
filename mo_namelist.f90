MODULE mo_namelist
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!namelists for input 
!
!---------------------------------------------------------
  USE mo_constants
  USE mo_control
  USE mo_fire

  IMPLICIT NONE

! program control namelist
  NAMELIST /control/output_file,rundir,date1,date2,nday,time,init_file, lstick,now,version

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
  print *,'file ',input//'ecfire.namelist'
  OPEN(8,file=input//'ecfire.namelist',status='OLD')
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
  IF (output_file=='screen') THEN
    iounit=6
  ELSE
    iounit=7
    OPEN(iounit,FILE=output//'ecfire.out')
  ENDIF

  WRITE(iounit,*) '-------------- ECFIRE '//version//' ---------------'
  WRITE(iounit,*) 'run date ',now
  WRITE(iounit,*)

  WRITE(iounit,*)' integration days: ',nday
  WRITE(iounit,*)' timestep: ',dt
  WRITE(iounit,*)' integration steps: ',nday/dt
  WRITE(iounit,*) 
   
  RETURN
END SUBROUTINE read_namelists

END MODULE mo_namelist

