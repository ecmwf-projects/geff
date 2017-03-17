MODULE mo_ncdf_tools
!--------------------------------------------------------- 
! ECFIRE: EC-Fire model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!tools for ncdf input output 
!
!---------------------------------------------------------

CONTAINS

SUBROUTINE check(status)

  USE netcdf

  IMPLICIT NONE

  INTEGER, INTENT(in) :: status

  IF (status /= nf90_noerr) THEN
    PRINT *,TRIM(NF90_STRERROR(status))
    STOP 'Bad NETCDF status'
  END IF
END SUBROUTINE check


SUBROUTINE define_ncdf_output(ivarid,name,title,iunits,type,ndiag,idarray)
  USE netcdf
  USE mo_control
  USE mo_fire

  IMPLICIT NONE

  INTEGER, INTENT(in) :: idarray(:) ! flexible vector to set coordinate dimensions
  INTEGER, INTENT(inout) :: ivarid(2), ndiag
  CHARACTER (len=*), INTENT(IN) :: name, title, iunits,type
  SELECT CASE (type)

  CASE ("FLOAT") 
     CALL check(NF90_DEF_VAR(ncidout, name, nf90_FLOAT, idarray , iVarID(1)))
     CALL check(NF90_PUT_ATT(ncidout, iVarID(1), "title", title) )
     CALL check(NF90_PUT_ATT(ncidout, iVarID(1), "units", iunits) )
     CALL check(NF90_PUT_ATT(ncidout, iVarID(1), "_FillValue", rfillvalue) )
     ndiag=ndiag+1   ! increase the diagnostic counter
     iVarid(2)=ndiag
  CASE ("INTEGER") 
     CALL check(NF90_DEF_VAR(ncidout, name, nf90_INT, idarray , iVarID(1)))
     CALL check(NF90_PUT_ATT(ncidout, iVarID(1), "title", title) )
     CALL check(NF90_PUT_ATT(ncidout, iVarID(1), "units", iunits) )
     CALL check(NF90_PUT_ATT(ncidout, iVarID(1), "_FillValue", ifillvalue) )
     ndiag=ndiag+1   ! increase the diagnostic counter
     iVarid(2)=ndiag
  END SELECT


END SUBROUTINE define_ncdf_output

END MODULE mo_ncdf_tools

