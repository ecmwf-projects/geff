#
# Makefile 
#
# Usage:  make all
#  will compile code and place the object files into 
# ./obj

# Set flags to point the compiler to the include files
# please make sure you have done the module load specified before running the 
# makefile
#module load netcdf4

FF           = gfortran
#FF           = pgf90
CC           = cc 

FFLAGS       = -O2
CCOMFLG      = -O3 
LIB          = ${NETCDF_LIB}
INCLUDE      = ${NETCDF_INCLUDE}
#FFLAGS= -g -C

.SUFFIXES: .f90 .c

.f90.o :
	$(FF) $(INCLUDE) -c $(FFLAGS)  $*.f90 -o $*.o $(LIB)
.c.o:
	$(CC) -c $(CCOMFLG) $*.c   -o $*.o

FORT = mo_constants.f90 mo_control.f90 mo_fire.f90 mo_namelist.f90	\
mo_ncdf_tools.f90 mo_nfdrs.f90 mo_mark5.f90 mo_fwi.f90  mo_vegstage.f90 mo_fuelmodel.f90		\
initialize.f90 set_grid.f90 getdata.f90 setup.f90 setdown.f90 open_output.f90	\
open_input.f90 geff.f90


CPP =\


OBJECT=$(FORT:.f90=.o) $(CPP:.c=.o)


geff:	$(OBJECT)
	$(FF) $(INLCUDE) $(OBJECT)  -o geff_exe  $(LIB)


all:	geff

clean:
	rm -f *.o *.mod geff_exe

