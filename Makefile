IFS	=	source_v1.3.a

include ../config.$(ARCH)
include ../options.$(ARCH)

SOURCE	=\

OBJECTS	=	$(SOURCE:.F=.o)

.SUFFIXES:
.SUFFIXES:	.o .F

.F.o :
	$(FC) -c $(FFLAGS) $(CP_OPT) $*.F

all:	$(OBJECTS)
	touch source_v1.3.a

clean:
	 \rm -f $(OBJECTS)
