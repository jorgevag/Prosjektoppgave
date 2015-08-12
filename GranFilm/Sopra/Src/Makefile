PROG =	SOPRA

SRCS =	ISO_FORTRAN_ENV.f90 SFL_Error_Handling.f90 SFL_Get_Options.f90 \
	SFL_Interpolation.f90 SFL_Logical_Units.f90 SFL_NR_Util.f90 \
	SFL_Precision.f90 SFL_Util.f90 SOPRA.f90 \
	sopra_dielectric_function_module.f90

OBJS =	ISO_FORTRAN_ENV.o SFL_Error_Handling.o SFL_Get_Options.o \
	SFL_Interpolation.o SFL_Logical_Units.o SFL_NR_Util.o SFL_Precision.o \
	SFL_Util.o SOPRA.o sopra_dielectric_function_module.o

F90         = pgf95
F90FLAGS    = -fast -I$(HOME)/Fortran/Modules/Modinfo/
F90LIBS     =  
F90LDFLAGS  = -s -Bstatic -L$(HOME)/Fortran/Lib/ 

LIBS        =  $(F90LIBS) 	

LDFLAGS     =  $(F90LDFLAGS) 	

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

.f.o:
	$(F90) $(F90FLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c  $<

.cc.o:
	$(CPP) $(CPPFLAGS) -c $<

.cpp.o:
	$(CPP) $(CPPFLAGS) -c $<

# Override the modula2 default built-in rule! 
#   (without it, make will try to execute m2c .....) 
%.o:%.mod 
SFL_Error_Handling.o: SFL_Logical_Units.o
SFL_Get_Options.o: SFL_Precision.o
SFL_Interpolation.o: SFL_Precision.o
SFL_Logical_Units.o: ISO_FORTRAN_ENV.o
SFL_NR_Util.o: SFL_Precision.o
SFL_Util.o: ISO_FORTRAN_ENV.o SFL_NR_Util.o SFL_Precision.o
SOPRA.o: SFL_Error_Handling.o SFL_Get_Options.o SFL_Precision.o \
	sopra_dielectric_function_module.o
sopra_dielectric_function_module.o: SFL_Error_Handling.o SFL_Interpolation.o \
	SFL_Precision.o SFL_Util.o
