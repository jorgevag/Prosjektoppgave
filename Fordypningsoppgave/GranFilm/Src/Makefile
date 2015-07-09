PROG_ROOT = GranFilm

SRCS =  broadening_module.f90					\
	check_required_parameter_module.f90			\
	check_parameters_module.f90				\
	command_line_handling_module.f90 			\
	compile_date_time_m.F90					\
	curvefit.f90						\
	default_parameter_module.f90				\
	derived_type_module.f90					\
	eigenmodes_module.f90					\
	epsilon_corrections_module.f90				\
	error_module.f90					\
	f2003_wrapper_mod.f90					\
	finalize_module.f90					\
	fit_experimental_data_module.f90			\
	GranFilm.f90						\
	initialize_module.f90					\
	integrals_module.f90					\
	integrals_sphere_module.f90				\
	integrals_spheroid_module.f90				\
	integrand_module.f90					\
	integrand_module_spheroid_oblate.f90			\
	integrand_module_spheroid_prolate.f90			\
	interaction_module.f90					\
	legendre_module.f90					\
	linear_algebra_module.f90				\
	matrix_system_sphere_module.f90				\
	matrix_system_spheroid_module.f90			\
	minpack.f90						\
	multipole_coefficient_module.f90			\
	optics_module.f90					\
	physical_constants_module.f90				\
	physical_observables_module.f90				\
	polarizabilities_module.f90				\
	post_process_parameters_module.f90			\
	potential_module.f90					\
	potential_derivatives_module.f90			\
	print_parameters_module.f90				\
	quadpack_wrapper_module.f90				\
	read_input_file_module.f90				\
	reflection_transmission_amplitudes_module.f90		\
	shared.f90						\
	SFL_Error_Handling.f90 					\
	SFL_Interpolation.f90					\
	SFL_ISO_Fortran_Env.f90					\
	SFL_NR_Util.f90						\
	SFL_Logical_Units.f90					\
	SFL_Precision.f90 					\
	SFL_Util.f90 						\
	software_info_mod.f90					\
	sopra_dielectric_function_module.f90			\
	special_functions_module.f90				\
	supported_commandline_options_module.f90		\
	supported_options_module.f90				\
	susceptibilities_module.f90				\
	tools_module.f90					\
	tools_read_input_file_module.f90			\
	working_precision.f90
#	granfilm_error_handling_module.f90			\
#	dielectric_function_module.f90				\

OBJS =	broadening_module.o					\
	check_required_parameter_module.o			\
	check_parameters_module.o				\
	command_line_handling_module.o				\
	compile_date_time_m.o					\
	curvefit.o						\
	default_parameter_module.o				\
	derived_type_module.o					\
	eigenmodes_module.o					\
	epsilon_corrections_module.o				\
	error_module.o						\
	f2003_wrapper_mod.o					\
	finalize_module.o					\
	fit_experimental_data_module.o				\
	GranFilm.o						\
	initialize_module.o					\
	integrals_module.o					\
	integrals_sphere_module.o				\
	integrals_spheroid_module.o				\
	integrand_module.o					\
	integrand_module_spheroid_oblate.o			\
	integrand_module_spheroid_prolate.o			\
	interaction_module.o					\
	legendre_module.o					\
	linear_algebra_module.o					\
	matrix_system_sphere_module.o				\
	matrix_system_spheroid_module.o				\
	minpack.o						\
	multipole_coefficient_module.o				\
	optics_module.o						\
	physical_constants_module.o				\
	physical_observables_module.o				\
	polarizabilities_module.o				\
	post_process_parameters_module.o			\
	potential_module.o					\
	potential_derivatives_module.o				\
	print_parameters_module.o				\
	quadpack_wrapper_module.o				\
	read_input_file_module.o				\
	reflection_transmission_amplitudes_module.o		\
	shared.o						\
	SFL_Error_Handling.o					\
	SFL_Interpolation.o					\
	SFL_ISO_Fortran_Env.o					\
	SFL_NR_Util.o						\
	SFL_Logical_Units.o					\
	SFL_Precision.o						\
	SFL_Util.o						\
	software_info_mod.o					\
	sopra_dielectric_function_module.o			\
	special_functions_module.o				\
	supported_commandline_options_module.o			\
	supported_options_module.o				\
	susceptibilities_module.o				\
	tools_module.o						\
	tools_read_input_file_module.o				\
	working_precision.o
#	granfilm_error_handling_module.o			\
#	dielectric_function_module.o				\



RCS_ID = .rcs_id 


# Numerical Library to use (NAG/SLATEC)
NUMLIB=GNU #NAG




# ----------------------------------------------------------------

#
# --- Are we running 32 or 64 bits OS....?
#      .. This at least seems to work on Linux....  
#
LBITS := $(shell getconf LONG_BIT)
#ifeq ($(LBITS),64)
   # do 64 bit stuff here
   #MODPATH =  $(HOME)/usr/include64/
   #LIBPATH =  $(HOME)/usr/lib64 
   MODPATH =  /usr/include/
   LIBPATH =  /usr/lib 
   EXT     =  
   ARCH    =   
#else
   ## do 32 bit stuff here
   ##MODPATH =  $(HOME)/usr/include
   ##LIBPATH =  $(HOME)/usr/lib
   #EXT     =  
   #ARCH    =  # -tp core2 -pg
#endif

#Redefine the PROG name to include the extension
PROG = $(PROG_ROOT)$(EXT)


# ------------------------------------------------





# ----------------------------------------------------------------

# Set default compiler suite
#ifndef $(F90)
#	F90=lf95
#endif

# Gfortran
#F90         = pgf90 #gfortran -fbounds-check #-mp
F90         = gfortran -O3 #-fbounds-check #-mp
F90FLAGS    = -D$(NUMLIB) -I$(MODPATH) $(ARCH) 



# PGI
#F90         = pgf95 #-mp
#F90FLAGS    = -D$(NUMLIB) -I$(MODPATH) $(ARCH) -fast #-Mbounds #-Mconcur -Minfo  #-mp #-Mbounds  # -mp #-C # -fast #-


LAPACK      = -llapack -lblas   #-lacml #-lacml_mp #-lblas #-lacml   # -lblas  ACML seems to be faster
F90LIBS     = $(LAPACK) -lSlatec41E                  # $(LAPACK) -lnetcdf -lSlatec41E 
#F90LIBS     = -lnag -lSlatec41E -lnetcdf 

F90LDFLAGS  = -L$(LIBPATH) 

LIBS        =  $(F90LIBS) 
LDFLAGS     =  $(F90LDFLAGS) #-Bstatic #-Mconcur  # -Bstatic

ETAGS	    = etags

DEPEND      = makedepf90

SVNVERSION  = `test -d .svn && svnversion` #svnversion
HGVERSION   = `hg log -r tip --template '{node|short}'`


all: etags rcs $(PROG) 

$(PROG): $(OBJS)  $(RCS_ID)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	$(RM) $(PROG) $(OBJS) *.mod TAGS .svn_revision .depend

etags: 
	$(ETAGS) $(SRCS) *.h90

depend .depend:
	$(RM) .depend
	$(DEPEND) $(SRCS) > .depend	

rcs $(RCS_ID): 
	@echo "! --- Automatically generated by : make rcs " >  $(RCS_ID)
	@echo "rcs_id = \"$(HGVERSION)\" " > $(RCS_ID)

#svn .svn_revision:
#	echo "! --- Automatically generated by : make svn " >  .svn_revision
#	echo "svn_revision = \"$(SVNVERSION)\" " > .svn_revision
#
#hg:
#	HGVERSION = $(shell hg id)
#	#CFLAGS = -DHGVERSION="\"${HGVERSION}\""


src:
	tar cvfz $(PROG).tgz $(SRCS) Makefile *.h90 *.h *.inc


.SUFFIXES: 
.SUFFIXES: $(SUFFIXES) .f90 .F90 .o .mod



.f90.o:
	$(F90) $(F90FLAGS) -c $<

.f90.mod:
	$(F90) $(F90FLAGS) -c $<

.F90.o:
	$(F90) $(F90FLAGS) -c $<

.F90.mod:
	$(F90) $(F90FLAGS) -c $<

.f.o:
	$(F90) $(F90FLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c  $<

.cc.o:
	$(CPP) $(CPPFLAGS) -c $<

.cpp.o:
	$(CPP) $(CPPFLAGS) -c $<

# Override the modula2 default 
#   (without it make will complain about not finding m2c.....)
%.o:%.mod





# ... Dependencies
# ......................


# Include the dependency list (created by $(DEPEND)) 
include .depend

