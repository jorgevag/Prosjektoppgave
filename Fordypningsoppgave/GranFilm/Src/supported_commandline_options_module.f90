!-------------------------------------------------!
Module Supported_Commandline_Option_Module
!-------------------------------------------------!
  !
  ! --------------------------------------------------------
  !
  ! PURPOSE 
  !   Defining and setting the commandline options used 
  !   by GranFilm 
  !
  ! AUTHOR
  !   Ingve Simonsen, Trondheim, Dec. 21.12.2013
  !
  ! --------------------------------------------------------
  !


  ! --- The Use Statements global to the module
  !Use Shared, only : Param, Software_Information


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: GF_CommandLine_Option_T
  Public :: Set_Granfilm_Commandline_Options

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private 





  !
  ! --- No. of potential commad line arguments.
  Integer, parameter :: GF_arg = 9
  !
  ! --- Derived option type
  Type GF_CommandLine_Option_T
     character(len=20), dimension(GF_arg)  :: option
     character(len=80), dimension(GF_arg)  :: text
     integer,           dimension(GF_arg)  :: arguments = 0
  End type GF_CommandLine_Option_T




!-------!  
contains
!-------!



  ! ------------------------------------------------------------- !
  Subroutine Set_Granfilm_Commandline_Options( GF_Option_Table )
  ! ------------------------------------------------------------- !
    implicit none
    
    Type(GF_CommandLine_Option_T), intent(inout) :: GF_Option_Table
    ! --- Local 
    integer :: i


    ! --- help
    i=1
    GF_Option_Table%option(i)    = "-help"
    GF_Option_Table%text(i)      = "Display this help message and exit (Flag)"
    GF_Option_Table%arguments(i) = 0
    
    ! --- input/parameter file
    i=i+1  
    GF_Option_Table%option(i)    = "-p"
    GF_Option_Table%text(i)      = "Parameter file name (sif-format)"
    GF_Option_Table%arguments(i) = 1
        
    ! --- output file
    i=i+1  
    GF_Option_Table%option(i)    = "-o"
    GF_Option_Table%text(i)      = "Output ASCII or HDF5 filename"
    GF_Option_Table%arguments(i) = 1
    

    ! --- Print example Parameter file
    i=i+1  
    GF_Option_Table%option(i)    = "-P"
    GF_Option_Table%text(i)      = "Print example parameter file (Flag) "
    GF_Option_Table%arguments(i) = 0


    ! --- Version
    i=i+1  
    GF_Option_Table%option(i)    = "-V"
    GF_Option_Table%text(i)      = "Version (Flag)"
    GF_Option_Table%arguments(i) = 0

    ! --- verbose
    i=i+1  
    GF_Option_Table%option(i)    = "-v"
    GF_Option_Table%text(i)      = "Verbose [mostly useful for debuging] (Flag)"
    GF_Option_Table%arguments(i) = 0

    ! --- Experimental data file used for fitting
    i=i+1  
    GF_Option_Table%option(i)    = "-f"
    GF_Option_Table%text(i)      = "Experimental data file used for fitting"
    GF_Option_Table%arguments(i) = 1

    ! --- GUI
    i=i+1  
    GF_Option_Table%option(i)    = "-gui_mode"
    GF_Option_Table%text(i)      = "Output format consistent with the GranFilm GUI (Flag)"
    GF_Option_Table%arguments(i) = 0

    ! --- python interface
    i=i+1  
    GF_Option_Table%option(i)    = "-py"
    GF_Option_Table%text(i)      = "Python interface [internal use only] (Flag)"
    GF_Option_Table%arguments(i) = 0



  End Subroutine Set_Granfilm_Commandline_Options
  ! ------------------------------------------------------------- !



End Module Supported_Commandline_Option_Module
  !----------------------------------!
