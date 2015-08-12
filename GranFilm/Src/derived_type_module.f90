

!-------------------------!
Module Derived_Type_Module 
!-------------------------! 



! ----------------------------------------------------------
!  This is the main derived type for the GRANFILM code.
!  The code uses allocatable components of derived types in 
!  order to make a nice derived data structure.
!
!  NOTE :  This will be part of F2003! 
!          It is described in the Technical Report (TR) 15581,
!          and many F95 compilers have already implmented this
!          (lahey, intel, but not gfortran)
!          
!  SOLUTION : To get around the problem use ponters insted....!
!             (http://www.codecomments.com/archive271-2004-7-233980.html)
!
!          
!  See : http://www.nag.co.uk/nagware/NP/doc/TR.asp
!        http://www.codecomments.com/archive271-2004-7-233980.html
!        http://www.dbforums.com/showthread.php?t=1572001 
!        http://gcc.gnu.org/ml/fortran/2005-09/msg00561.html
!        http://publib.boulder.ibm.com/infocenter/lnxpcomp/v8v101/index.jsp?topic=/com.ibm.xlf101l.doc/xlflr/derivedtypes.htm ) 
!
! ----------------------------------------------------------
!

  !
  ! --- The main derived types are named GranFilm_XX_Type
  !


  ! --- The Use Statments
  Use Working_Precision,    only : wp     ! Defines the default precission





  ! -----------------------------------------------
  ! --- The Publicly avaiable Variables/Constants
  ! -----------------------------------------------


  ! -------------------------------------------
  ! --- The Publicly avaiable Derived Types
  ! -------------------------------------------
  Public :: GranFilm_Integral_Type
  Public :: GranFilm_Material_Type
  Public :: GranFilm_Parameter_Type
  Public :: GranFilm_Results_Type
            
  ! --- Internal to  GranFilm_Parameter_Type
  Public :: Geometry_Type
  Public :: Global_Type
  Public :: Interaction_Type
  Public :: Media_Type
  Public :: Numerics_Type
  Public :: Potential_Type
  Public :: EELS_Type
  Public :: Source_Type
  Public :: InOut_Type
  Public :: Physical_Quantities_to_Calculate_Type
  Public :: Curvefitting_Type

  ! --- Internal to GranFilm_Results_Type
  Public :: Polarizability_Type
  Public :: Susceptibility_Type
  Public :: Ref_Trans_Amp_Type
  Public :: Physical_Quantities_Type
  Public :: MultiPole_Coef_Type


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private







  ! --- Some (private) module parameters 
  Integer,                parameter  :: STR_TINY   = 25
  Integer,                parameter  :: STR_SHORT  = 100
  Integer,                parameter  :: STR_LONG   = 500  
  Integer,                parameter  :: STR_FILE   = 100  
  Integer,                parameter  :: STR_PATH   = 500  

  Character(len=*),       parameter  :: NOT_SET = "NOT_SET"

  !
  ! ------------------------
  ! --- BASE TYPES
  ! ------------------------
  !



  !------------------------------------------------------
  ! -------- TO BE MOVED ??? ----------------------------
  !------------------------------------------------------

 !---------------!  
 Type InOut_Type
 !---------------!   
     !
     ! --- Container for Input output of data.....
     !
     Character(len=STR_FILE)   :: Input_File_Name        ! The parameter filename 
     Character(len=STR_FILE)   :: Output_Base_Name       ! The Output base name....
     Character(len=STR_FILE)   :: Output_File_Name       ! The data output filename (Base_Name.nc) 
     Character(len=STR_FILE)   :: Experimental_File_Name
     Character(len=STR_PATH)   :: Output_Type            ! The output type
     
     ! Command line Flags
     Logical                   :: Verbose           = .false.  ! Verbose option
     Logical                   :: Print_Parameters  = .false.  
     Logical                   :: Do_Curve_Fitting  = .false.


     ! --- Other parameters .... (should all be set to .false. for production code ...)
     Logical                   :: Testing  = .false.     ! Testing option for tuning 
     Logical                   :: Debug    = .false.     ! Debug option 


     ! File extensions
     !Character(len=3)          :: NC_Extension = ".nc"   ! The NetCDF file extension....
     !Character(len=4)          :: Log_Extension = ".log" ! The log file extension 
     !Character(len=4)          :: TXT_Extension = ".dat" ! ASCII file extension
     ! Software Info

     !Logical                   :: Test_IO  = .true.      ! Testing IO option for tuning.... (can be removed later...)
     !Logical                   :: Timing   = .false.     ! Timing of central elements of the calculation.....
     !Logical                   :: Logging  = .false.     ! For logging 
     !Integer                   :: LogUnit  =   6          !
     !Logical                   :: TestInput= .false.     ! Test Input and exit (if set)

     ! GUI mode
     Logical                   :: Gui_Mode = .false.     ! In GUI mode?

     ! python mode
     Logical                   :: python_mode = .false.     ! In GUI mode?
      
  End Type InOut_Type
  !------------------!


  !------------------!
  Type :: Global_Type
  !------------------!   
     Character(len=200)         :: Title
     Character(len=1000)        :: SOPRA_ROOT         ! The root to the SOPRA dielectric database
  End Type Global_Type
  !-------------------!



  !--------------------!
  Type :: Geometry_Type
  !--------------------!
     !
     ! --- From Input file
     !Real(wp)                                       :: Radius                  ! Radius of the outer layer
     Real(wp),           dimension(:), allocatable  :: Radius                  ! Radius of the outer layer. Two values if spheroidal.
     Real(wp)                                       :: Truncation_Ratio        ! Truncation ratio of the outer layer.....
     Real(wp),           dimension(:), allocatable  :: Radius_Ratios           ! The radii of the coating normalized by the outer radius; $R_i/R_1 \equiv R_i/R$
     Character(len=100), dimension(:), allocatable  :: Media
     !
     Real(wp)                                       :: Broadening_Par
     Real(wp)                                       :: Broadening_Perp
     !
     ! --- Derived parameters below this line ---
     Real(wp),           dimension(:), allocatable  :: Radius_Vector   ! The radius of the varios coatings so that Radii(1)=Radius
     Real(wp),           dimension(:), allocatable  :: Distance        ! Distance from substrate to the center fo the spherical coating
     Real(wp),           dimension(:), allocatable  :: Truncation_Ratio_Vector  ! Truncations ratio vector
     !
     ! --- Prolate/Oblate flags. ONE of these must be true if the particle is spheroidal, both false if particle is spherical 
     Logical        :: isOblate      
     Logical        :: isProlate 
     ! --- Derived type containg some derived info paramterers about the Geometry
     Real(wp)       :: Contact_Angle   ! Contant angle of outer interface
     Real(wp)       :: Apparent_Radius ! The apparent radius (seen from above) of the island
     Real(wp)       :: Coverage        ! 

  End Type Geometry_Type
  !--------------------!



  !--------------!
  Type Media_Type
  !--------------!
     !
     ! --- From Input file
     Character(len=STR_SHORT)               :: Material          ! The material to use
     Character(len=STR_LONG)                :: Path              ! Options : Use Global%SOPRA_Root if not given
     Real(wp)                               :: Epsilon_Scale(2)  ! Scale(1) *Re(eps)+imu*Scale(2)*Im(eps)    
     Logical                                :: Do_Temperature_Correction
     Logical                                :: Do_Size_Correction
     ! --- For corrections to the dielectric function:
     logical                                :: temp_corr
     logical                                :: finitesize_corr
     real(wp)                               :: temperature           ! K
     real(wp)                               :: Ep                    ! eV
     real(wp)                               :: d_Ep_dT               ! eV/K
     real(wp)                               :: hbar_fermi_velocity   ! eV nm
     real(wp)                               :: hbar_inv_tau          ! eV
     real(wp)                               :: B_plasmonshift        ! eV^2 nm
     !
     ! --- Derived parameters below this line ---
     complex(wp), dimension(:), allocatable :: epsilon
     complex(wp), dimension(:), allocatable :: mu
     ! --- Internal use
     Character(len=STR_SHORT)               :: Tag             ! The name of the NML given in the input file
  End Type Media_Type
  !-----------------!



  !---------------!
  Type Source_Type
  !---------------!
     Real(wp)               :: Theta0
     Real(wp)               :: Phi0
     Character(len=2)       :: Polarization
     Real(wp), dimension(2) :: Energy_Range       
     !Real(wp), dimension(2) :: Wavelength_Range      
     Real(wp)               :: ThetaE                 ! ESKIL
     Real(wp)               :: PhiE                   ! ESKIL
     !
     ! --- Derived parameters below this line ---
     Integer                :: iambient       ! index for the ambient
     Integer                :: isubstrate     ! index for the substarte
  End Type Source_Type
  !-------------------!



  !-----------------!
  Type Numerics_Type
  !-----------------!
     Integer                             :: Multipole_Order     
     Real(wp)                            :: Multipole_Position_Ratio 
     Integer                             :: No_Energy_Points           ! No. of energy points
     Integer                             :: Lattice_Sum_Level          ! No. of levels used in the lattice sum
     Logical                             :: All_MP_Coef_Needed 
     ! --- Derived parameters below this line ---
     Integer                             :: MP_Expansion_Medium        ! Index of the medium where the MPs are located 
     Logical                             :: Multipole_Above_Substrate
     Integer                             :: M_MAX
     Integer,  dimension(:),     allocatable :: MP_Storage_Offset
     Real(wp), dimension(:),     allocatable :: Energy
     Real(wp), dimension(:,:,:), allocatable :: Zeta
  End Type Numerics_Type
  !---------------------!




  !-----------------!
  Type Potential_Type
  !-----------------!
     Logical                             :: Potential_Calculation
     Character(len=1000)                 :: Points_File
     Real(wp), dimension(:), allocatable :: Energy
  End Type Potential_Type
  !---------------------!



  !-----------------!
  Type EELS_Type
  !-----------------!
     Logical                             :: EELS_Calculation
     Real(wp)                            :: Impact_Energy        !
     Real(wp)                            :: Wavevector_Transfer  ! relative angle of incidense
  End Type EELS_Type
  !---------------------!


  !--------------------!
  Type Interaction_Type
  !--------------------!
     Character(len=STR_SHORT)   :: Arrangement
     Character(len=STR_SHORT)   :: Lattice_Type
     Real(wp)                   :: Lattice_Constant 
    !Real(wp)                   :: Coverage
     Character(len=STR_SHORT)   :: Island_Island_Interaction
     ! --- Derived parameters below this line ---
     Real(wp)                   :: Density     ! Density of islands on the surface     
     Real(wp)                   :: Coverage    ! Surface fraction covered by islands

  End Type Interaction_Type
  !---------------------!



  !---------------------!
  Type Curvefitting_Type
  !---------------------!
     Real(wp), dimension(3)   ::  Lower_Constraint, Upper_Constraint
     Real(wp)                 ::  sigma, broadening_zero
     !Real(wp)                 :: Lower_Constraint(3),Upper_Constraint(3),sigma
     Logical                   :: freeze_broadening
  End Type Curvefitting_Type
  !--------------------------!  



  ! ---------------------------------
  ! --- Results ---------------------
  ! ---------------------------------



  !-------------------------!
  Type Polarizability_Type
  !-------------------------!
     !
     ! --- Type used to store calculated polarizabilities used in 
     !     the GranFilm. 
     !     It is the NORMALIZED pol. that should be stored.
     !
     !     The dimensions are 
     !        1 : paralell to the surface       =  Results%par
     !        2 : perpendicular to the surface  =  Results%perp
     !
     Real(wp)                   :: weights     =  1._wp
     Complex(wp), dimension(2)  :: Dipole            
     Complex(wp), dimension(2)  :: Quadrupole  
     !Complex(wp), dimension(:,:), allocatable :: Dipolar      ! (2)
     !Complex(wp), dimension(:,:), allocatable :: Quadropolar  ! (2)
     !Real(wp),    dimension(:),   allocatable :: weights      !  =  1._wp
  End Type Polarizability_Type
  !---------------------------!



       
  !-------------------------!
  Type Susceptibility_Type
  !-------------------------!
     !
     ! --- Type used to store surface suseptibilities
     !
     Complex(wp)     :: gamma
     Complex(wp)     :: beta
     Complex(wp)     :: tau
     Complex(wp)     :: delta
  End Type Susceptibility_Type
  !--------------------------!


  !-------------------------!
  Type Ref_Trans_Amp_Type
  !-------------------------!
     !
     ! --- Type used to store reflection/transmission ampitudes
     !
     Complex(wp)     :: p            ! For the island film
     Complex(wp)     :: s            !     -- " --
     Complex(wp)     :: fresnel_p    ! For the flat substrate
     Complex(wp)     :: fresnel_s    !     -- " --
  End Type Ref_Trans_Amp_Type
  !--------------------------!


  !----------------------------!
  Type Physical_Quantities_Type
  !----------------------------!
     !
     ! --- Type used to store the measurable physical quantities
     !
     !     Dimensions: 
     !                  1 : Energy
     !
     !     QUESTION : Should we allocate the derived type OR the components....?
     !
     Real(wp),    dimension(:,:), allocatable :: Reflectivity
     Real(wp),    dimension(:,:), allocatable :: Transmissivity
     Real(wp),    dimension(:,:), allocatable :: Delta_R_over_R
     !Real(wp),    dimension(:,:), allocatable :: Delta_T_over_T
     !Real(wp),    dimension(:,:), allocatable :: Ellipsometric_Coefficients
     Real(wp),    dimension(:),   allocatable ::  EELS                   ! Scattering probability per unit energy (dP/dE)  
     !
  End Type Physical_Quantities_Type
  !--------------------------------!


  !-----------------------------------------!
  Type Physical_Quantities_to_Calculate_Type
  !-----------------------------------------!
     !
     ! --- Which physical quanteties to calculate?
     !  
     !     This type is part of the param type and used to define this
     !
     Logical  :: reflectivity   
     Logical  :: transmissivity 
     Logical  :: Delta_R_over_R 
     !
     ! etc.....
     !
  End Type Physical_Quantities_to_Calculate_Type
  !--------------------------------!



  
  ! ==========================================================================

  !-----------------------!
  Type MultiPole_Coef_Type
  !-----------------------!
     !
     !  -- To Store the Multipole Coefficients
     !
     ! Container for the MPs (1st index: energy;   2nd: ell;  3rd:  m)
     Complex(wp), dimension(:,:,:), allocatable :: Coefficients
  End Type MultiPole_Coef_Type
  !---------------------------!




  ! ==========================================================================




  ! ---------------------------------
  ! --- Main GranFilm Types ---------
  ! ---------------------------------




  !-------------------------!
  Type GranFilm_Results_Type
  !-------------------------!
     !
     ! --- Main type used to store the simulation 
     !     results obtained by GranFilm
     !
     Integer                                                 ::  par          =  1    ! Paralell Component 
     Integer                                                 ::  perp         =  2    ! Perpendicular Component
     !Integer                                                 ::  p_pol        =  1    ! P-polarization index
     !Integer                                                 ::  s_pol        =  2    ! S-polarization index
     Type(Polarizability_Type),  dimension(:,:), allocatable ::  Polarizabilities
     Type(Susceptibility_Type),  dimension(:),   allocatable ::  Susceptibilities
     Type(Ref_Trans_Amp_Type),   dimension(:),   allocatable ::  Reflection_Amplitudes 
     Type(Ref_Trans_Amp_Type),   dimension(:),   allocatable ::  Transmission_Amplitudes
     !
     Type(MultiPole_Coef_Type)                               ::  MultiPoles
     Type(Physical_Quantities_Type)                          ::  Observables 

  End Type GranFilm_Results_Type
  !-----------------------------!



  !--------------------------!
  Type GranFilm_Integral_Type
  !--------------------------!
     !
     ! --- Type used to store the various integrals that are needed 
     !     the GranFilm 
     !
     !     Dimensions:
     !                  1 : \ell
     !                  2 : \ell'
     !                  3 : m
     !                  4 : multipole index 
     !                  5 : upper integration limit index
     !
     
     ! -- Index parameters ( For internal use only )
     Integer    :: MultiPole            
     Integer    :: ImageMultiPole  
     Integer    :: UpperLimit_tr
     Integer    :: UpperLimit_One 
     ! --- Data
     Real(wp)                                    :: Radius_Ratio 
     Real(wp)                                    :: truncation_ratio
     Real(wp), dimension(:,:,:,:,:), allocatable :: Q
     Real(wp), dimension(:,:,:,:,:), allocatable :: K
     Real(wp), dimension(:,:,:,:,:), allocatable :: L
     Real(wp), dimension(:,:,:,:,:), allocatable :: M
     Real(wp), dimension(:,:,:,:,:), allocatable :: N

  End Type GranFilm_Integral_Type
  !------------------------------!




  !------------------------------!
  Type :: GranFilm_Parameter_Type
  !------------------------------!
     !
     ! --- Main type used to store the parameters used by GranFilm
     !     during simulation
     !
     Type(Global_Type)                            :: Global
     Type(Geometry_Type)                          :: Geometry
     Type(Interaction_Type)                       :: Interaction
     Type(Curvefitting_Type)                      :: Curvefitting
     Type(Source_Type)                            :: Source
     Type(Numerics_Type)                          :: Numerics
     Type(Potential_Type)                         :: Potential
     Type(EELS_Type)                              :: EELS
     
     !
     Type(Media_Type), dimension(:), allocatable  :: Media
     !
     Type(InOut_type)                             :: InOut
     !
     Type(Physical_Quantities_to_Calculate_Type)  :: Should_Calc  ! What Physical Qunateties to calculate

  End Type GranFilm_Parameter_Type
  !-------------------------------!




  

End Module Derived_Type_Module
           



!!$
!!$! FROM OLD CODE
!!$
!!$
!!$
!!$  ! Not given in input.dat, but set in Initialize.f90
!!$  Type:: Misc_type
!!$     ! I/O
!!$     Character(len=20)          :: Fresnel_DR_Formulae   ! Type of formulae for DR
!!$     Character(len=5)           :: Output_Type           ! Nature of output
!!$     Logical                    :: Output_Strenght       ! Output for the multipole coefficients
!!$     Logical                    :: Output_Polarizability ! Output for the polarizabilities
!!$     Logical                    :: Normalization         ! Normalization by the volume for the polarizability
!!$     Character(len=100)         :: Outfilename           ! Output filename (basename)
!!$     Character(len=100)         :: Infilename
!!$
!!$     ! Size distribution parameters
!!$     Real(wp)                   :: Distribution_Sigma    ! Standard deviation of size distribution
!!$     Real(wp)                   :: Distribution_xRmin    ! Minimal radius of island
!!$     Real(wp)                   :: Distribution_xRmax    ! Maximal radius of island
!!$     Integer                    :: Distribution_Classes  ! Number of radius classes
!!$
!!$     ! Numerics
!!$     Integer                    :: Levels                ! Number of levels used in the lattice sum
!!$
!!$     ! Some control Parameters for the metal
!!$     Character(len=10)          :: Mean_Free_Path        ! Nature of mean free path 'none' 'finite_Size' 'manual'
!!$     Logical                    :: Surface_Effects       ! Surface effects
!!$     Real(wp)                   :: Temperature           ! Temperature (K)
!!$     Logical                    :: Thermoreflectance     ! Thermoreflectance
!!$
!!$     ! Control Parameters for the boundary conditions
!!$     Character(len=10)          :: Bound                 ! Type of calculation (none-BC-Pot)
!!$     Real(wp)                   :: BC_Energy             ! Energy for the calculation
!!$     Character(len=15)          :: Scaling               ! Nature of the scaling Parameter : none,incident_field,mean_value
!!$
!!$     ! Some physical constants
!!$!     Real(wp)                   :: Eps_vacuum 
!!$!     Real(wp)                   :: hbar                ! Plancks constant (eV s)
!!$!     Real(wp)                   :: c                   ! Speed of light
!!$!     Real(wp)                   :: me                  ! Mass of the electron in eV.nm-2.s2 
!!$
!!$     ! Various Parameters
!!$!     Complex(wp),  Pointer, Dimension(:)  :: Eps_Island   
!!$!     Complex(wp),  Pointer, Dimension(:)  :: Eps_Island_File   
!!$!     Complex(wp),  Pointer, Dimension(:)  :: Eps_Substrate
!!$!     Complex(wp),  Pointer, Dimension(:)  :: Eps_Substrate_File 
!!$!     Complex(wp),  Pointer, Dimension(:)  :: Eps_Coating
!!$
!!$
!!$     ! Convolution parameters(?)
!!$     Real(wp)    :: sigEpar             ! Parallel and perpendicular values of the convolution width (eV)
!!$     Real(wp)    :: sigEperp
!!$
!!$     ! Surface Information Type
!!$     Type(Software_Info_Type) :: SW_Info
!!$
!!$  End Type Misc_type
!!$
!!$
!!$
!!$
