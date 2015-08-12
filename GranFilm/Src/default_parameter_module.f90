! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     Provides a routine for setting the default GranFilm 
!     parameters
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!------------------------------!
Module Default_Parameter_Module
!------------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp, Param 


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Set_Default_Parameters
  Public :: Set_Default_Internal_Parameters


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private



  !-------!
Contains
  !-------!




  !----------------------------------!
  Subroutine Set_Default_Parameters()
  !----------------------------------!

    !
    ! --- Set the default values for the parameters 
    !     that are comtrolled by the parameter file
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    !     Modified :
    !                       May 2012, IS
    !
    !
    ! --------------------------------------------------------------
    !
    !   The default parameters:
    !
    !   Truncated silver spheres (t_r=0.1) of radius 8nm, supported by
    !   a MgO substrate. The distance between the particles is 20nm!
    !
    !
    Implicit None
    ! --- Local 
    Character(len=*), parameter :: routine = "Set_Default_Parameters"

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    !
    ! ============================================
    ! ===  SETTING DEFAULT PARAMETERS
    ! ============================================
    !


    ! --- Global
    ! ----------------------------
    !
    ! --- Changable via the parameter file
    param%Global%Title                       = 'Simulations done with GranFilm ver 2.0'
    param%Global%SOPRA_ROOT                  = './Dielectric/'     




    ! --- Geomtry
    ! ----------------------------
    !
    ! --- Changable via the parameter file
    allocate(param%Geometry%Radius(1))
    allocate(param%Geometry%Radius_Ratios(1))   
    allocate(param%Geometry%Media(4))   
    param%Geometry%Radius            =    8._wp      ! [nm]
    param%Geometry%Truncation_Ratio  =    0.1_wp          
    param%Geometry%Radius_Ratios     =    1._wp        
    param%Geometry%Media(1)          =   "Air"       ! These variables are not needed but given anyway
    param%Geometry%Media(2)          =   "MgO"
    param%Geometry%Media(3)          =   "Ag"
    param%Geometry%Media(4)          =   "MgO"
    !
    ! ESKILs definition
    ! the parameter is set if the -f flag is given param%inout%do_curve_fitting 
    if (param%inout%do_curve_fitting) then
       param%Geometry%Broadening_Perp = 0.1_wp
       param%Geometry%Broadening_Par  = 0.1_wp
    else 
       param%Geometry%Broadening_Perp = 0.0_wp
       param%Geometry%Broadening_Par  = 0.0_wp
    end if



    ! --- Source
    ! ----------------------------
    !
    ! --- Changable via the parameter file
    param%Source%Theta0                      = 45._wp              ! Angles of incidence (deg)
    param%Source%Phi0                        = 0._wp
    param%Source%Polarization                = 'p'                 ! Polarization of the incident beam
    param%Source%Energy_Range                = (/1.5_wp,5._wp/)    ! Photon energy range (eV)

 
    ! --- Intercation
    ! ----------------------------
    !
    ! --- Changable via the parameter file
    param%Interaction%Arrangement                 =  'Lattice'      
    param%Interaction%Lattice_Type                =  'Square'        ! (square, hexagonal, MFT, RPT)
    param%Interaction%Lattice_Constant            =   20._wp	     ! (nm)
    param%Interaction%Island_Island_Interaction   =  'Dipole'          ! (None, Dipole, Quadropole, Size)
    



    ! --- Numerics
    ! ----------------------------
    !
    ! --- Changable via the parameter file
    param%Numerics%Multipole_Order             =  16     ! All these could have default values....
    param%Numerics%Multipole_Position_Ratio    =  0.0_wp
    param%Numerics%No_Energy_Points            =  512
    ! --- Internal parameters
    Param%Numerics%All_MP_Coef_Needed         =   .false.   ! We will later not need all MP coeffisients...
    Param%Numerics%Lattice_Sum_Level          =    500
 

    ! --- Media
    ! ----------------------------
    !
    ! --- Defaults are set in the routine Read_Media_nml for technical
    !     reasons (since param%Media will be deallocated later, and
    !     settings made here lost)...
    !       



    ! .......................................


    !
    ! ----------------------------------------
    ! --- The optional NameLists
    ! ----------------------------------------
    !



    ! --- Curvefitting
    ! ----------------------------
    !


    ! --- Potential
    ! ----------------------------
    !



    !
    ! .......................................




    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine


  End Subroutine Set_Default_Parameters
  !------------------------------------!








  !----------------------------------------------!
  Subroutine Set_Default_Internal_Parameters()
  !----------------------------------------------!

    !
    ! --- Set the default INTERNAL parameters
    !
    !     Ingve Simonsen, Paris, May 2012
    !
    Implicit None
    ! --- Local 
    Character(len=*), parameter :: routine = "Set_Default_Internal_Parameters"

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    !
    ! ============================================
    ! ===  SETTING DEFAULT INTERNAL PARAMETERS
    ! ============================================
    !


 
    ! --- InOut
    ! ----------------------------
    Param%InOut%Verbose             =   .false.
    Param%InOut%Print_Parameters    =   .false.
    Param%InOut%Do_Curve_Fitting    =   .false.
    Param%InOut%Gui_Mode            =   .false.     ! In GUI mode?
    !
    Param%InOut%debug               =   .false.
    Param%InOut%Testing             =   .false.




    ! --- Global
    ! ----------------------------
    !

    ! --- Geomtry
    ! ----------------------------
    !

    ! --- Source
    ! ----------------------------
    !
 
    ! --- Intercation
    ! ----------------------------
    !

    ! --- Numerics
    ! ----------------------------
    Param%Numerics%All_MP_Coef_Needed         =   .false.   ! We will later not need all MP coeffisients...
    Param%Numerics%Lattice_Sum_Level          =    500
    !Param%Numerics%Multipole_Above_Substrate  =   .true.   ! Most of the time we will use the MP above 

    ! --- Media
    ! ----------------------------
    !

    ! .......................................


    !
    ! ----------------------------------------
    ! --- The optional NameLists
    ! ----------------------------------------
    !

    ! --- Curvefitting
    ! ----------------------------
    !

    ! --- Potential
    ! ----------------------------
    !

    !
    ! .......................................




    ! ---------------------------------
    ! --- What should we Calculate
    ! ------------------------------------
    Param%Should_Calc%reflectivity     = .true.
    Param%Should_Calc%transmissivity   = .true.
    Param%Should_Calc%Delta_R_over_R   = .true.
    


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine


  End Subroutine Set_Default_Internal_Parameters
  !-----------------------------------------------!



End Module Default_Parameter_Module
!----------------------------------!












