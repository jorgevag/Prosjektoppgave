! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!       To make consistancy checks of the input parameters.
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!-----------------------------!
Module Check_Parameters_Module
!-----------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param
  Use Supported_Options_Module

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Check_Input_Parameters
  Public :: Check_Derived_Parameters

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  
  ! --- Local paraneters
  Integer, parameter :: ZERO = 0._wp
  Integer, parameter :: ONE  = 1._wp



!-------!
Contains
!-------!




  !--------------------------------!
  Subroutine Checking( OK, message)
  !--------------------------------!
    !
    ! --- PURPOSE
    !     Rapper for reporting errors from the check parameter routines
    !
    !     Ingve Simonsen, Paris, Jun, 2010
    !
    Use Error_module, only : Error_Failure
    Implicit None
    Logical,          intent(in)  :: OK
    Character(len=*), intent(in)  :: message
    ! --- Local
    Character(len=*), parameter :: routine = "Check_Parameters"

    if ( .not. OK) then
       call Error_Failure( routine, message )
    endif

  End Subroutine Checking
  !---------------------!
  





  !----------------------------------!
  Subroutine Check_Input_Parameters()
  !----------------------------------!
    !
    ! --- PURPOSE
    !     Make sure that the input parameters are consistent
    !
    !     Ingve Simonsen, Paris, Jul 2010.
    !
    Use  SFL_NR_Util, only : assert
    Implicit None
    Integer  :: i
    !Real(wp) :: tr_inner_core
    ! --- Local
    Character(len=*), parameter :: routine = "Check_Input_Parameters"


    
    ! ------------------------------------------------------


    !Write(*,*) " +++++ Complete the module : Check_Parameters !"
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine



    ! ------------------
    ! --- Global
    !-------------------
    ! ... Noting yet to do ...
    

    ! ------------------
    ! --- Geometry
    !-------------------
    
    call Checking(size(Param%Geometry%Radius) <= 2, &
                  "Invalid number of radii (or possibly a trailing comma?)")
    if (size(Param%Geometry%Radius)==1) then 
      if (Param%InOut%Verbose) then 
        Write(*,*) "*** Single radius read ==> Spherical calculations."
        Write(*,*) "*** Radius = ",Param%Geometry%Radius
      end if
    else 
      if (Param%InOut%Verbose) then 
        if (Param%Geometry%Radius(1) > Param%Geometry%Radius(2)) then
           Write(*,*) "*** Two radii read ==> Prolate spheroidal calculations"
        else
           Write(*,*) "*** Two radii read ==> Oblate spheroidal calculations"
        endif
        Write(*,*) "*** Radius_perp = ",Param%Geometry%Radius(1)
        Write(*,*) "*** Radius_parallel = ",Param%Geometry%Radius(2)
      end if
      
      ! --- Commented out by Sindre ---
      !
      !call Checking(Param%Geometry%Radius(1) < Param%Geometry%Radius(2), &
      !                "Prolate spheroids currently not supported!")
      ! -------------------------------
    end if
 

    ! ... The size for the Interafces and Media must be consistent.....
    call Checking( 2*size(Param%Geometry%Radius_Ratios,1)+2 == size(Param%Geometry%Media,1), &
          "Incompatible sizes given for Radius_Ratios and Media ! ")

    ! ... Radius_Ratios
    call Checking( all(Param%Geometry%Radius_Ratios > ZERO), "Radius_Ratios must be positive!" )
    call Checking( abs(Param%Geometry%Radius_Ratios(1)-ONE)< 1000*epsilon(1._wp), "Radius_Ratios(1) must be one!" )
    do i=1,size(Param%Geometry%Radius_Ratios,1)-1
       call Checking( Param%Geometry%Radius_Ratios(i)>=Param%Geometry%Radius_Ratios(i+1), &
            "Radius_Ratios NOT given in decreasing order!")
    enddo

    ! --- QUESTION : Is this needed???????
    ! -----------------------------------------
    ! ... Inner core above the substarte
    !tr_inner_core = Param%Geometry%Truncation_Ratio / Param%Geometry%Radius_Ratios(size(Param%Geometry%Radius_Ratios,1))
    !call Checking( tr_inner_core > -1._wp, "Inner-core NOT above the substrate!" )



    ! ------------------
    ! --- Source
    !-------------------
    
    ! --- Angles
    call Checking( Param%Source%Theta0>=0._wp .and. Param%Source%Theta0<=180._wp ,   &
                   "Polar angle (Theta0) not in range [0,180]!")
    call Checking( Param%Source%Phi0>=0._wp .and. Param%Source%Theta0<360._wp ,      &
                   "Azimuthal angle (Phi0) not in range [0,360>!")

    ! --- Energy range.....
    call Checking( Param%Source%Energy_Range(2)>Param%Source%Energy_Range(1), "Inconsistent energy range!") 

    ! --- Polarization
    call Check_for_Supported_Option( "Polarization", Param%Source%Polarization, POLARIZATION_OPTION_TABEL ) 
     !if ( .not. Supported_Option( Param%Source%Polarization, POLARIZATION_OPTION_TABEL ) ) &
    !     call List_Supported_Options( "Polarization", POLARIZATION_OPTION_TABEL , quit=.true. )
    !call checking( Param%Source%Polarization=='p' .or. Param%Source%Polarization=='s',    &
    !   "Illegal value for the source polarization ('p', or 's')!")



    ! ------------------
    ! --- Numerics
    !-------------------
    call Checking( Param%Numerics%Multipole_Order>0,"Multipole_Order must be positive!")
    call Checking( Param%Numerics%No_Energy_Points>0,"No_Energy_Points  must be positive!")


    ! ------------------
    ! --- Interaction
    !-------------------
    !
    ! --- Island_Island_Interaction
    call Check_for_Supported_Option("Island_Island_Interaction", &
        Param%Interaction%Island_Island_Interaction, ISLAND_ISLAND_INTERACTION_OPTION_TABEL ) 

    ! --- Arrangment
    call Check_for_Supported_Option( "Arrangement", Param%Interaction%Arrangement, ARRANGEMENT_OPTION_TABEL)

    ! --- Lattice Type
    call Check_for_Supported_Option( "Lattice_Type", Param%Interaction%Lattice_Type, LATTICE_TYPE_OPTION_TABEL )

    ! ---Lattice constant
    call Checking( Param%Interaction%Lattice_Constant>ZERO, "Lattice constant must be positive!")




    ! ------------------
    ! --- Media
    !-------------------
    !do i=1,size(Param%Geometry

    !ENDDO

       
    ! ------------------
    ! --- InOut
    !-------------------
    ! ... Noting yet to do ...


    ! ------------------
    ! --- Potenial
    !-------------------
    ! ... Noting yet to do ...
    !
    !  HOWEVER, TESTES FROM FILE read_input_file_module.f90
    !  SHOULD PROBABLY GO HERE......
    !




    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving   : ", routine


  End Subroutine Check_Input_Parameters
  !-----------------------------------!








  
  !------------------------------------!
  Subroutine Check_Derived_Parameters()
  !------------------------------------!
    Use Error_Module, only : Error_Failure, Error_Warning
    Implicit None
    Real(wp),  parameter :: ONE = 1._wp

    ! --- Local
    Character(len=*), parameter :: routine = "Check_Derived_Parameters"


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! Check for cluster overlapping
    If( 2*param%Geometry%Apparent_Radius > param%Interaction%Lattice_Constant ) Then
       call Error_Failure( routine, " Inconsistent input: Islands are overlapping...!")
    Endif



    ! ------------------
    ! --- Numerics
    !-------------------
    ! 
    ! --- The MP should be inside the particle.....
    call Checking(  all( (/ param%Numerics%Multipole_Position_Ratio <= ONE,              &
                            param%Numerics%Multipole_Position_Ratio >= -ONE /) ),        &
                         "Multipoles (Multipole_Position_Ratio) not inside particle!" ) 


    ! --- MP should be above the substrate (and inside the inner-core), i.e.  \mu_i < t_r
    !call Checking( param%Numerics%Multipole_Position_Ratio( size(param%Numerics%Multipole_Position_Ratio,1) ) &
    !     < Param%Geometry%Truncation_Ratio, "Multipoles expantion point below the substrate !" )

    !write(*,*) Param%Geometry%Truncation_Ratio_Vector
    !stop


!!$
!!$    ! QUESTION : Could one use an absolute value here so that only one test is needed.....
!!$    If ( Param%Numerics%Multipole_Above_Substrate ) then
!!$       ! --- MP Above substrate
!!$       call Checking(  all( (/ param%Numerics%Multipole_Position <= param%Geometry%Truncation_Ratio,      &
!!$                               param%Numerics%Multipole_Position >=  -ONE /) ),                           &
!!$                      "Multipole_Position not inside particle" )         
!!$    else
!!$       ! --- MP Below substrate
!!$       call Checking(  all( (/ param%Numerics%Multipole_Position <= ONE,                                  &
!!$                               param%Numerics%Multipole_Position >= param%Geometry%Truncation_Ratio /) ), &
!!$                     "Multipole_Position not inside particle" )         
!!$    End If



    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine


  End Subroutine Check_Derived_Parameters
  !---------------------------------------!    





End Module Check_Parameters_Module
!---------------------------------!
