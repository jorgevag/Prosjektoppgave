! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!

!------------------------------------!
Module Post_Process_Parameters_Module
!------------------------------------!

  ! --- The Use Statements global to the module
  Use Shared, only : wp, pi, imu, param	   

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Post_Process_Parameters


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  


!-------!
Contains
!-------!




  !-----------------------------------!
  Subroutine Post_Process_Parameters()
  !-----------------------------------!
    !
    ! --- PURPOSE
    !     To post-process the various section of the 
    !     simulation parameters
    !
    !     Ingve Simonsen, Paris, Jul 2010.
    !
    Use SFL_NR_Util,                      only : arth
    Use Error_Module,                     only : Error_Fatal, Error_Failure, Error_Warning
    Use SOPRA_Dielectric_Function_Module, only : SOPRA_dielectric_function
    Use Epsilon_Corrections_Module,       only : Epsilon_Corrections, Epsilon_Scaling
    Use Tools_Module,                     only : rad2deg
    Use Supported_Options_Module,         only : LATTICE_TYPE_OPTION_TABEL, &
                                                 ISLAND_ISLAND_INTERACTION_OPTION_TABEL
    Implicit None                           
    ! --- Local
    Character(len=*), parameter :: routine = "Post_Process_Parameters"
    Character(len=100)          :: str
    Integer    :: i, MPO, M_MAX, isurface, imedium
    Integer, allocatable, dimension(:) :: Unknown_MP_Types
    Real(wp)   :: denergy
    

    ! ------------------------------------------------------


    ! ------------------
    ! --- Global
    !-------------------
    ! ... Noting yet to do ...
    



    ! ------------------
    ! --- Geometry
    !-------------------
    !
    ! ... Radii
    if( allocated( Param%Geometry%Radius_Vector )) deallocate( Param%Geometry%Radius_Vector )
    allocate( Param%Geometry%Radius_Vector( size(Param%Geometry%Radius_Ratios,1) ) )
    Param%Geometry%Radius_Vector   =   Param%Geometry%Radius(1) * Param%Geometry%Radius_Ratios
    !
    ! ... Distance    (ASSUMPTION : All coatings are concentric.... so that all distances are the same....)
    if( allocated( Param%Geometry%Distance )) deallocate( Param%Geometry%Distance )
    allocate( Param%Geometry%Distance( size(Param%Geometry%Radius_Ratios,1) ) )
    Param%Geometry%Distance   =   Param%Geometry%Radius(1) * Param%Geometry%Truncation_Ratio
    !
    ! ... Truncation ratio vector 
    if( allocated( Param%Geometry%Truncation_Ratio_Vector )) deallocate( Param%Geometry%Truncation_Ratio_Vector )
    allocate( Param%Geometry%Truncation_Ratio_Vector( size(Param%Geometry%Radius_Ratios,1) ) )
    Param%Geometry%Truncation_Ratio_Vector =  Param%Geometry%Distance / Param%Geometry%Radius_Vector 
    !
    ! ...Setting the Oblate/Prolate spheroid flag ...
    if( size(Param%Geometry%Radius, 1) == 2 ) then
       if( Param%Geometry%Radius(1) < Param%Geometry%Radius(2) ) then
          Param%Geometry%isOblate = .true.
          Param%Geometry%isProlate = .false.
       Else if ( Param%Geometry%Radius(1) > Param%Geometry%Radius(2) ) then
          Param%Geometry%isOblate = .false.
          Param%Geometry%isProlate = .true.
       Else
          !This should give a warning: Neither prolate nor oblate
          call Error_Failure( routine,"Spheroids must have different radii in the horizontal and vertical direction. &
               Use a single radius for spherical particles." )
       Endif
    Else
       Param%Geometry%isOblate = .false.
       Param%Geometry%isProlate = .false.
    Endif
    
    !write(*,*) "Flag isOblate =", Param%Geometry%isOblate
    !write(*,*) "Flag isProlate =", Param%Geometry%isProlate
    !
    ! ...Contact angle calculation
    Param%Geometry%Contact_Angle = acos(-param%Geometry%Truncation_Ratio) * rad2deg()    
    !
    ! ... Calculate the apparent radius of the particle
    If(param%Geometry%Truncation_Ratio>=0._wp) then
       param%Geometry%Apparent_Radius = param%Geometry%Radius(1)
    Else
       param%Geometry%Apparent_Radius = param%Geometry%Radius(1) * Sqrt(1._wp - param%Geometry%Truncation_Ratio**2)
    Endif



    ! ------------------
    ! --- InOut
    !-------------------
    !
    i =  Scan(Param%InOut%Output_File_Name, ".", back=.true.)
    Param%InOut%Output_Base_Name = trim(adjustl( Param%InOut%Output_File_Name(1:max(1,i-1)) ))






    !
    ! ------------------
    ! --- Source
    !-------------------
    !
    if ( (param%Source%Theta0<90._wp).and.(param%Source%Theta0>=-epsilon(1._wp)) ) then
       param%Source%iambient     = 1    ! Medium no. 1 is the ambient
       param%Source%isubstrate   = 2    ! Medium no. 2 is the ambient
    else
       call Error_Failure(routine, "This large angle of incidence is currently NOT supported!")
    endif
    !
    ! ---- Code added by Eskil regarding the 
    !      E-field angles of incidence.
    !
    !     Checks this when polarization is ps or sp
    !
    Write(*,*) " TODO : Check this"
    if (param%Source%Polarization == 'p') then
       ! --- p-polarization
      if (param%Source%theta0 < 90.0_wp) then  
        param%Source%thetaE = 90.0_wp - param%Source%theta0
      else
        param%Source%thetaE = param%Source%theta0 - 90.0_wp
      end if
      if (param%Source%Phi0 < 180.0_wp) then
        param%Source%PhiE = param%Source%Phi0 + 180.0_wp
      else
        param%Source%PhiE = param%Source%Phi0 - 180.0_wp
      end if
    else if (param%Source%Polarization == 's') then
       ! --- s-polarization
      param%Source%ThetaE = 90.0_wp
      if (param%Source%Phi0 < 270.0_wp) then
        param%Source%PhiE = param%Source%Phi0 + 90.0_wp
      else
        param%Source%PhiE = param%Source%Phi0 - 270.0_wp
      end if 
    else
      call Error_Failure(routine,"Unsupported polarization.")
    end if
    !
    If (Param%InOut%Verbose) then
      write(*,'(A19,F5.1)') "  ***E-field theta: ", param%Source%ThetaE
      write(*,'(A19,F5.1)') "  ***E-field phi:   ", param%Source%PhiE
    end if



    !
    ! ------------------
    ! --- Numerics
    !-------------------
    !
    ! --- Energy Vector
    if ( allocated(Param%Numerics%Energy) ) deallocate( Param%Numerics%Energy )
    allocate( Param%Numerics%Energy( Param%Numerics%No_Energy_Points) )
    denergy = (Param%Source%Energy_Range(2)-Param%Source%Energy_Range(1)) / &
                     (Param%Numerics%No_Energy_Points-1)
    Param%Numerics%Energy = arth(Param%Source%Energy_Range(1), denergy, Param%Numerics%No_Energy_Points )  
    !
    ! --- m_max
    !     m limits for integral and system solutions
    Select Case(param%Interaction%Island_Island_Interaction)
    Case( ISLAND_ISLAND_INTERACTION_OPTION_TABEL(1), ISLAND_ISLAND_INTERACTION_OPTION_TABEL(2) )
       param%Numerics%m_max = 1
    Case( ISLAND_ISLAND_INTERACTION_OPTION_TABEL(3) ) !'quadrupole')
       param%Numerics%m_max = 2
    Case Default 
    End Select
    !
    ! --- Zeta array
    !     M_Max must be defined first....
    if ( allocated(Param%Numerics%Zeta) ) deallocate( Param%Numerics%Zeta )
    MPO   = Param%Numerics%Multipole_Order
    M_MAX = param%Numerics%m_max
    allocate( Param%Numerics%Zeta(0:MPO, 0:MPO, 0:M_MAX ) )
    call Get_Zeta( Param%Numerics%Zeta )
    !
    ! ---  Set Multipole_Above_Substrate value
    !      This condition depends on the DIRECTION of the z-axis.
    If ( param%Geometry%Truncation_Ratio >= param%Numerics%Multipole_Position_Ratio ) Then
       param%Numerics%Multipole_Above_Substrate   = .True.
    Else
       param%Numerics%Multipole_Above_Substrate   = .False.
    Endif
    !
    ! --- Medium where the MPs are located Expansion_Medium
    !
    isurface=Size(Param%Geometry%Radius_Ratios,1)
    Do While ( abs(Param%Numerics%Multipole_Position_Ratio)  > Param%Geometry%Radius_Ratios( isurface ) )   
       if ( isurface == 1) then
          ! --- Medium NOT found
          call Error_Failure(routine, "Parm%Numerics%MP_Expansion_Medium  NOT found ")
       else
          isurface = isurface - 1
      endif
   Enddo
   if ( param%Numerics%Multipole_Above_Substrate ) then
      ! --- Above : MP expansion point above the substrate
      !             Medium 1,3,5,....
      Param%Numerics%MP_Expansion_Medium = 2*isurface+1      
   else
      ! --- Below : MP expansion point below the substrate
      !             Medium 2,4,6,....
      Param%Numerics%MP_Expansion_Medium = 2*isurface+2      
   endif
   !
   ! --- Set MP_Storage_Order : Gives where the MPs for a given medium starts 
   !
   if ( allocated(Param%Numerics%MP_Storage_Offset) ) deallocate( Param%Numerics%MP_Storage_Offset )
   if (.not.allocated(Param%Media)) &
        call Error_Fatal( routine, "Internal error: Param%Media NOT allocated!")
   allocate( Param%Numerics%MP_Storage_Offset( size(Param%Media,1) ) ) 
   ! ... Set no. of unknown MP types for each medium
   allocate( Unknown_MP_Types( size(Param%Media,1) ) )
   Unknown_MP_Types = 0
   do imedium = 1, size(Param%Media,1),2
      if (imedium==1) then
         Unknown_MP_Types(imedium) = 1 
      elseif ( imedium==Param%Numerics%MP_Expansion_Medium) then
         Unknown_MP_Types(imedium) = 1 
      else
         Unknown_MP_Types(imedium) = 2
      endif
   enddo
   !
   Param%Numerics%MP_Storage_Offset(1:2) = 0
   do imedium = 3, size(Param%Media,1),2
      Param%Numerics%MP_Storage_Offset(imedium)    = sum(Unknown_MP_Types(1:imedium-1)) 
      Param%Numerics%MP_Storage_Offset(imedium+1)  = Param%Numerics%MP_Storage_Offset(imedium) 
   enddo
   Param%Numerics%MP_Storage_Offset = Param%Numerics%MP_Storage_Offset * Param%Numerics%Multipole_Order
   !
   if(allocated(Unknown_MP_Types)) deallocate(Unknown_MP_Types)




    !
    ! ------------------
    ! --- Interaction
    !-------------------
    !
    ! ... Density
    ! ----------------------
    Select Case( Param%Interaction%Lattice_Type )
       
    Case( LATTICE_TYPE_OPTION_TABEL(1) )
       ! --- Square
       ! ------------------
       param%Interaction%Density  =  1._wp/(param%Interaction%Lattice_Constant**2)
    Case( LATTICE_TYPE_OPTION_TABEL(2) )
       ! --- Hexagonal
       ! ------------------
       param%Interaction%Density  =  1._wp/(Sqrt(3._wp)/2._wp*param%Interaction%Lattice_Constant**2)

    !Case('MFT','RPT')
    !   param%Interaction%Derived%Density  =  param%Interaction%Coverage/(pi*param%Island%Derived%Radius_Apparent**2)
    !   param%Interaction%Lattice_Constant =  1._wp/(Sqrt(param%Interaction%Derived%Density))
    Case Default
       ! -- Should never happen, but .....
       str = "Option Island_Island_Interaction="// trim(adjustl(Param%Interaction%Lattice_Type))// "not supported!"
       Call Error_Failure( routine, str )

    End Select
    !
    !
    ! ... Coverage
    param%Interaction%Coverage  = param%Interaction%Density * pi * param%Geometry%Apparent_Radius**2
    




    !
    ! ------------------
    ! --- Media
    !-------------------
    !
    ! --- Allocate the epsilon/mu vectors for all media
    if (.not.allocated(Param%Media)) &
         call Error_Fatal( routine, "Internal error: Param%Media NOT allocated!")
    ! --- Loop over ALL media
    do i=1,size(Param%Media,1)
       !
       ! --- Epsilon
       if ( allocated( Param%Media(i)%Epsilon) ) deallocate( Param%Media(i)%Epsilon ) 
       allocate( Param%Media(i)%Epsilon ( Param%Numerics%No_Energy_Points ) )    
       !
       ! --- Mu
       !.... Not used yet ....
    End do
    !
    ! ---Get the dielectric functions
    do i=1,size(Param%Media,1)
       call SOPRA_dielectric_function( Param%Numerics%Energy,    Param%Media(i)%Epsilon,       &
                                       param%Media(i)%Material,  Param%Media(i)%Path,    "eV")
    End do
    ! --- Get Mu
    !.... Not used yet ....


    ! --- Correct dielectric functions for temperature and finite-size effects
    call Epsilon_Corrections()

    ! === DEBUGGING =============================================
    ! Setting epsilon scaling manually
    !
    !Do i=1,size(Param%Media, 1)
    !   Param%Media(i)%Epsilon_Scale = (/ 1._wp, 0.01_wp /)
    !   write(*,*) Param%Media(i)%Epsilon_Scale(:)
    !End Do
    ! 
    !call Error_Warning(routine, "REMOVE THIS LATER: Hard-coded Epsilon scaling")
    !
    ! ===========================================================

    ! --- Scale Epsilon with a constant amplitude (if needed)
    call Epsilon_Scaling()


    ! --- Write data to file?
    if (Param%InOut%Debug) call Write_Epsilon_to_File()

    
  End Subroutine Post_Process_Parameters
  !------------------------------------!



  ! ---------------------------------------
  ! ---- Local Routnes --------------------
  ! ---------------------------------------

  
  !------------------------!
  Subroutine Get_Zeta(zeta)
  !------------------------!
    !
    ! PURPOSE : Calculating the function containg the 
    !           combination of factorials
    !
    !
    Use Error_Module, only : Error_Failure
    Implicit None
    Real(wp), intent(InOut)   :: zeta(0:,0:,0:)   ! Important.....
    !  --- Local
    Character(len=*), parameter :: routine = "Get_Zeta"
    Integer              :: l1,l2,m
    Real(wp)             :: factorial

    zeta(:,:,:) = 0._wp

    Do m = lbound(zeta,3),ubound(zeta,3)   !0,param%Numerics%m   ! 
       SelectCase(m)
       Case(0)
          ! ---  m = 0
          ! -------------
          Do l1=0,param%Numerics%Multipole_Order
             Do l2 =  0, param%Numerics%Multipole_Order
                zeta(l1,l2,m) = 0.5_wp *  Sqrt( (2*l1+1._wp)*(2*l2+1._wp) )
             Enddo
          Enddo
       Case(1)
          ! ---  m = 1
          ! -------------
          Do l1=1,param%Numerics%Multipole_Order
             Do l2 = 1, param%Numerics%Multipole_Order
                factorial = 1._wp/( l1*(l1+1._wp)*l2*(l2+1._wp) )
                zeta(l1,l2,m) = 0.5_wp *  Sqrt( (2*l1+1._wp)*(2*l2+1._wp)*factorial ) 
             Enddo
          Enddo
       Case(2)
          ! ---  m = 2
          ! -------------
          Do l1=2,param%Numerics%Multipole_Order
             Do l2 = 2, param%Numerics%Multipole_Order
                factorial = 1._wp/( (l1-1._wp)*l1*(l1+1._wp)*(l1+2._wp)* &
                     (l2-1._wp)*l2*(l2+1._wp)*(l2+2._wp) )
                zeta(l1,l2,m) = 0.5_wp *  Sqrt( (2*l1+1._wp)*(2*l2+1._wp)*factorial )
             Enddo
          Enddo
       Case default
          ! --- This should never hapen in our case
          call Error_Failure( routine, " Only 0<=m<=2 is supported!") 
       End Select
    Enddo

  End Subroutine Get_Zeta
  !----------------------!




  !---------------------------------!
  Subroutine Write_Epsilon_to_File()
  !---------------------------------!
    !
    ! --------------------------------------------------------------------
    ! --- Writing the Dielectric functions to file....
    ! ---------------------------------------------------------------------
    !
    Use SFL_Logical_Units,           only : SFL_Get_Free_Unit
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Write_Epsilon_to_File"
    Integer      :: ienergy, imedia, file_id

    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    !-- Open the file
    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_Epsilon.dat")
    Write(file_id,"(a)") "# Format: energy, real(eps), imag(eps) for all media starting with medium no. 1"
    
    ! --- Energy loop
    do ienergy=1,size(Param%Numerics%Energy,1)
       ! --- Write the result to file....
       Write(file_id,'(f10.5,3x,40(2f10.4,4x))')           &
            Param%Numerics%Energy(ienergy),                &
            ! --- Epsilon
            ( Param%Media(imedia)%Epsilon(ienergy), imedia=1,size(Param%Media,1) )
 
    enddo
    ! --- Close the file
    Close( file_id )
        
    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine


  End Subroutine Write_Epsilon_to_File
  !---------------------------------!



End Module Post_Process_Parameters_Module
!----------------------------------------!



