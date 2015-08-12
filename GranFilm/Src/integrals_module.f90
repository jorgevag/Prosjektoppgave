! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module calculates the varous integrals needed
!     by GRANFILM
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!----------------------!
Module Integrals_Module
!----------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Integrals	   
  

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Allocate_Integrals
  Public :: Get_Integrals


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  
Contains



  !------------------------------------!
  Subroutine Get_Integrals(  )
  !------------------------------------!
    !
    ! --- This generic routine calculates the integrals needed for
    !     GranFilm. The answer is returned in the variable Integrals. 
    !
    !     Ingve Simonsen, Copenhagen, May 2012.
    !
    Use Integrals_Sphere_Module,   only : Get_Integrals_Sphere
    Use Integrals_Spheroid_Module, only : Get_Integrals_Spheroid
    Use Error_Module,              only : Error_Failure
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Integrals"
    Integer :: i

    ! === SET DEBUGGING ===============================
    ! ---
    !Param%InOut%Debug = .true.
    ! ---
    ! =================================================


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    
    ! --- Some ERROR Checking
    ! ----------------------------------
    ! --- Check that the Integral container is allocated
    if (.not. allocated( Integrals ) ) then
       call Error_Failure( routine, "INTERNAL ERROR : Integral container not allocated")
    endif


    ! --- Calculate the integrals
    ! --------------------------------------------
    select case( size(Param%Geometry%Radius,1) )

    case( 1 ) 
       ! --- Spherical geometry
       call Get_Integrals_Sphere( )

    case( 2 ) 
       ! --- Spheroidal geometry
       call Get_Integrals_Spheroid( )

    case Default
       ! --- Default give an error message
       !     this should only happen due to an Iernal error
       call  Error_Failure( routine, "Unsupported size for Param%Geometry%Radius")

    End select



    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    
    
  End Subroutine Get_Integrals
  !---------------------------!
  





  !-----------------------------------------!
  Subroutine Allocate_Integrals(  )
  !-----------------------------------------!
    !
    ! --- This routine allocates the vector of integrals, as well as
    !     the needed components.

    !     The vector structure of the integrals is
    !
    !          Integrals( coating no. )
    !
    !     The components are :
    !
    !           Integrals( isurface ) % Q( l1, l2, m, iPosMP, iUpperLimit ) 
    !
    !     where iPosMP denotes the position of the multipole
    !     (iPosMP=1) or image multipole (iPosMP=2). Morover,
    !     iUpperLimit denotes the the upper limit of the integral
    !     defniend so that if iUpperLimit=1 the upper limit it t_r,
    !     and if it is 2 the limit is ONE.
    !
    !      Similar structure applies for the  K, L, M and N intergrals.
    !
    Implicit None
    ! --- Local
    Integer :: isurface, Nsurface

    ! ---------------------------------
    ! --- Allocate Storage
    ! ---------------------------------

    ! --- Clean the content if presently allocated
    ! --------------------------------------------
    if ( allocated(Integrals) ) then
       ! ... Clean the components
       do isurface=1, size(Integrals,1)
          call Integral_Component_Deallocateion( Integrals(isurface) )
       enddo
       ! ... Deallocate the vector
       deallocate( Integrals )
    end if
       
    ! --- Allocate the Integral vector
    ! ---------------------------------------
    Nsurface = size( param % Geometry % Radius_Ratios, 1 )
    if (.not. allocated( Integrals ) ) allocate( Integrals(Nsurface) )
    
    ! --- Allocate the components
    ! ---------------------------------------
    do isurface = 1, size(Integrals,1)
       call Integral_Component_Allocation_and_Initialization( Integrals(isurface) )
    end do

  End Subroutine Allocate_Integrals
  !---------------------------------------!







  !
  ! ================================================
  ! === Local Routines
  ! ================================================
  !





  !--------------------------------------------!
  Subroutine Set_Integral_Index_Defaults( Int )
  !--------------------------------------------!
    ! 
    ! --- Sets the index defaults used internally for storage...
    !
    !     ASSUMPTION :  Int % MultiPole < Int % ImageMultiPole 
    !
    Use Derived_Type_Module, only : GranFilm_Integral_Type
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int
    
    ! -- The MultiPole index
    Int % MultiPole      = 1 
    Int % ImageMultiPole = 2 
    ! --- the UpperLimit integration index
    Int % UpperLimit_tr  = 1
    Int % UpperLimit_One = 2  

  End Subroutine Set_Integral_Index_Defaults
  !--------------------------------------------!



  !----------------------------------------------------------------------!
  Subroutine Integral_Component_Allocation_and_Initialization( Int_Comp )
  !----------------------------------------------------------------------!
    !
    ! --- PURPOSE 
    !     Allocate the allocatable components of the 
    !     GranFilm_Integral_Type
    !     
    !     AUTHOR : Ingve Simonsen, Paris, Jun 2010
    !
    Use Derived_Type_Module,  only : GranFilm_Integral_Type
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int_Comp
    ! --- Local
    Integer, parameter :: NPosMP = 2, NUpperLimit=2
    Integer :: L1, L2, M
    

    ! --- Make sure that the components are deallocated before we start
    call Integral_Component_Deallocateion( Int_Comp )

    ! --- Some shorthands for the dimensions
    L1       =  Param % Numerics % Multipole_Order
    L2       =  Param % Numerics % Multipole_Order 

    ! --- Allocate the components
    allocate( Int_Comp%Q( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
    allocate( Int_Comp%K( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
    allocate( Int_Comp%L( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
    allocate( Int_Comp%M( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
    allocate( Int_Comp%N( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )

    ! --- Set the default indices...
    call Set_Integral_Index_Defaults( Int_Comp )
       
    
    Write(*,*) " ... WARNING : The size for the Q-integrals is too large....."


  End Subroutine Integral_Component_Allocation_and_Initialization
  !----------------------------------------------------------------!




  !-----------------------------------------------------!
  Subroutine Integral_Component_Deallocateion( Int_Comp )
  !-----------------------------------------------------!
    !
    ! --- PURPOSE 
    !     Deallocate the allocatable components of the 
    !     GranFilm_Integral_Type
    !     
    !     AUTHOR : Ingve Simonsen, Paris, Jun 2010
    !
    Use Derived_Type_Module,  only : GranFilm_Integral_Type
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int_Comp
    
    if (allocated( Int_Comp%Q ))  deallocate( Int_Comp%Q )
    if (allocated( Int_Comp%K ))  deallocate( Int_Comp%K )
    if (allocated( Int_Comp%L ))  deallocate( Int_Comp%L )
    if (allocated( Int_Comp%M ))  deallocate( Int_Comp%M )
    if (allocated( Int_Comp%N ))  deallocate( Int_Comp%N )

   End Subroutine Integral_Component_Deallocateion
  !-----------------------------------------------------!





End Module Integrals_Module
!--------------------------!




