! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!   
!     This module calculates the Mutipole coefficients 
!     up to the needed order.
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!     Edited for Spheroid support: Eskil Aursand, Trondheim, Mar 2012.
!
! ----------------------------------------------------------
!


!----------------------------------!
Module MultiPole_Coefficient_Module
!----------------------------------! 


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param	   




  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Multipole_Coefficients


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


!-------!
Contains	
!-------!



  !---------------------------------------!
  Subroutine Get_Multipole_Coefficients( )
  !---------------------------------------!  
    !
    !  --- Calculate the Multipole Coefficients for the system at hand.
    !      Note that we only store (in Results%MultiPoles%Coefficients) 
    !      the coefficients that will be needed later.
    !
    !      Potentially ALL MP can be stored, but this requires more storge!
    !
    !      Moreover, the following storage convetion is used 
    !          
    !         Results%MultiPoles%Coefficients(:,1:2,0:1) 
    !
    !      are ALWAYS the coefficients in medium 1 (the incident medium).
    ! 
    !      The calculated MP coefficients are nomalized (see routine 
    !      Get_Matrix_System)  by apropriate powers of the the outer 
    !      radius R.
    !      
    !      Ingve Simonsen, Paris, Jul 2010
    !
    Use Shared,                         only : pi, Results
    Use Matrix_System_Sphere_Module,    only : Get_Matrix_System_Sphere
    Use Matrix_System_Spheroid_Module,  only : Get_Matrix_System_Spheroid
    Use Linear_Algebra_Module,          only : Solve_Linear_System 
    Use Error_Module,                   only : Error_Fatal, Error_Warning
    Use SFL_Logical_Units,              only : SFL_Get_Free_Unit
    ! --- Local
    Implicit None
    Character(len=*), parameter :: routine = "Get_Mulipole_Coefficients"
    Integer, parameter          :: L_MAX_DEFAULT_STORE = 2
    Integer                     :: ienergy, m, N, file_id
    Integer                     :: dim, i, j
    Real(wp)                    :: condition_number 
    Complex(wp), allocatable    :: A(:,:), b(:)
    

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Allocate the Storage for the results
    ! ------------------------------------------
    N = 2 * param%Numerics%Multipole_Order *  size( Param%Geometry%Radius_Ratios,1)
    if (allocated(A)) deallocate(A);     Allocate( A( N, N ) ) 
    if (allocated(b)) deallocate(b);     Allocate( b( N    ) )


    ! --- Allocate Storage for the Multipole Coefficients (note only A-Coefficients)
    if ( allocated(Results%MultiPoles%Coefficients) ) &
         deallocate( Results%MultiPoles%Coefficients )
    dim = L_MAX_DEFAULT_STORE
    Allocate( Results%MultiPoles%Coefficients(size(param%Numerics%Energy,1), dim, 0:Param%Numerics%M_MAX)  )
    
   

 
    ! -------------------
    ! --- Energy loop
    ! -------------------
    Energy_Loop : Do ienergy = 1, size(param%Numerics%Energy,1)


       ! --- If verbose
       If (Param%InOut%Verbose) then
          if ( mod(ienergy,100)==0) then 
             Write(*,*) "     ... Processing energy (eV) :", param%Numerics%Energy(ienergy) 
          endif
       endif


       ! --- The m quantum number
       m_loop : Do m=0,1
          
          ! --- Set up the matrix system
          !call Get_Matrix_System( A, b, m, ienergy )
          if (size(Param%Geometry%Radius)==1) then 
            call Get_Matrix_System_Sphere( A, b, m, ienergy )
          else 
            call Get_Matrix_System_Spheroid( A, b, m, ienergy )
          end if 


          ! --- Solve the matrix system
          call Solve_Linear_System( A, b, condition_number )

          ! --- Store the dimensionless multipoles coefficients 
          !     needed later to calculate the polarizability
          Results%MultiPoles%Coefficients(ienergy,:, m) = b(1:L_MAX_DEFAULT_STORE)

          ! --- Store all the dimensionless Multipole Coefficients 
          !     (needed to calculate the potenatials).....
          !======= THIS SHOULD GO HERE WHEN IMPLEMENTED ==================
          

       enddo m_loop


    End Do Energy_Loop




    
    ! -------------------------------------------
    ! --- Writing MultiPole Coeffisients to file
    !     Only coef with l<=2 are written
    ! -------------------------------------------
    if (Param%InOut%Debug) then
       
       !-- Open Files
       call SFL_Get_Free_Unit( file_id )
       Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_Multipole_Coefficients.dat")
       Write(file_id,"(a)") &
            "# Format: energy, real(alpha_par), imag(alpha_par), real(alpha_perp), imag(alpha_perp), etc."       

       ! --- Energy loop
       do ienergy=1,size(Results%Polarizabilities,1)

          ! --- Write the result to file....
          Write(file_id,'(f10.5,5x,4(2f13.5,8x))')             &
             param%Numerics%Energy(ienergy),                   &
             ! --- \ell=1
             Results%MultiPoles%Coefficients( ienergy, 1, 0 ), &
             Results%MultiPoles%Coefficients( ienergy, 1, 1 ), &
             ! --- \ell=2
             Results%MultiPoles%Coefficients( ienergy, 2, 0 ), &
             Results%MultiPoles%Coefficients( ienergy, 2, 1 )
          
       enddo

       ! --- Close the file
       Close( file_id )
       
    endif




    ! --- Deallocate storeage
    if (allocated(A))           deallocate(A)
    if (allocated(b))           deallocate(b)


    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    
  End Subroutine Get_Multipole_Coefficients
  !---------------------------------------!



End Module MultiPole_Coefficient_Module
!--------------------------------------!











!
!
! ======================================================
! ==== REMOVED CODE ====================================
! ======================================================
!
!




!!$
!!$  ! --- Derived type
!!$  Type :: MP_Storage_Type
!!$     Logical :: MP_calculated 
!!$     Integer :: No_MP
!!$     Integer :: A_Start
!!$     Integer :: B_Start
!!$     Integer :: Not_Defined = -99
!!$  End Type MP_Storage_Type
!!$






!!$
!!$  !------------------------------------------------------------------------------!
!!$  Subroutine Get_Storage_for_Multipole_Coefficients( MP_Storage )
!!$  !------------------------------------------------------------------------------!  
!!$    !
!!$    !  --- Returns index information for the storage of the Multipole Coefficients
!!$    !      The informatation for medium is stored in MP_Storage(medium).
!!$    !
!!$    !      The information provided is :
!!$    !
!!$    !         Type :: MP_Storage_Type
!!$    !            Logical :: MP_calculated       ! Calculated or derived.....
!!$    !            Integer :: No_MP               ! # of MP coefficients
!!$    !            Integer :: A_Start             ! Start index position for A-coefficients
!!$    !            Integer :: B_Start             ! Start index position for B-coefficients
!!$    !            Integer :: Not_Defined = -99
!!$    !         End Type MP_Storage_Type
!!$    !
!!$    !       If an index is not needed it is given the value MP_Storage(:)%Not_Defined
!!$    !
!!$    !
!!$    !      Ingve Simonsen, Paris, Jan 2012
!!$    !
!!$    Use Shared,                only : pi, Results
!!$    Use Error_Module,          only : Error_Failure, Error_Warning
!!$    !
!!$    Type(MP_Storage_Type), dimension(:) :: MP_Storage
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Get_Storage_for_Multipole_Coefficients"
!!$    Integer   :: imedium,MP_Expansion_Medium
!!$    Real(wp), allocatable, dimension(:)  :: mu_s
!!$
!!$
!!$    ! --- We make sure that the MP expansion point is ABOVE the substrate....
!!$    !     This is an assumption!
!!$    !if ( .not. MP_Exp_Point_Above_Substrate() ) &
!!$    if ( .not. Param%Numerics%Multipole_Above_Substrate ) &
!!$         Call Error_Failure( routine, "MP expansion point NOT above the substrate (as assumed)!" )
!!$
!!$ 
!!$    ! --- Find the medium where the MultiPole expansion point is located
!!$    if (allocated( Param%Geometry%Radius_Ratios ) ) then
!!$
!!$       allocate( mu_s ( size( Param%Geometry%Radius_Ratios ) ) )
!!$       mu_s = Param%Numerics%Multipole_Position_Ratio / Param%Geometry%Radius_Ratios
!!$       do imedium=1,size( MP_Storage, 1 ),2
!!$          if (mu_s(imedium) <= 1._wp) MP_Expansion_Medium = imedium
!!$       enddo
!!$       deallocate( mu_s ) 
!!$    else
!!$       ! ... Give an error
!!$       Call Error_Failure( routine, "Param%Numerics%Radius_Ratios not allocated" )
!!$    End if
!!$
!!$
!!$    ! --- Set Number of Multipole Coefficients for this medium
!!$    do imedium = 1, size( MP_Storage, 1 )
!!$
!!$
!!$       if (mod(imedium,2)==1) then
!!$          
!!$          ! -------------
!!$          ! --- Odd -----
!!$          ! -------------
!!$          !
!!$
!!$          MP_Storage(imedium)%MP_Calculated = .true.
!!$
!!$          if (imedium==1) then
!!$             ! --- Outer medium
!!$             MP_Storage(imedium)%No_MP   = Param%Numerics%Multipole_Order
!!$             MP_Storage(imedium)%A_Start = 1
!!$             MP_Storage(imedium)%B_Start = MP_Storage(imedium)%Not_Defined
!!$
!!$          elseif( imedium == MP_Expansion_Medium )  then
!!$             ! --- medium where the MP expansion point is located
!!$             MP_Storage(imedium)%No_MP   = Param%Numerics%Multipole_Order
!!$             offset = Cummulative_No_of_MP_Coef( MP_Storage, imedium-1 ) 
!!$             MP_Storage(imedium)%A_Start = MP_Storage(imedium)%Not_Defined 
!!$             MP_Storage(imedium)%B_Start = offset + 1 
!!$
!!$          else
!!$
!!$             ! ---  Default medium
!!$             ! ---  i.e. Neither outer medium nor medium where the MP expansion point is located
!!$             MP_Storage(imedium)%No_MP   = 2 * Param%Numerics%Multipole_Order
!!$             offset = Cummulative_No_of_MP_Coef( MP_Storage, imedium-1 ) 
!!$             MP_Storage(imedium)%A_Start = offset + 1
!!$             MP_Storage(imedium)%B_Start = offset + 1 + Param%Numerics%Multipole_Order
!!$
!!$          end if
!!$  
!!$
!!$
!!$       else
!!$
!!$          ! -------------
!!$          ! --- Even ----
!!$          ! -------------
!!$          !
!!$          MP_Storage(imedium)%MP_Calculated = .false.   ! I.e. Coefs need to be calculated
!!$          MP_Storage(imedium) % No_MP    =  MP_Storage(imedium-1) % No_MP
!!$          MP_Storage(imedium) % A_Start  =  MP_Storage(imedium-1) % A_Start  
!!$          MP_Storage(imedium) % B_Start  =  MP_Storage(imedium-1) % B_Start 
!!$
!!$
!!$       end if
!!$
!!$    enddo
!!$    
!!$  End Subroutine Get_Storage_for_Multipole_Coefficients
!!$  !----------------------------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !---------------------------------------------------------------------!
!!$  Function Cummulative_No_of_MP_Coef( MP_Storage, imedium )  Result(Res)
!!$  !---------------------------------------------------------------------!
!!$    !
!!$    ! --- Calculates the number of expansion coefficients needed to be then 
!!$    !     the media from imedium and outwards (i.e. indices lower than and 
!!$    !     equal to imedium). 
!!$    !
!!$    !     If imedium is not given, the size of PM_Storage is used.
!!$    !
!!$    !  
!!$    !     Ingve Simonsen, Paris, Jan 2011
!!$    !
!!$    Use Error_Module,    only : Error_Fatal
!!$    Implicit None
!!$    Type(MP_Storage_Type), Intent(In), dimension(:) :: MP_Storage
!!$    Integer,               Intent(In), optional     :: imedium
!!$    Integer :: Res
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Cummulative_MP_Coef_Index"
!!$    Integer :: i, imedium_tmp
!!$
!!$
!!$    ! --- Set imedium_tmp
!!$    if (present(imedium)) then
!!$       imedium_tmp =  imedium
!!$    else
!!$       imedium_tmp =  Size(MP_Storage,1)
!!$    endif
!!$
!!$    ! --- Error checking
!!$    If (imedium_tmp>size(MP_Storage)) &
!!$         call Error_Fatal( routine , "dimensions not compatible")    
!!$
!!$    ! --- Main loop
!!$    res = 0
!!$    do i=1,imedium_tmp
!!$       if (MP_Storage(i)%MP_calculated) res = res + MP_Storage(i)%No_MP  
!!$    enddo
!!$    
!!$  End Function Cummulative_No_of_MP_Coef
!!$  !----------------------------------------------------------------------!
!!$
!!$


!!$
!!$  !-------------------------------------------------------!
!!$  Function MP_Exp_Point_Above_Substrate() Result(res)
!!$  !-------------------------------------------------------!
!!$    !
!!$    ! --- Utility routine to make chack is the expanison point is 
!!$    !     above the substrate
!!$    !
!!$    !     Ingve Simonsen, Paris, Jan 2011
!!$    !
!!$    Logical :: Res
!!$
!!$
!!$    if ( Param%Geometry%Truncation_Ratio > Param%Numerics%Multipole_Position_Ratio ) then
!!$       Res = .true.
!!$    else
!!$       Res = .false.
!!$    endif
!!$
!!$  End Function MP_Exp_Point_Above_Substrate
!!$  !-------------------------------------------------------!

