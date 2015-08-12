! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module calculates the various integrals for a spherical
!     geometry needed by GRANFILM
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!------------------------------!
Module Integrals_Sphere_Module
!------------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Integrals	   
  

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  !Public :: Allocate_Integrals_Sphere
  Public :: Get_Integrals_Sphere


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  
Contains



  !------------------------------------!
  Subroutine Get_Integrals_Sphere(  )
  !------------------------------------!
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Integrals_Sphere"
    Integer :: i


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    
    ! --- Allocate Storage
    ! ---------------------------------
    !call Integral_Allocation( Integrals )
    !call Allocate_Integrals(  )

        
    ! --- Set the truncation ratios
    Integrals(:) % Truncation_Ratio  =  Param%Geometry%Truncation_Ratio_Vector
    ! --- Set the radius of the spherical surface relative the outer shperical surface 
    Integrals(:) % Radius_Ratio      =  Param%Geometry%Radius_Ratios
    
    ! ---------------------------------
    ! --- Get the the Integrals
    ! ---------------------------------
        

    ! -------------------------------------------------------------------------------
    ! --- Calculate the needed Integrals for the various interfaces (i.e. coatings)
    ! -------------------------------------------------------------------------------
    do i= 1, size(Integrals,1)
      
       ! --- Calulate the integrals
       call Calculate_Integrals_Sphere( Integrals(i) )

    end do


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Write(*,*) " ... Un-needed calculation of the tr=1 integrals for the image multipole...."
    !Write(*,*) "     This can be MUCH better optimized .....!!!!"
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! -------------------------------------------------
    ! --- Get Integrals from pre-calculated table
    ! -------------------------------------------------
    !call Get_Integrals_From_Table()
    


    ! --- Should we write out the integrals.....
    if (Param%InOut%Debug) &
         call Write_Integrals_to_File()

    

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    
    
  End Subroutine Get_Integrals_Sphere
  !----------------------------------!
  


!!$
!!$  !-----------------------------------------!
!!$  Subroutine Allocate_Integrals_Sphere(  )
!!$  !-----------------------------------------!
!!$    !
!!$    ! --- This routine allocates the vector of integrals, as well as
!!$    !     the needed components.
!!$
!!$    !     The vector structure of the integrals is
!!$    !
!!$    !          Integrals( coating no. )
!!$    !
!!$    !     The components are :
!!$    !
!!$    !           Integrals( isurface ) % Q( l1, l2, m, iPosMP, iUpperLimit ) 
!!$    !
!!$    !     where iPosMP denotes the position of the multipole
!!$    !     (iPosMP=1) or image multipole (iPosMP=2). Morover,
!!$    !     iUpperLimit denotes the the upper limit of the integral
!!$    !     defniend so that if iUpperLimit=1 the upper limit it t_r,
!!$    !     and if it is 2 the limit is ONE.
!!$    !
!!$    !      Similar structure applies for the  K, L, M and N intergrals.
!!$    !
!!$    Implicit None
!!$    ! --- Local
!!$    Integer :: isurface, Nsurface
!!$
!!$    ! ---------------------------------
!!$    ! --- Allocate Storage
!!$    ! ---------------------------------
!!$
!!$    ! --- Clean the content if presently allocated
!!$    ! --------------------------------------------
!!$    if ( allocated(Integrals) ) then
!!$       ! ... Clean the components
!!$       do isurface=1, size(Integrals,1)
!!$          call Integral_Component_Deallocateion( Integrals(isurface) )
!!$       enddo
!!$       ! ... Deallocate the vector
!!$       deallocate( Integrals )
!!$    end if
!!$       
!!$    ! --- Allocate the Integral vector
!!$    ! ---------------------------------------
!!$    Nsurface = size( param % Geometry % Radius_Ratios, 1 )
!!$    if (.not. allocated( Integrals ) ) allocate( Integrals(Nsurface) )
!!$    
!!$    ! --- Allocate the components
!!$    ! ---------------------------------------
!!$    do isurface = 1, size(Integrals,1)
!!$       call Integral_Component_Allocation_and_Initialization( Integrals(isurface) )
!!$    end do
!!$
!!$  End Subroutine Allocate_Integrals_Sphere
!!$  !---------------------------------------!






  !
  ! -----------------------------------------------
  ! ---- Private Auxilliary Integral Routines
  ! -----------------------------------------------
  !






!!$  !--------------------------------------------!
!!$  Subroutine Set_Integral_Index_Defaults( Int )
!!$  !--------------------------------------------!
!!$    ! 
!!$    ! --- Sets the index defaults used internally for storage...
!!$    !
!!$    !     ASSUMPTION :  Int % MultiPole < Int % ImageMultiPole 
!!$    !
!!$    Use Derived_Type_Module, only : GranFilm_Integral_Type
!!$    Implicit None
!!$    Type(GranFilm_Integral_Type), Intent(InOut) :: Int
!!$    
!!$    ! -- The MultiPole index
!!$    Int % MultiPole      = 1 
!!$    Int % ImageMultiPole = 2 
!!$    ! --- the UpperLimit integration index
!!$    Int % UpperLimit_tr  = 1
!!$    Int % UpperLimit_One = 2  
!!$
!!$  End Subroutine Set_Integral_Index_Defaults
!!$  !--------------------------------------------!







  !------------------------------------------------------!
  Subroutine Calculate_Integrals_Sphere( Int )
  !------------------------------------------------------!
    !
    !  All of the Integrals have the array structure, e.g.
    !
    !    Integral(:,:) % K( l1, l2, m, iPosMP, iUpperLimit )
    !
    !
    Use Derived_Type_Module,     only : GranFilm_Integral_Type
    Use Error_Module,            only : Error_Warning
    Use Quadpack_Wrapper_Module, only : Quadpack, Set_QuadPack_Error_Limits
    Use Integrand_Module,        only : l1, l2, m, PosMP, &
                                        Q_Integrand, K_Integrand, L_Integrand, M_Integrand, N_Integrand
    ! ---------- TESTING -----------
    Use Integrand_Module,    only : Test_Integrand
    ! ---------- TESTING -----------
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int
    ! --- Local Parameters
    !Integer,          parameter :: MultiPole=1,     ImageMultiPole=2   ! For internal use only
    !Integer,          parameter :: UpperLimit_tr=1, UpperLimit_One=2   ! For internal use only
    Real(wp),         parameter :: ONE = 1._wp
    Character(len=*), parameter :: routine = "Calculate_Integrals_Sphere"

    ! --- Local Variables
    Integer              :: N_MultiPole, iPosMP 
    Real(wp)             :: tr
    Real(wp)             :: MultiPole_Position_Ratio(2)
    ! --- Testing
    Real(wp) :: Kast



    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    


    If (Param%InOut%Verbose) &
         Write(*,*) " ... Calculating Integrals for tr = ", Int%Truncation_Ratio


 
    ! --- Set Accuracy
    !call Set_QuadPack_Error_Limits( Absolute_Error = 10._wp**(-wp), Relative_Error=10._wp**(-wp) )


    


    ! ---- TESTING ----------
    !Call Quadpack( Test_Integrand, -ONE, 0.1_wp, tr )
    !Write(*,*) " Integrals : ", tr 
    !stop
    ! ---- TESTING ----------


    ! --- Some abbreviations
    tr  =  Int%Truncation_Ratio



    ! -- Define the Multipole Positions (relative the radius of the spherical surface in question)
    MultiPole_Position_Ratio( Int%MultiPole      )   =  Param%Numerics%MultiPole_Position_Ratio  / Int%Radius_Ratio
    MultiPole_Position_Ratio( Int%ImageMultiPole )   =  2*Int%Truncation_Ratio - MultiPole_Position_Ratio( Int%MultiPole  )



    ! ---- TESTING -----
    !Write(*,*)   " position  MP : ",  MultiPole_Position_Ratio( Int%MultiPole      )   
    !Write(*,*)   " position IMP : ",  MultiPole_Position_Ratio( Int%ImageMultiPole )   
    !Write(*,*)   " t_r          : ",  tr
    !stop
    ! ---- TESTING -----




    !
    !----------------------------------------------
    !---  Calculates the Integrals
    !---------------------------------------------- 
    !

    ! --- Initialization of integrals
    Int % Q   = 0._wp
    Int % K   = 0._wp
    Int % L   = 0._wp
    Int % M   = 0._wp
    Int % N   = 0._wp


    
    !------------------------------------
    ! Special Case for l1=0,l2=0,m=0
    !------------------------------------
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = Int%Truncation_Ratio + 1._wp    
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ) = ONE + 1._wp    


    ! ------------- TESTING ---------------
    !l1=0;  l2=0; m=0
    !Write(*,*) "-------------------------"
    !Write(*,*) " tr      :", tr
    !Call Quadpack( Q_Integrand, -ONE, tr, Kast )
    !Write(*,*) "Analytic :", Int%Q(0,0,0,MultiPole) 
    !Write(*,*) "Numeric  :", Kast 
    !Write(*,*) "-------------------------"
    ! ------------- TESTING ---------------


    ! --- Comment
    !     Maybe one should get out the Qintegration and only calculate them 
    !Call Error_Warning( routine, "Do we need all these integrals for l1=0 or l2=0?")



    !--------------------------------------------------   
    ! --- Loops over all "quantum numbers" for m=0,1
    !--------------------------------------------------
    Do l1=0,param%Numerics%Multipole_Order
       Do l2=0,param%Numerics%Multipole_Order
          Do m=0,1


             ! ... Make sure the combination (l1,l2,m) is allowed 
             if(  ((l1==0).or.(l2==0)) .and. (m==1) ) cycle

             !
             ! --- Calculate the integrals for Int%UpperLimit_tr
             ! -------------------------------------------------------------------
             !
             !      These integrals are needed for both MultiPole and ImageMultiPole
             !
             Do iPosMP=Int%MultiPole,Int%ImageMultiPole

                ! -----------------------------------------------
                ! --- Share the multipole position
                ! ----------------------------------------------
                PosMP =  MultiPole_Position_Ratio(iPosMP) 

                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                Call Quadpack( Q_Integrand, -ONE, tr,  Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) ) ! ??? Needed ???
                
                !------------------------------------------------
                ! --- K-integrals 
                !------------------------------------------------         
                Call Quadpack( K_Integrand, -ONE, tr,  Int%K(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !Call Quadpack( K_Integrand, -ONE, ONE, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- L-integrals 
                !------------------------------------------------
                Call Quadpack( L_Integrand, -ONE, tr,  Int%L(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !Call Quadpack( L_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- M-integrals 
                !------------------------------------------------
                Call Quadpack( M_Integrand, -ONE, tr,  Int%M(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !Call Quadpack( M_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- N-integrals 
                !------------------------------------------------
                Call Quadpack( N_Integrand, -ONE, tr,  Int%N(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !Call Quadpack( N_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )

             Enddo


             !
             ! --- Calculate the integrals for Int%UpperLimit_One
             ! -------------------------------------------------------------------
             !     These integrals are only neede for the Multipole Position
             !
             !     NOTE : the loop limit on iPosMP
             !
             Do iPosMP=Int%MultiPole,Int%MultiPole


                ! -----------------------------------------------
                ! --- Share the multipole position
                ! ----------------------------------------------
                PosMP =  MultiPole_Position_Ratio(iPosMP) 

                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                !Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) ) ! ??? Needed ???
                
                !------------------------------------------------
                ! --- K-integrals 
                !------------------------------------------------         
                Call Quadpack( K_Integrand, -ONE, ONE, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- L-integrals 
                !------------------------------------------------
                Call Quadpack( L_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- M-integrals 
                !------------------------------------------------
                Call Quadpack( M_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- N-integrals 
                !------------------------------------------------
                Call Quadpack( N_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )

             Enddo

          Enddo   ! m
       Enddo      ! l2 
    Enddo         ! l1


    ! --- Testing
    !Write(*,*) "Numeric, Analytic Results for Q-Integrals"
    !Write(*,*) Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr ) ,  Int%Truncation_Ratio + 1._wp    
    !Write(*,*) Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ),  ONE + 1._wp    



    ! --- Analytic results .....
    !------------------------------------
    !     Special Case for l1=0,l2=0,m=0
    !------------------------------------
    !     We judge that overhead in calculating these integrals 
    !     first numerically to be negligible
    !
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = Int%Truncation_Ratio + 1._wp    
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ) = ONE + 1._wp    




    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    

  End Subroutine Calculate_Integrals_Sphere
  !----------------------------------------!
















  !--------------------------------------------------------!
  Subroutine Calculate_Integrals_Hidden_But_Working( Int )
  !------------------------------------------------------!
    !
    !  All of the Integrals have the array structure, e.g.
    !
    !    Integral(:,:) % K( l1, l2, m, iPosMP, iUpperLimit )
    !
    !
    Use Derived_Type_Module, only : GranFilm_Integral_Type
    Use Quadpack_Wrapper_Module, only : Quadpack, Set_QuadPack_Error_Limits
    Use Integrand_Module,        only : l1, l2, m, PosMP, &
                        Q_Integrand, K_Integrand, L_Integrand, M_Integrand, N_Integrand
    ! ---------- TESTING -----------
    Use Integrand_Module,    only : Test_Integrand
    ! ---------- TESTING -----------
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int
    ! --- Local Parameters
    !Integer,          parameter :: MultiPole=1,     ImageMultiPole=2   ! For internal use only
    !Integer,          parameter :: UpperLimit_tr=1, UpperLimit_One=2   ! For internal use only
    Real(wp),         parameter :: ONE = 1._wp
    Character(len=*), parameter :: routine = "Calculate_Integrals"

    ! --- Local Variables
    Integer              :: N_MultiPole, iPosMP 
    Real(wp)             :: tr
    Real(wp)             :: MultiPole_Position_Ratio(2)
    ! --- Testing
    Real(wp) :: Kast



    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    


    If (Param%InOut%Verbose) &
         Write(*,*) " ... Calculating Integrals for tr = ", Int%Truncation_Ratio


 
    ! --- Set Accuracy
    !call Set_QuadPack_Error_Limits( Absolute_Error = 10._wp**(-wp), Relative_Error=10._wp**(-wp) )



    ! ---- TESTING ----------
    !Call Quadpack( Test_Integrand, -ONE, 0.1_wp, tr )
    !Write(*,*) " Integrals : ", tr 
    !stop
    ! ---- TESTING ----------


    ! --- Some abbreviations
    tr  =  Int%Truncation_Ratio





    ! --- Initialization of integrals
    Int % Q   = 0._wp
    Int % K   = 0._wp
    Int % L   = 0._wp
    Int % M   = 0._wp
    Int % N   = 0._wp





    ! --- Calculate mu position in the main coordiante system (relative the radius of the spherical surface in question)
    MultiPole_Position_Ratio( Int%MultiPole      )   =  Param%Numerics%MultiPole_Position_Ratio  / Int%Radius_Ratio
    MultiPole_Position_Ratio( Int%ImageMultiPole )   =  2._wp*Int%Truncation_Ratio - MultiPole_Position_Ratio( Int%MultiPole )




    ! ---- TESTING -----
    !Write(*,*) " WARNING : TESTING  ***************** REDEFINING MULTIPOLE_POSITION *****************"
    !MultiPole_Position( MultiPole      )   =  0._wp
    !MultiPole_Position( ImageMultiPole )   =  0._wp

    Write(*,*)   " position  MP : ",  MultiPole_Position_Ratio( Int%MultiPole      )   
    Write(*,*)   " position IMP : ",  MultiPole_Position_Ratio( Int%ImageMultiPole )   
    Write(*,*)   " t_r          : ",  tr
    !stop
    ! ---- TESTING -----

    !Do imp =1,2
    !   MultiPole_Position(imp)  =  Calculate_Multipole_Position(param%Island%Truncation_Ratio,imu,param%Numerics%Multipole_Position)
    !Enddo


    !
    !----------------------------------------------
    !---  Calculates the Integrals
    !---------------------------------------------- 
    !
    
    !------------------------------------
    ! Special Case for l1=0,l2=0,m=0
    !------------------------------------
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = Int%Truncation_Ratio + 1._wp    
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ) = ONE + 1._wp    


    ! ------------- TESTING ---------------
    !l1=0;  l2=0; m=0
    !Write(*,*) "-------------------------"
    !Write(*,*) " tr      :", tr
    !Call Quadpack( Q_Integrand, -ONE, tr, Kast )
    !Write(*,*) "Analytic :", Int%Q(0,0,0,MultiPole) 
    !Write(*,*) "Numeric  :", Kast 
    !Write(*,*) "-------------------------"
    ! ------------- TESTING ---------------

    

    !Write(*,*) " l1 ", size(Int%K,1)
    !Write(*,*) " l2 ", size(Int%K,2)
    !Write(*,*) " m  ", size(Int%K,3), lbound(Int%K,3), ubound(Int%K,3)
    !Write(*,*) " mp ", size(Int%K,4)
    !Write(*,*) "param%Numerics%Multipole_Order",param%Numerics%Multipole_Order
    !stop
    
    !Write(*,*) " *** 0 ***"

    !--------------------------
    ! Special Case for l1=0,m=0 
    !--------------------------
    l1   =  0
    m    =  0      
    !exch%l1 = l1
    !exch%m  = m                  
    Do l2=1,param%Numerics%Multipole_Order

       !exch%l2 = l2

       Do iPosMP=Int%MultiPole,Int%ImageMultiPole ! Loop over MultPole / ImageMultiPole

          ! -----------------------------------------------
          ! --- Share the multipole position
          ! ----------------------------------------------
          PosMP =  MultiPole_Position_Ratio(iPosMP) 
          
          !------------------------------------------------
          ! --- Q-integrals 
          !------------------------------------------------
          !if (imp==Int%MultiPole) &
          Call Quadpack( Q_Integrand, -ONE, tr,  Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) )  ! ??? Needed ???

          !------------------------------------------------
          ! --- K-integrals 
          !------------------------------------------------         
          Call Quadpack( K_Integrand, -ONE, tr,  Int%K(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( K_Integrand, -ONE, ONE, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

          !------------------------------------------------
          ! --- L-integrals 
          !------------------------------------------------
          Call Quadpack( L_Integrand, -ONE, tr,  Int%L(l1,l2,m,iPosMP,Int%UpperLimit_tr) )
          Call Quadpack( L_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

          !------------------------------------------------
          ! --- M-integrals 
          !------------------------------------------------
          Call Quadpack( M_Integrand, -ONE, tr,  Int%M(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( M_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

          !------------------------------------------------
          ! --- N-integrals 
          !------------------------------------------------
          Call Quadpack( N_Integrand, -ONE, tr,  Int%N(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( N_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )



          ! ------ TESTING -------------
          !  Write(*,'(a25,i5,i5,i5,i5,f10.6,f25.16)')  " l1, l2, m, imp, tr   = ", l1, l2, m, imp,tr
          !  Write(*,*)    "       Q =   ", Int%Q(l1,l2,m,imp)
          !  Write(*,*)    "       K =   ", Int%K(l1,l2,m,imp)
          !  Write(*,*)    "       L =   ", Int%L(l1,l2,m,imp)
          !  Write(*,*)    "       M =   ", Int%M(l1,l2,m,imp)
          !  Write(*,*)    "       N =   ", Int%N(l1,l2,m,imp)
          ! ------ TESTING -------------

       Enddo
    Enddo

    !Write(*,*) " *** 1 ***"

    !--------------------------
    ! Special Case for l2=0,m=0 
    !--------------------------
    l2   =       0
    m    =       0                        
    Do l1=1,param%Numerics%Multipole_Order

       Do iPosMP=Int%MultiPole,Int%ImageMultiPole

          ! -----------------------------------------------
          ! --- Share the multipole position
          ! ----------------------------------------------
          PosMP =  MultiPole_Position_Ratio(iPosMP) 

          !------------------------------------------------
          ! --- Q-integrals 
          !------------------------------------------------
          !if (iPosMP==MultiPole) &
          Call Quadpack( Q_Integrand, -ONE, tr,  Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) )  ! ??? Needed ???

          !------------------------------------------------
          ! --- K-integrals 
          !------------------------------------------------         
          Call Quadpack( K_Integrand, -ONE, tr,  Int%K(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( K_Integrand, -ONE, One, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

          !------------------------------------------------
          ! --- L-integrals 
          !------------------------------------------------
          Call Quadpack( L_Integrand, -ONE, tr,  Int%L(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( L_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

          !------------------------------------------------
          ! --- M-integrals 
          !------------------------------------------------
          Call Quadpack( M_Integrand, -ONE, tr,  Int%M(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( M_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

          !------------------------------------------------
          ! --- N-integrals 
          !------------------------------------------------
          Call Quadpack( N_Integrand, -ONE, tr,  Int%N(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
          Call Quadpack( N_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )



          ! ------ TESTING -------------
          !   Write(*,'(a25,i5,i5,i5,i5,f10.6,f25.16)')  " l1, l2, m, iPosMP, tr   = ", l1, l2, m, iPosMP,tr
          !   Write(*,*)    "       Q =   ", Int%Q(l1,l2,m,iPosMP)
          !   Write(*,*)    "       K =   ", Int%K(l1,l2,m,iPosMP)
          !   Write(*,*)    "       L =   ", Int%L(l1,l2,m,iPosMP)
          !   Write(*,*)    "       M =   ", Int%M(l1,l2,m,iPosMP)
          !   Write(*,*)    "       N =   ", Int%N(l1,l2,m,iPosMP)
          ! ------ TESTING -------------

       Enddo
    Enddo

    !Write(*,*) " *** 2 ***"

    !-------------------------------------------   
    ! Loops over all "quantum numbers" for m=0,1
    !-------------------------------------------
    Do l1=1,param%Numerics%Multipole_Order
       Do l2=1,param%Numerics%Multipole_Order
          Do m=0,1


             Do iPosMP=Int%MultiPole,Int%ImageMultiPole

                ! -----------------------------------------------
                ! --- Share the multipole position
                ! ----------------------------------------------
                PosMP =  MultiPole_Position_Ratio(iPosMP) 

                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                !if (iPosMP==MultiPole) &
                Call Quadpack( Q_Integrand, -ONE, tr,  Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) ) ! ??? Needed ???
                
                !------------------------------------------------
                ! --- K-integrals 
                !------------------------------------------------         
                Call Quadpack( K_Integrand, -ONE, tr,  Int%K(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                Call Quadpack( K_Integrand, -ONE, ONE, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- L-integrals 
                !------------------------------------------------
                Call Quadpack( L_Integrand, -ONE, tr,  Int%L(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                Call Quadpack( L_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- M-integrals 
                !------------------------------------------------
                Call Quadpack( M_Integrand, -ONE, tr,  Int%M(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                Call Quadpack( M_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- N-integrals 
                !------------------------------------------------
                Call Quadpack( N_Integrand, -ONE, tr,  Int%N(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                Call Quadpack( N_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )



                ! ------ TESTING -------------
                !if (iPosMP==1) then
                !   Write(*,'(a25,i5,i5,i5,i5,f10.6,f25.16)')  " l1, l2, m, iPosMP, tr   = ", l1, l2, m, iPosMP,tr
                !   Write(*,*)    "       Q =   ", Int%Q(l1,l2,m,iPosMP)
                !   Write(*,*)    "       K =   ", Int%K(l1,l2,m,iPosMP)
                !   Write(*,*)    "       L =   ", Int%L(l1,l2,m,iPosMP)
                !   Write(*,*)    "       M =   ", Int%M(l1,l2,m,iPosMP)
                !   Write(*,*)    "       N =   ", Int%N(l1,l2,m,iPosMP)
                !endif
                ! ------ TESTING -------------

             Enddo
          Enddo
       Enddo
    Enddo


   !Write(*,*) " *** 3 ***"

!!$    If(mm==2) Then
!!$       ! Loops over all "quantum numbers" for m=2 If necessary
!!$       m = mm
!!$       Do l1=2,mpo
!!$          Do l2=2,mpo
!!$             !------------------------------------------------
!!$             ! --- Q-integrals 
!!$             !------------------------------------------------
!!$             Call Quadpack(Int_Q,-1._wp,tr,Integ(l1,l2,m)%Q)
!!$             Do imu=1,2  ! Loop over mu-type
!!$                mupos = mu(imu)
!!$                !------------------------------------------------
!!$                ! --- K-integrals 
!!$                !------------------------------------------------         
!!$                Call Quadpack(Int_K,-1._wp,tr,Integ(l1,l2,m)%K%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- L-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_L,-1._wp,tr,Integ(l1,l2,m)%L%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- M-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_M,-1._wp,tr,Integ(l1,l2,m)%M%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- N-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_N,-1._wp,tr,Integ(l1,l2,m)%N%mu(imu))
!!$             Enddo
!!$          Enddo
!!$       Enddo
!!$    Endif

    




    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    

  End Subroutine Calculate_Integrals_Hidden_But_Working
  !---------------------------------!








!!$
!!$
!!$  Module Integral_mod
!!$
!!$  Use Share_Parameters_Mod
!!$  Use Special_functions_mod, Only : AssLegPoly1
!!$  Use Quadpack_mod
!!$
!!$  Implicit None
!!$
!!$Contains
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$  !----------------------------------------------------------------
!!$  !               The main routine for getting the integrals
!!$  !       Adaptative quadrature integration method using QUADPACK
!!$  !----------------------------------------------------------------
!!$  Subroutine Get_integrals(tr,Integ)
!!$    Implicit None
!!$    Type(integrals_type) :: Integ(0:,0:,0:)
!!$    Integer              :: l1,l2,m
!!$    Real(wp)             :: tr,mu(2),mupos
!!$    Integer              :: imu,mpo,mm
!!$
!!$    Common /index/     l1,l2,m
!!$    Common /mupoint/   mupos
!!$
!!$    ! Some abbreviations
!!$    mpo      = param%Numerics%Multipole_Order
!!$    mm       = param%Numerics%m
!!$
!!$    ! Calculate mu position in the main coordiante system
!!$    Do imu =1,2
!!$       mu(imu)  =  mu_func(param%Island%Truncation_Ratio,imu,param%Numerics%Multipole_Position)
!!$    Enddo
!!$
!!$    ! Initialization of integrals
!!$    Integ(:,:,:)%Q         = 0._wp
!!$    Do m=1,2
!!$       Integ(:,:,:)%K%mu(m)  = 0._wp
!!$       Integ(:,:,:)%L%mu(m)  = 0._wp
!!$       Integ(:,:,:)%M%mu(m)  = 0._wp
!!$    Enddo
!!$
!!$    !----------------------------------------------
!!$    !---  Calculates the Integrals
!!$    !---------------------------------------------- 
!!$
!!$    !-------------------------------
!!$    ! Special Case for l1=0,l2=0,m=0 
!!$    !-------------------------------
!!$    Integ(0,0,0)%Q = tr + 1._wp    
!!$
!!$    !--------------------------
!!$    ! Special Case for l1=0,m=0 
!!$    !--------------------------
!!$    l1   =       0
!!$    m    =       0                        
!!$    Do l2=1,mpo
!!$       !------------------------------------------------
!!$       ! --- Q-integrals 
!!$       !------------------------------------------------
!!$       Call Quadpack(Int_Q,-1._wp,tr,Integ(l1,l2,m)%Q)
!!$       Do imu=1,2  ! Loop over mu-type
!!$          mupos = mu(imu)
!!$          !------------------------------------------------
!!$          ! --- K-integrals 
!!$          !------------------------------------------------         
!!$          Call Quadpack(Int_K,-1._wp,tr,Integ(l1,l2,m)%K%mu(imu))
!!$          !------------------------------------------------
!!$          ! --- L-integrals 
!!$          !------------------------------------------------
!!$          Call Quadpack(Int_L,-1._wp,tr,Integ(l1,l2,m)%L%mu(imu))
!!$          !------------------------------------------------
!!$          ! --- M-integrals 
!!$          !------------------------------------------------
!!$          Call Quadpack(Int_M,-1._wp,tr,Integ(l1,l2,m)%M%mu(imu))
!!$          !------------------------------------------------
!!$          ! --- N-integrals 
!!$          !------------------------------------------------
!!$          Call Quadpack(Int_N,-1._wp,tr,Integ(l1,l2,m)%N%mu(imu))
!!$       Enddo
!!$    Enddo
!!$
!!$    !--------------------------
!!$    ! Special Case for l2=0,m=0 
!!$    !--------------------------
!!$    l2   =       0
!!$    m    =       0                        
!!$    Do l1=1,mpo
!!$       !------------------------------------------------
!!$       ! --- Q-integrals 
!!$       !------------------------------------------------
!!$       Call Quadpack(Int_Q,-1._wp,tr,Integ(l1,l2,m)%Q)
!!$       Do imu=1,2  ! Loop over mu-type
!!$          mupos = mu(imu)
!!$          !------------------------------------------------
!!$          ! --- K-integrals 
!!$          !------------------------------------------------         
!!$          Call Quadpack(Int_K,-1._wp,tr,Integ(l1,l2,m)%K%mu(imu))
!!$          !------------------------------------------------
!!$          ! --- L-integrals 
!!$          !------------------------------------------------
!!$          Call Quadpack(Int_L,-1._wp,tr,Integ(l1,l2,m)%L%mu(imu))
!!$          !------------------------------------------------
!!$          ! --- M-integrals 
!!$          !------------------------------------------------
!!$          Call Quadpack(Int_M,-1._wp,tr,Integ(l1,l2,m)%M%mu(imu))
!!$          !------------------------------------------------
!!$          ! --- N-integrals 
!!$          !------------------------------------------------
!!$          Call Quadpack(Int_N,-1._wp,tr,Integ(l1,l2,m)%N%mu(imu))
!!$       Enddo
!!$    Enddo
!!$
!!$
!!$    !-------------------------------------------   
!!$    ! Loops over all "quantum numbers" for m=0,1
!!$    !-------------------------------------------
!!$    Do l1=1,mpo
!!$       Do l2=1,mpo
!!$          Do m=0,1
!!$             !------------------------------------------------
!!$             ! --- Q-integrals 
!!$             !------------------------------------------------
!!$             Call Quadpack(Int_Q,-1._wp,tr,Integ(l1,l2,m)%Q)
!!$             Do imu=1,2  ! Loop over mu-type
!!$                mupos = mu(imu)
!!$                !------------------------------------------------
!!$                ! --- K-integrals 
!!$                !------------------------------------------------         
!!$                Call Quadpack(Int_K,-1._wp,tr,Integ(l1,l2,m)%K%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- L-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_L,-1._wp,tr,Integ(l1,l2,m)%L%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- M-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_M,-1._wp,tr,Integ(l1,l2,m)%M%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- N-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_N,-1._wp,tr,Integ(l1,l2,m)%N%mu(imu))
!!$             Enddo
!!$          Enddo
!!$       Enddo
!!$    Enddo
!!$
!!$    If(mm==2) Then
!!$       ! Loops over all "quantum numbers" for m=2 If necessary
!!$       m = mm
!!$       Do l1=2,mpo
!!$          Do l2=2,mpo
!!$             !------------------------------------------------
!!$             ! --- Q-integrals 
!!$             !------------------------------------------------
!!$             Call Quadpack(Int_Q,-1._wp,tr,Integ(l1,l2,m)%Q)
!!$             Do imu=1,2  ! Loop over mu-type
!!$                mupos = mu(imu)
!!$                !------------------------------------------------
!!$                ! --- K-integrals 
!!$                !------------------------------------------------         
!!$                Call Quadpack(Int_K,-1._wp,tr,Integ(l1,l2,m)%K%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- L-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_L,-1._wp,tr,Integ(l1,l2,m)%L%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- M-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_M,-1._wp,tr,Integ(l1,l2,m)%M%mu(imu))
!!$                !------------------------------------------------
!!$                ! --- N-integrals 
!!$                !------------------------------------------------
!!$                Call Quadpack(Int_N,-1._wp,tr,Integ(l1,l2,m)%N%mu(imu))
!!$             Enddo
!!$          Enddo
!!$       Enddo
!!$    Endif
!!$
!!$    Return
!!$
!!$  End Subroutine Get_integrals
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$  Function mu_func(tr,Type,mupos)  Result(mu)
!!$    ! calculated the z-coordiate of the multipole (MP) and image MP
!!$    Implicit None
!!$    Real(wp)           :: mupos     
!!$    Integer            :: Type
!!$    Real(wp)           :: tr,mu
!!$
!!$    Select Case(Type)
!!$    Case(1)
!!$       ! The MP
!!$       mu = mupos
!!$    Case(2)
!!$       ! The image MP
!!$       mu = 2._wp*tr - mupos
!!$    End Select
!!$
!!$  End Function mu_func
!!$
!!$  !-------------------------------------------------------------------------------------------------------

!!$
!!$
!!$  Function gamma_func(x,mu)  Result(gamma)
!!$    Implicit None
!!$    Real(wp)   :: x, mu, gamma     
!!$
!!$    gamma= 1._wp-2*x*mu+mu**2
!!$
!!$  End Function gamma_func
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$  Function chi_func(x,mu)  Result(chi)
!!$    Implicit None
!!$    Real(wp)   :: x, mu, chi
!!$
!!$    chi = (x-mu)/Sqrt(gamma_func(x,mu))   
!!$
!!$  End Function chi_func
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$
!!$  Function Deriv_Ass_Legendre(l,m,x) Result(dalp)
!!$    ! Calculates the first derivative of the Associate Legendre polynomials
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Integer,   Intent(In)   :: l, m
!!$    Real(wp),  Intent(in)   :: x
!!$    Real(wp)                :: dalp
!!$
!!$    ! Recursive relation
!!$    dalp  = (l-m+1)*AssLegPoly1(l+1,m,x)-(l+1)*x*AssLegPoly1(l,m,x)
!!$    dalp = dalp/(x**2-1)
!!$
!!$  End Function  Deriv_Ass_Legendre
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$
!!$
!!$  Function Q_Integrand(x) Result(res)
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)           ::      x
!!$    Real(wp)           ::      Res
!!$
!!$    res  =  AssLegPoly1(l1,m,x) * AssLegPoly1(l2,m,x)
!!$
!!$    Return
!!$  End Function Q_Integrand
!!$
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$
!!$  Function K_Integrand(x) Result(res)
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)        ::      x
!!$    Real(wp)        ::      res
!!$    !  --- Local
!!$    Real(wp)        ::      mu,chi,gamma
!!$
!!$    mu    =   Multipole_Position_Shared
!!$    chi   =   chi_func(x, mu )
!!$    gamma =   gamma_func(x, mu )
!!$    
!!$    res   =   AssLegPoly1(l1,m,x)*AssLegPoly1(l2,m,chi)*gamma**(-(l2+1._wp)/2._wp)
!!$
!!$    Return
!!$  End Function K_Integrand
!!$
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$
!!$
!!$  Function L_Integrand(x) Result(res)
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)           ::    x
!!$    Real(wp)           ::    res
!!$    ! --- Local
!!$    Real(wp)           ::      mu,chi,gamma
!!$
!!$    mu = Multipole_Position_Shared
!!$    chi    =  chi_func(x, mu )
!!$    gamma  =  gamma_func(x, mu )
!!$
!!$    res         =       &
!!$         AssLegPoly1(l1,m,x) * ( -(l2+1._wp)*gamma**(-0.5_wp*l2-1.5_wp)*(1._wp-mu*x)*AssLegPoly1(l2,m,chi) + &
!!$         gamma**(-0.5_wp*l2-2._wp)*(x*gamma-(x-mu)*(1._wp-mu*x))*Deriv_Ass_Legendre(l2,m,chi) )
!!$
!!$    Return
!!$  End Function L_Integrand
!!$
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$
!!$
!!$  Function M_Integrand(x) Result(res)
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)           :: x
!!$    Real(wp)           :: res
!!$    ! --- Local
!!$    Real(wp)           ::      mu,chi,gamma
!!$
!!$    mu     =  Multipole_Position_Shared
!!$    chi    =  chi_func(x, mu )
!!$    gamma  =  gamma_func(x, mu )
!!$
!!$    res    =  AssLegPoly1(l1,m,x)*AssLegPoly1(l2,m,chi)*gamma**(l2/2._wp)
!!$
!!$    Return
!!$  End Function M_Integrand
!!$
!!$
!!$  !-------------------------------------------------------------------------------------------------------
!!$
!!$
!!$  Function N_Integrand(x) Result(res)
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)          :: x
!!$    Real(wp)          :: res
!!$    ! --- Local
!!$    Real(wp)          ::      mu,chi,gamma
!!$
!!$    mu      =  Multipole_Position_Shared
!!$    chi     =  chi_func(x, MultiPole_Position_Shared )
!!$    gamma   =  gamma_func(x, MultiPole_Position_Shared )
!!$
!!$    res            =       &
!!$         AssLegPoly1(l1,m,x) * ( l2*gamma**(0.5_wp*l2-1._wp)*(1._wp-mu*x)*AssLegPoly1(l2,m,chi) + &
!!$         gamma**(0.5_wp*l2-1.5_wp)*(x*gamma-(x-mu)*(1._wp-mu*x))*Deriv_Ass_Legendre(l2,m,chi) )
!!$
!!$    Return
!!$  End Function N_Integrand
!!$
!!$
!!$






  !
  ! ----------------------------------
  ! ---- Private Auxilliary Routines
  ! ----------------------------------
  !

  !-------------------------------------!
  Subroutine Write_Integrals_to_File()
  !-------------------------------------!
    Use SFL_Logical_Units,   only : SFL_Get_Free_Unit
    Use Error_Module,        only : Error_Warning   
    Implicit None
    character(len=*), parameter :: routine = "Write_Integrals_to_File"
    Integer :: file_id, ienergy, l1,l2,m, ipos, iupl,s
    character(len=1) :: coating
    

    do s=1,size(Integrals,1)
      write(coating,'(i1)') s

      ! ------------------------------------
      ! --- WHICH COMPONENT TO WRITE OUT
      ! ------------------------------------
      !
      ! ... Multipole Position
      !ipos = Integrals(1) % MultiPole
      ipos = Integrals(s) % ImageMultiPole
      

      ! ... Upper Limit of the Integral....
      iupl = Integrals(s) % UpperLimit_tr
      !iupl = Integrals(1) % UpperLimit_One



      ! ------------------------------------
      ! --- Q-Integrals
      ! ------------------------------------
      !
      !-- Open Files
      call SFL_Get_Free_Unit( file_id )
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_Q.dat")
      Write(file_id,"(a)") "# Format:  Q-Integral    # l1, l2, m"
      Write(file_id,"(a,f10.5)") "# t_r  = ", Integrals(s)%Truncation_Ratio
      Write(file_id,"(a,i5,i5)") "# iPosMP, iUpperLimit  = ", ipos, iupl
      Write(file_id,"(a,f10.5)") "# Multpole Postion     = ", Param%Numerics%MultiPole_Position_Ratio
      Do l1=0,param%Numerics%Multipole_Order
         Do l2=0,param%Numerics%Multipole_Order
            do m=0,1
               Write(file_id,*) Integrals(s)%Q(l1,l2,m,ipos,iupl),   "       # ", l1,l2,m
            enddo
         enddo
      enddo
      close(file_id)


      ! ------------------------------------
      ! --- K-Integrals
      ! ------------------------------------
      !
      !-- Open Files
      call SFL_Get_Free_Unit( file_id )
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_K.dat")
      Write(file_id,"(a)") "# Format:  K-Integral    # l1, l2, m"
      Write(file_id,"(a,f10.5)") "# t_r  = ", Integrals(s)%Truncation_Ratio
      Write(file_id,"(a,i5,i5)") "# iPosMP, iUpperLimit  = ", ipos, iupl
      Write(file_id,"(a,f10.5)") "# Multpole Postion     = ", Param%Numerics%MultiPole_Position_Ratio
      Do l1=0,param%Numerics%Multipole_Order
         Do l2=0,param%Numerics%Multipole_Order
            Do m=0,1
               Write(file_id,*) Integrals(s)%K(l1,l2,m,ipos,iupl),   "       # ", l1,l2,m
            enddo
         enddo
      enddo
      close(file_id)


      ! ------------------------------------
      ! --- L-Integrals
      ! ------------------------------------
      !
      !-- Open Files
      call SFL_Get_Free_Unit( file_id )
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_L.dat")
      Write(file_id,"(a)") "# Format:  L-Integral    # l1, l2, m"
      Write(file_id,"(a,f10.5)") "# t_r  = ", Integrals(s)%Truncation_Ratio
      Write(file_id,"(a,i5,i5)") "# iPosMP, iUpperLimit  = ", ipos, iupl
      Do l1=0,param%Numerics%Multipole_Order
         Do l2=0,param%Numerics%Multipole_Order
            Do m=0,1
               Write(file_id,*) Integrals(s)%L(l1,l2,m,ipos,iupl),   "       # ", l1,l2,m
            enddo
         enddo
      enddo
      close(file_id)


      ! ------------------------------------
      ! --- M-Integrals
      ! ------------------------------------
      !
      !-- Open Files
      call SFL_Get_Free_Unit( file_id )
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_M.dat")
      Write(file_id,"(a)") "# Format:  M-Integral    # l1, l2, m"
      Write(file_id,"(a,f10.5)") "# t_r  = ", Integrals(s)%Truncation_Ratio
      Write(file_id,"(a,i5,i5)") "# iPosMP, iUpperLimit  = ", ipos, iupl
      Write(file_id,"(a,f10.5)") "# Multpole Postion     = ", Param%Numerics%MultiPole_Position_Ratio
      Do l1=0,param%Numerics%Multipole_Order
         Do l2=0,param%Numerics%Multipole_Order
            Do m=0,1
               Write(file_id,*) Integrals(s)%M(l1,l2,m,ipos,iupl),   "       # ", l1,l2,m
            enddo
         enddo
      enddo
      close(file_id)



      ! ------------------------------------
      ! --- N-Integrals
      ! ------------------------------------
      !
      !-- Open Files
      call SFL_Get_Free_Unit( file_id )
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_N.dat")
      Write(file_id,"(a)") "# Format:  N-Integral    # l1, l2, m"
      Write(file_id,"(a,f10.5)") "# t_r  = ", Integrals(s)%Truncation_Ratio
      Write(file_id,"(a,i5,i5)") "# iPosMP, iUpperLimit  = ", ipos, iupl
      Write(file_id,"(a,f10.5)") "# Multpole Postion     = ", Param%Numerics%MultiPole_Position_Ratio
      Do l1=0,param%Numerics%Multipole_Order
         Do l2=0,param%Numerics%Multipole_Order
            Do m=0,1
               Write(file_id,*) Integrals(s)%N(l1,l2,m,ipos,iupl),   "       # ", l1,l2,m
            enddo
         enddo
      enddo
      close(file_id)
    end do

  End Subroutine Write_Integrals_to_File
  !-------------------------------------!


!!$
!!$
!!$  !----------------------------------------------------------------------!
!!$  Subroutine Integral_Component_Allocation_and_Initialization( Int_Comp )
!!$  !----------------------------------------------------------------------!
!!$    !
!!$    ! --- PURPOSE 
!!$    !     Allocate the allocatable components of the 
!!$    !     GranFilm_Integral_Type
!!$    !     
!!$    !     AUTHOR : Ingve Simonsen, Paris, Jun 2010
!!$    !
!!$    Use Derived_Type_Module,  only : GranFilm_Integral_Type
!!$    Implicit None
!!$    Type(GranFilm_Integral_Type), Intent(InOut) :: Int_Comp
!!$    ! --- Local
!!$    Integer, parameter :: NPosMP = 2, NUpperLimit=2
!!$    Integer :: L1, L2, M
!!$    
!!$
!!$    ! --- Make sure that the components are deallocated before we start
!!$    call Integral_Component_Deallocateion( Int_Comp )
!!$
!!$    ! --- Some shorthands for the dimensions
!!$    L1       =  Param % Numerics % Multipole_Order
!!$    L2       =  Param % Numerics % Multipole_Order 
!!$
!!$    ! --- Allocate the components
!!$    allocate( Int_Comp%Q( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
!!$    allocate( Int_Comp%K( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
!!$    allocate( Int_Comp%L( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
!!$    allocate( Int_Comp%M( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
!!$    allocate( Int_Comp%N( 0:L1, 0:L2, 0:1, NPosMP, NUpperLimit ) )
!!$
!!$    ! --- Set the default indices...
!!$    call Set_Integral_Index_Defaults( Int_Comp )
!!$       
!!$    
!!$    Write(*,*) " ... WARNING : The size for the Q-integrals is too large....."
!!$
!!$
!!$  End Subroutine Integral_Component_Allocation_and_Initialization
!!$  !----------------------------------------------------------------!

!!$
!!$
!!$
!!$
!!$  !-----------------------------------------------------!
!!$  Subroutine Integral_Component_Deallocateion( Int_Comp )
!!$  !-----------------------------------------------------!
!!$    !
!!$    ! --- PURPOSE 
!!$    !     Deallocate the allocatable components of the 
!!$    !     GranFilm_Integral_Type
!!$    !     
!!$    !     AUTHOR : Ingve Simonsen, Paris, Jun 2010
!!$    !
!!$    Use Derived_Type_Module,  only : GranFilm_Integral_Type
!!$    Implicit None
!!$    Type(GranFilm_Integral_Type), Intent(InOut) :: Int_Comp
!!$    
!!$    if (allocated( Int_Comp%Q ))  deallocate( Int_Comp%Q )
!!$    if (allocated( Int_Comp%K ))  deallocate( Int_Comp%K )
!!$    if (allocated( Int_Comp%L ))  deallocate( Int_Comp%L )
!!$    if (allocated( Int_Comp%M ))  deallocate( Int_Comp%M )
!!$    if (allocated( Int_Comp%N ))  deallocate( Int_Comp%N )
!!$
!!$   End Subroutine Integral_Component_Deallocateion
!!$  !-----------------------------------------------------!
!!$
!!$

!!$
!!$  Function Int_Kast(x) Result(M_integrand)
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)           ::      x,M_integrand
!!$    Real(wp)           ::      mu
!!$    Real(wp)           ::      chi,gamma
!!$
!!$    !Common /index/                  l1,l2,m
!!$    !Common /mupoint/                mu
!!$
!!$    M_integrand  =  x
!!$
!!$    Return
!!$  End Function Int_Kast



End Module Integrals_Sphere_Module
!---------------------------------!
