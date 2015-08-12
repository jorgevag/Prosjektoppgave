! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module calculates the varous integrals needed
!     by the spheroid part of GRANFILM 
!     
!     This module only contains the calculations, with no routines for 
!     allocation or setting index defaults. Routines from the 
!     Sphere integrals module are used for this.
!
!     Indices:  Integrals( isurface ) % Q( l1, l2, m, iPosMP, iUpperLimit ) 
!
!     Q <--> Q
!     K <--> V
!     L <--> dV
!     M <--> W
!     N <--> dW
!
! --- AUTHOR : Eskil Aursand, Trondheim, Mar 2012.
! --- MODIFIED : Sindre Stavseng, Trondheim, Feb 2013
!
! ----------------------------------------------------------
!


!--------------------------------!
Module Integrals_Spheroid_Module
!--------------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Integrals  
  

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Integrals_Spheroid


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  
Contains




  !------------------------------------!
  Subroutine Get_Integrals_Spheroid()
  !------------------------------------!
    
    Use Error_Module,               only : Error_Warning, Error_Failure
    
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Integrals_Spheroid"
    Integer :: i


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

        
    ! --- Set the truncation ratios
    Integrals(:) % Truncation_Ratio  =  Param%Geometry%Truncation_Ratio_Vector
    ! --- Set the radius of the spherical surface relative the outer spherical surface 
    Integrals(:) % Radius_Ratio      =  Param%Geometry%Radius_Ratios

    
    ! ---------------------------------
    ! --- Get the the Integrals
    ! ---------------------------------
        

    ! -------------------------------------------------------------------------------
    ! --- Calculate the needed Integrals for the various interfaces (i.e. coatings)
    ! -------------------------------------------------------------------------------
    !
    !----Added by Sindre---------------------
    !
    ! HERE THE SPHEROID CODE BRANCHES INTO AN OBLATE AND A PROLATE PART
    !
    If (Param%Geometry%isOblate) Then
       do i= 1, size(Integrals,1)
           ! --- Calculate the integrals
           call Calculate_Integrals_Spheroid_Oblate( Integrals(i) )
       end do
    Else if (Param%Geometry%isProlate) Then 
       do i= 1, size(Integrals,1)
           ! --- Calculate the integrals
           call Calculate_Integrals_Spheroid_Prolate( Integrals(i) )
       end do
    Else
       !This should never happen, call error routine
       call Error_Failure( routine, "Spheroid type not specified internally!")
    End If
    !
    !----------------------------------------
    
    ! ----OLD LOOP----
    !do i= 1, size(Integrals,1)
    !  
    !   ! --- Calulate the integrals
    !   call Calculate_Integrals_Spheroid_Oblate( Integrals(i) )
    !
    !end do
    !
    ! -------------------

    ! --- Should we write out the integrals.....
    if (Param%InOut%Debug) &
         call Write_Integrals_to_File()

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
    
    
  End Subroutine Get_Integrals_Spheroid
  !--------------------------------------!
  



  !
  ! -----------------------------------------------
  ! ---- Private Auxilliary Integral Routines
  ! -----------------------------------------------
  !

  ! ============================================
  ! OBLATE SPHEROIDS
  ! ============================================

  !--------------------------------------------------------!
  Subroutine Calculate_Integrals_Spheroid_Oblate( Int )
  !------------------------------------------------------!
    !
    !  All of the Integrals have the array structure, e.g.
    !
    !    Integral(:,:) % K( l1, l2, m, iPosMP, iUpperLimit )
    !
    !
    Use Derived_Type_Module,               only : GranFilm_Integral_Type
    Use Error_Module,                      only : Error_Warning, Error_Failure
    Use Quadpack_Wrapper_Module,           only : Quadpack, Set_QuadPack_Error_Limits
    Use Integrand_Module_Spheroid_Oblate,  only : l1, l2, m, delta_z_over_a, xi_0, &
                      Q_Integrand, V_Integrand, W_Integrand, dV_Integrand,dW_Integrand!, & 
                      !Transformed_xi_eta , Legendre_Qlm_Aimag, Z_oblate
                            !TESTING
    ! ---------- TESTING -----------
    !Use Integrand_Module_Spheroid,  only : Test_Integrand
    !Use Legendre_Module, only : Legendre_Plm
    ! ---------- TESTING -----------
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int
    ! --- Local Parameters
    !Integer,          parameter :: MultiPole=1,     ImageMultiPole=2   ! For internal use only
    !Integer,          parameter :: UpperLimit_tr=1, UpperLimit_One=2   ! For internal use only
    Real(wp),         parameter :: ONE = 1._wp
    Character(len=*), parameter :: routine = "Calculate_Integrals_Spheroid_Oblate"

    ! --- Local Variables
    Integer              :: N_MultiPole, iPosMP 
    Real(wp)             :: tr
    Real(wp)             :: MultiPole_Position_Ratio(2)
    Real(wp)             :: a
    
    !TESTING
    !real(wp) :: Vtest, Vtest2, testeta, res, xitrans, etatrans
    !integer :: testi


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    

    
    ! --- a = sqrt(R_\parallel^2 - R_\perp^2)
    a = sqrt(Param%Geometry%Radius(2)**2 - Param%Geometry%Radius(1)**2)
    ! --- \xi_{0,s} R_{\perp,s} / a
    xi_0 =  Int%Radius_Ratio*Param%Geometry%Radius(1) / a

    If (Param%InOut%Verbose) then 
        Write(*,*) " ... Calculating Integrals for tr = ", Int%Truncation_Ratio
        write(*,*) " ... a=",a
        write(*,*) " ... xi_0_s=",xi_0
    end if
    
    ! =========================================================================
    ! --- TESTING 
    ! Is it sufficient to replace the truncation ratio in the integrals by 1.0 if a
    ! layer is entirely above the substrate?
    !

    ! --- Some abbreviations
    tr  =  Int%Truncation_Ratio
    !tr  =  min( Int%Truncation_Ratio, ONE )

    ! Layers with truncation ratio > 1 are currently not allowed.
    ! This should be studied in more detail later. 
    if (abs(Int%Truncation_Ratio) > 1.0_wp) then
      Call Error_Failure( routine, "abs(t_r^{(s)}) > 1 !")
    end if

    ! ========================================================================
    
    
    ! --- Set Accuracy
    !call Set_QuadPack_Error_Limits( Absolute_Error = 10._wp**(-wp), Relative_Error=10._wp**(-wp) )


    


    ! ---- TESTING ----------
    !Call Quadpack( Test_Integrand, -ONE, 0.1_wp, tr )
    !Write(*,*) " Integrals : ", tr 
    !stop
    ! ---- TESTING ----------



    ! --- Define the Multipole Positions. OBS: These are not
    !     normalized in the same was as in the spherical code. Here
    !     they are always normalized with the outer radius, not the
    !     radius of the current interface.
    MultiPole_Position_Ratio( Int%MultiPole      )   =  Param%Numerics%MultiPole_Position_Ratio
    MultiPole_Position_Ratio( Int%ImageMultiPole )   =  2*param%geometry%truncation_ratio &
                                                          - MultiPole_Position_Ratio( Int%MultiPole  )


    !TESTING
    !write(*,*) "TESTING"
    !l1 = 8
    !l2 = 8
    !m = 1
    !write(*,*) 'PosMP = ',MultiPole_Position_Ratio(2) 
    !write(*,*) 'tr = ',tr
    !delta_z_over_a =  param%geometry%radius(1)*MultiPole_Position_Ratio(2) / a
    !write(*,*) 'xi_0=',xi_0 
    !Vtest = dV_Integrand(-0.93_wp)
    !testeta = -ONE + epsilon(ONE)
    !open(unit=11,file='Vtest.dat',action='write')
    !do testi=1,1000
      !write(*,*) "eta = ", testeta
    !  res = W_integrand(testeta)
      !call Transformed_xi_eta(delta_z_over_a,xi_0,testeta,xitrans,etatrans)
      !res = Legendre_Plm(l1,m,testeta)
      !res = Legendre_Plm(l2,m,etatrans)
      !res = -Aimag(Legendre_Qlm_Aimag(l2,m,xitrans))
      !res = Z_oblate(l2,m,xitrans)
      !res = Legendre_Plm(l1,m,testeta)*Legendre_Plm(l2,m,etatrans)*Legendre_Qlm_Aimag(l2,m,xitrans)
      !res = Legendre_Plm(l1,m,testeta)*Legendre_Plm(l2,m,etatrans)*Z_oblate(l2,m,xitrans)
      !write(*,*) testeta, res
    !  write(11,*) testeta, res
    !  testeta = testeta + 0.002_wp
    !end do
    !close(unit=11)
    !write(*,*) '************'
    !!res = -Aimag(Legendre_Qlm_Aimag(l2,m,7.9535418724955962_wp))
    !write(*,*) res

    !call Quadpack( V_Integrand, -ONE, tr,  Vtest )
    !write(*,*) "Integral: ",Vtest
    !stop
    !!!!!!!!


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
    !Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = Int%Truncation_Ratio + 1._wp    
    !Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ) = ONE + 1._wp    


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
              
             !if (l1==2) stop
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
                delta_z_over_a =  param%geometry%radius(1)*MultiPole_Position_Ratio(iPosMP) / a 

                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                Call Quadpack( Q_Integrand, -ONE, tr,  Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                                
                !------------------------------------------------
                ! --- K-integrals (V-integrals)
                !------------------------------------------------         
                Call Quadpack( V_Integrand, -ONE, tr,  Int%K(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !------------------------------------------------
                ! --- L-integrals (dV-integrals)
                !------------------------------------------------
                Call Quadpack( dV_Integrand, -ONE, tr,  Int%L(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )

                !------------------------------------------------
                ! --- M-integrals (W-integrals)
                !------------------------------------------------
                Call Quadpack( W_Integrand, -ONE, tr,  Int%M(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )

                !------------------------------------------------
                ! --- N-integrals (dW-integrals)
                !------------------------------------------------
                Call Quadpack( dW_Integrand, -ONE, tr,  Int%N(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )

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
                delta_z_over_a =  param%geometry%radius(1)*MultiPole_Position_Ratio(iPosMP) / a 
                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                !Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) ) ! ??? Needed ???
                
                !------------------------------------------------
                ! --- K-integrals (V-integrals) 
                !------------------------------------------------         
                Call Quadpack( V_Integrand, -ONE, ONE, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- L-integrals (dV-integrals) 
                !------------------------------------------------
                Call Quadpack( dV_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- M-integrals (W-integrals) 
                !------------------------------------------------
                Call Quadpack( W_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- N-integrals (dW-integrals) 
                !------------------------------------------------
                Call Quadpack( dW_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )

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
    
    !Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = Int%Truncation_Ratio + 1._wp    
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = tr + 1._wp    ! Here tr must be used in case Int%Truncation_ratio > 1
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ) = ONE + 1._wp    




    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    

  End Subroutine Calculate_Integrals_Spheroid_Oblate
  !---------------------------------!

  ! ============================================
  ! PROLATE SPHEROIDS
  ! ============================================

  !--------------------------------------------------------!
  Subroutine Calculate_Integrals_Spheroid_Prolate( Int )
  !--------------------------------------------------------!
    !
    !  All of the Integrals have the same array structure as in the Oblate subroutine, e.g.
    !
    !    Integral(:,:) % K( l1, l2, m, iPosMP, iUpperLimit )
    !
    !
    Use Derived_Type_Module,                only : GranFilm_Integral_Type
    Use Error_Module,                       only : Error_Warning, Error_Failure
    Use Quadpack_Wrapper_Module,            only : Quadpack, Set_QuadPack_Error_Limits
    Use Integrand_Module_Spheroid_Prolate,  only : l1, l2, m, xi_0, delta_z_over_a,& 
                                                   Q_Integrand, V_Integrand, W_Integrand,& 
                                                   dV_Integrand, dW_Integrand 
    Implicit None
    Type(GranFilm_Integral_Type), Intent(InOut) :: Int
    ! --- Local Parameters
    Real(wp),         parameter :: ONE = 1._wp
    Character(len=*), parameter :: routine = "Calculate_Integrals_Spheroid_Prolate"
    ! --- Local Variables
    Real(wp)                    :: a, tr
    Integer                     :: iPosMP
    Real(wp)                    :: MultiPole_Position_Ratio(2)
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    
    ! Calculation of parameters specific to prolate spheroids --------------
    !
    ! --- a = sqrt(R_\perp^2 - R_\parallell^2)
    a = sqrt(Param%Geometry%Radius(1)**2 - Param%Geometry%Radius(2)**2)
    !
    ! --- \xi_{0,s} R_{\perp,s} / a
    xi_0 =  Int%Radius_Ratio*Param%Geometry%Radius(1) / a     ! Elongation parameter
    !
    ! --- Note that although the parameter a has the same name as its
    ! --- counterpart in the oblate spheroidal case, their definitions are 
    ! --- different. 
    ! --------------------------------------------------------------------------

    If (Param%InOut%Verbose) then 
        Write(*,*) " ... Calculating Integrals for tr = ", Int%Truncation_Ratio
        write(*,*) " ... a=",a
        write(*,*) " ... xi_0_s=",xi_0
    end if
    
    if (abs(Int%Truncation_Ratio) > 1.0_wp) then
      Call Error_Failure( routine, "abs(t_r^{(s)}) > 1 !")
    end if
    
    ! === TESTING ==========================================================================
    ! This must be checked in more detail: In the cases where an entire layer lies above the
    ! substrate, is it sufficient to replace tr by 1._wp ?
    ! --- Some abbreviations
    tr  =  Int%Truncation_Ratio
    !tr  =  min( Int%Truncation_Ratio, ONE )

    ! ======================================================================================
    
    ! --- Define the Multipole Positions. OBS: These are not
    !     normalized in the same was as in the spherical code. Here
    !     they are always normalized with the outer radius, not the
    !     radius of the current interface.
    MultiPole_Position_Ratio( Int%MultiPole      )   =  Param%Numerics%MultiPole_Position_Ratio
    MultiPole_Position_Ratio( Int%ImageMultiPole )   =  2*param%geometry%truncation_ratio &
                                                          - MultiPole_Position_Ratio( Int%MultiPole  )
    
    ! --- Initialization of integrals
    Int % Q   = 0._wp
    Int % K   = 0._wp
    Int % L   = 0._wp
    Int % M   = 0._wp
    Int % N   = 0._wp

    ! --------------------------------
    ! - Calculation loop
    !--------------------------------------------------   
    ! --- Loops over all "quantum numbers" for m=0,1
    !--------------------------------------------------
    Do l1=0,param%Numerics%Multipole_Order
       Do l2=0,param%Numerics%Multipole_Order
          Do m=0,1  

             ! ... Make sure the combination (l1,l2,m) is allowed 
             if(  ((l1==0).or.(l2==0)) .and. (m==1) ) cycle

             ! ---------------
             ! - Solving integrals with integration limits (-1,t_r).
             ! --- These integrals are needed for both the MP and image MP positions
             ! ---------------
             Do iPosMP=Int%MultiPole,Int%ImageMultiPole
                ! -----------------------------------------------
                ! --- Share the multipole position
                ! ----------------------------------------------
                delta_z_over_a =  param%geometry%radius(1)*MultiPole_Position_Ratio(iPosMP) / a 

                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                Call Quadpack( Q_Integrand, -ONE, tr,  Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                                
                !------------------------------------------------
                ! --- K-integrals (V-integrals)
                !------------------------------------------------         
                Call Quadpack( V_Integrand, -ONE, tr,  Int%K(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )
                !------------------------------------------------
                ! --- L-integrals (dV-integrals)
                !------------------------------------------------
                Call Quadpack( dV_Integrand, -ONE, tr,  Int%L(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )

                !------------------------------------------------
                ! --- M-integrals (W-integrals)
                !------------------------------------------------
                Call Quadpack( W_Integrand, -ONE, tr,  Int%M(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )

                !------------------------------------------------
                ! --- N-integrals (dW-integrals)
                !------------------------------------------------
                Call Quadpack( dW_Integrand, -ONE, tr,  Int%N(l1,l2,m,iPosMP,Int%UpperLimit_tr ) )

                !call Error_Failure(routine, "This functionality is not yet implemented!")
                !cycle
             Enddo

             ! ---------------
             ! - Solving integrals with integration limits (-1,1).
             ! --- These integrals are only needed for the MP position
             ! ---------------
             Do iPosMP=Int%Multipole, Int%Multipole       ! NOTE: This loop starts AND ends at int%multipole
                
                ! -----------------------------------------------
                ! --- Share the multipole position
                ! ----------------------------------------------
                delta_z_over_a =  param%geometry%radius(1)*MultiPole_Position_Ratio(iPosMP) / a 
                !------------------------------------------------
                ! --- Q-integrals 
                !------------------------------------------------
                !Call Quadpack( Q_Integrand, -ONE, ONE, Int%Q(l1,l2,m,iPosMP,Int%UpperLimit_One) ) ! ??? Needed ???
                
                !------------------------------------------------
                ! --- K-integrals (V-integrals) 
                !------------------------------------------------  
                !===DEBUGGING============
                !write(*,*) "V-integrals"
                !========================
                Call Quadpack( V_Integrand, -ONE, ONE, Int%K(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- L-integrals (dV-integrals) 
                !------------------------------------------------
                !===DEBUGGING============
                !write(*,*) "dV-integrals"
                !========================
                Call Quadpack( dV_Integrand, -ONE, ONE, Int%L(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- M-integrals (W-integrals) 
                !------------------------------------------------
                !===DEBUGGING============
                !write(*,*) "W-integrals"
                !========================
                Call Quadpack( W_Integrand, -ONE, ONE, Int%M(l1,l2,m,iPosMP,Int%UpperLimit_One) )

                !------------------------------------------------
                ! --- N-integrals (dW-integrals) 
                !------------------------------------------------
                !===DEBUGGING============
                !write(*,*) "dW-integrals"
                !========================
                Call Quadpack( dW_Integrand, -ONE, ONE, Int%N(l1,l2,m,iPosMP,Int%UpperLimit_One) )
             Enddo

          Enddo ! m
       Enddo    ! l2
    Enddo       ! l1

    
    ! --- Analytic results .....
    !------------------------------------
    !     Special Case for l1=0,l2=0,m=0
    !------------------------------------
    !     We judge that overhead in calculating these integrals 
    !     first numerically to be negligible
    !
    ! Calculate the needed Q-integrals with limits (-1,1) analytically here and store in Int%Q(.....).

    !Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = Int%Truncation_Ratio + 1._wp    
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_tr )  = tr + 1._wp    ! Here tr must be used, since Int%truncation_ration can be > 1
    Int%Q(0, 0, 0, Int%MultiPole, Int%UpperLimit_One ) = ONE + 1._wp    
    ! ===================================

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
    !
    ! --------------------------------

  End Subroutine Calculate_Integrals_Spheroid_Prolate


  ! ============================================
  ! AUXILLIARY ROUTINES
  ! ============================================

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
    Integer :: file_id, ienergy, l1,l2,m, ipos, iupl, s
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
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_V.dat")
      Write(file_id,"(a)") "# Format:  V-Integral    # l1, l2, m"
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
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_dV.dat")
      Write(file_id,"(a)") "# Format:  dV-Integral    # l1, l2, m"
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
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_W.dat")
      Write(file_id,"(a)") "# Format:  W-Integral    # l1, l2, m"
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
      Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_"//coating//"_Integrals_dW.dat")
      Write(file_id,"(a)") "# Format: dW-Integral    # l1, l2, m"
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




End Module Integrals_Spheroid_Module
!------------------------------------!
