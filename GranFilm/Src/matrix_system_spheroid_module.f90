! ---------------------------------------------------------------------
! $Id:$
! ---------------------------------------------------------------------

!
! ---------------------------------------------------------------------
! 
! --- PURPOSE
!
!       This module contains the routine for setting upt the relevant
!       matrix systems needed to determine the multipole coeficients
!       for the various geometries of interest.
!       
!       For SPHEROIDAL islands..
! 
! --- AUTHOR 
!
!       Eskil Aursand, Trondheim, Mar 2012.
!
! --- Modified
!
!       Ingve Simonsen, Copenhage,   May 2012.
!
! --- Modified for prolate spheroid support
!
!       Sindre Stavseng, Trondheim, Feb. 2013
!
! ---------------------------------------------------------------------
!


!-----------------------------------------!
Module Matrix_System_Spheroid_Module
!-----------------------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Integrals	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Matrix_System_Spheroid


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!



  !----------------------------------------------------!
  Subroutine Get_Matrix_System_Spheroid( A, b, m, ienergy )
  !----------------------------------------------------!
    !
    ! --- Setting up the matrix system that determines the 
    !     multipole expansion coefficients for given m and energy.
    !
    !     For SPHEROIDAL islands.
    !
    !     Eskil Aursand, Trondheim, Mar 2012.
    !
    Use Error_Module,        only : Error_Failure
    Implicit None
    Complex(wp), dimension(:,:), Intent(Out)  ::  A
    Complex(wp), dimension(:),   Intent(Out)  ::  b
    Integer,                     Intent(In)   ::  m
    Integer,                     Intent(In)   ::  ienergy
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Matrix_System_Spheroid"


    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    
    if ( Param%Numerics%Multipole_Above_Substrate ) then
       ! ------------------------
       ! --- MP ABOVE Substrate
       ! ------------------------
       !
       ! ... Left hand side
       call Matrix_System_MP_Above_Spheroid(A, m, ienergy)
       ! ... Right hand side
       call Right_Hand_Side_MP_Above_Spheroid(b, m, Ienergy )
       
    else
       
       ! ------------------------
       ! --- MP BELOW Substrate
       ! ------------------------
       call Error_Failure( routine, "Support for MP below the substrate currently not implmented" )
       

    end if

    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine

  End Subroutine Get_Matrix_System_Spheroid
  !-----------------------------------------------!




  !------------------------------------------------!
  Subroutine Matrix_System_MP_Above_Spheroid(A, m, ienergy)
  !------------------------------------------------!
    !
    ! --- The matrix system for the coated truncated spheroidal model
    !
    !     The MultiPole expansion coefficients are scaled according to
    !
    !      {\mathcal A}_{\ell m}^{(i)}  =   R{-\ell-2} A_{\ell m}^{(i)}
    !      {\mathcal B}_{\ell m}^{(i)}  =   R{-\ell-1} B_{\ell m}^{(i)}
    ! 
    !     where R \equiv R_1. Hence, when i\=1, a factor to some power of 
    !     R_s/R appears in the matrix system.
    !    
    !     Note that it is only the expansion coefficients in the odd
    !     number media i=1,3,5,7,... that is actually
    !     callculated. those in the even number media are calulcated
    !     from theose of the odd number.
    !
    !     For SPHEROIDAL islands.
    ! 
    !     Eskil Aursand, Trondheim, Mar 2012.
    !
    ! --- Modified for prolate spheroid support
    !     Sindre Stavseng, Trondheim, Feb. 2013
    !
    Use Error_Module,        only : Error_Failure
    Implicit None
    Complex(wp), dimension(:,:), Intent(Out)  ::  A
    Integer,                     Intent(In)   ::  m
    Integer,                     Intent(In)   ::  ienergy
    ! --- Local
    Character(len=*), parameter :: routine = "Matrix_System_MP_Above_Spheroid"
    Real(wp), parameter ::  Minus_One  =  - 1._wp
    !
    Integer     :: Incident_Medium, Expansion_Medium
    Integer     :: Nsurfaces, Nmedia, Nmp
    Integer     :: isurface, imedium, l1, l2
    Integer     :: pos_mp, pos_mp_image
    Integer     :: UpperLimit_tr, UpperLimit_One
    Integer     :: row_offset, col_offset
    Real(wp)    :: R_s_over_R_1, prefactor, R_power, minus_one_power, xi_0_s, xi_0_1
    Complex(wp) :: I_term, J_term, K_term, L_term
    Complex(wp),              dimension(2) :: IK_factors, JL_factors
    Complex(wp), allocatable, dimension(:) :: eps


    ! --- Some constants
    NSurfaces  =  size( Integrals, 1)
    NMedia     =  2 * Nsurfaces + 2   
    Nmp        =  param%Numerics%Multipole_Order 
    !
    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    
    ! -- Added by Sindre -----------------------------------------------------------------------------
    ! ----- Calculation of flattening/elongation parameter for oblate/prolate spheroids
    !
    if ( Param%Geometry%isOblate ) Then         ! Oblate spheroids flattening parameter
       xi_0_1 = param%geometry%radius(1)/sqrt( param%geometry%radius(2)**2 - param%geometry%radius(1)**2)
    Else if ( Param%Geometry%isProlate ) Then   ! Prolate spheroids elongation parameter
       xi_0_1 = param%geometry%radius(1)/sqrt( param%geometry%radius(1)**2 - param%geometry%radius(2)**2)
    Else
       call Error_Failure( routine, "Spheroid type not specified internally!" )
    End if
    !
    ! -------------------------------------------------------------------------------------------------


    ! --- Dielectric constants
    if ( .not.allocated(eps) ) allocate( eps(Nmedia) )
    do imedium = 1, Nmedia
       eps(imedium) = param % Media( imedium ) % epsilon(ienergy)
    enddo


    ! --- Initialization
    A           =  0._wp
    row_offset  =  0
    col_offset  =  0



    ! ------------------------------------------
    ! --- Loop over the spherical surfaces
    ! ------------------------------------------
    Spherical_Surface : do isurface = 1,Nsurfaces


       ! --- Define the radius for this spherical surface ( = R_s/R_1 = \chi_s)
       R_s_over_R_1 =  Param%Geometry%Radius_Ratios( isurface )
        
       ! --- Flattening/elongation parameter \xi for this surface:
       xi_0_s = Param%Geometry%Radius_Ratios( isurface ) * xi_0_1

       ! --- Some constants for this surface
       pos_mp         = Integrals(isurface) % MultiPole           
       pos_mp_image   = Integrals(isurface) % ImageMultiPole       

       UpperLimit_tr  = Integrals(isurface) % UpperLimit_tr   
       UpperLimit_One = Integrals(isurface) % UpperLimit_One   




       !
       ! ==================================
       ! === ABOVE the spherical surface
       ! ==================================
       !
       imedium = 2 * isurface - 1 


       ! --- Initialize the Column Offset 
       col_offset  =  Param%Numerics%MP_Storage_OffSet(imedium)
       
       ! --- Common factors
       call get_eps_factors( imedium, IK_factors, JL_factors)

              
       ! ----------------------------
       ! --- A coefficients
       ! ----------------------------

       If (imedium /= Expansion_Medium ) then

          Do l2 = 1, Nmp

             ! --- Powers of the scaled radius and minus one (needs only to be updated for changing l2)
             R_power         = R_s_over_R_1**(-l2-2)
             minus_one_power = (-1._wp)**(l2+m)

             Do l1 = 1, Nmp


                ! --- Prefactor 
                prefactor =  param%Numerics%zeta(l1,l2,m) * R_power
            

                !
                ! --- Potential
                ! -------------------------------------
                !
                ! --- I-integral term
                I_term = xi_0_s**(l2+1)*IK_factors(1) * (  &
                              - Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%K(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + IK_factors(2)*Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_One) &
                              ) 


                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  = prefactor * I_term




                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- J-integral term
                J_term = xi_0_s**(l2+2)*JL_factors(1) * ( &
                              Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%L(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + JL_factors(2)*Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_One) &
                              )

                ! --- Matrix element
                A( l1 + Nmp + row_offset, l2 + col_offset )  = prefactor * J_term


             EndDo
          EndDo

          ! --- Column Offset Update 
          col_offset = col_offset + Nmp

       EndIf  ! imedium /= Expansion_Medium



       ! ----------------------------
       ! --- B coefficients
       ! ----------------------------
       if (imedium /= Incident_Medium ) then

          Do l2 = 1, Nmp

             ! --- Powers of the scaled radius and minus one (needs only to be updated for changing l2)
             R_power         = R_s_over_R_1**(l2-1)
             minus_one_power = (-1._wp)**(l2+m)

             Do l1 = 1, Nmp


                ! --- Prefactor 
                prefactor =  param%Numerics%zeta(l1,l2,m) * R_power

            
                !
                ! --- Potential
                ! -------------------------------------
                !
                ! --- K-integral term
                K_term = xi_0_s**(-l2) * IK_factors(1) * ( &
                              -Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%M(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + IK_factors(2) * Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_One) &
                              )

                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  = prefactor * K_term



                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- L-integral term
                L_term = xi_0_s**(-l2+1) * JL_factors(1) * ( &
                              Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%N(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + JL_factors(2)*Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_One) &
                              )
                ! --- Matrix element
                A( l1 + Nmp + row_offset, l2 + col_offset )  =  prefactor * L_term


             EndDo
          EndDo


          ! --- Column Offset Update 
          col_offset = col_offset + Nmp


       EndIf  !  imedium /= Incident_Medium




       !
       ! ==================================
       ! === BELOW the spherical surface
       ! ==================================
       !
       imedium = 2 * isurface + 1 
       
       ! --- Common factors
       call get_eps_factors( imedium, IK_factors, JL_factors)


       ! ----------------------------
       ! --- A coefficients
       ! ----------------------------

       If (imedium /= Expansion_Medium ) then

          Do l2 = 1, Nmp

             ! --- Powers of the scaled radius and minus one (needs only to be updated for changing l2)
             R_power         = R_s_over_R_1**(-l2-2)
             minus_one_power = (-1._wp)**(l2+m)

             Do l1 = 1, Nmp


                ! --- Prefactor 
                prefactor =  param%Numerics%zeta(l1,l2,m) * R_power



                !
                ! --- Potential
                ! -------------------------------------
                !
                ! --- I-integral term
                I_term = xi_0_s**(l2+1)*IK_factors(1) * (  &
                              - Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%K(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + IK_factors(2)*Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_One) &
                              ) 
               
                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  = Minus_One * prefactor * I_term



                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- J-integral term
                J_term = xi_0_s**(l2+2)*JL_factors(1) * ( &
                              Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%L(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + JL_factors(2)*Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_One) &
                              )
               
                ! --- Matrix element
                A( l1 + Nmp + row_offset, l2 + col_offset )  = Minus_One * prefactor * J_term


             Enddo
          Enddo


          ! --- Column Offset Update 
          col_offset = col_offset + Nmp

       EndIf   ! imedium /= Expansion_Medium




       ! ----------------------------
       ! --- B coefficients
       ! ----------------------------
       if (imedium /= Incident_Medium ) then

          Do l2 = 1, Nmp

             ! --- Powers of the scaled radius and minus one (needs only to be updated for changing l2)
             R_power         = R_s_over_R_1**(l2-1)
             minus_one_power = (-1._wp)**(l2+m)

             Do l1 = 1, Nmp


                ! --- Prefactor 
                prefactor =  param%Numerics%zeta(l1,l2,m) * R_power


                !
                ! --- Potential
                ! -------------------------------------
                !
                ! --- K-integral term
                K_term = xi_0_s**(-l2) * IK_factors(1) * ( &
                              -Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%M(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + IK_factors(2) * Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_One) &
                              )

                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  =  Minus_One * prefactor * K_term



                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- L-integral term
                L_term = xi_0_s**(-l2+1) * JL_factors(1) * ( &
                              Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_tr) &
                              + minus_one_power*Integrals(isurface)%N(l1,l2,m,pos_mp_image,UpperLimit_tr) &
                              + JL_factors(2)*Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_One) &
                              )

                ! --- Matrix element
                A( l1 + Nmp + row_offset, l2 + col_offset )  =  Minus_One * prefactor * L_term


             EndDo
          EndDo


          ! --- Column Offset Update 
          col_offset = col_offset + Nmp

       EndIf  !  imedium /= Incident_Medium



       ! --- Row Offset Update      (add 2*Nmp since we always have two sets of BCs)
       row_offset = row_offset + 2 * Nmp
       

    End Do Spherical_Surface





    ! --- Deallocate storage
    if ( allocated(eps) ) deallocate( eps )
   

  !--------!
  Contains
  !--------!

    !------------------------------------------------------------!
    Subroutine get_eps_factors( imedium, IK_factors, JL_factors)
    !------------------------------------------------------------!

      Implicit None
      Integer :: imedium
      Complex(wp), dimension(2) :: IK_factors
      Complex(wp), dimension(2) :: JL_factors
      ! --- Locals
      
      IK_factors(1)  =  (     eps(imedium) - eps(imedium+1) ) / ( eps(imedium) + eps(imedium+1) ) 
      IK_factors(2)  =  ( 2 * eps(imedium)                  ) / ( eps(imedium) - eps(imedium+1) ) 
      JL_factors(1)  =  eps(imedium)   * IK_factors(1)
      JL_factors(2)  =  ( 2 * eps(imedium+1)                  ) / ( eps(imedium) - eps(imedium+1) ) 

     
    End Subroutine get_eps_factors
    !------------------------------------------------------------!

  End Subroutine Matrix_System_MP_Above_Spheroid
  !------------------------------------------------!





  !-----------------------------------------------------!
  Subroutine Right_Hand_Side_MP_Above_Spheroid(b,m,ienergy )
  !-----------------------------------------------------!
    !
    ! This routine sets up the right-hand-side of the equation
    ! system determining the multipole expansion coefficients.
    !
    ! See the GranFilm Note for documentation!
    ! 
    ! It is assumed that size(b)>= 2*param%Numerics%Multipole_Order, 
    ! but this is not checked due to keep the routine fast....
    !
    ! Eskil Aursand, Trondheim,  Mar 2012.
    !
    ! --- Modified for prolate spheroid support
    !     Sindre Stavseng, Feb. 2013
    !
    Use Shared, only       : pi, imu
    Use Tools_Module, only : deg2rad
    Use Error_Module, only : Error_Failure
    Implicit None
    Complex(wp), Intent(Out) ::  b(:)
    Integer,     Intent(in)  ::  m           ! The m "quantum number"
    Integer,     Intent(in)  ::  ienergy     ! 
    ! --- Local
    Character(len=*), parameter :: routine = "Right_Hand_Side_MP_Above_Spheroid"
    Integer,          parameter :: isurface = 1
    Integer        ::  l,Nmp, iMultipole, iUpperlimit_tr
    Real(wp)       ::  tr,sin0,cos0,kronl
    Real(wp)       ::  factor, sq_root_2, sq_root_3, xi_0_1_inv
    complex(wp )   ::  e1, e2, ratio, exp0 




   
    ! --- Some initial abrevations
    ! ---------------------------------------------------
    !
    tr   = param%Geometry%Truncation_Ratio_Vector(isurface)
    sin0 = sin(param%Source%ThetaE * deg2rad() )
    cos0 = Cos(param%Source%ThetaE * deg2rad() )
    exp0 = Exp(-imu*param%Source%PhiE * deg2rad() )
    Nmp  = param%Numerics%Multipole_Order 

    ! --- The dieletctric constants
    e1  = param % Media(1) % epsilon(ienergy)  ! Ambient 
    e2  = param % Media(2) % epsilon(ienergy)  ! Substrate

    !
    iMultipole = Integrals(1)%Multipole
    iUpperlimit_tr = Integrals(1)%Upperlimit_tr
    
    ! --- Some Common factors
    factor    = sqrt(2*pi/3._wp)
    sq_root_2 = sqrt(2._wp)
    sq_root_3 = sqrt(3._wp)
    ratio     = e1 / e2

    ! --- Initialize the Right-Hand-Side vector
    b = 0._wp

    ! ----------------------------------- Added by Sindre -------------------------------------------
    !
    ! ----- Calculation of b-vector for oblate/prolate spheroids
    !
    ! ===== OBLATE SPHEROIDS ==================================================
    if ( Param%Geometry%isOblate ) Then         ! Oblate spheroids
       ! --
       ! Old code by Eskil below. Should perhaps be rewritten?
       ! --
       
       ! Inverse flattening parameter
       xi_0_1_inv =  sqrt( param%geometry%radius(2)**2 - param%geometry%radius(1)**2) / (param%geometry%radius(1))

       ! ------------------------------------------    
       ! --- Loop over the l "quantum number" -----
       ! ------------------------------------------
       Do l=1,Nmp     
       
          ! ---  Calculated the kronecker delta
          If(l==1) Then
             kronl=1._wp
          Else
             kronl=0._wp
          Endif
       
          Select Case(m)
          Case(0)
             ! -- m=0
             ! ------------
             !
             ! --- Potentials
             b(l) = sq_root_2 * factor * cos0 *                                              &
                  ( ratio * kronl                                                            &
                  + (ratio-1._wp) *                                                        &
                  (sq_root_3*tr*param%Numerics%zeta(l,0,0) * Integrals(1)%Q(l,0,0,iMultipole,iUpperlimit_tr)   &
                  - param%Numerics%zeta(l,1,0) * Integrals(1)%Q(l,1,0,iMultipole,iUpperlimit_tr) ) &
                  )   
             ! --- Normal derivative of the Potential (times the dielectric function)
             b(Nmp+l) = sq_root_2 * factor * e1 * cos0 * kronl   

          Case(1)
             ! -- m=1
             ! ------------

             ! --- Potentials
             b(l)     = - sqrt(xi_0_1_inv**2 + 1.0_wp)   * factor * sin0 * exp0 *  kronl       
             ! --- Normal derivative of the Potential (times the dielectric function)
             b(Nmp+l) = - factor * sin0 * exp0  * &
                  ( e2*kronl + (e1-e2) * param%Numerics%zeta(l,1,1) * Integrals(1)%Q(l,1,1,iMultipole,iUpperlimit_tr) ) &
                  /sqrt(xi_0_1_inv**2 + 1.0_wp)


          Case Default
             !
             ! --- Should never happen......
             call Error_Failure( routine, "m/=0 and m/=1. This should in principle never happen...." )

          
          End Select
       Enddo
       ! --
       ! --
    ! ===== PROLATE SPHEROIDS ==================================================
    Else if ( Param%Geometry%isProlate ) Then   ! Prolate spheroids
       !call Error_Failure( routine, "Prolate spheroid support not yet implemented")
       ! --
       ! Inverse elongation parameter
       xi_0_1_inv =  sqrt( param%geometry%radius(1)**2 - param%geometry%radius(2)**2) / (param%geometry%radius(1))
       
       ! ------------------------------------------    
       ! --- Loop over the l "quantum number" -----
       ! ------------------------------------------
       Do l=1,Nmp     
       
          Select case( m )

          Case( 0 )
             If (l==1) Then     ! Special case for l == 1
                ! First BC
                b(1) = sq_root_2 * factor * cos0 * ( ratio + (ratio - 1._wp) * &
                     (sq_root_3*tr* param%Numerics%zeta(l,0,0) * Integrals(1)%Q(l,0,0,iMultipole,iUpperlimit_tr) &
                     - param%Numerics%zeta(l,1,0) * Integrals(1)%Q(l,1,0,iMultipole,iUpperlimit_tr) ) ) 
                ! Second BC
                b(Nmp+1) = e1 * sq_root_2 * factor * cos0

             Else               ! All other l's
                 ! First BC
                b(l) = sq_root_2 * factor * cos0 * (ratio - 1._wp) * &
                     (sq_root_3*tr* param%Numerics%zeta(l,0,0) * Integrals(1)%Q(l,0,0,iMultipole,iUpperlimit_tr) &
                     - param%Numerics%zeta(l,1,0) * Integrals(1)%Q(l,1,0,iMultipole,iUpperlimit_tr) )
                ! Second BC
                b(Nmp+l) = 0._wp
             End if

          Case( 1 )
             If ( l==1 ) Then   ! Special case for l == 1
                ! First BC
                b(1) = -factor * sin0 * exp0 * sqrt( 1 - xi_0_1_inv**2 )
                ! Second BC
                b(Nmp+1) = -factor * sin0 * exp0 * ( (e1 - e2) * Param%Numerics%zeta(l,1,1) * &
                           Integrals(1)%Q(l,1,1,iMultipole, iUpperlimit_tr) + e2 ) / sqrt( 1 - xi_0_1_inv**2 )

             Else               ! All other l's
                ! First BC
                b(l) = 0._wp
                ! Second BC
                b(Nmp+l) = -factor * sin0 * exp0 * (e1 - e2) * Param%Numerics%zeta(l,1,1) * &
                           Integrals(1)%Q(l,1,1,iMultipole, iUpperlimit_tr) / sqrt( 1 - xi_0_1_inv**2 )
             End if

          Case default
             call Error_Failure( routine, "m /= 0 and m /= 1. This should never happen.")

          End select
       Enddo
       ! --
    ! ==========================================================================
    Else
       call Error_Failure( routine, "Spheroid type not specified internally!" )
    End if
    !
    ! -------------------------------------------------------------------------------------------------

    
  End Subroutine Right_Hand_Side_MP_Above_Spheroid
  !----------------------------------------------!



End Module Matrix_System_Spheroid_Module
!----------------------------------------!
