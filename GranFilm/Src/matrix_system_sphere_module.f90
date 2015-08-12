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
! 
! --- AUTHOR 
!
!       Ingve Simosnen, Paris, Jul 2010.
!
! ---------------------------------------------------------------------
!


!---------------------------------!
Module Matrix_System_Sphere_Module
!---------------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Integrals	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Matrix_System_Sphere 
            
  !Public :: Get_Matrix_System_Sphere,  &
  !          Get_Matrix_System


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!


  !------------------------------------------------------!
  Subroutine Get_Matrix_System_Sphere( A, b, m, ienergy )
  !------------------------------------------------------!
    !
    ! --- Setting up the matrix system that determines the 
    !     multipole expansion coefficients for given m and energy
    !
    !     Ingve Simonsen, Paris, Jul 2010.
    !
    Use Error_Module,        only : Error_Failure
    Implicit None
    Complex(wp), dimension(:,:), Intent(Out)  ::  A
    Complex(wp), dimension(:),   Intent(Out)  ::  b
    Integer,                     Intent(In)   ::  m
    Integer,                     Intent(In)   ::  ienergy
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Matrix_System_Sphere"


    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    
    if ( Param%Numerics%Multipole_Above_Substrate ) then
       ! ------------------------
       ! --- MP ABOVE Substrate
       ! ------------------------
       !
       ! ... Left hand side
       call Matrix_System_MP_Above(A, m, ienergy)
       ! ... Right hand side
       call Right_Hand_Side_MP_Above(b, m, Ienergy )
       
    else
       
       ! ------------------------
       ! --- MP BELOW Substrate
       ! ------------------------
       call Error_Failure( routine, "Support for MP below the substrate currently not implmented" )
       

    end if

    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine

  End Subroutine Get_Matrix_System_Sphere
  !-----------------------------------------------!




  !------------------------------------------------!
  Subroutine Matrix_System_MP_Above(A, m, ienergy)
  !------------------------------------------------!
    !
    ! --- The matrix system for the coated truncated sperical model
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
    ! 
    !     Ingve Simonsen, Paris, Jan 2012.
    !
    Use Error_Module,        only : Error_Failure
    Implicit None
    Complex(wp), dimension(:,:), Intent(Out)  ::  A
    Integer,                     Intent(In)   ::  m
    Integer,                     Intent(In)   ::  ienergy
    ! --- Local
    Real(wp), parameter ::  Minus_One  =  - 1._wp
    !
    Integer     :: Incident_Medium, Expansion_Medium
    Integer     :: Nsurfaces, Nmedia, Nmp
    Integer     :: isurface, imedium, l1, l2
    Integer     :: pos_mp, pos_mp_image
    Integer     :: UpperLimit_tr, UpperLimit_One
    Integer     :: row_offset, col_offset
    Real(wp)    :: R_s_over_R_1, prefactor, R_power, minus_one_power
    Complex(wp) :: I_term, J_term
    Complex(wp),              dimension(2) :: I_factor, J_factor
    Complex(wp), allocatable, dimension(:) :: eps


    ! --- Some constants
    NSurfaces  =  size( Integrals, 1)
    NMedia     =  2 * Nsurfaces + 2   
    Nmp        =  param%Numerics%Multipole_Order 
    !
    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium


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
       call get_I_and_J_factors( imedium, I_factor, J_factor)

              
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
                I_term =  Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_tr)                                            &
                          + minus_one_power * I_factor(1) * Integrals(isurface)%K(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + I_factor(2) * (   Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_One)                       &
                                            - Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_tr) )   

                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  = prefactor * I_term




                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- J-integral term
                J_term =  eps(imedium) * Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_tr)                             &
                          + minus_one_power * J_factor(1) * Integrals(isurface)%L(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + J_factor(2) * (   Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_One)                       &
                                            - Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_tr) )   

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
                ! --- I-integral term
                I_term =  Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_tr)                                            &
                          + minus_one_power * I_factor(1) * Integrals(isurface)%M(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + I_factor(2) * (   Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_One)                       &
                                            - Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_tr) )   

                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  = prefactor * I_term



                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- J-integral term
                J_term =  eps(imedium) * Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_tr)                             &
                          + minus_one_power * J_factor(1) * Integrals(isurface)%N(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + J_factor(2) * (   Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_One)                       &
                                            - Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_tr) )   

                ! --- Matrix element
                A( l1 + Nmp + row_offset, l2 + col_offset )  =  prefactor * J_term


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
       call get_I_and_J_factors( imedium, I_factor, J_factor )


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
                I_term =  Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_tr)                                            &
                          + minus_one_power * I_factor(1) * Integrals(isurface)%K(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + I_factor(2) * (   Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_One)                       &
                          - Integrals(isurface)%K(l1,l2,m,pos_mp,UpperLimit_tr) )   
                
                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  = Minus_One * prefactor * I_term



                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- J-integral term
                J_term =  eps(imedium) * Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_tr)                             &
                          + minus_one_power * J_factor(1) * Integrals(isurface)%L(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + J_factor(2) * (   Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_One)                       &
                          - Integrals(isurface)%L(l1,l2,m,pos_mp,UpperLimit_tr) )   
                
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
                ! --- I-integral term
                I_term =  Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_tr)                                            &
                          + minus_one_power * I_factor(1) * Integrals(isurface)%M(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + I_factor(2) * (   Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_One)                       &
                                            - Integrals(isurface)%M(l1,l2,m,pos_mp,UpperLimit_tr) )   

                ! --- Matrix element
                A( l1 + row_offset, l2 + col_offset )  =  Minus_One * prefactor * I_term



                !
                ! --- Normal derivative of the Potential (times epsilon)
                ! ----------------------------------------------------------
                !
                ! --- J-integral term
                J_term =  eps(imedium) * Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_tr)                             &
                          + minus_one_power * J_factor(1) * Integrals(isurface)%N(l1,l2,m,pos_mp_image,UpperLimit_tr)    &
                          + J_factor(2) * (   Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_One)                       &
                                            - Integrals(isurface)%N(l1,l2,m,pos_mp,UpperLimit_tr) )   

                ! --- Matrix element
                A( l1 + Nmp + row_offset, l2 + col_offset )  =  Minus_One * prefactor * J_term


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
    Subroutine get_I_and_J_factors( imedium, I_factor, J_factor )
    !------------------------------------------------------------!
      ! 
      ! --- Calulates the common factors containing dielectric functions
      !     and used in the {\mathcal I} and {\mathcal J} integrals.
      !
      !     Eps is shared with the main routine......
      !
      Implicit None
      Integer :: imedium
      Complex(wp), dimension(:) :: I_factor
      Complex(wp), dimension(:) :: J_factor
      ! --- Locals
      
      I_factor(1)  =  (     eps(imedium) - eps(imedium+1) ) / ( eps(imedium) + eps(imedium+1) ) 
      I_factor(2)  =  ( 2 * eps(imedium)                  ) / ( eps(imedium) + eps(imedium+1) ) 
      J_factor(1)  =  eps(imedium)   * I_factor(1)
      J_factor(2)  =  eps(imedium+1) * I_factor(2)
     
    End Subroutine get_I_and_J_factors
    !------------------------------------------------------------!

  End Subroutine Matrix_System_MP_Above
  !------------------------------------------------!





  !-----------------------------------------------------!
  Subroutine Right_Hand_Side_MP_Above(b,m,ienergy )
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
    ! Ingve Simonsen, Paris Jan 2012.
    !
    Use Shared, only       : pi, imu
    Use Tools_Module, only : deg2rad
    Use Error_Module, only : Error_Failure
    Implicit None
    Complex(wp), Intent(Out) ::  b(:)
    Integer,     Intent(in)  ::  m           ! The m "quantum number"
    Integer,     Intent(in)  ::  ienergy     ! 
    ! --- Local
    Character(len=*), parameter :: routine = "Right_Hand_Side_MP_Above"
    Integer,          parameter :: isurface = 1
    Integer        ::  l,Nmp   !, pos_mp, pos_imp 
    Real(wp)       ::  tr,sin0,cos0,kronl
    Real(wp)       ::  factor, sq_root_2, sq_root_3
    complex(wp )   ::  e1, e2, ratio, exp0 




   
    ! --- Some initial abrevations
    ! ---------------------------------------------------
    !
    tr   = param%Geometry%Truncation_Ratio_Vector(isurface)
    sin0 = Sin( param%Source%ThetaE * deg2rad() )
    cos0 = Cos( param%Source%ThetaE * deg2rad() )
    exp0 = Exp( -imu*param%Source%PhiE * deg2rad() )
    Nmp  = param%Numerics%Multipole_Order 

    ! --- The dieletctric constants
    e1  = param % Media(1) % epsilon(ienergy)  ! Ambient 
    e2  = param % Media(2) % epsilon(ienergy)  ! Substrate


    ! --- Multipole/ Image-Multipole positions
    !pos_mp  = Integrals % MultiPole  
    !pos_imp = Integrals % ImageMultiPole  


    ! --- Some Common factors
    factor    = sqrt(2*pi/3._wp)
    sq_root_2 = sqrt(2._wp)
    sq_root_3 = sqrt(3._wp)
    ratio     = e1 / e2



    ! --- Initialize the Right-Hand-Side vector
    b = 0._wp


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
                   (sq_root_3*tr*param%Numerics%zeta(l,0,0) * Integrals(1)%Q(l,0,0,1,1)   &
                               - param%Numerics%zeta(l,1,0) * Integrals(1)%Q(l,1,0,1,1) ) &
               )   
          ! --- Normal derivative of the Potential (times the dielectric function)
          b(Nmp+l) = sq_root_2 * factor * e1 * cos0 * kronl   

       Case(1)
          ! -- m=1
          ! ------------

          ! --- Potentials
          b(l)     = - factor * sin0 * exp0 *  kronl       
          ! --- Normal derivative of the Potential (times the dielectric function)
          b(Nmp+l) = - factor * sin0 * exp0 *  &
               ( e2*kronl + (e1-e2) * param%Numerics%zeta(l,1,1) * Integrals(1)%Q(l,1,1,1,1) )


       Case Default
          !
          ! --- Should never happen......
          call Error_Failure( routine, "This should in principle never happen...." )

          
       End Select
    Enddo

    
  End Subroutine Right_Hand_Side_MP_Above
  !----------------------------------------------!














!=======================================================================
!=======================================================================

!!$
!!$
!!$
!!$  !-----------------------------------------------!
!!$  Subroutine Get_Matrix_System( A, b, m, ienergy )
!!$  !-----------------------------------------------!
!!$    !
!!$    ! --- Setting up the matrix system that determines the 
!!$    !     multipole expansion coefficients for given m and energy
!!$    !
!!$    !     Ingve Simonsen, Paris, Jul 2010.
!!$    !
!!$    Use Error_Module,        only : Error_Failure
!!$    Implicit None
!!$    Complex(wp), dimension(:,:), Intent(Out)  ::  A
!!$    Complex(wp), dimension(:),   Intent(Out)  ::  b
!!$    Integer,                     Intent(In)   ::  m
!!$    Integer,                     Intent(In)   ::  ienergy
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Get_Matrix_System"
!!$
!!$
!!$    ! --- If verbose
!!$    !If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
!!$
!!$    
!!$    if ( Param%Numerics%Multipole_Above_Substrate ) then
!!$       ! ------------------------
!!$       ! --- MP ABOVE Substrate
!!$       ! ------------------------
!!$       !
!!$       ! ... Left hand side
!!$       call Matrix_system_above(m,A,Ienergy)
!!$       ! ... Right hand side
!!$       call Right_Dipole_above(m,b,Ienergy )
!!$       
!!$    else
!!$       
!!$       ! ------------------------
!!$       ! --- MP BELOW Substrate
!!$       ! ------------------------
!!$       call Error_Failure( routine, "Support for MP below the substrate currently not implmented" )
!!$       
!!$
!!$    end if
!!$
!!$    ! --- If verbose
!!$    !If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine
!!$
!!$  End Subroutine Get_Matrix_System
!!$  !-----------------------------------------------!
!!$
!!$




  ! ================================================
  ! === TESTING 
  ! ================================================






  !--------------------------------------------------------------!
  Subroutine Matrix_system_above( m, A, Ienergy )
  !--------------------------------------------------------------!
    ! NOTICE : 
    ! This routine sets up the reduced dimensionless linear system 
    ! for the determination of the multipole coeficients, when 
    ! the multipoles are located ABOVE the substrate. 
    Implicit None
    integer               :: m      
    !Type(integrals_type)  :: Integ(0:,0:,0:), Integ_tr1(0:,0:,0:)
    integer               :: Ienergy
    complex(wp)           :: A(:,:)
    integer               :: mpo,l1,l2
    !Real(wp)              :: zeta(0:,0:,0:)
    complex(wp)           :: e1,e2,e3,e4,eps(4)

    ! Some initial abrevations
    mpo  = param%Numerics%Multipole_Order 

    ! The dieletctric constants
    e1  = param % Media(1) % epsilon(ienergy)
    e2  = param % Media(2) % epsilon(ienergy)
    e3  = param % Media(3) % epsilon(ienergy)
    e4  = param % Media(4) % epsilon(ienergy)
    !e1  = param%Materials%Epsilon%Ambient(ienergy) 
    !e2  = param%Materials%Epsilon%Substrate(ienergy)
    !e3  = param%Materials%Epsilon%Island(ienergy)
    !e4  = e2

    !
    ! Note that the integral structure is :
    !
    !           Integrals(Ninterface,tr) % K( l1, l2,m,imultipole/image_multipole)
    !

    ! Build the matrix system
    A = 0._wp
    Do l1 = 1, mpo
       Do l2 = 1, mpo

          eps  = (/  (e1-e2),2*e1,2*e1*e2,e1*(e1-e2) /) /(e1+e2) 
          ! --- C
          A(l1,l2)     =    Integrals(1)%K(l1,l2,m,1,1)                               &
               + (-1._wp)**(l2+m)*eps(1)*Integrals(1)%K(l1,l2,m,2,1)                  & 
               + eps(2)*( Integrals(1)%K(l1,l2,m,1,2)-Integrals(1)%K(l1,l2,m,1,1) )   
          A(l1,l2) =  param%Numerics%zeta(l1,l2,m) * A(l1,l2) 
!
!!$          A(l1,l2)     =    Integ(l1,l2,m)%K%mu(1)                             &
!!$               + (-1._wp)**(l2+m)*eps(1)*Integ(l1,l2,m)%K%mu(2)                & 
!!$               + eps(2)*( Integ_tr1(l1,l2,m)%K%mu(1)-Integ(l1,l2,m)%K%mu(1) )  
!!$          A(l1,l2) =  zeta(l1,l2,m) * A(l1,l2) 
!!$


          ! --- F
          A(mpo+l1,l2) = eps(3)*Integrals(1)%L(l1,l2,m,1,2)                         &
               + eps(4)*(  Integrals(1)%L(l1,l2,m,1,1)                              &
               +(-1._wp)**(l2+m)*Integrals(1)%L(l1,l2,m,2,1) )
          A(mpo+l1,l2) = param%Numerics%zeta(l1,l2,m) * A(mpo+l1,l2)
!
!!$          A(mpo+l1,l2) = eps(3)*Integ_tr1(l1,l2,m)%L%mu(1)                     &
!!$               + eps(4)*(  Integ(l1,l2,m)%L%mu(1)                              &
!!$               +(-1._wp)**(l2+m)*Integ(l1,l2,m)%L%mu(2) )
!!$          A(mpo+l1,l2) = zeta(l1,l2,m) * A(mpo+l1,l2)
!!$


          eps = (/ (e3-e4),2*e3,2*e3*e4,e3*(e3-e4) /) /(e3+e4)
          ! --- D
          A(l1,mpo+l2) =  Integrals(1)%M(l1,l2,m,1,1)                               &  
               +(-1._wp)**(l2+m)*eps(1)*Integrals(1)%M(l1,l2,m,2,1)                 & 
               +eps(2)*( Integrals(1)%M(l1,l2,m,1,2)-Integrals(1)%M(l1,l2,m,1,1) )
          A(l1,mpo+l2) =  - param%Numerics%zeta(l1,l2,m) * A(l1,mpo+l2)

!
!!$          A(l1,mpo+l2) =  Integ(l1,l2,m)%M%mu(1)                               &  
!!$               +(-1._wp)**(l2+m)*eps(1)*Integ(l1,l2,m)%M%mu(2)                 & 
!!$               +eps(2)*( Integ_tr1(l1,l2,m)%M%mu(1)-Integ(l1,l2,m)%M%mu(1) )
!!$          A(l1,mpo+l2) =  - zeta(l1,l2,m) * A(l1,mpo+l2)
!!$

          ! --- G
          A(mpo+l1,mpo+l2) = eps(3)*Integrals(1)%N(l1,l2,m,1,2)                     &
               + eps(4)*(  Integrals(1)%N(l1,l2,m,1,1)                              &
               +(-1._wp)**(l2+m)*Integrals(1)%N(l1,l2,m,2,1) )
          A(mpo+l1,mpo+l2) = - param%Numerics%zeta(l1,l2,m) * A(mpo+l1,mpo+l2)
!
!!$          A(mpo+l1,mpo+l2) = eps(3)*Integ_tr1(l1,l2,m)%N%mu(1)                  &
!!$               + eps(4)*(  Integ(l1,l2,m)%N%mu(1)                               &
!!$               +(-1._wp)**(l2+m)*Integ(l1,l2,m)%N%mu(2) )
!!$          A(mpo+l1,mpo+l2) = - zeta(l1,l2,m) * A(mpo+l1,mpo+l2)

       Enddo   ! End loop over l2
    Enddo      ! End loop over l1

  End Subroutine Matrix_system_above
  !---------------------------------!




  !---------------------------------------------------!
  Subroutine Right_Dipole_above(m,b,Ienergy )
  !---------------------------------------------------!
    Use Shared, only       : pi, imu
    Use Tools_Module, only : deg2rad
    Implicit None
    integer                   ::      m,ienergy
    complex(wp )              ::      b(:)
    integer                   ::      l1,mpo
    Real(wp)                  ::      tr,sin0,cos0,kronl
    complex(wp )              ::      e1,e2,exp0
    !Real(wp)                  ::      deg2rad



    ! Some initial abrevations
    tr = param%Geometry%Truncation_Ratio_Vector(1)
    !    tr   = param%Island%Truncation_Ratio
    sin0 = sin(param%Source%ThetaE * deg2rad() )
    !    sin0 = Sin(param%Source%Derived%ThetaE_calc)
    cos0 = Cos(param%Source%ThetaE * deg2rad() )
    !    cos0 = Cos(param%Source%Derived%ThetaE_calc)
    exp0 = Exp(-imu*param%Source%PhiE * deg2rad() )
    !    exp0 = Exp(-imu*param%Source%Derived%PhiE_calc)
    mpo  = param%Numerics%Multipole_Order 

    ! The dieletctric constants
    e1  = param % Media(1) % epsilon(ienergy)  ! Ambient 
    e2  = param % Media(2) % epsilon(ienergy)  ! Substrate
    !    e1  = param%Materials%Epsilon%Ambient(ienergy) 
    !    e2  = param%Materials%Epsilon%Substrate(ienergy)



    !Write(*,*) "---------------------------"
    !Write(*,*) 

    Do l1=1,mpo      
      ! Calculated the kronecker delta
       If(l1==1) Then
          kronl=1._wp
       Else
          kronl=0._wp
       Endif
       
       Select Case(m)
       Case(0)
          !--- H
          b(l1) = Sqrt(4._wp*pi/3._wp)*cos0* &
               ( e1/e2*kronl + (e1-e2)/e2*   &
               (Sqrt(3._wp)*tr*param%Numerics%zeta(l1,0,0)*Integrals(1)%Q(l1,0,0,1,1) &
               - param%Numerics%zeta(l1,1,0)*Integrals(1)%Q(l1,1,0,1,1) ) )   
          !--- J 
          b(mpo+l1) = Sqrt(4._wp*pi/3._wp)*e1*cos0 * kronl   

       Case(1)
          !--- H 
          b(l1)     = -Sqrt(2._wp*pi/3._wp)*sin0*exp0 *  kronl       
          !--- J
          b(mpo+l1) = -Sqrt(2._wp*pi/3._wp)*sin0*exp0* &
               ( e2*kronl+(e1-e2)*param%Numerics%zeta(l1,1,1)*Integrals(1)%Q(l1,1,1,1,1) )

          !----TESTING ----

          !Write(*,*) " b  : ", abs( b(l1)+b(mpo+l1) )
          !Write(*,*) l1, param%Source%ThetaE * deg2rad(), cos0, sin0, -Sqrt(2._wp*pi/3._wp)*sin0*exp0, &
          !              e2*kronl,(e1-e2)*param%Numerics%zeta(l1,1,1)
          !
          !Write(*,*) Integrals(1,1)%Q(l1,1,1,1,1)

          ! --- TESTING ----


       End Select
    Enddo




    !Write(*,*) "---------------------------"
    !if (m==1) stop
    
  End Subroutine Right_Dipole_above
  !--------------------------------!

!=======================================================================
!=======================================================================
!=======================================================================
!=======================================================================





!!$
!!$  !---------------------------------------------------------!
!!$  Subroutine Matrix_system(m,A,Ienergy,Integ,Integ_tr1,zeta)
!!$  !---------------------------------------------------------!
!!$
!!$    ! NOTICE : Generic routine for an uniform or quadrupolar field
!!$    ! This routine sets up the reduced dimensionless linear system 
!!$    ! for the determination of the multipole coeficients.
!!$    Implicit None
!!$    integer               :: m       
!!$    Type(integrals_type)  :: Integ(0:,0:,0:), Integ_tr1(0:,0:,0:)
!!$    integer               :: Ienergy
!!$    complex(wp )          :: A(:,:)
!!$    Real(wp)              :: zeta(0:,0:,0:)
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       ! MP ABOVE substrate 
!!$       Call Matrix_system_above( m,A,Ienergy,Integ,Integ_tr1,zeta )
!!$    Else
!!$       ! MP BELOW substrate
!!$       Call Matrix_system_below( m,A,Ienergy,Integ,Integ_tr1,zeta )
!!$    Endif
!!$
!!$  End Subroutine Matrix_system
!!$  !--------------------------!
!!$


  !
  !----------------------------------------------------------------------------------------
  !
  
!!$
!!$
!!$  !---------------------------------------------!
!!$  Subroutine Right_Dipole(m,b,Ienergy,Integ,zeta)
!!$  !---------------------------------------------!
!!$
!!$    ! NOTICE :
!!$    ! This routine sets up the reduced dimensionless right
!!$    ! hand side of the linear system for the determination 
!!$    ! of the multipole coeficients in the Case of an uniform  
!!$    ! incident electric field
!!$    Implicit None
!!$    integer               ::      m,ienergy
!!$    complex(wp )          ::      b(:)
!!$    Type(integrals_type)  ::      Integ(0:,0:,0:)
!!$    Real(wp)              ::      zeta(0:,0:,0:)
!!$
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       ! MP ABOVE substrate 
!!$       Call Right_Dipole_above(m,b,Ienergy,Integ,zeta)
!!$    Else
!!$       ! MP BELOW substrate 
!!$       Call Right_Dipole_below(m,b,Ienergy,Integ,zeta)
!!$    Endif
!!$
!!$  End Subroutine Right_Dipole
!!$  !--------------------------!


  !
  !----------------------------------------------------------------------------------------
  !


!!$
!!$  !-------------------------------------------------!
!!$  Subroutine Right_Quadrupole(m,b,Ienergy,Integ,zeta)
!!$  !-------------------------------------------------!
!!$
!!$    ! NOTICE :
!!$    ! This routine sets up the reduced dimensionless right
!!$    ! hand side of the linear system for the determination 
!!$    ! of the multipole coeficients in the Case of a non uniform  
!!$    ! field (quadrupolar)
!!$    Implicit None
!!$    integer               ::      m,ienergy
!!$    complex(wp )          ::      b(:)
!!$    Type(integrals_type)  ::      Integ(0:,0:,0:)
!!$    Real(wp)              ::      zeta(0:,0:,0:)
!!$
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       ! MP ABOVE substrate 
!!$       Call Right_Quadrupole_above(m,b,Ienergy,Integ,zeta)
!!$    Else
!!$       ! MP BELOW substrate 
!!$       Call Right_Quadrupole_below(m,b,Ienergy,Integ,zeta)
!!$    Endif
!!$
!!$  End Subroutine Right_Quadrupole
!!$  !------------------------------!
!!$

  !
  !----------------------------------------------------------------------------------------
  !
  

!!$  !--------------------------------------------------------------!
!!$  Subroutine Matrix_system_above(m,A,Ienergy,Integ,Integ_tr1,zeta)
!!$  !--------------------------------------------------------------!
!!$    ! NOTICE : 
!!$    ! This routine sets up the reduced dimensionless linear system 
!!$    ! for the determination of the multipole coeficients, when 
!!$    ! the multipoles are located ABOVE the substrate. 
!!$    Implicit None
!!$    integer               :: m      
!!$    Type(integrals_type)  :: Integ(0:,0:,0:), Integ_tr1(0:,0:,0:)
!!$    integer               :: Ienergy
!!$    complex(wp )          :: A(:,:)
!!$    integer               :: mpo,l1,l2
!!$    Real(wp)              :: zeta(0:,0:,0:)
!!$    complex(wp )          :: e1,e2,e3,e4,eps(4)
!!$
!!$    ! Some initial abrevations
!!$    mpo  = param%Numerics%Multipole_Order 
!!$
!!$    ! The dieletctric constants
!!$    e1  = param%Materials%Epsilon%Ambient(ienergy) 
!!$    e2  = param%Materials%Epsilon%Substrate(ienergy)
!!$    e3  = param%Materials%Epsilon%Island(ienergy)
!!$    e4  = e2
!!$
!!$    ! Build the matrix system
!!$    A(:,:) = 0._wp
!!$    Do l1=1,mpo
!!$       Do l2=1,mpo
!!$
!!$          eps  = (/  (e1-e2),2*e1,2*e1*e2,e1*(e1-e2) /) /(e1+e2) 
!!$          ! --- C
!!$          A(l1,l2)     =    Integ(l1,l2,m)%K%mu(1)                    &
!!$               + (-1._wp)**(l2+m)*eps(1)*Integ(l1,l2,m)%K%mu(2)            & 
!!$               + eps(2)*( Integ_tr1(l1,l2,m)%K%mu(1)-Integ(l1,l2,m)%K%mu(1) )  
!!$          A(l1,l2) =  zeta(l1,l2,m) * A(l1,l2) 
!!$
!!$          ! --- F
!!$          A(mpo+l1,l2) = eps(3)*Integ_tr1(l1,l2,m)%L%mu(1)            &
!!$               + eps(4)*(  Integ(l1,l2,m)%L%mu(1)                         &
!!$               +(-1._wp)**(l2+m)*Integ(l1,l2,m)%L%mu(2) )
!!$          A(mpo+l1,l2) = zeta(l1,l2,m) * A(mpo+l1,l2)
!!$
!!$          eps = (/ (e3-e4),2*e3,2*e3*e4,e3*(e3-e4) /) /(e3+e4)
!!$          ! --- D
!!$          A(l1,mpo+l2) =  Integ(l1,l2,m)%M%mu(1)                      &  
!!$               +(-1._wp)**(l2+m)*eps(1)*Integ(l1,l2,m)%M%mu(2)            & 
!!$               +eps(2)*( Integ_tr1(l1,l2,m)%M%mu(1)-Integ(l1,l2,m)%M%mu(1) )
!!$          A(l1,mpo+l2) =  - zeta(l1,l2,m) * A(l1,mpo+l2)
!!$
!!$          ! --- G
!!$          A(mpo+l1,mpo+l2) = eps(3)*Integ_tr1(l1,l2,m)%N%mu(1)        &
!!$               + eps(4)*(  Integ(l1,l2,m)%N%mu(1)                         &
!!$               +(-1._wp)**(l2+m)*Integ(l1,l2,m)%N%mu(2) )
!!$          A(mpo+l1,mpo+l2) = - zeta(l1,l2,m) * A(mpo+l1,mpo+l2)
!!$
!!$       Enddo   ! End loop over l2
!!$    Enddo      ! End loop over l1
!!$
!!$  End Subroutine Matrix_system_above
!!$  !---------------------------------!
!!$
!!$
!!$  !
!!$  !----------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$  !--------------------------------------------------------------!
!!$  Subroutine Matrix_system_below(m,A,Ienergy,Integ,Integ_tr1,zeta)
!!$  !--------------------------------------------------------------!
!!$    ! NOTICE :
!!$    ! This routine sets up the reduced dimensionless linear system 
!!$    ! for the determination of the multipole coeficients, when 
!!$    ! the multipoles are located BELOW the substrate. 
!!$    Implicit None
!!$    integer                   :: m       
!!$    Type(integrals_type)      :: Integ(0:,0:,0:), Integ_tr1(0:,0:,0:)
!!$    integer                   :: Ienergy
!!$    complex(wp )              :: A(:,:)
!!$    integer                   :: mpo,l1,l2
!!$    Real(wp)                  :: zeta(0:,0:,0:)
!!$    Real(wp)                  :: factor
!!$    complex(wp )              :: e1,e2,e3,e4,eps(4)
!!$
!!$    ! Some initial abrevations
!!$    mpo  = param%Numerics%Multipole_Order 
!!$
!!$    ! The dieletctric constants
!!$    e1   = param%Materials%Epsilon%Ambient(ienergy) 
!!$    e2   = param%Materials%Epsilon%Substrate(ienergy)
!!$    e3   = param%Materials%Epsilon%Island(ienergy)
!!$    e4   = e2
!!$
!!$    ! Build the matrix system   
!!$    ! Notice :  As compared to the above Case (Get_linear_system_above)
!!$    !           e1 <--> e2
!!$    !           e3 <--> e4
!!$    A(:,:) = 0._wp
!!$    Do l1=1,mpo
!!$       Do l2=1,mpo
!!$
!!$          eps  = (/  (e2-e1),2*e2,2*e2*e1,e2*(e2-e1) /) /(e2+e1)
!!$          ! Unique for this MP below subsrate 
!!$          factor =  (-1._wp)**(l1+l2)
!!$          ! --- C
!!$          A(l1,l2)     =    Integ(l1,l2,m)%K%mu(1)                    &
!!$               + (-1._wp)**(l2+m)*eps(1)*Integ(l1,l2,m)%K%mu(2)            & 
!!$               + eps(2)*( Integ_tr1(l1,l2,m)%K%mu(1)-Integ(l1,l2,m)%K%mu(1) )  
!!$          A(l1,l2) =  factor * zeta(l1,l2,m) * A(l1,l2) 
!!$
!!$          ! --- F
!!$          A(mpo+l1,l2) = eps(3)*Integ_tr1(l1,l2,m)%L%mu(1)            &
!!$               + eps(4)*(  Integ(l1,l2,m)%L%mu(1)                         &
!!$               +(-1._wp)**(l2+m)*Integ(l1,l2,m)%L%mu(2) )
!!$          A(mpo+l1,l2) = factor * zeta(l1,l2,m) * A(mpo+l1,l2)  
!!$
!!$
!!$          eps = (/ (e4-e3),2*e4,2*e4*e3,e4*(e4-e3) /) /(e4+e3)
!!$          ! --- D
!!$          A(l1,mpo+l2) =  Integ(l1,l2,m)%M%mu(1)                      &  
!!$               +(-1._wp)**(l2+m)*eps(1)*Integ(l1,l2,m)%M%mu(2)            & 
!!$               +eps(2)*( Integ_tr1(l1,l2,m)%M%mu(1)-Integ(l1,l2,m)%M%mu(1) )
!!$          A(l1,mpo+l2) =  - factor*zeta(l1,l2,m) * A(l1,mpo+l2)
!!$
!!$          ! --- G
!!$          A(mpo+l1,mpo+l2) = eps(3)*Integ_tr1(l1,l2,m)%N%mu(1)        &
!!$               + eps(4)*(  Integ(l1,l2,m)%N%mu(1)                         &
!!$               +(-1._wp)**(l2+m)*Integ(l1,l2,m)%N%mu(2)    )
!!$          A(mpo+l1,mpo+l2) = - factor * zeta(l1,l2,m) * A(mpo+l1,mpo+l2) 
!!$
!!$       Enddo   ! End loop over l2
!!$    Enddo      ! End loop over l1
!!$
!!$  End Subroutine Matrix_system_below
!!$  !---------------------------------!
!!$
!!$
!!$  !
!!$  !----------------------------------------------------------------------------------------
!!$  !
!!$
!!$  
!!$  !---------------------------------------------------!
!!$  Subroutine Right_Dipole_above(m,b,Ienergy,Integ,zeta)
!!$  !---------------------------------------------------!
!!$    Implicit None
!!$    integer                   ::      m,ienergy
!!$    complex(wp )              ::      b(:)
!!$    Type(integrals_type)      ::      Integ(0:,0:,0:)
!!$    Real(wp)                  ::      zeta(0:,0:,0:)
!!$    integer                   ::      l1,mpo
!!$    Real(wp)                  ::      tr,sin0,cos0,kronl
!!$    complex(wp )              ::      e1,e2,exp0
!!$
!!$    ! Some initial abrevations
!!$    tr   = param%Island%Truncation_Ratio
!!$    sin0 = Sin(param%Source%Derived%ThetaE_calc)
!!$    cos0 = Cos(param%Source%Derived%ThetaE_calc)
!!$    exp0 = Exp(-imu*param%Source%Derived%PhiE_calc)
!!$    mpo  = param%Numerics%Multipole_Order 
!!$
!!$    ! The dieletctric constants
!!$    e1  = param%Materials%Epsilon%Ambient(ienergy) 
!!$    e2  = param%Materials%Epsilon%Substrate(ienergy)
!!$
!!$    Do l1=1,mpo      
!!$       ! Calculated the kronecker delta
!!$       If(l1==1) Then
!!$          kronl=1._wp
!!$       Else
!!$          kronl=0._wp
!!$       Endif
!!$       Select Case(m)
!!$       Case(0)
!!$          !--- H
!!$          b(l1) = Sqrt(4._wp*pi/3._wp)*cos0* &
!!$               ( e1/e2*kronl + (e1-e2)/e2*   &
!!$               (Sqrt(3._wp)*tr*zeta(l1,0,0)*Integ(l1,0,0)%Q &
!!$               -zeta(l1,1,0)*Integ(l1,1,0)%Q ) )   
!!$          !--- J 
!!$          b(mpo+l1) = Sqrt(4._wp*pi/3._wp)*e1*cos0*kronl   
!!$
!!$       Case(1)
!!$          !--- H 
!!$          b(l1) = -Sqrt(2._wp*pi/3._wp)*sin0*kronl*exp0       
!!$          !--- J
!!$          b(mpo+l1) = -Sqrt(2._wp*pi/3._wp)*sin0*exp0* &
!!$               ( e2*kronl+(e1-e2)*zeta(l1,1,1)*Integ(l1,1,1)%Q )   
!!$       End Select
!!$    Enddo
!!$
!!$  End Subroutine Right_Dipole_above
!!$  !--------------------------------!
!!$
!!$
!!$  !----------------------------------------------------------------------------------------
!!$
!!$  !---------------------------------------------------!
!!$  Subroutine Right_Dipole_below(m,b,Ienergy,Integ,zeta)
!!$  !---------------------------------------------------!
!!$    Implicit None
!!$    integer                   ::      m,ienergy
!!$    complex(wp )              ::      b(:)
!!$    Type(integrals_type)      ::      Integ(0:,0:,0:)
!!$    Real(wp)                  ::      zeta(0:,0:,0:)
!!$    integer                   ::      l1,mpo
!!$    Real(wp)                  ::      tr,sin0,cos0,kronl,factor
!!$    complex(wp )              ::      e1,e2,exp0
!!$
!!$    ! Some initial abrevations
!!$    tr   = param%Island%Truncation_Ratio 
!!$    sin0 = Sin(param%Source%Derived%ThetaE_calc)
!!$    cos0 = Cos(param%Source%Derived%ThetaE_calc)
!!$    exp0 = Exp(-imu*param%Source%Derived%PhiE_calc)
!!$    mpo  = param%Numerics%Multipole_Order 
!!$
!!$    ! The dieletctric constants
!!$    e1  = param%Materials%Epsilon%Ambient(ienergy) 
!!$    e2  = param%Materials%Epsilon%Substrate(ienergy)
!!$
!!$
!!$    ! Notice :  As compared to the above Case (Get_linear_system_above)
!!$    !           e1 <--> e2
!!$    !           e3 <--> e4
!!$    Do l1=1,mpo       
!!$       ! Calculated the kronecker delta
!!$       If(l1==1) Then
!!$          kronl=1._wp
!!$       Else
!!$          kronl=0._wp
!!$       Endif
!!$
!!$       ! Unique for this MP below subsrate 
!!$       factor =  (-1._wp)**(l1+1)
!!$       Select Case(m)
!!$       Case(0)
!!$          !--- H
!!$          b(l1) = Sqrt(4._wp*pi/3._wp)*cos0*   &
!!$               ( kronl + (e2-e1)/e2*factor*   &
!!$               ( -Sqrt(3._wp)*tr*zeta(l1,0,0)*Integ(l1,0,0)%Q - &
!!$               zeta(l1,1,0)*Integ(l1,1,0)%Q ) )  
!!$          !--- J 
!!$          b(mpo+l1) = Sqrt(4._wp*pi/3._wp)*e1*cos0*kronl   
!!$
!!$       Case(1)
!!$          !--- H 
!!$          b(l1) = -Sqrt(2._wp*pi/3._wp)*sin0*kronl*exp0      
!!$          !--- J
!!$          b(mpo+l1) = -Sqrt(2._wp*pi/3._wp)*sin0*exp0*   &
!!$               ( kronl*e1+(e2-e1)*factor*zeta(l1,1,1)*Integ(l1,1,1)%Q )   
!!$       End Select
!!$    Enddo
!!$
!!$  End Subroutine Right_Dipole_below
!!$  !--------------------------------!
!!$
!!$  !----------------------------------------------------------------------------------------
!!$
!!$
!!$  !---------------------------------------------------------!
!!$  Subroutine Right_Quadrupole_above( m,b,Ienergy,Integ,zeta )
!!$  !---------------------------------------------------------!
!!$    Implicit None
!!$    complex(wp )              ::      b(:)
!!$    integer                   ::      m,Ienergy
!!$    Type(integrals_type)      ::      Integ(0:,0:,0:)
!!$    Real(wp)                  ::      zeta(0:,0:,0:)
!!$    integer                   ::      l1,mpo
!!$    Real(wp)                  ::      kronl1,kronl2,tr
!!$    complex(wp )              ::      e1,e2,sqr
!!$
!!$
!!$    ! Some initial abrevations
!!$    tr   = param%Island%Truncation_Ratio
!!$    mpo  = param%Numerics%Multipole_Order
!!$    sqr      = Sqrt(2._wp*pi/15._wp)           
!!$    ! The dieletctric constants
!!$    e1   = param%Materials%Epsilon%Ambient(ienergy)  
!!$    e2   = param%Materials%Epsilon%Substrate(ienergy)
!!$
!!$    Do l1=1,mpo 
!!$
!!$       ! Calculate the kronecker delta
!!$       If(l1==1) Then
!!$          kronl1=1._wp
!!$       Else
!!$          kronl1=0._wp
!!$       Endif
!!$       If(l1==2) Then
!!$          kronl2=1._wp
!!$       Else
!!$          kronl2=0._wp
!!$       Endif
!!$       ! Formulas 10-71 10-72 10-73 10-74 10-76 10-77
!!$       Select Case(m)
!!$       Case(0)
!!$          ! --- K
!!$          b(l1)           = -2._wp*Sqrt(4._wp*pi/3._wp)* &
!!$               ( Sqrt(3._wp/5._wp)/2*kronl2 + (e1/e2-1._wp)*tr* &
!!$               (Sqrt(3._wp)*tr*zeta(l1,0,0)*Integ(l1,0,0)%Q + kronl1 - &
!!$               zeta(l1,1,0)*Integ(l1,1,0)%Q ) )
!!$          ! --- L
!!$          b(l1+mpo)       = 4._wp*Sqrt(pi/5._wp)* &
!!$               ( (e2-e1)*zeta(l1,2,0)*Integ(l1,2,0)%Q - e2*kronl2 + &
!!$               Sqrt(5._wp/3._wp)*(e2-e1)*tr*(kronl1-zeta(l1,1,0)*Integ(l1,1,0)%Q) )
!!$
!!$       Case(1)
!!$          ! --- K
!!$          b(l1)           = -sqr*(1._wp-imu)* &
!!$               ( e1/e2*kronl2 - (e1/e2-1._wp)*( zeta(l1,2,1)*Integ(l1,2,1)%Q + &
!!$               Sqrt(5._wp)*tr*(kronl1-zeta(l1,1,1)*Integ(l1,1,1)%Q) ) )
!!$          ! --- L
!!$          b(l1+mpo)       = -sqr*(1._wp-imu)* &
!!$               ( 2._wp*e1*kronl2 + Sqrt(5._wp)*(e2-e1)*tr* &
!!$               (kronl1-zeta(l1,1,1)*Integ(l1,1,1)%Q) )
!!$       Case(2)
!!$          If(l1==1) cycle
!!$          ! --- K
!!$          b(l1)           = -sqr*imu*kronl2
!!$          ! --- L
!!$          b(l1+mpo)       = -2._wp*sqr*imu* &
!!$               ( e2*kronl2 - (e2-e1)*zeta(l1,2,2)*Integ(l1,2,2)%Q )
!!$
!!$       End Select
!!$
!!$    Enddo
!!$
!!$  End Subroutine Right_Quadrupole_above
!!$  !------------------------------------!
!!$
!!$
!!$
!!$  !
!!$  !----------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$
!!$  !-------------------------------------------------------!
!!$  Subroutine Right_Quadrupole_below(m,b,Ienergy,Integ,zeta)
!!$  !-------------------------------------------------------!
!!$    Implicit None
!!$    complex(wp )              ::      b(:)
!!$    integer                   ::      m,Ienergy
!!$    Type(integrals_type)      ::      Integ(0:,0:,0:)
!!$    Real(wp)                  ::      zeta(0:,0:,0:)
!!$    integer                   ::      l1,mpo
!!$    Real(wp)                  ::      kronl1,kronl2,tr
!!$    complex(wp )              ::      e1,e2,sqr
!!$
!!$
!!$    ! Some initial abrevations
!!$    tr   = -Abs(param%Island%Truncation_Ratio) ! Inversion of the system axis in the Case param%Numerics%Derived%Above=.false.
!!$    mpo  = param%Numerics%Multipole_Order
!!$    sqr      = Sqrt(2._wp*pi/15._wp)           
!!$    ! The dieletctric constants
!!$    e1   = param%Materials%Epsilon%Ambient(ienergy)  
!!$    e2   = param%Materials%Epsilon%Substrate(ienergy)
!!$
!!$    Do l1=1,mpo 
!!$
!!$       ! Calculate the kronecker delta
!!$       If(l1==1) Then
!!$          kronl1=1._wp
!!$       Else
!!$          kronl1=0._wp
!!$       Endif
!!$       If(l1==2) Then
!!$          kronl2=1._wp
!!$       Else
!!$          kronl2=0._wp
!!$       Endif
!!$       ! Formulas 10-71 10-72 10-73 10-74 10-76 10-77
!!$       Select Case(m)
!!$       Case(0)
!!$          ! --- K
!!$          b(l1)           = -2._wp*Sqrt(4._wp*pi/3._wp)* &
!!$               ( Sqrt(3._wp/5._wp)/2*kronl2 + (e2/e1-1._wp)*tr* &
!!$               (Sqrt(3._wp)*tr*zeta(l1,0,0)*Integ(l1,0,0)%Q + kronl1 - &
!!$               zeta(l1,1,0)*Integ(l1,1,0)%Q ) )
!!$          ! --- L
!!$          b(l1+mpo)       = 4._wp*Sqrt(pi/5._wp)* &
!!$               ( (e1-e2)*zeta(l1,2,0)*Integ(l1,2,0)%Q - e1*kronl2 + &
!!$               Sqrt(5._wp/3._wp)*(e1-e2)*tr*(kronl1-zeta(l1,1,0)*Integ(l1,1,0)%Q) )
!!$
!!$       Case(1)
!!$          ! --- K
!!$          b(l1)           = -sqr*(1._wp-imu)* &
!!$               ( e2/e1*kronl2 - (e2/e1-1._wp)*( zeta(l1,2,1)*Integ(l1,2,1)%Q + &
!!$               Sqrt(5._wp)*tr*(kronl1-zeta(l1,1,1)*Integ(l1,1,1)%Q) ) )
!!$          ! --- L
!!$          b(l1+mpo)       = -sqr*(1._wp-imu)* &
!!$               ( 2._wp*e2*kronl2 + Sqrt(5._wp)*(e1-e2)*tr* &
!!$               (kronl1-zeta(l1,1,1)*Integ(l1,1,1)%Q) )
!!$       Case(2)
!!$          If(l1==1) cycle
!!$          ! --- K
!!$          b(l1)           = -sqr*imu*kronl2
!!$          ! --- L
!!$          b(l1+mpo)       = -2._wp*sqr*imu* &
!!$               ( e1*kronl2 - (e1-e2)*zeta(l1,2,2)*Integ(l1,2,2)%Q )
!!$
!!$       End Select
!!$
!!$    Enddo
!!$
!!$
!!$  End Subroutine Right_Quadrupole_below
!!$  !------------------------------------!
!!$
!!$
!!$
!!$End Module Matrix_System_Mod
!!$!---------------------------!
!!$
!!$
!!$
!!$





End Module Matrix_System_Sphere_Module
!--------------------------------------!
