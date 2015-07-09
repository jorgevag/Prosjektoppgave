! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module contains the integrands for the integrals
!     of prolate truncated spheroidal particles.
!
!     Plm and dPlm from Legendre module are used for the spherical harmonics.
!
!     For the radial functions X_prolate and Z_prolate, new Ass. Legendre 
!     polynomial functions are implemented valid for real arguments xi > 1.
!     
!     Plm_xGrOne, Plm_xGrOne_Diff, Qlm and dQlm are implemented here, but 
!     should perhaps be moved to the Legendre module...
!
!     NOTE : The parameters l1,l2,m,delta_z_over_a and xi_0 are exchanged
!            via global variables in this routine. )
! 
! --- AUTHOR : Sindre Stavseng, Trondheim, Feb 2013.
! --- Based on module written by E.Aursand for oblate spheroids.
!
! ----------------------------------------------------------
!

Module Integrand_Module_Spheroid_Prolate

  ! --- The Use Statements global to the module
  Use Shared,            only : wp, imu  
  Use Legendre_Module,   only : Legendre_Plm, Legendre_Plm_Diff

  ! --------------------------------------
  ! --- The Publicly available Parameters
  !     used to exchange info with the
  !     integrands 
  ! --------------------------------------
  Public :: l1
  Public :: l2
  Public :: m
  Public :: delta_z_over_a
  Public :: xi_0

  ! --------------------------------------
  ! --- The Publicly available routines
  ! --------------------------------------
  Public :: Q_Integrand  
  Public :: V_Integrand  
  Public :: W_Integrand  
  Public :: dV_Integrand 
  Public :: dW_Integrand   
  Public :: Legendre_Qlm 
  Public :: Legendre_Plm_xGrOne
  ! TESTING
  Public :: Transformed_xi_eta
  Public :: Deriv_Transformed_xi_eta
  !Public :: Test_Integrand 
  !Public :: Legendre_Qlm_Aimag 
  !Public :: Z_oblate
  !Public :: Z_Prolate
  !Public :: X_Prolate
  Public :: X_Prolate_Diff
  Public :: Z_Prolate_Diff


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  ! --- The parameters to exchange when calling the integration routine (QUADPACK)
  ! -------------------------------------------------------------------------------
  Integer  :: l1            ! The l1 "quantum number"
  Integer  :: l2            ! The l2 "quantum number"
  Integer  :: m             ! The m  "quantum number"
  Real(wp) :: delta_z_over_a  ! The z-coordinate of the Multipoles or Image MultiPoles, 
                            ! divided by the parameter a.
  real(wp) :: xi_0          ! Elongation parameter of the current prolate spheroid to
                            ! integrate over.
 

Contains
  !----------------------------------!
  Subroutine Transformed_xi_eta(delta_z_over_a, xi, eta, xi_trans, eta_trans)
  !----------------------------------!
  !
  ! This routine is based on the equivalent routine for the oblate spheroidal case. 
  ! The coordinate transformations are equal in the prolate and oblate cases, 
  ! except for different signs on the last terms in tmp.
  !
  ! --- Sindre Stavseng 2013 ---
  !
    Implicit None
    Real(wp), Intent(in)  ::  delta_z_over_a, xi, eta
    Real(wp), Intent(out) ::  xi_trans, eta_trans
    ! Internal
    Real(wp)              ::  tmp,tmp2
    
    !write(*,*) 'delta_z_over_a = ', delta_z_over_a
    !write(*,*) 'xi_1 = ', delta_z_over_a*0.5_wp

    if (abs(delta_z_over_a) <= 10._wp*epsilon(1._wp)) then   ! If delta_z_over_a is smaller than
      xi_trans = xi                                          ! 10 times the working precision,
      eta_trans = eta                                        ! the shifted coordinate system is 
    else                                                     ! set equal to the original one.
      tmp2 = delta_z_over_a/xi
      tmp = 1._wp + (tmp2)**2 - 2._wp*tmp2*eta + (eta/xi)**2
      tmp = sqrt(tmp + sqrt(tmp**2 - ((2._wp/xi)*(tmp2-eta))**2))
      
      xi_trans = xi*tmp/sqrt(2.0_wp)
      eta_trans = sqrt(2.0_wp)*(eta-tmp2)/tmp
      
      ! This might make things slow... Sindre: Is this necessary?
      if (eta_trans > 1.0_wp) then 
        eta_trans = 1.0_wp
      else if (eta_trans < -1.0_wp) then
        eta_trans = -1.0_wp
      end if
    end if

  End subroutine Transformed_xi_eta
  !----------------------------------!

  !----------------------------------!
  Subroutine Deriv_transformed_xi_eta(delta_z_over_a, xi, eta, deriv_xi_trans, deriv_eta_trans)
  !----------------------------------!
  ! Derivatives of xi and eta in coordinate system at z=\Delta z 
  ! with respect to the coordinate xi in the main coordinate system.
  !
  ! This routine is based on the equivalent routine for the oblate spheroidal case. The derivatives of the 
  ! coordinate transformations are equal in the prolate and oblate cases, except for different signs in 
  ! some of the C-terms.
  !
  ! Sindre Stavseng, Feb. 2013. 

    Implicit None
    Real(wp), Intent(in)  ::  delta_z_over_a, xi, eta
    Real(wp), Intent(out) ::  deriv_xi_trans, deriv_eta_trans
    ! Local variables
    Real(wp)              ::  C(8),tmp
    
    tmp = delta_z_over_a/xi
    C(1) = 1._wp + (tmp)**2 - 2._wp*tmp*eta + (eta/xi)**2
    C(2) = (2._wp/xi)*(tmp-eta)
    C(3) = -2._wp*(delta_z_over_a**2)/xi**3 & 
           +2._wp*delta_z_over_a*eta/xi**2 &
           -2._wp*(eta**2)/(xi**3)
    C(4) = -4._wp*delta_z_over_a/xi**3 &
           +2._wp*eta/xi**2
    C(5) = sqrt(C(1)**2 - C(2)**2)
    C(6) = C(1)*C(3) - C(2)*C(4)
    C(7) = sqrt(C(1)+C(5))
    C(8) = C(3) + C(6)/C(5)
   
    deriv_xi_trans = (C(7) + (C(8)*xi)/(C(7)*2._wp) )/sqrt(2._wp)    ! OK
    deriv_eta_trans = sqrt(2._wp)*(delta_z_over_a/(C(7)*xi**2) &     ! OK
                            - (eta - tmp)*C(8)/(2._wp*C(7)**3))

  End subroutine Deriv_transformed_xi_eta
  !----------------------------------!

  
  !----------------------------------!
  Function Legendre_Plm_xGrOne(l,m,x)   Result(Plm)
  !----------------------------------!
    ! --------------------------------------------------
    ! Routine for calculation of the associated Legendre 
    ! polynomials of the first kind with real argument x > 1.
    ! The first few terms are found from the expression in
    ! Bedeaux p.129. All other terms are calculated using the
    ! recurrence formula for the Legendre polynomials (see for
    ! instance K.Rottmann p.93).
    !
    ! --- Sindre Stavseng, Feb.2013
    ! ---------------------------------------------------
    Use Error_Module, only : Error_Failure
    Use SFL_Precision, only : qp       
    
    Implicit None
    !Global
    Integer,      Intent(In)   :: l,m
    Real(wp),     Intent(In)   :: x
    Complex(wp)                :: Plm
    !Local
    character(len=*),  parameter :: routine = "Legendre_Plm_xGrOne"
    Character(len=100)           :: str
    Real(qp)                     :: t, x_quad
    Integer                      :: ll
    Complex(qp)                  :: P1, P2, P3, Pmm

    ! --- Error Checking ----------
    If(.not.(m >= 0 .and. m <= l)) Then
       Write(str,fmt='(a,i2,a,i2)') 'l = ',l,';   m= ',m
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    !
    ! Include a check for x>1 here?
    ! -----------------------------

    ! --- The main calculation ----

    ! === CHECK THIS ==================================
    
    x_quad = x
    
    t = sqrt(x_quad**2 - 1._wp)
    
    select case(m)
       case(0)
          Pmm = 1._wp                         ! P_0^0
          P2 = x_quad                              ! P_1^0
       case(1)
          Pmm = - imu * t                     ! P_1^1
          P2 = - 3*imu * x_quad * t                ! P_2^1
       case default
          call Error_Failure( routine, "m > 1 is not supported")
    end select
    
    if (l == m) then
       Plm = Pmm
    else if (l == (m+1)) then
       Plm = P2
    else            ! All Q's of higher l are calculated using recursion formulas:
       P1 = Pmm
       do ll = m+2,l
          P3 = ((2*ll - 1)*x_quad*P2 - (ll + m - 1)*P1 )/(ll - m)
          P1 = P2
          P2 = P3
       end do
       Plm = P3
    end if
    return

  !----------------------------------!
  End Function Legendre_Plm_xGrOne
  !----------------------------------!

  !----------------------------------!
  Function Legendre_Plm_xGrOne_Diff(l,m,x)   Result(dPlm)
  !----------------------------------!
    ! --------------------------------------------------
    ! Routine for calculation of the differentiated associated Legendre 
    ! polynomials of the first kind with real argument x > 1.
    ! All terms are calculated using the
    ! recurrence formula for the Legendre polynomials (see for
    ! instance K.Rottmann p.93).
    !
    ! --- Sindre Stavseng, Feb.2013
    ! ---------------------------------------------------
    Use Error_Module, only : Error_Failure
    Use SFL_Precision, only : qp       

    Implicit None
    !Global
    Integer,      Intent(In)   :: l,m
    Real(wp),     Intent(In)   :: x
    Complex(wp)                :: dPlm
    !Local
    character(len=*),  parameter :: routine = "Legendre_Plm_xGrOne_Diff"
    Character(len=100)           :: str
    Real(qp)                     :: x_quad

    ! --- Error Checking ----------
    If(.not.(m >= 0 .and. m <= l)) Then
       Write(str,fmt='(a,i2,a,i2)') 'l = ',l,';   m= ',m
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    ! -----------------------------
    
    ! ====================================
    !Testing
    !write(*,*) "Legendre Qlm Diff"
    ! ====================================
    
    ! --- The main calculation ----
    x_quad = x
    
    dPlm = ( (l - m + 1)*Legendre_Plm_xGrOne(l+1, m, x) - &
         (l + 1)*x_quad*Legendre_Plm_xGrOne(l, m, x) )/(x_quad**2 - 1._wp)
    ! -----------------------------

  End Function Legendre_Plm_xGrOne_Diff
  !----------------------------------!
  

  !----------------------------------!
  Function Legendre_Qlm(l,m,x)   Result(Qlm)
  !----------------------------------!
    ! --------------------------------------------------
    ! Routine for calculation of the associated Legendre 
    ! polynomials of the second kind with real argument x.
    ! The first few terms are found from the expression in
    ! Bedeaux p.129. All other terms are calculated using the
    ! recurrence formula for the Legendre polynomials (see for
    ! instance K.Rottmann p.93).
    !
    ! Note that Qlm is always a real number
    !
    ! --- Sindre Stavseng, Feb.2013
    ! ---------------------------------------------------
    Use Error_Module, only : Error_Failure
    Use SFL_Precision, only : qp       

    Implicit None
    !Global
    Integer,      Intent(In)   :: l,m
    Real(wp),     Intent(In)   :: x
    !Complex(wp)                :: Qlm
    Real(wp)                   :: Qlm
    !Local
    character(len=*),  parameter :: routine = "Legendre_Qlm"
    Character(len=100)           :: str
    Real(qp)                     :: x_quad, t
    Integer                      :: ll
    !Complex(qp)                  :: Q1, Q2, Q3, Qmm
    Real(qp)                     :: Q1, Q2, Q3, Qmm

    ! --- Error Checking ----------
    If(.not.(m >= 0 .and. m <= l)) Then
       Write(str,fmt='(a,i2,a,i2)') 'l = ',l,';   m= ',m
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    ! -----------------------------

    ! --- The main calculation ----
    
    x_quad = x
    t = log((x_quad + 1._wp)/(x_quad - 1._wp))/2._wp  
    
    !t = log((x_quad + 1)/(x_quad - 1))/2  ! TEST THIS, ANY DIFFERENCE?
    
    select case(m)
       case(0)
          Qmm = t                         ! Q_0^0
          Q2 = x_quad*t - 1._wp           ! Q_1^0
       case(1)
          Qmm = -((x_quad**2 - 1._wp)*t - x_quad)/sqrt(x_quad**2 - 1._wp)                     ! Q_1^1
          Q2 = -((3*x_quad**3 - 3*x_quad)*t - 3*x_quad**2 + 2._wp)/sqrt(x_quad**2 - 1._wp)    ! Q_2^1
       case default
          call Error_Failure( routine, "m > 1 is not supported")
    end select
    
    if (l == m) then
       Qlm = Qmm
    else if (l == (m+1)) then
       Qlm = Q2
    else            ! All Q's of higher l are calculated using recursion formulas:
       Q1 = Qmm
       do ll = m+2,l
          Q3 = ((2*ll - 1)*x_quad*Q2 - (ll + m - 1)*Q1 )/(ll - m)
          Q1 = Q2
          Q2 = Q3
       end do
       Qlm = Q3
    end if
    return

    ! -----------------------------

  End Function Legendre_Qlm
  !----------------------------------!

  !----------------------------------!
  Function Legendre_Qlm_Diff(l,m,x)   Result(dQlm)
  !----------------------------------!
    ! --------------------------------------------------
    ! Routine for calculation of the differentiated associated Legendre 
    ! polynomials of the second kind with real argument x.
    ! All terms are calculated using the
    ! recurrence formula for the Legendre polynomials (see for
    ! instance K.Rottmann p.93).
    !
    ! --- Sindre Stavseng, Feb.2013
    ! ---------------------------------------------------
    Use Error_Module, only : Error_Failure
    Use SFL_Precision, only : qp       

    Implicit None
    !Global
    Integer,      Intent(In)   :: l,m
    Real(wp),     Intent(In)   :: x
    Complex(wp)                :: dQlm
    !Local
    character(len=*),  parameter :: routine = "Legendre_Qlm_Diff"
    Character(len=100)           :: str
    Real(qp)                     :: x_quad

    ! --- Error Checking ----------
    If(.not.(m >= 0 .and. m <= l)) Then
       Write(str,fmt='(a,i2,a,i2)') 'l = ',l,';   m= ',m
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    ! -----------------------------
    
    ! --- The main calculation ----
    x_quad = x
    dQlm = ( (l - m + 1)*Legendre_Qlm(l+1, m, x) - (l + 1)*x_quad*Legendre_Qlm(l, m, x) )/(x_quad**2 - 1._wp)
    ! -----------------------------

  End Function Legendre_Qlm_Diff
  !----------------------------------!

  !----------------------------------!
  Function X_Prolate(l, m, xi)        Result(X)
  !----------------------------------!
    ! Calculates the prolate X-function defined in Eq. 6.58
    ! in Bedeaux. 
    !
    ! X is always real
    !
    ! --- Sindre Stavseng, Feb.2013
    Use Error_Module, only : Error_Failure

    implicit none
    Real(wp), Intent(In)    :: xi
    Integer, Intent(In)     :: l,m
    !Complex(wp)             :: X
    Real(wp)                :: X
    !Local
    character(len=*),  parameter :: routine = "X_Prolate"
    Integer                      :: j
    Real(wp)                     :: factor

    ! Calculate the factorial prefactor
    factor = 1._wp
    do j = 1, l
       factor = factor*real(j, wp)/(2*j - 1._wp)
    enddo

    select case( m )
       case(0)
          X = Real( factor*Legendre_Plm_xGrOne(l,m,xi) )
       case(1)
          X = Real( imu*factor*Legendre_Plm_xGrOne(l,m,xi)/l )
       case default
          call Error_Failure(routine, "Function not implemented for m != 0, 1.")
    end select
    
    return

  !----------------------------------!
  End Function X_Prolate
  !----------------------------------!
  

  !----------------------------------!
  Function Z_Prolate(l, m, xi)        Result(Z)
  !----------------------------------!
    ! Calculates the prolate Z-function defined in Eq. 6.59
    ! in Bedeaux. 
    !
    ! Z is always real
    !
    ! --- Sindre Stavseng, Feb.2013
    Use Error_Module, only : Error_Failure

    implicit none
    Real(wp), Intent(In)    :: xi
    Integer, Intent(In)     :: l,m
    !Complex(wp)             :: Z
    Real(wp)                :: Z
    !Local
    character(len=*),  parameter :: routine = "Z_Prolate"
    Integer                      :: j
    Real(wp)                     :: factor

    ! Calculate the factorials prefactor
    factor = 1._wp
    do j = 1, l
       factor = factor*real( (2*j + 1), wp)/j
    enddo

    select case( m )
       case(0)
          Z = factor*Legendre_Qlm(l,m,xi)
       case(1)
          Z = factor*Legendre_Qlm(l,m,xi)/(l + 1._wp)
       case default
          call Error_Failure(routine, "Function not implemented for m > 1.")
    end select
    
    return

  !----------------------------------!
  End Function Z_Prolate
  !----------------------------------!
  

  !----------------------------------!
  Function X_Prolate_Diff(l, m, xi)        Result(dX)
  !----------------------------------!
    ! Calculates the xi-derivative of the prolate X-function 
    ! defined in Eq. 6.58 in Bedeaux. 
    !
    ! --- Sindre Stavseng, Feb.2013
    Use Error_Module, only : Error_Failure

    implicit none
    Real(wp), Intent(In)    :: xi
    Integer, Intent(In)     :: l,m
    !Complex(wp)             :: dX
    Real(wp)                :: dX
    !Local
    character(len=*),  parameter :: routine = "X_Prolate_Diff"
    Integer                      :: j
    Real(wp)                     :: factor

    ! Calculate the factorial prefactor
    factor = 1._wp
    do j = 1, l
       factor = factor*j/(2*j - 1._wp)
    enddo

    select case( m )
       case(0)
          dX = Real( factor*Legendre_Plm_xGrOne_Diff(l,m,xi) )
       case(1)
          dX = Real( imu*factor*Legendre_Plm_xGrOne_Diff(l,m,xi)/l )
       case default
          call Error_Failure(routine, "Function not implemented for m > 1.")
    end select
    
    return

  !----------------------------------!
  End Function X_Prolate_Diff
  !----------------------------------!
  

  !----------------------------------!
  Function Z_Prolate_Diff(l, m, xi)        Result(dZ)
  !----------------------------------!
    ! Calculates the xi-derivative of the prolate Z-function 
    ! defined in Eq. 6.59 in Bedeaux. 
    !
    ! --- Sindre Stavseng, Feb.2013
    Use Error_Module, only : Error_Failure

    implicit none
    Real(wp), Intent(In)    :: xi
    Integer, Intent(In)     :: l,m
    !Complex(wp)             :: dZ
    Real(wp)             :: dZ
    !Local
    character(len=*),  parameter :: routine = "Z_Prolate_Diff"
    Integer                      :: j
    Real(wp)                     :: factor

    ! Calculate the factorials prefactor
    factor = 1._wp
    do j = 1, l
       factor = factor*(2*j + 1._wp)/j
    enddo

    select case( m )
       case(0)
          dZ = factor*Legendre_Qlm_Diff(l,m,xi)
       case(1)
          dZ = factor*Legendre_Qlm_Diff(l,m,xi)/(l + 1._wp)
       case default
          call Error_Failure(routine, "Function not implemented for m > 1.")
    end select
    
    return

  !----------------------------------!
  End Function Z_Prolate_Diff
  !----------------------------------!

  ! ================================ !
  !            INTEGRANDS
  ! ================================ !

  !----------------------------------!
  Function Q_Integrand(x) Result(res)
  !----------------------------------!
  !Needs a separate but identical Q-integrand here, 
  !to have a version connected to l1,l2 and m of this module.
  !
    Implicit None
    Real(wp), Intent(in)  ::  x
    Real(wp)              ::  Res

    res  =  Legendre_Plm(l1,m,x) * Legendre_Plm(l2,m,x)

    Return
  End Function Q_Integrand
  !----------------------------------!

  !----------------------------------!
  Function V_Integrand(eta) Result(res)
  !----------------------------------!
    Implicit None
    Real(wp), Intent(In)    :: eta
    Real(wp)                :: res
    !-- Local variables --
    Real(wp)                :: xi_trans, eta_trans
    ! Global variables: delta_z_over_a, xi_0, l1, l2, m

    call Transformed_xi_eta( delta_z_over_a, xi_0, eta, xi_trans, eta_trans)

    res = Legendre_Plm(l1, m, eta)*Legendre_Plm(l2, m, eta_trans)*Z_Prolate(l2, m, xi_trans)
    
    return
  !----------------------------------!
  End Function V_Integrand
  !----------------------------------!
  
  !----------------------------------!
  Function W_Integrand(eta) Result(res)
  !----------------------------------!
    Implicit None
    Real(wp), Intent(In)    :: eta
    Real(wp)                :: res
    !-- Local variables --
    Real(wp)                :: xi_trans, eta_trans
    ! Global variables: delta_z_over_a, xi_0, l1, l2, m

    call Transformed_xi_eta( delta_z_over_a, xi_0, eta, xi_trans, eta_trans)

    res = Legendre_Plm(l1, m, eta)*Legendre_Plm(l2, m, eta_trans)*X_Prolate(l2, m, xi_trans)
    
    return
  !----------------------------------!
  End Function W_Integrand
  !----------------------------------!

  !----------------------------------!
  Function dV_Integrand(eta) Result(res)
  !----------------------------------!
    Implicit None
    Real(wp), Intent(In)    :: eta
    Real(wp)                :: res
    !-- Local variables --
    Real(wp)                :: xi_trans, eta_trans, deriv_xi_trans, deriv_eta_trans
    ! Global variables: delta_z_over_a, xi_0, l1, l2, m
    
    call Transformed_xi_eta(delta_z_over_a, xi_0, eta, xi_trans, eta_trans)
    call Deriv_transformed_xi_eta(delta_z_over_a, xi_0, eta, deriv_xi_trans, deriv_eta_trans)

    res = Legendre_Plm(l1, m, eta) &
          *( deriv_eta_trans*Legendre_Plm_Diff(l2, m, eta_trans)*Z_Prolate(l2, m, xi_trans) &
          + Legendre_Plm(l2, m, eta_trans)*deriv_xi_trans*Z_Prolate_Diff(l2, m, xi_trans) )

    return

  !----------------------------------!
  End Function dV_Integrand
  !----------------------------------!
  
  !----------------------------------!
  Function dW_Integrand(eta) Result(res)
  !----------------------------------!
    Implicit None
    Real(wp), Intent(In)    :: eta
    Real(wp)                :: res
    !-- Local variables --
    Real(wp)                :: xi_trans, eta_trans, deriv_xi_trans, deriv_eta_trans
    ! Global variables: delta_z_over_a, xi_0, l1, l2, m
    
    call Transformed_xi_eta(delta_z_over_a, xi_0, eta, xi_trans, eta_trans)
    call Deriv_transformed_xi_eta(delta_z_over_a, xi_0, eta, deriv_xi_trans, deriv_eta_trans)

    res = Legendre_Plm(l1, m, eta) &
          *( deriv_eta_trans*Legendre_Plm_Diff(l2, m, eta_trans)*X_Prolate(l2, m, xi_trans) &
          + Legendre_Plm(l2, m, eta_trans)*deriv_xi_trans*X_Prolate_Diff(l2, m, xi_trans) )

    return
  !----------------------------------!
  End Function dW_Integrand
  !----------------------------------!
  
End Module Integrand_Module_Spheroid_Prolate
