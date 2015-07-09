! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module contains the integrands for the integrals
!     of truncated spheroidal particles.
!
!     Using Plm and dPlm for real arguments from 
!     Legendre module.
!
!     NOTE : The parameters l1,l2,m,delta_z_over_a and xi_0 are exchanged
!            via global variables in this routine.
! 
! --- AUTHOR : Eskil Aursand, Trondheim, Mar 2012.
!
! ----------------------------------------------------------
!

Module Integrand_Module_Spheroid_Oblate



  ! --- The Use Statements global to the module
  Use Shared, only : wp, imu  

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
  Public :: Legendre_Qlm_Aimag 
  Public :: Legendre_Plm_Aimag 
  ! -----
  Public :: Z_Oblate_diff
  Public :: X_Oblate_diff
  Public :: Deriv_transformed_xi_eta
  ! TESTING
  !Public :: Transformed_xi_eta
  !Public :: Test_Integrand 
  !Public :: Legendre_Qlm_Aimag 
  !Public :: Z_Oblate
  !Public :: X_Oblate


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
  real(wp) :: xi_0          ! Flattening parameter of the current spheroid to
                            ! integrate over.
  




Contains
  !----------------------------------!
  Subroutine Transformed_xi_eta(delta_z_over_a, xi, eta, xi_trans, eta_trans)
  !----------------------------------!
  ! Eskil Aursand 2012
    Implicit None
    Real(wp), Intent(in)  ::  delta_z_over_a, xi, eta
    Real(wp), Intent(out) ::  xi_trans, eta_trans
    Real(wp)              ::  tmp,tmp2
    
    !write(*,*) 'delta_z_over_a = ', delta_z_over_a
    !write(*,*) 'xi_1 = ', delta_z_over_a*0.5_wp

    if (abs(delta_z_over_a) <= 10._wp*epsilon(1._wp)) then 
      xi_trans = xi
      eta_trans = eta
    else
      tmp2 = delta_z_over_a/xi
      tmp = 1._wp + (tmp2)**2 - 2._wp*tmp2*eta - (eta/xi)**2
      tmp = sqrt(tmp + sqrt(tmp**2 + ((2._wp/xi)*(tmp2-eta))**2))
      
      xi_trans = xi*tmp/sqrt(2.0_wp)
      eta_trans = sqrt(2.0_wp)*(eta-tmp2)/tmp
      
      ! This might make things slow...
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
  ! Eskil Aursand 2012
    Implicit None
    Real(wp), Intent(in)  ::  delta_z_over_a, xi, eta
    Real(wp), Intent(out) ::  deriv_xi_trans, deriv_eta_trans
    Real(wp)              ::  C(8),tmp
    
    tmp = delta_z_over_a/xi
    C(1) = 1._wp + (tmp)**2 - 2._wp*tmp*eta - (eta/xi)**2
    C(2) = (2._wp/xi)*(tmp-eta)
    C(3) = -2._wp*(delta_z_over_a**2)/xi**3 & 
           +2._wp*delta_z_over_a*eta/xi**2 &
           +2._wp*(eta**2)/(xi**3)
    C(4) = -4._wp*delta_z_over_a/xi**3 &
           +2._wp*eta/xi**2
    C(5) = sqrt(C(1)**2 + C(2)**2)
    C(6) = C(1)*C(3) + C(2)*C(4)
    C(7) = sqrt(C(1)+C(5))
    C(8) = C(3) + C(6)/C(5)
   
    deriv_xi_trans = (C(7) + (C(8)*xi)/(C(7)*2._wp) )/sqrt(2._wp)
    deriv_eta_trans = sqrt(2._wp)*(delta_z_over_a/(C(7)*xi**2) &
                            - (eta - tmp)*C(8)/(2._wp*C(7)**3))

  End subroutine deriv_transformed_xi_eta
  !----------------------------------!

    !----------------------------------------!
Function Legendre_Plm_Aimag(l,m,y)    Result(Plm)
    ! ----------------------------------------------------------
    ! ---  Routine for the calculation of the associated 
    ! ---  Legendre polynomial of the first kind with purely 
    ! ---  imaginary argument z=iy.
    ! ---------------------------------------------------------- 
    Use Error_Module, only : Error_Failure
    Implicit none
    ! INPUT
    Integer,Intent(in)  ::  l,m
    Real(wp),Intent(in) ::  y
    Complex(wp)        ::  Plm

    character(len=*),  parameter :: routine = "Legendre_Plm_Aimag"
    character(len=100)          :: str
    Real(wp)            ::  z
    Complex(wp)        ::  Pmm, P1,P2,P3
    Integer             ::  i,ll
    
    ! --- Error Checking
    If(.not.(m >= 0 .and. m <= l)) Then
       Write(str,fmt='(a,i2,a,i2)') 'l = ',l,';   m= ',m
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    !
    ! --- The main calculation
    z=1+y**2

    select case(m)
    case(0) 
      Pmm = 1._wp                                 !P_0^0
      P2 = imu*y                                  !P_1^0
    case(1)
      Pmm = sqrt(z)                               !P_1^1
      P2 =  3*imu*y*sqrt(z)                       !P_2^1
    case default
       call Error_Failure( routine, "m>1 not supported")
    end select

    if (l==m) then
      Plm = Pmm
    else if (l==(m+1)) then
      Plm = P2
    else !When l>=m+2: Use recurrence relation:
      P1 = Pmm
      do ll = m+2,l
        P3 = ( imu*y*(2*ll-1)*P2 - (ll+m-1)*P1 )/(ll-m)
        P1 = P2
        P2 = P3
      end do
      Plm = P3
    end if
    return
  End Function Legendre_Plm_Aimag
  !----------------------------------------!


  !----------------------------------------!
  Function Legendre_Plm_Aimag_Diff(l,m,y) Result(dPlm)
  !----------------------------------------!
  Implicit none
  integer, intent(in)           :: l,m
  real(wp), intent(in)          :: y
  complex(wp)                   :: dPlm
  
  !character(len=*),  parameter  :: routine = "Legendre_Plm_Aimag_Diff"
  !character(len=100)            :: str

  dPlm = (-imu*(l-m+1)*Legendre_Plm_Aimag(l+1,m,y)- (l+1)*y*Legendre_Plm_Aimag(l,m,y)) &
         /(y**2+1._wp)
  return
  end function Legendre_Plm_Aimag_Diff
  !----------------------------------------!

  !----------------------------------------!
  Function Legendre_Qlm_Aimag(l,m,y)  Result(Qlm)
  !----------------------------------------!    
    ! ----------------------------------------------------------
    ! ---  Routine for the calculation of the associated 
    ! ---  Legendre polynomial of the second kind with purely 
    ! ---  imaginary argument z=iy.
    ! ----------------------------------------------------------
    Use Error_Module, only : Error_Failure 
    Use SFL_Precision, only: qp

    Implicit None
    ! Global
    Integer,      Intent(In)    :: l
    Integer,      Intent(In)    :: m
    Real(wp),     Intent(In)    :: y
    Complex(wp)                 :: Qlm
    ! Local
    character(len=*),  parameter :: routine = "Legendre_Qlm_Aimag"
    Character(len=100)          :: str
    Real(qp)         ::  z,w,y_quad
    Complex(qp)      ::  Qmm, Q1,Q2,Q3
    Integer          ::  i,ll
    

    ! --- Error Checking
    If(.not.(m >= 0 .and. m <= l)) Then
       Write(str,fmt='(a,i2,a,i2)') 'l = ',l,';   m= ',m
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    !
    ! --- The main calculation
    
    y_quad = y
    z=1+y_quad**2
    w=atan(1/y_quad)

    select case(m)
    case(0) 
      Qmm = -imu*w                                            !Q_0^0
      Q2 = y_quad*w - 1._wp                                   !Q_1^0
    case(1)
      Qmm = -sqrt(z)*(w-y_quad/z)                             !Q_1^1
      Q2 = imu*sqrt(z)*((2._wp + 3*y_quad**2)/z-3*y_quad*w)   !Q_2^1
    case default
       call Error_Failure( routine, "m>1 not supported")
    end select
    
    if (l==m) then
      Qlm = Qmm
    else if (l==(m+1)) then
      Qlm = Q2
    else !When l>=m+2: Use recurrence relation:
      Q1 = Qmm
      do ll = m+2,l
        Q3 = ( imu*y_quad*(2*ll-1)*Q2 - (ll+m-1)*Q1 )/(ll-m)
        Q1 = Q2
        Q2 = Q3
      end do
      Qlm = Q3
    end if
    return

  End Function Legendre_Qlm_Aimag
  !------------------------!

  !----------------------------------------!
  Function Legendre_Qlm_Aimag_Diff(l,m,y) Result(dQlm)
  !----------------------------------------!
  Implicit none
  integer, intent(in)           :: l,m
  real(wp), intent(in)          :: y
  complex(wp)                   :: dQlm
  
  !character(len=*),  parameter  :: routine = "Legendre_Qlm_Aimag_Diff"
  !character(len=100)            :: str

  dQlm = (-imu*(l-m+1)*Legendre_Qlm_Aimag(l+1,m,y)- (l+1)*y*Legendre_Qlm_Aimag(l,m,y)) &
         /(y**2+1._wp)
  return
  end function Legendre_Qlm_Aimag_Diff
  !----------------------------------------!

  !------------------------!
  Function X_Oblate(l,m,xi)    Result(X)
  !------------------------!
  ! X should always be real
    Implicit None
    Real(wp)            ::  X
    Real(wp),Intent(in) ::  xi
    Integer,Intent(in)  ::  l,m
    real(wp)            :: num
    real(wp)            :: denom
    real(wp)            :: factor
    integer             :: i

    denom   = 1._wp
    If(m>0) Then
      do i=0,(m-1)
        denom = denom*(l-i)
      end do
    Endif
    
    num = 1._wp
    Do i=1,l
       num = num*i/(2*i-1._wp)
    Enddo
 
    factor = num/denom

    X = Real( imu**(m-l)*factor*Legendre_Plm_Aimag(l,m,xi) )
    Return
    
  End Function X_Oblate
  
  !-------------------------------------------------------------------------

  !------------------------!
  Function Z_Oblate(l,m,xi)   Result(Z)
  !------------------------!
  ! Z should always be real
    Implicit None
    Real(wp)            ::  Z
    Real(wp),Intent(in) ::  xi
    Integer,Intent(in)  ::  l,m
    real(wp)            :: num,denom,factor
    integer             :: i

    denom   = 1._wp
    If(m>0) Then
      do i=1,m
        denom = denom*(l+i)
      end do
    Endif
    
    num = 1._wp
    Do i=1,l
       num = num*(2*i+1)/i
    Enddo
 
    factor = num/denom
    
    Z = Real( imu**(l+1)*factor*Legendre_Qlm_Aimag(l,m,xi) )
    Return
    
  !------------------------!
  End Function Z_Oblate

  !------------------------!
  Function X_Oblate_diff(l,m,xi)    Result(dX)
  !------------------------!
  ! X should always be real
    Implicit None
    Real(wp)            ::  dX
    Real(wp),Intent(in) ::  xi
    Integer,Intent(in)  ::  l,m
    real(wp)            :: num
    real(wp)            :: denom
    real(wp)            :: factor
    integer             :: i

    denom   = 1._wp
    If(m>0) Then
      do i=0,(m-1)
        denom = denom*(l-i)
      end do
    Endif
    
    num = 1._wp
    Do i=1,l
       num = num*i/(2*i-1._wp)
    Enddo
 
    factor = num/denom
    
    dX = Real( imu**(m-l)*factor*Legendre_Plm_Aimag_diff(l,m,xi) )
    Return
    
  End Function X_Oblate_diff
  
  !-------------------------------------------------------------------------

  !------------------------!
  Function Z_Oblate_diff(l,m,xi)   Result(dZ)
  !------------------------!
  ! Z should always be real
    Implicit None
    Real(wp)            ::  dZ
    Real(wp),Intent(in) ::  xi
    Integer,Intent(in)  ::  l,m
    real(wp)            :: num
    real(wp)            :: denom
    real(wp)            :: factor
    integer             :: i

    denom   = 1._wp
    If(m>0) Then
      do i=1,m
        denom = denom*(l+i)
      end do
    Endif
    
    num = 1._wp
    Do i=1,l
       num = num*(2*i+1)/i
    Enddo
 
    factor = num/denom

    dZ = Real( imu**(l+1)*factor*Legendre_Qlm_Aimag_diff(l,m,xi) )
    Return
    
  !------------------------!
  End Function Z_Oblate_diff




  !----------------------------------!
  Function Q_Integrand(x) Result(res)
  !----------------------------------!
  !Needs a separate but identical Q-integrand here, 
  !to have a version connected to l1,l1 ann m of this module.
  !
    Use Legendre_Module, only : Legendre_Plm
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
    Use Legendre_Module, only : Legendre_Plm
    Implicit None
    Real(wp), Intent(in)  ::  eta
    Real(wp)              ::  res
    Real(wp)              ::  xi_trans, eta_trans
    ! Global: delta_z_over_a, xi_0, l1, l2, m

    ! Testing
    !complex(wp) :: Z

    call Transformed_xi_eta(delta_z_over_a, xi_0, eta, xi_trans, eta_trans)
    !write(*,*) 'xi_0,eta = ', xi_0,eta
    !write(*,*) 'xi_trans,eta_trans = ', xi_trans,eta_trans

    !write(*,*) 'Legendre_Plm(l1,m,eta) = ', Legendre_Plm(l1,m,eta)
    !write(*,*) 'Legendre_Plm(l2,m,eta_trans) = ', Legendre_Plm(l2,m,eta_trans)
    !Z = Z_Oblate(l2,m,xi_trans)
    !write(*,*) 'Z_Oblate(l2,m,xi_trans) = ',Z

    res   =    Legendre_Plm(l1,m,eta)*Legendre_Plm(l2,m,eta_trans)*Z_Oblate(l2,m,xi_trans)
    Return
  End Function V_Integrand
  !-----------------------!

  !----------------------------------!
  Function W_Integrand(eta) Result(res)
  !----------------------------------!
    Use Legendre_Module, only : Legendre_Plm
    Implicit None
    Real(wp), Intent(in)  ::  eta
    Real(wp)              ::  res
    Real(wp)              ::  xi_trans, eta_trans
    ! Global: delta_z_over_a, xi_0, l1, l2, m

    call Transformed_xi_eta(delta_z_over_a, xi_0, eta, xi_trans, eta_trans)
    res   = Legendre_Plm(l1,m,eta)*Legendre_Plm(l2,m,eta_trans)*X_Oblate(l2,m,xi_trans)
    Return
  End Function W_Integrand
  !-----------------------!

  !----------------------------------!
  Function dV_Integrand(eta) Result(res)
  !----------------------------------!
    Use Legendre_Module, only : Legendre_Plm, Legendre_Plm_Diff
    Implicit None
    Real(wp), Intent(in)  ::  eta
    Real(wp)              ::  res
    Real(wp)              ::  xi_trans, eta_trans, deriv_xi_trans, deriv_eta_trans
    ! Global: delta_z_over_a, xi_0, l1, l2, m

    call Transformed_xi_eta(delta_z_over_a, xi_0, eta, xi_trans, eta_trans)
    call Deriv_transformed_xi_eta(delta_z_over_a, xi_0, eta, deriv_xi_trans, deriv_eta_trans)

    !write(*,*) "xi_trans,eta_trans = ", xi_trans, eta_trans
    !write(*,*) "deriv_xi_trans,deriv_eta_trans = ", deriv_xi_trans, deriv_eta_trans

    res = Legendre_Plm(l1,m,eta) &
          * (deriv_eta_trans*Legendre_Plm_Diff(l2,m,eta_trans)*Z_oblate(l2,m,xi_trans) &
            + deriv_xi_trans*Z_oblate_diff(l2,m,xi_trans)*Legendre_Plm(l2,m,eta_trans)&
            )
    Return
  End Function dV_Integrand
  !-----------------------!

  !----------------------------------!
  Function dW_Integrand(eta) Result(res)
  !----------------------------------!
    Use Legendre_Module, only : Legendre_Plm, Legendre_Plm_Diff 
    Implicit None
    Real(wp), Intent(in)  ::  eta
    Real(wp)              ::  res
    Real(wp)              ::  xi_trans, eta_trans, deriv_xi_trans, deriv_eta_trans
    ! Global: delta_z_over_a, xi_0, l1, l2, m

    call Transformed_xi_eta(delta_z_over_a, xi_0, eta, xi_trans, eta_trans)
    call Deriv_transformed_xi_eta(delta_z_over_a, xi_0, eta, deriv_xi_trans, deriv_eta_trans)
    res = Legendre_Plm(l1,m,eta) &
           * (deriv_eta_trans*Legendre_Plm_Diff(l2,m,eta_trans)*X_oblate(l2,m,xi_trans) &
            + deriv_xi_trans*X_oblate_diff(l2,m,xi_trans)*Legendre_Plm(l2,m,eta_trans)&
            )
    Return
  End Function dW_Integrand
  !-----------------------!



  !--------------------------------------!
  Function Test_Integrand(x) Result(res)
  !--------------------------------------!
    !
    !   For testing'
    !
    Implicit None
    Real(wp)          :: x
    Real(wp)          :: res
    ! --- Local

    res = x**2

    Return
  End Function Test_Integrand
  !--------------------------!


End Module Integrand_Module_Spheroid_Oblate
