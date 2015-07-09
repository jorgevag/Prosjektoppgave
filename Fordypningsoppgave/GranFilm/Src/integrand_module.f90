! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module contains the integrands for the integrals
!     of truncated spherical particles.
!
!     NOTE : The parameters l1,l2,m, and PosMP are exchanged
!            vis global variables in this routine.
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!

Module Integrand_Module



  ! --- The Use Statements global to the module
  Use Shared, only : wp	   

  ! --------------------------------------
  ! --- The Publicly avaiable Parameters
  !     used to exchange info with the
  !     integrands 
  ! --------------------------------------
  Public :: l1
  Public :: l2
  Public :: m
  Public :: PosMP

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Q_Integrand 
  Public :: K_Integrand 
  Public :: L_Integrand 
  Public :: M_Integrand 
  Public :: N_Integrand 

  Public :: Test_Integrand 



  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  ! --- The parameters to exchange when calling the integration routine (QUADPACK)
  ! -------------------------------------------------------------------------------
  Integer  :: l1       ! The l1 "quantum number"
  Integer  :: l2       ! The l2 "quantum number"
  Integer  :: m        ! The m  "quantum number"
  Real(wp) :: PosMP    ! The z-coordinate of the Multipoles or Image MultiPoles
  




Contains



  !----------------------------------!
  Function Q_Integrand(x) Result(res)
  !----------------------------------!
    Use Legendre_Module, only : Legendre_Plm
    Implicit None
    Real(wp), Intent(in)  ::  x
    Real(wp)              ::  Res

    res  =  Legendre_Plm(l1,m,x) * Legendre_Plm(l2,m,x)

    Return
  End Function Q_Integrand
  !----------------------------------!




  !----------------------------------!
  Function K_Integrand(x) Result(res)
  !----------------------------------!
    Use Legendre_Module, only : Legendre_Plm
    Implicit None
    Real(wp), Intent(in)  ::  x
    Real(wp)              ::  res
    !  --- Local
    Real(wp)  :: RR, RR2, chi, eta

    ! -- Some conatntas
    call Get_Internal_Parameters(x, RR, RR2, chi, eta)

    ! --- The function calculation
    res   =    Legendre_Plm(l1,m,x)         &
             * Legendre_Plm(l2,m,chi)       &
             * RR**(-(l2+1._wp) )

    Return
  End Function K_Integrand
  !-----------------------!




  !----------------------------------!
  Function L_Integrand(x) Result(res)
  !----------------------------------!
    !
    !   alpha = -\ell'-1
    !
    Use Legendre_Module, only : Legendre_Plm, Legendre_Plm_Diff
    Implicit None
        Real(wp), Intent(in)  ::  x
    Real(wp)                  ::  res
    ! --- Local
    Real(wp)  :: RR, RR2, chi, eta


    ! -- Some conatntas
    call Get_Internal_Parameters(x, RR, RR2, chi, eta)

    ! --- The function calculation
    res  =  Legendre_Plm(l1,m,x) * RR**(-l2-3)                                   &
            *( -(l2+1) *(1._wp-eta*x)              * Legendre_Plm(l2,m,chi)      &
               +(x*RR - (x-eta)*(1._wp-eta*x)/RR ) * Legendre_Plm_Diff(l2,m,chi) &
             )

!    res  =  Legendre_Plm(l1,m,x)                                                                  &
!            *(-(l2+1._wp) * RR2**(-0.5_wp*l2-1.5_wp)*(1._wp-eta*x) * Legendre_Plm(l2,m,chi)       &
!              + RR2**(-0.5_wp*l2-2._wp)*(x*RR2-(x-eta)*(1._wp-eta*x))*Legendre_Plm_Diff(l2,m,chi) &
!             )

    Return
  End Function L_Integrand
  !-----------------------!




  !---------------------------------!
  Function M_Integrand(x) Result(res)
  !---------------------------------!
    Use Legendre_Module, only : Legendre_Plm
    Implicit None
        Real(wp), Intent(in)  ::  x
    Real(wp)                  :: res
    ! --- Local
    Real(wp)  :: RR, RR2, chi, eta


    ! -- Some conatntas
    call Get_Internal_Parameters(x, RR, RR2, chi, eta)

    ! --- The function calculation
    res    =   Legendre_Plm(l1,m,x)     &
             * Legendre_Plm(l2,m,chi)   &
             * RR**l2

    Return
  End Function M_Integrand
  !-----------------------!





  !----------------------------------!
  Function N_Integrand(x) Result(res)
  !----------------------------------!
    !
    !   alpha = \ell'
    !
    Use Legendre_Module, only : Legendre_Plm, Legendre_Plm_Diff
    Implicit None
    Real(wp)          :: x
    Real(wp)          :: res
    ! --- Local
    Real(wp)  :: RR, RR2, chi, eta

    ! -- Some conatntas
    call Get_Internal_Parameters(x, RR, RR2, chi, eta)

    ! --- The function calculation
    res = Legendre_Plm(l1,m,x) * RR**(l2-2)                                     &
          *(  l2 * (1._wp-eta*x)                  * Legendre_Plm(l2,m,chi)      &
            + ( x*RR - (x-eta)*(1._wp-eta*x)/RR ) * Legendre_Plm_Diff(l2,m,chi) &
           ) 
!    res = Legendre_Plm(l1,m,x)                                                                  &
!          *( l2 * RR2**(0.5_wp*l2-1._wp) * (1._wp-eta*x) * Legendre_Plm(l2,m,chi)               &
!             +RR2**(0.5_wp*l2-1.5_wp)*(x*RR2-(x-eta)*(1._wp-eta*x))*Legendre_Plm_Diff(l2,m,chi) &
!           ) 

    Return
  End Function N_Integrand
  !-----------------------!



  ! ----------------------------------------------------------
  ! ---- Some Privte functions
  ! ----------------------------------------------------------



  !-------------------------------------------------------!
  Subroutine Get_Internal_Parameters(x, RR, RR2, chi, eta)
  !-------------------------------------------------------!
    Implicit none
    Real(wp), Intent(In)  :: x
    Real(wp), Intent(Out) :: RR   ! Radius
    Real(wp), Intent(Out) :: RR2  ! Radius-squared
    Real(wp), Intent(Out) :: chi  ! cos-related function
    Real(wp), Intent(Out) :: eta  ! The MultiPole/ImageMultiPole Position

    eta = PosMP
    RR2 = 1._wp - 2*x*eta + eta**2 
    RR  = sqrt( RR2 )
    chi = ( x - eta ) / RR
    
  End Subroutine Get_Internal_Parameters
  !-------------------------------------!




!!$
!!$
!!$
!!$
!!$
!!$  !----------------------------------!
!!$  Function Q_Integrand(x) Result(res)
!!$  !----------------------------------!
!!$    Use Legendre_Module, only : Legendre_Plm
!!$    !Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp), Intent(in)  ::  x
!!$    Real(wp)              ::  Res
!!$    !res  =  AssLegPoly1(l1,m,x) * AssLegPoly1(l2,m,x)
!!$    res  =  Legendre_Plm(l1,m,x) * Legendre_Plm(l2,m,x)
!!$
!!$    Return
!!$  End Function Q_Integrand
!!$  !----------------------------------!
!!$
!!$
!!$  !----------------------------------!
!!$  Function K_Integrand(x) Result(res)
!!$  !----------------------------------!
!!$    Use Legendre_Module, only : Legendre_Plm
!!$    Use Special_Functions_Module
!!$    Implicit None
!!$    Real(wp)        ::  x
!!$    Real(wp)        ::  res
!!$    !  --- Local
!!$    Real(wp)        ::  chi, gamma
!!$
!!$    chi   =   chi_func(x, PosMP )
!!$    gamma =   gamma_func(x, PosMP )
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
!!$    Real(wp)           ::  x
!!$    Real(wp)           ::  res
!!$    ! --- Local
!!$    Real(wp)           ::  mu,chi,gamma
!!$
!!$    mu     =  PosMP
!!$    chi    =  chi_func(x, mu )
!!$    gamma  =  gamma_func(x, mu )
!!$
!!$    res         =          &
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
!!$    Real(wp)           :: chi,gamma
!!$
!!$    chi    =  chi_func(x, PosMP )
!!$    gamma  =  gamma_func(x, PosMP )
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
!!$    Real(wp)          :: mu, chi, gamma
!!$
!!$    mu      =  PosMP
!!$    chi     =  chi_func(x, PosMP )
!!$    gamma   =  gamma_func(x, PosMP)
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
!!$
!!$

  

  ! ----------------------------------------------------------
  ! ---- Some Privte functions
  ! ----------------------------------------------------------
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
!!$
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


End Module Integrand_Module
