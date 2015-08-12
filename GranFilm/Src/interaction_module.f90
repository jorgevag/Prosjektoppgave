! -----------------------------------------------------------------------
! $Id:$
! -----------------------------------------------------------------------

!
! -----------------------------------------------------------------------
! 
! --- PURPOSE
!
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! -----------------------------------------------------------------------
!


!------------------------!
Module Interaction_Module
!------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, imu, pi, param	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Lattice_Sum
  !Public :: Renorm_Polarizability



  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private







  


!-------!
Contains
!-------!



  !-----------------------------------------!
  Function Lattice_Sum(d_mu, n)    Result(S)
  !-----------------------------------------!
    !
    !--------------------------------------------------------
    ! See Physica A143, 164 (1987); Eq. (3.4)
    ! Bedeaux Chapter 7 (ver. 2) :  Eqs. (7.14) and (7.63)
    !--------------------------------------------------------

    !--------------------------------------------------------
    ! d_mu = distance between a multipole and the surface
    !--------------------------------------------------------
    !
    Use Error_Module,              only : Error_Failure
    Use Supported_Options_Module,  only : LATTICE_TYPE_OPTION_TABEL
    Implicit None
    Real(wp),  Intent(In) :: d_mu
    Integer,   Intent(In) :: n
    Real(wp)              :: S
    ! --- Local
    Character(len=*), parameter :: routine = "Lattice_Sum"
    Character(len=100)          :: str
    Real(wp)                    :: d, norm
    
    
    !-----------------------------------------------
    ! d=distance between a multipole and its image
    ! normalized (dimensionless) by L
    !----------------------------------------------- 

    ! Nature of the network
    Select Case( Param%Interaction%Lattice_Type )

    Case( LATTICE_TYPE_OPTION_TABEL(1) )
       ! --- Square
       ! ------------------
       d =  2 * d_mu * param%Geometry%Radius(1) / param%Interaction%Lattice_Constant
       S =  Lattice_Sum_square( d, n )

    Case( LATTICE_TYPE_OPTION_TABEL(2) )
       ! --- Hexagonal
       ! ------------------
       d =  2 * d_mu * param%Geometry%Radius(1) / param%Interaction%Lattice_Constant
       S =  Lattice_Sum_hexagonal( d, n )

    !Case('MFT','RPT')
    !   ! Normalization factor
    !   norm  =       1._wp/Sqrt(param%Interaction%Derived%Density)/(2*param%Island%Derived%Radius_Apparent)
    !   ! Dimensionless by normalization with 2*Rapp
    !   d     =       2*d_mu*param%Island%Radius/(2*param%Island%Derived%Radius_Apparent)
    !   S     =       2*pi*norm**(n-1)*Ir_random(param%Island%Derived%Radius_Apparent,d,n)

    Case Default
       str = "Option Lattice_Type="// trim(adjustl(Param%Interaction%Lattice_Type))//"not supported!"
       Call Error_Failure( routine, str )
    End Select


 !--------!   
  Contains
 !--------!   


    !-----------------------------------------------------------------------------------------------------------------------
    ! Lattice_Sum_square : Lattice sum in the particle/particle interaction on a square lattice
    !-----------------------------------------------------------------------------------------------------------------------
    Function Lattice_Sum_square(x,l)  Result(LattSum)
      Implicit None
      Real(wp), Intent(in)        :: x
      Integer,  Intent(in)        :: l
      Real(wp)                    :: LattSum
      ! --- Local
      Integer,  Parameter         :: nlevel = 3000
      Real(wp), Parameter, Dimension(2:4)    ::  Zeta_Function = &
           (/ 1.644934066848226436472415166646025189_wp,         &
              1.202056903159594285399738161511449990_wp,         &
              1.082323233711138191516003696541167902_wp   /)
      !
      Integer                     :: n, m
      Real(wp)                    :: x2, r
      Real(wp)                    :: LattSum_1, LattSum_2, dLattSum
      Real(wp)                    :: C_n, Zeta_n
      Real(wp), Parameter         :: dLattSum_Acc = 0.0000001_wp ! = 1e-7
      
      ! Convergence trick of Bedeaux/Vlieger's book (ver. 2) Eq. (7.125) and (7.126); page 215. 
      x2          = x**2
      LattSum     = 0._wp
      LattSum_1   = 0._wp
      LattSum_2   = 0._wp
      Zeta_n      = 0._wp
      do n = 1, nlevel
         ! Sum over 1/8 of the square exlcuding border
         C_n = 0._wp
         do m = 1, n-1
            r   = sqrt(n**2 + m**2 + x2)
            C_n = C_n + Arg_Lattice_Sum(l,x,r)
         enddo
         ! Symmetry 2
         C_n = 2*C_n
         ! Add the diagonal term
         r   = sqrt(2*n**2 + x2)
         C_n = C_n + Arg_Lattice_Sum(l,x,r)
         ! Add the n-axis term
         r   = sqrt(n**2 + x2)
         C_n = C_n + Arg_Lattice_Sum(l,x,r)
         ! Convergence trick
         LattSum_1   = LattSum_1 + C_n
         Zeta_n      = Zeta_n + 1._wp/(n**l)
         LattSum_2   = LattSum_1 + C_n * n**l * (Zeta_Function(l) - Zeta_n)
         ! Check the convergence
         dLattSum = abs(LattSum-LattSum_2)/max(abs(LattSum),dLattSum_Acc)
         LattSum  = LattSum_2
         if( dLattSum<dLattSum_Acc ) exit
      Enddo
      
      ! Symmetry 4 of the square lattice
      LattSum = 4*LattSum

      ! Prefactor of spherical harmonics
      LattSum = LattSum*Sqrt((2*l+1._wp)/(4*pi))
      
    End Function Lattice_Sum_square
    !--------------------------------------------------------------------------------------!


    !-----------------------------------------------------------------------------------------------------------------------
    ! Sum_hexagonal : Lattice sum in the particle/particle interaction on an hexagonal lattice
    !-----------------------------------------------------------------------------------------------------------------------
    Function Lattice_Sum_hexagonal(x,l)  Result(LattSum)
      Implicit None
      Real(wp), Intent(in)        :: x
      Integer,  Intent(in)        :: l
      Real(wp)                    :: LattSum
      ! --- Local 
      Integer, Parameter          :: nlevel = 3000
      Real(wp), Parameter, Dimension(2:4)    ::  Zeta_Function = &
           (/ 1.644934066848226436472415166646025189_wp,         &
              1.202056903159594285399738161511449990_wp,         &
              1.082323233711138191516003696541167902_wp   /)
      !
      Integer                     :: n, m
      Real(wp)                    :: x2, r
      Real(wp)                    :: LattSum_1, LattSum_2, dLattSum
      Real(wp)                    :: C_n, Zeta_n
      Real(wp), Parameter         :: dLattSum_Acc = 0.0000001_wp ! = 1e-7
      
      ! Convergence trick of Bedeaux/Vlieger book's (ver. 2) Eqs. (7.125) and (7.126); page 215. 
      x2          = x**2
      LattSum     = 0._wp
      LattSum_1   = 0._wp
      LattSum_2   = 0._wp
      Zeta_n      = 0._wp
      do n = 1, nlevel
         ! Sum over 1/6 of the hexagon
         C_n = 0._wp
         do m = 0, n-1 
            r   = sqrt(n**2 + m**2 - m*n + x2)
            C_n = C_n + Arg_Lattice_Sum(l,x,r)
         enddo
         ! Convergence trick
         LattSum_1   = LattSum_1 + C_n
         Zeta_n      = Zeta_n + 1._wp/(n**l)
         LattSum_2   = LattSum_1 + C_n * n**l * (Zeta_Function(l) - Zeta_n)
         ! Check the convergence
         dLattSum = abs(LattSum-LattSum_2)/max(abs(LattSum),dLattSum_Acc)
         LattSum  = LattSum_2
         if( dLattSum < dLattSum_Acc ) exit
      enddo
      
      ! Symmetry 6 of the hexagonal lattice
      LattSum = 6*LattSum
      
      ! Prefactor of spherical harmonics
      LattSum = LattSum*sqrt((2*l+1._wp)/(4*pi))
      
    End Function Lattice_Sum_hexagonal
    !----------------------------------------------------------------------------------------

    !---------------------------------------------------------------!
    ! Arg_Lattice_Sum : Argument of the lattice sum 
    !---------------------------------------------------------------!
    Function Arg_Lattice_Sum(l,d,r)  Result(Arg)
      Implicit None
      Integer,  intent(in)  :: l      
      Real(wp), intent(in)  :: d
      Real(wp), intent(in)  :: r
      Real(wp)              :: Arg  
      ! --- Local
      Real(wp)              :: cosi
      
      cosi = d/r
      !
      Select Case(l)
      Case(2)
         ! P(2,0)/r**3
         Arg = ( 3*cosi**2 - 1._wp)/2/(r**3)
      Case(3)
         ! P(3,0)/r**4
         Arg = ( 5*cosi**3 - 3*cosi)/2/(r**4)
      Case(4)
         ! P(4,0)/r**5
         Arg = ( 35*cosi**4 -30*cosi**2 + 3._wp)/8/(r**5) 
      Case default
         Stop " Internal Error : Illegal value of l"
      End Select

    End Function Arg_Lattice_Sum
    !--------------------------------------------------------------!



  End Function Lattice_Sum
  !-----------------------!







!!$
!!$
!!$  !-------------------------------------------!
!!$  Subroutine Renorm_Polarizability(d_mu,alpha)
!!$  !-------------------------------------------!
!!$    !
!!$    !------------------------------------------------------
!!$    ! Renormalization of the polarizabibities by the scheme
!!$    ! of Barrera PRB 43-17 P13819 (1991)
!!$    !------------------------------------------------------
!!$    !
!!$    Implicit None
!!$    Real(wp)                  ::   d_mu
!!$    Complex(wp )              ::   alpha(2,param%Numerics%NEnergy)
!!$    ! --- Local
!!$    Real(wp),Parameter        ::   epsilon=0.000001_wp  != 1e-6
!!$    Integer, Parameter        ::   nitmax=35
!!$    Integer                   ::   N,ienergy
!!$    Integer                   ::   nit
!!$    Real(wp)                  ::   Integral(6)
!!$    Real(wp)                  ::   d,phi,error(2),norm
!!$    Complex(wp )              ::   pol_old(2),pol_new(2)
!!$    Complex(wp)               ::   Fxx,Fzz,Fxz
!!$    Complex(wp )              ::   e1,e2,eps
!!$
!!$
!!$    ! Some energy independent abbreviations
!!$    N           =       param%Numerics%NEnergy
!!$    phi         =       param%Interaction%Coverage
!!$    norm        =       param%Island%Radius/param%Island%Derived%Radius_Apparent
!!$
!!$    ! Dimensionless polarizability
!!$    alpha(:,:)  =       alpha(:,:)*norm
!!$
!!$    ! Dimensionless distance between multipoles and surface
!!$    d           =       d_mu*norm
!!$
!!$
!!$    ! Evaluation of the integrals Fxx,Fzz,Fxz
!!$    Call Integral_RPT(d,param%Island%Derived%Radius_Apparent,Integral)
!!$
!!$    ! Self consistent evaluation of the renormalized polarizabilities
!!$    Do ienergy=1,N
!!$       e1                       =       param%Materials%Epsilon%Ambient(ienergy) 
!!$       e2                       =       param%Materials%Epsilon%Substrate(ienergy)  ! param%Misc%Eps_Substrate(ienergy)
!!$       If(param%Numerics%MP_Above) Then
!!$          alpha(:,ienergy)      =       alpha(:,ienergy)/(4*pi*e1)
!!$          eps                   =       (e2-e1)/(e2+e1)
!!$       Else
!!$          alpha(:,ienergy)      =       alpha(:,ienergy)/(4*pi*e2)
!!$          eps                   =       (e1-e2)/(e2+e1)
!!$       Endif
!!$
!!$       Fxx    =       5*phi*( Integral(1)-2*eps*Integral(2)+eps**2*Integral(3) )/4
!!$       Fzz    =       phi*( Integral(1)+2*eps*Integral(4)+eps**2*Integral(5) )/2
!!$       Fxz    =       -9*phi*( eps**2*Integral(6) )/16
!!$
!!$       !index  1 : parallel component
!!$       !       2 : perpendicular component
!!$       pol_old(:)     =       alpha(:,ienergy)
!!$       error(:)       =       1._wp
!!$       nit            =       1
!!$       Do While (error(1)>=epsilon.or.error(2)>=epsilon)
!!$          pol_new(1) = 1._wp+Fxx/4*pol_old(1)**2+Fxz*pol_old(1)*pol_old(2)
!!$          pol_new(2) = 1._wp+2*Fxz*pol_old(1)*pol_old(2)+Fzz*pol_old(2)**2/4
!!$          pol_new(:) = pol_new(:)*alpha(:,ienergy)
!!$          error(:)   = Abs((pol_new(:) - pol_old(:))/pol_old(:))
!!$          pol_old(:) = pol_new(:)
!!$          nit        = nit+1
!!$          If(nit>=nitmax) Goto 10 
!!$       Enddo
!!$
!!$       ! Renormalized polarizabilities
!!$10     If(param%Numerics%MP_Above) Then
!!$          alpha(:,ienergy) = 4*pi*e1*pol_old(:)/norm
!!$       Else
!!$          alpha(:,ienergy) = 4*pi*e2*pol_old(:)/norm
!!$       Endif
!!$
!!$    Enddo
!!$
!!$  End Subroutine Renorm_Polarizability
!!$  !-----------------------------------!
!!$  
!!$
!!$
!!$
!!$
!!$
!!$  !---------------------------------------!
!!$  Subroutine Integral_RPT(d,Rapp,Integral)
!!$  !---------------------------------------!
!!$    !
!!$    !-----------------------------------------------------
!!$    ! Routine for evaluation of the integrals Fxx Fxz Fzz
!!$    ! in the Barrera's renormalization scheme
!!$    ! PRB 43-17 (1991)
!!$    !-----------------------------------------------------
!!$    !
!!$    Use Quadpack_mod, only : Quadpack_I 
!!$    Implicit None
!!$    Real(wp), Intent(In)          ::      d,Rapp
!!$    Real(wp), Intent(InOut)       ::      Integral(6)
!!$    Real(wp)                      ::      Rapparent,Ddipole               
!!$
!!$
!!$    Common /RDcom/ Rapparent,Ddipole
!!$    Rapparent       = Rapp
!!$    Ddipole         = d
!!$
!!$    ! The integrals         
!!$    ! Integral(1)
!!$    Call Quadpack_I(Integrand_RPT1,1._wp,Integral(1))
!!$    ! Integral(2)
!!$    Call Quadpack_I(Integrand_RPT2,1._wp,Integral(2))
!!$    ! Integral(3)   
!!$    Call Quadpack_I(Integrand_RPT3,1._wp,Integral(3))
!!$    ! Integral(4)
!!$    Call Quadpack_I(Integrand_RPT4,1._wp,Integral(4))
!!$    ! Integral(5)
!!$    Call Quadpack_I(Integrand_RPT5,1._wp,Integral(5))
!!$    ! Integral(6)
!!$    Call Quadpack_I(Integrand_RPT6,1._wp,Integral(6))
!!$
!!$  End Subroutine Integral_RPT
!!$  !--------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Function Integrand_RPT1(x) Result(y)
!!$  !-----------------------------------!  
!!$    Implicit None
!!$    Real(wp), Intent(In)           ::      x
!!$    Real(wp)                       ::      y
!!$    Real(wp)                       ::      Rapp,d
!!$
!!$    Common /RDcom/ Rapp,d
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    y = y/(x**5)
!!$
!!$  End Function Integrand_RPT1
!!$  !--------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Function Integrand_RPT2(x) Result(y)
!!$  !-----------------------------------!
!!$    Implicit None
!!$    Real(wp), Intent(In)            ::      x
!!$    Real(wp)                        ::      y
!!$    Real(wp)                        ::      Rapp,d,r
!!$
!!$    Common /RDcom/ Rapp,d
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    r = Sqrt(x**2+d**2)
!!$    y = y*(x**2-d**2/5)/(x**2)/(r**5)
!!$
!!$  End Function Integrand_RPT2
!!$  !-----------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Function Integrand_RPT3(x)  Result(y)
!!$  !-----------------------------------!
!!$    Implicit None
!!$    Real(wp), Intent(In)            ::      x
!!$    Real(wp)                        ::      y
!!$    Real(wp)                        ::      Rapp,d,r
!!$
!!$    Common /RDcom/ Rapp,d
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    r = Sqrt(x**2+d**2)
!!$    y = y*x*( x**4-2*(d*x)**2/5+2*d**4/5 )/(r**10)
!!$
!!$  End Function Integrand_RPT3
!!$  !-----------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!  
!!$  Function Integrand_RPT4(x) Result(y)
!!$  !-----------------------------------!
!!$    Implicit None
!!$    Real(wp), Intent(In)   ::   x
!!$    Real(wp)               ::   y
!!$    !  --- Local
!!$    Real(wp)               ::   Rapp,d,r
!!$
!!$    Common /RDcom/ Rapp,d
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    r = Sqrt(x**2+d**2)
!!$    y = y*(x**2-2*d**2)/(x**2)/(r**5)
!!$
!!$  End Function Integrand_RPT4
!!$  !-----------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Function Integrand_RPT5(x) Result(y)
!!$  !-----------------------------------!
!!$    Implicit None
!!$    Real(wp), Intent(In)  ::  x
!!$    Real(wp)              ::  y
!!$    !  --- Local
!!$    Real(wp)              ::  Rapp,d,r
!!$
!!$    Common /RDcom/ Rapp,d
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    r = Sqrt(x**2+d**2)
!!$    y = y*x*(x**4-4*(d*x)**2+4*d**4)/(r**10)
!!$
!!$  End Function Integrand_RPT5
!!$  !-----------------------------------!
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Function Integrand_RPT6(x)  Result(y)
!!$  !-----------------------------------!
!!$   Implicit None
!!$    Real(wp), Intent(In)  ::  x
!!$    Real(wp)              ::  y
!!$    ! --- Local
!!$    Real(wp)              ::  Rapp,d,r
!!$
!!$    Common /RDcom/ Rapp,d
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    r = Sqrt(x**2+d**2)
!!$    y = y*d**2*x**3/(r**10)
!!$
!!$  End Function Integrand_RPT6
!!$  !-----------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !----------------------------------------!
!!$  Function Ir_random(Rapp,d,l)  Result(Ir)
!!$  !----------------------------------------!
!!$    Use Quadpack_mod, only : Quadpack_I 
!!$    Implicit None
!!$    Real(wp), Intent(In)        ::      d,Rapp
!!$    Integer, Intent(In)         ::      l
!!$    Real(wp)                    ::      Ir                                      
!!$    ! Local
!!$    Real(wp)                    ::      Rapparent,Ddipole
!!$    Integer                     ::      lindex
!!$
!!$    Common /Ir_random_com/ Rapparent,Ddipole,lindex
!!$    Rapparent       =       Rapp
!!$    Ddipole         =       d
!!$    lindex          =       l
!!$
!!$    ! Formulas 10-13 10-14 + P269
!!$    ! The integral is dimensionless (unit 'L/2*Rapp')
!!$    Call Quadpack_I(Ir_random_integrand,1._wp,Ir)
!!$    Ir =    Ir*Sqrt( (2*l+1._wp)/(4*pi) )
!!$
!!$  End Function Ir_random
!!$  !---------------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------------!
!!$  Function Ir_random_integrand(x)  Result(y)
!!$  !-----------------------------------------!
!!$    Implicit None
!!$    Real(wp), Intent(In)  ::      x
!!$    Real(wp)              ::      y
!!$    ! Local
!!$    Real(wp)              ::      Rapp, d
!!$    Integer               ::      l
!!$
!!$    Common /Ir_random_com/ Rapp,d,l
!!$
!!$    y = Pair_Correlation(2._wp*Rapp*x)
!!$    y = y*x*Argument(l,d,Sqrt(x**2+d**2))
!!$
!!$  End Function Ir_random_integrand
!!$  !-----------------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$

!!$
!!$
!!$  !-------------------------------------!
!!$  Function Pair_Correlation(x)  Result(g)
!!$  !-------------------------------------!
!!$    ! Unit : nanometer like Rapp (dimension)
!!$    Implicit None
!!$    Real(wp)    ::      x, g
!!$
!!$    x = x 
!!$    g = 1._wp
!!$
!!$  End Function Pair_Correlation
!!$  !-------------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Subroutine Get_Dielectric_Thin_Layer
!!$  !-----------------------------------!
!!$    !
!!$    !----------------------------------------------------------------------------
!!$    ! Calculates the  dielectric constant of a substrate+layer
!!$    ! equivalent in terms of dipolar coucpling
!!$    !----------------------------------------------------------------------------
!!$    !
!!$    Use Quadpack_mod,         Only  : Quadpack_I
!!$    Implicit None
!!$    Integer      ::  ienergy,RI
!!$    Complex(wp)  ::  e1,e2,e3,A12,A23,imgs
!!$    Real(wp)     ::  d,t
!!$    Real(wp)     ::  tmp(2)
!!$
!!$    Common /dielectric/ A12,A23,d,t,RI
!!$
!!$    ! Some abreviations
!!$    d   =  Abs(param%Island%Truncation_Ratio)
!!$    t   =  param%Island%Coating_Thickness/param%Island%Radius
!!$
!!$    ! Loop over energy                      
!!$    Do ienergy=1,param%Numerics%NEnergy
!!$       e1   =   param%Materials%Epsilon%Ambient(ienergy) 
!!$       e2   =   param%Materials%Epsilon%Coating(ienergy)        ! param%Misc%Eps_Coating(ienergy)
!!$       e3   =   param%Materials%Epsilon%Substrate_file(ienergy) !param%Misc%Eps_Substrate_file(ienergy)
!!$       A12  =   (e2-e1)/(e1+e2)
!!$       A23  =   (e3-e2)/(e2+e3)
!!$       RI   =   1
!!$       Call Quadpack_I(Ik,0._wp,tmp(1))
!!$       RI   =   2
!!$       Call Quadpack_I(Ik,0._wp,tmp(2))               
!!$       imgs =   tmp(1)+imu*tmp(2)
!!$       !param%Misc%Eps_Substrate(ienergy) = e1*(1+4*d**3*imgs)/(1-4*d**3*imgs)
!!$       param%Materials%Epsilon%Substrate(ienergy)  = e1*(1+4*d**3*imgs)/(1-4*d**3*imgs)
!!$    Enddo
!!$
!!$  End Subroutine Get_Dielectric_Thin_Layer
!!$  !----------------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$  !-----------------------------------!
!!$  Function Ik(k)  Result(I)
!!$  !-----------------------------------!
!!$    !
!!$    ! Function to get the depolarization factor for an ellipsoid on
!!$    ! a layered structure
!!$    !
!!$    Implicit None
!!$    Real(wp)        ::      k,I
!!$    Complex(wp)     ::      A12,A23,tmp
!!$    Real(wp)        ::      d,t
!!$    Integer         ::      RI
!!$
!!$    Common /dielectric/ A12,A23,d,t,RI
!!$
!!$    tmp     = k**2*exp(-2*k*d)*(A12+A23*exp(-2*k*t))  &
!!$         /(1._wp+A23*A12*exp(-2*k*t))
!!$
!!$    Select Case(RI)
!!$       ! Real part
!!$    Case(1)
!!$       I  =  Real(tmp,wp)
!!$       ! Imaginary part
!!$    Case(2)
!!$       I  =  Aimag(tmp)
!!$    End Select
!!$    Return
!!$
!!$  End Function Ik
!!$  !-----------------------------------!
!!$
!!$




End Module Interaction_Module
!-----------------------------!



