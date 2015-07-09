! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

! ----------------------------------------------------------
! 
! --- PURPOSE
! Module for calculating the normal derivative (d/dr or d/d(xi)) at the points
! specified by a points-file. The derivatives are used to check that the 
! second boundary condition holds.
! 
! --- AUTHOR : Sindre Stavseng, Trondheim, 2013.
!
! ----------------------------------------------------------
!
!----------------------------------!
Module Potential_Derivatives_Module
!----------------------------------!

  ! --- The Use Statements global to the module
  Use Shared
  Use Error_Module,                     only : Error_Failure, Error_Warning

  ! --------------------------------------
  ! --- The Publicly available routines
  ! --------------------------------------
  Public :: Get_potential_derivatives_prolate
  Public :: Get_potential_derivatives_oblate
  Public :: Get_potential_derivatives_sphere
  Public :: write_derivatives_to_file

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  ! --- Module-global data



!-------!
Contains
!-------!

  !-----------------------------------------------------------------------------------------------------!
  Subroutine Get_potential_derivatives_prolate(x,y,z,Npts,iMedium,nMedia,eps,system_dim,coeff,diff_pot)
  !-----------------------------------------------------------------------------------------------------!
    ! --------------------------------------------------------------------------------------------!
    ! Calculates the normal (xi) derivative of the potential at the points specified. 
    ! 
    ! FOR PROLATE SPHEROIDS
    !
    ! Written by:
    ! Sindre Stavseng, Trondheim, March 2013
    ! --------------------------------------------------------------------------------------------!

    Use Tools_Module, only : deg2rad
    Use Integrand_Module_Spheroid_Prolate, only : Deriv_transformed_xi_eta
    Implicit None
    Integer, intent(in)         :: Npts, iMedium(Npts), nMedia, system_dim
    Real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    Complex(wp), intent(in)     :: eps(nMedia), coeff(system_dim,0:1)
    Complex(wp), intent(out)    :: diff_pot(Npts)
    ! --- Local variables ---------------
    Integer                     :: Incident_Medium, Expansion_Medium, Mp, &
                                   iMed, offset, i, l
    Real(wp)                    :: a, R1, mu_z, mubar_z, xi_mu(Npts), eta_mu(Npts), phi_mu(Npts),&
                                   xi_mubar(Npts), eta_mubar(Npts), phi_mubar(Npts), deriv_xi_mubar, &
                                   deriv_eta_mubar, thetaE, phiE, sinthetaE,costhetaE
    Complex(wp)                 :: expi2phiE, R, T, epsfrac, expiphiE,expMiniphiE
    
    write(*,*) "***Calculating potential normal derivatives..."

    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    Mp = param%numerics%multipole_order
    a = sqrt(Param%Geometry%Radius(1)**2 - Param%Geometry%Radius(2)**2)
    R1 = param%geometry%radius(1)
    expi2phiE = exp(imu * 2.0_wp * param%source%PhiE * deg2rad())
    ! Getting prolate spheroidal coordinates at multipole and image multipole.
    mu_z = param%numerics%multipole_position_ratio*param%geometry%radius(1)
    mubar_z = 2.0_wp*param%geometry%distance(1) - mu_z
    call Cartesian_to_Spheroidal_Prolate(x,y,z,Npts,a,mu_z,xi_mu,eta_mu,phi_mu) 
    call Cartesian_to_Spheroidal_Prolate(x,y,z,Npts,a,mubar_z,xi_mubar,eta_mubar,phi_mubar) 

    ! For the incident and transmitted terms:
    epsfrac = eps(1)/eps(2)
    thetaE = param%source%ThetaE * deg2rad()
    phiE = param%source%PhiE * deg2rad()
    sinthetaE = sin(thetaE)
    costhetaE = cos(thetaE)
    expiphiE = exp(imu * param%source%PhiE * deg2rad())
    expMiniphiE = exp(- imu * param%source%PhiE * deg2rad())

    Do i=1,Npts

       diff_pot(i) = 0.0_wp
       iMed = iMedium(i)
       offset = Param%Numerics%MP_Storage_Offset(iMed)
       
       call Deriv_transformed_xi_eta(mubar_z/a, xi_mu(i), eta_mu(i), deriv_xi_mubar, deriv_eta_mubar)

       If (mod(iMed,2)==0) then            ! Below the substrate
          T = (2.0_wp*eps(iMed-1))/(eps(iMed-1)+eps(iMed))
          If ((iMed-1) /= Expansion_Medium) then
             ! A-terms (zero if expansion medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,0)*Z_func_prolate_diff(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))          
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,1)*Z_func_prolate_diff(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1)*Z_func_prolate_diff(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))
             End do
             offset = offset + Mp 
          End if
          If ((iMed-1) /= Incident_Medium) then
             ! B-terms (zero if incident medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,0)*X_func_prolate_diff(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))          
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,1)*X_func_prolate_diff(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1)*X_func_prolate_diff(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))       
             End do
          End if
          diff_pot(i) = T*diff_pot(i)   ! Final prefactor

       Else                                ! Above the substrate
          R = (eps(iMed)-eps(iMed+1))/(eps(iMed)+eps(iMed+1))
          If ((iMed) /= Expansion_Medium) then
             ! A-terms (zero if expansion medium)
             Do l=1,Mp
                !m=0
                ! Not implemented
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,0) * ( &
                           Z_func_prolate_diff(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*( Z_func_prolate_diff(l,0,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,0,eta_mubar(i),phi_mubar(i)) + &
                            Z_func_prolate(l,0,xi_mubar(i),a)*spher_harm_diff(l,0,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,1) * ( &
                           Z_func_prolate_diff(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*( Z_func_prolate_diff(l,1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,1,eta_mubar(i),phi_mubar(i)) + &
                            Z_func_prolate(l,1,xi_mubar(i),a)*spher_harm_diff(l,1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1) * ( &
                           Z_func_prolate_diff(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*( Z_func_prolate_diff(l,-1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) + &
                            Z_func_prolate(l,-1,xi_mubar(i),a)*spher_harm_diff(l,-1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
             End do
             offset = offset + Mp 
          End if
          If ((iMed) /= Incident_Medium) then
             ! B-terms (zero if incident medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,0) * ( &
                           X_func_prolate_diff(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*( X_func_prolate_diff(l,0,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,0,eta_mubar(i),phi_mubar(i)) + &
                            X_func_prolate(l,0,xi_mubar(i),a)*spher_harm_diff(l,0,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,1) * ( &
                           X_func_prolate_diff(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*( X_func_prolate_diff(l,1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,1,eta_mubar(i),phi_mubar(i)) + &
                            X_func_prolate(l,1,xi_mubar(i),a)*spher_harm_diff(l,1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1) * ( &
                           X_func_prolate_diff(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*( X_func_prolate_diff(l,-1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) + &
                            X_func_prolate(l,-1,xi_mubar(i),a)*spher_harm_diff(l,-1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
             End do
          End if
       End if
       
       ! Add incident and transmitted potentials
       If (iMed == Incident_Medium) Then  ! Ambient
          diff_pot(i) = diff_pot(i) + sqrt(2.0_wp*pi/3.0_wp)*( - sqrt(2.0_wp)*costhetaE* &
                                      spher_harm(1,0,eta_mu(i),phi_mu(i))) + sinthetaE*expMiniPhiE * &
                                      X_func_prolate_diff(1,1,xi_mu(i),a)*spher_harm(1,1,eta_mu(i),phi_mu(i))&
                                      - sinthetaE*expiPhiE*X_func_prolate_diff(1,-1,xi_mu(i),a)* &
                                      spher_harm(1,-1,eta_mu(i),phi_mu(i))
       Else if (iMed == 2) Then  ! Substrate
          diff_pot(i) = diff_pot(i) + sqrt(2.0_wp*pi/3.0_wp)*( - epsfrac* sqrt(2.0_wp)*costhetaE* &
                                      spher_harm(1,0,eta_mu(i),phi_mu(i))) + sinthetaE*expMiniPhiE * &
                                      X_func_prolate_diff(1,1,xi_mu(i),a)*spher_harm(1,1,eta_mu(i),phi_mu(i))&
                                      - sinthetaE*expiPhiE*X_func_prolate_diff(1,-1,xi_mu(i),a)* &
                                      spher_harm(1,-1,eta_mu(i),phi_mu(i))
       End if
       
       diff_pot(i) = diff_pot(i)*eps(iMed)
       
    Enddo
    

    ! --- Internal functions ------------------------------------
  contains
    ! -----------------------------------------------------------

    !--------------------------------!
    function X_func_prolate(l,m,xi,a)
    !--------------------------------!
      ! Prolate X function with dimensions
      ! Only for m = -1, 0, 1
      Use Integrand_Module_Spheroid_Prolate, only : Legendre_Plm_xGrOne
      implicit none
      complex(wp)           :: X_func_prolate
      integer, Intent(In)   :: l, m
      real(wp), Intent(In)  :: xi, a 
      integer               :: j
      real(wp)              :: factor

      !Calculate factorial prefactor
      factor = 1._wp
      do j = 1, l
         factor = factor*real(j, wp)/(2*j - 1._wp)
      enddo

      select case( abs(m) )
      case(0)
         X_func_prolate = factor*a**(l)*Legendre_Plm_xGrOne(l,0,xi)
      case(1)
         X_func_prolate = imu*factor*a**(l)*Legendre_Plm_xGrOne(l,1,xi)/l
      case default
         call Error_Failure("X_func_prolate","abs(m) > 1 is not supported.")
      end select

    end function X_func_prolate
    !--------------------------------!
    
    !--------------------------------!
    function X_func_prolate_diff(l,m,xi,a)
    !--------------------------------!
      ! Differentiated prolate X function with dimensions
      ! Only for m = -1, 0, 1
      Use Integrand_Module_Spheroid_Prolate, only : X_Prolate_Diff
      implicit none
      complex(wp)           :: X_func_prolate_diff
      integer, Intent(In)   :: l, m
      real(wp), Intent(In)  :: xi, a 
      
      X_func_prolate_diff =  X_Prolate_Diff(l,abs(m),xi)*a**(l)

    end function X_func_prolate_diff
    !--------------------------------!
    
    !--------------------------------!
    function Z_func_prolate(l,m,xi,a) 
    !--------------------------------!
      ! Prolate Z function with dimensions.
      ! ONLY for m = -1,0,1 ! 
      Use Integrand_Module_Spheroid_Prolate, only : Legendre_Qlm
      implicit none
      complex(wp)           :: Z_func_prolate
      integer, intent(in)   :: l, m
      real(wp), intent(in)  :: xi,a
      integer               :: j
      real(wp)              :: factor

      ! Calculate the factorials prefactor
      factor = 1._wp
      do j = 1, l
         factor = factor*real( (2*j + 1), wp)/j
      enddo

      select case( abs(m) )
      case(0)
         Z_func_prolate = factor*a**(-l-1)*Legendre_Qlm(l,0,xi)
      case(1)
         Z_func_prolate = factor*a**(-l-1)*Legendre_Qlm(l,1,xi)/(l + 1._wp)
      case default
         call Error_Failure("Z_func_prolate","abs(m) > 1 is not supported.")
      end select

    end function Z_func_prolate
    !--------------------------------!

    !--------------------------------!
    function Z_func_prolate_diff(l,m,xi,a)
    !--------------------------------!
      ! Differentiated prolate X function with dimensions
      ! Only for m = -1, 0, 1
      Use Integrand_Module_Spheroid_Prolate, only : Z_Prolate_Diff
      implicit none
      complex(wp)           :: Z_func_prolate_diff
      integer, Intent(In)   :: l, m
      real(wp), Intent(In)  :: xi, a 
      
      Z_func_prolate_diff =  Z_Prolate_Diff(l,abs(m),xi)*a**(-l-1)

    end function Z_func_prolate_diff
    !--------------------------------!

    
  End Subroutine Get_potential_derivatives_prolate
  ! ----------------------------------------------------------------------------------------------- !
  
  !-----------------------------------------------------------------------------------------------------!
  Subroutine Get_potential_derivatives_oblate(x,y,z,Npts,iMedium,nMedia,eps,system_dim,coeff,diff_pot)
  !-----------------------------------------------------------------------------------------------------!
    ! --------------------------------------------------------------------------------------------!
    ! Calculates the normal (xi) derivative of the potential at the points specified. 
    ! 
    ! FOR OBLATE SPHEROIDS
    !
    ! Written by:
    ! Sindre Stavseng, Trondheim, April 2013
    ! --------------------------------------------------------------------------------------------!

    Use Tools_Module, only : deg2rad
    Use Integrand_Module_Spheroid_Oblate, only : Deriv_transformed_xi_eta
    Implicit None
    Integer, intent(in)         :: Npts, iMedium(Npts), nMedia, system_dim
    Real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    Complex(wp), intent(in)     :: eps(nMedia), coeff(system_dim,0:1)
    Complex(wp), intent(out)    :: diff_pot(Npts)
    ! --- Local variables ---------------
    Integer                     :: Incident_Medium, Expansion_Medium, Mp, &
                                   iMed, offset, i, l
    Real(wp)                    :: a, R1, mu_z, mubar_z, xi_mu(Npts), eta_mu(Npts), phi_mu(Npts),&
                                   xi_mubar(Npts), eta_mubar(Npts), phi_mubar(Npts), deriv_xi_mubar, &
                                   deriv_eta_mubar, thetaE, phiE, sinthetaE,costhetaE
    Complex(wp)                 :: expi2phiE, R, T, epsfrac, expiphiE,expMiniphiE
    
    write(*,*) "***Calculating potential normal derivatives..."

    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    Mp = param%numerics%multipole_order
    a = sqrt(Param%Geometry%Radius(2)**2 - Param%Geometry%Radius(1)**2)
    R1 = param%geometry%radius(1)
    expi2phiE = exp(imu * 2.0_wp * param%source%PhiE * deg2rad())
    ! Getting prolate spheroidal coordinates at multipole and image multipole.
    mu_z = param%numerics%multipole_position_ratio*param%geometry%radius(1)
    mubar_z = 2.0_wp*param%geometry%distance(1) - mu_z
    call Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,mu_z,xi_mu,eta_mu,phi_mu) 
    call Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,mubar_z,xi_mubar,eta_mubar,phi_mubar) 

    ! For the incident and transmitted terms:
    epsfrac = eps(1)/eps(2)
    thetaE = param%source%ThetaE * deg2rad()
    phiE = param%source%PhiE * deg2rad()
    sinthetaE = sin(thetaE)
    costhetaE = cos(thetaE)
    expiphiE = exp(imu * param%source%PhiE * deg2rad())
    expMiniphiE = exp(- imu * param%source%PhiE * deg2rad())

    Do i=1,Npts

       diff_pot(i) = 0.0_wp
       iMed = iMedium(i)
       offset = Param%Numerics%MP_Storage_Offset(iMed)
       
       call Deriv_transformed_xi_eta(mubar_z/a, xi_mu(i), eta_mu(i), deriv_xi_mubar, deriv_eta_mubar)

       If (mod(iMed,2)==0) then            ! Below the substrate
          T = (2.0_wp*eps(iMed-1))/(eps(iMed-1)+eps(iMed))
          If ((iMed-1) /= Expansion_Medium) then
             ! A-terms (zero if expansion medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,0)*Z_func_oblate_diff(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))          
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,1)*Z_func_oblate_diff(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1)*Z_func_oblate_diff(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))
             End do
             offset = offset + Mp 
          End if
          If ((iMed-1) /= Incident_Medium) then
             ! B-terms (zero if incident medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,0)*X_func_oblate_diff(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))          
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,1)*X_func_oblate_diff(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1)*X_func_oblate_diff(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))       
             End do
          End if
          diff_pot(i) = T*diff_pot(i)   ! Final prefactor

       Else                                ! Above the substrate
          R = (eps(iMed)-eps(iMed+1))/(eps(iMed)+eps(iMed+1))
          If ((iMed) /= Expansion_Medium) then
             ! A-terms (zero if expansion medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,0) * ( &
                           Z_func_oblate_diff(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*( Z_func_oblate_diff(l,0,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,0,eta_mubar(i),phi_mubar(i)) + &
                            Z_func_oblate(l,0,xi_mubar(i),a)*spher_harm_diff(l,0,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,1) * ( &
                           Z_func_oblate_diff(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*( Z_func_oblate_diff(l,1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,1,eta_mubar(i),phi_mubar(i)) + &
                            Z_func_oblate(l,1,xi_mubar(i),a)*spher_harm_diff(l,1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1) * ( &
                           Z_func_oblate_diff(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*( Z_func_oblate_diff(l,-1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) + &
                            Z_func_oblate(l,-1,xi_mubar(i),a)*spher_harm_diff(l,-1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
             End do
             offset = offset + Mp 
          End if
          If ((iMed) /= Incident_Medium) then
             ! B-terms (zero if incident medium)
             Do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,0) * ( &
                           X_func_oblate_diff(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*( X_func_oblate_diff(l,0,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,0,eta_mubar(i),phi_mubar(i)) + &
                            X_func_oblate(l,0,xi_mubar(i),a)*spher_harm_diff(l,0,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,1) * ( &
                           X_func_oblate_diff(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*( X_func_oblate_diff(l,1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,1,eta_mubar(i),phi_mubar(i)) + &
                            X_func_oblate(l,1,xi_mubar(i),a)*spher_harm_diff(l,1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1) * ( &
                           X_func_oblate_diff(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*( X_func_oblate_diff(l,-1,xi_mubar(i),a) * deriv_xi_mubar * &
                            spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) + &
                            X_func_oblate(l,-1,xi_mubar(i),a)*spher_harm_diff(l,-1,eta_mubar(i),phi_mubar(i))* &
                            deriv_eta_mubar ) )
             End do
          End if
       End if
       
       
       !=============================================
       ! CHECK THIS FOR OBLATE SPHEROIDS !!!
       !
       ! Add incident and transmitted potentials
       If (iMed == Incident_Medium) Then  ! Ambient
          diff_pot(i) = diff_pot(i) + sqrt(2.0_wp*pi/3.0_wp)*( - sqrt(2.0_wp)*costhetaE* &
                                      spher_harm(1,0,eta_mu(i),phi_mu(i))) + sinthetaE*expMiniPhiE * &
                                      X_func_oblate_diff(1,1,xi_mu(i),a)*spher_harm(1,1,eta_mu(i),phi_mu(i))&
                                      - sinthetaE*expiPhiE*X_func_oblate_diff(1,-1,xi_mu(i),a)* &
                                      spher_harm(1,-1,eta_mu(i),phi_mu(i))
       Else if (iMed == 2) Then  ! Substrate
          diff_pot(i) = diff_pot(i) + sqrt(2.0_wp*pi/3.0_wp)*( - epsfrac* sqrt(2.0_wp)*costhetaE* &
                                      spher_harm(1,0,eta_mu(i),phi_mu(i))) + sinthetaE*expMiniPhiE * &
                                      X_func_oblate_diff(1,1,xi_mu(i),a)*spher_harm(1,1,eta_mu(i),phi_mu(i))&
                                      - sinthetaE*expiPhiE*X_func_oblate_diff(1,-1,xi_mu(i),a)* &
                                      spher_harm(1,-1,eta_mu(i),phi_mu(i))
       End if
       
       !=====================================================

       
       diff_pot(i) = diff_pot(i)*eps(iMed)
       
    Enddo
    

    ! --- Internal functions ------------------------------------
  contains
    ! -----------------------------------------------------------
    
    !-------------------------------!
    function X_func_oblate(l,m,xi,a)
    !-------------------------------!
    ! Function X with dimensions.
    ! ONLY m=-1,0,1 ! This is not checked, for speed.
      Use Integrand_Module_Spheroid_Oblate, only : Legendre_Plm_Aimag
      implicit none
      complex(wp)           :: X_func_oblate
      integer, intent(in)   :: l,m
      real(wp), intent(in)  :: xi,a
      integer               :: i
      
      X_func_oblate = 1.0_wp
      do i=1,l
         X_func_oblate = X_func_oblate*real(i,wp)/(2*i-1)
      end do
      X_func_oblate = X_func_oblate * imu**(abs(m)-l)*a**(l)*Legendre_Plm_Aimag(l,abs(m),xi)
      if (.not. m==0) then
         X_func_oblate = X_func_oblate/l
      end if
    end function X_func_oblate
    !--------------------------!
    
    !------------------------------------!
    function X_func_oblate_diff(l,m,xi,a)
    !------------------------------------!
      Use Integrand_Module_Spheroid_Oblate, only : X_Oblate_diff
      implicit none
      real(wp)           :: X_func_oblate_diff
      integer, intent(in)   :: l,m
      real(wp), intent(in)  :: xi,a
      
      X_func_oblate_diff = a**(l)*X_Oblate_diff(l,abs(m),xi)

    !------------------------------------!      
    end function X_func_oblate_diff
    !------------------------------------!
    
    
    !-------------------------------!
    function Z_func_oblate(l,m,xi,a)
    !-------------------------------!
      ! Function Z with dimensions.
      ! ONLY m=-1,0,1 ! This is not checked, for speed.
      Use Integrand_Module_Spheroid_Oblate, only : Legendre_Qlm_Aimag
      implicit none
      complex(wp)           :: Z_func_oblate
      integer, intent(in)   :: l, m
      real(wp), intent(in)  :: xi,a
      integer               :: i
      
      Z_func_oblate = 1.0_wp
      do i=1,l
         Z_func_oblate = Z_func_oblate * real((2*i+1),wp)/i
      end do
      Z_func_oblate = Z_func_oblate * imu**(l+1) * a**(-l-1) * Legendre_Qlm_Aimag(l,abs(m),xi)
      if (.not. m==0) then
         Z_func_oblate = Z_func_oblate/(l+1)
      end if
    
    end function Z_func_oblate
    !--------------------------!

    !------------------------------------!
    function Z_func_oblate_diff(l,m,xi,a)
    !------------------------------------!
      Use Integrand_Module_Spheroid_Oblate, only : Z_Oblate_diff
      implicit none
      real(wp)           :: Z_func_oblate_diff
      integer, intent(in)   :: l,m
      real(wp), intent(in)  :: xi,a
      
      Z_func_oblate_diff = a**(-l-1)*Z_Oblate_diff(l,abs(m),xi)

    !------------------------------------!    
    end function Z_func_oblate_diff
    !------------------------------------!
    
  ! ----------------------------------------------------------------------------------------------- !
  End Subroutine Get_potential_derivatives_oblate
  ! ----------------------------------------------------------------------------------------------- !
  
  !-----------------------------------------------------------------------------------------------------!
  Subroutine Get_potential_derivatives_sphere(x,y,z,Npts,iMedium,nMedia,eps,system_dim,coeff,diff_pot)
  !-----------------------------------------------------------------------------------------------------!
    ! --------------------------------------------------------------------------------------------!
    ! Calculates the normal (r) derivative of the potential at the points specified. 
    ! 
    ! FOR SPHERES
    !
    ! Written by:
    ! Sindre Stavseng, Trondheim, April 2013
    ! --------------------------------------------------------------------------------------------!
    
    Use Tools_Module, only : deg2rad
    implicit none
    integer, intent(in)         :: Npts, iMedium(Npts), nMedia, system_dim
    real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    complex(wp), intent(in)     :: eps(nMedia), coeff(system_dim,0:1)
    complex(wp), intent(out)    :: diff_pot(Npts)
    integer                     :: i,l,m,Mp,iMed,offset, Incident_Medium, &
                                   Expansion_Medium
    real(wp)                    :: R1, r_mu(Npts),theta_mu(Npts),phi_mu(Npts),&
                                    r_mubar(Npts),theta_mubar(Npts), &
                                    phi_mubar(Npts), mu_z, mubar_z, deriv_r_mubar,&
                                    deriv_theta_mubar, thetaE, phiE,sinthetaE, costhetaE
    complex(wp)                 :: R,T,expi2phiE, epsfrac, expiphiE, expMiniphiE

    write(*,*) "***Calculating potential normal derivatives..."

    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    Mp = param%numerics%multipole_order
    R1 = param%geometry%radius(1)
    expi2phiE = exp(imu * 2.0_wp * param%source%PhiE * deg2rad())
    ! Getting Spherical coordinates at multipole and image multipole.
    mu_z = param%numerics%multipole_position_ratio*R1
    mubar_z = 2.0_wp*param%geometry%distance(1) - mu_z
    call Cartesian_to_Spherical(x,y,z,Npts,mu_z,r_mu,theta_mu,phi_mu) 
    call Cartesian_to_Spherical(x,y,z,Npts,mubar_z,r_mubar,theta_mubar,phi_mubar) 

    ! For the incident and transmitted terms:
    epsfrac = eps(1)/eps(2)
    thetaE = param%source%ThetaE * deg2rad()
    phiE = param%source%PhiE * deg2rad()
    sinthetaE = sin(thetaE)
    costhetaE = cos(thetaE)
    expiphiE = exp(imu * param%source%PhiE * deg2rad())
    expMiniphiE = exp(- imu * param%source%PhiE * deg2rad())

    do i=1,Npts
       diff_pot(i) = 0.0_wp
       iMed = iMedium(i)
       offset = Param%Numerics%MP_Storage_Offset(iMed)
       
       deriv_r_mubar = r_mubar_diff(r_mu(i),theta_mu(i),mubar_z)
       deriv_theta_mubar = cosTheta_mubar_diff(r_mu(i),theta_mu(i),mubar_z)

       if (mod(iMed,2)==0) then            ! Below the substrate
          T = (2.0_wp*eps(iMed-1))/(eps(iMed-1)+eps(iMed))
          if ((iMed-1) /= Expansion_Medium) then
             ! A-TERMS
             do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,0)*(-l-1)*r_mu(i)**(-l-2) * &
                                            spher_harm(l,0,cos(theta_mu(i)),phi_mu(i))
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,1)*(-l-1)*r_mu(i)**(-l-2) * &
                                            spher_harm(l,1,cos(theta_mu(i)),phi_mu(i))
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1)*(-l-1)*r_mu(i)**(-l-2) * &
                                            spher_harm(l,-1,cos(theta_mu(i)),phi_mu(i))
             end do
             offset = offset + Mp 
          end if
          ! B-TERMS
          if ((iMed-1) /= Incident_Medium) then
             do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,0)*l*r_mu(i)**(l-1) * &
                                            spher_harm(l,0,cos(theta_mu(i)),phi_mu(i))       
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,1)*l*r_mu(i)**(l-1) * &
                                            spher_harm(l,1,cos(theta_mu(i)),phi_mu(i))      
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1)*l*r_mu(i)**(l-1) * &
                                            spher_harm(l,-1,cos(theta_mu(i)),phi_mu(i))     
             end do
          end if
          ! Final prefactor
          diff_pot(i) = T*diff_pot(i)
       else                                ! Above the substrate
          R = (eps(iMed)-eps(iMed+1))/(eps(iMed)+eps(iMed+1))
          if (iMed /= Expansion_Medium) then
             ! A-TERMS
             do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,0) * ( &
                                            (-l-1)*r_mu(i)**(-l-2)*spher_harm(l,0,cos(theta_mu(i)),phi_mu(i)) &
                                            +(-1.0_wp)**(l)*R*( (-l-1)*r_mubar(i)**(-l-2)*deriv_r_mubar* &
                                            spher_harm(l,0,cos(theta_mubar(i)),phi_mubar(i)) + r_mubar(i)**(-l-1)* &
                                            spher_harm_diff(l,0,cos(theta_mubar(i)),phi_mubar(i))* &
                                            deriv_theta_mubar ) &
                                            )
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(l+2)*coeff(offset+l,1) * ( &
                                            (-l-1)*r_mu(i)**(-l-2)*spher_harm(l,1,cos(theta_mu(i)),phi_mu(i)) &
                                            +(-1.0_wp)**(l+1)*R*( (-l-1)*r_mubar(i)**(-l-2)*deriv_r_mubar* &
                                            spher_harm(l,1,cos(theta_mubar(i)),phi_mubar(i)) + r_mubar(i)**(-l-1)* &
                                            spher_harm_diff(l,1,cos(theta_mubar(i)),phi_mubar(i))* &
                                            deriv_theta_mubar ) &
                                            )
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1) * ( &
                                            (-l-1)*r_mu(i)**(-l-2)*spher_harm(l,-1,cos(theta_mu(i)),phi_mu(i)) &
                                            +(-1.0_wp)**(l-1)*R*( (-l-1)*r_mubar(i)**(-l-2)*deriv_r_mubar* &
                                            spher_harm(l,-1,cos(theta_mubar(i)),phi_mubar(i)) + &
                                            r_mubar(i)**(-l-1)* &
                                            spher_harm_diff(l,-1,cos(theta_mubar(i)),phi_mubar(i))* &
                                            deriv_theta_mubar ) &
                                            )
             end do
             offset = offset + Mp 
          end if
          if (iMed /= Incident_Medium) then
             ! B-TERMS
             do l=1,Mp
                !m=0
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,0) * ( &
                                            l*r_mu(i)**(l-1)*spher_harm(l,0,cos(theta_mu(i)),phi_mu(i)) &
                                            +(-1.0_wp)**(l)*R*( l*r_mubar(i)**(l-1)*deriv_r_mubar* &
                                            spher_harm(l,0,cos(theta_mubar(i)),phi_mubar(i)) &
                                            + r_mubar(i)**(l)* &
                                            spher_harm_diff(l,0,cos(theta_mubar(i)),phi_mubar(i))* &
                                            deriv_theta_mubar ) &
                                            )
                !m=1
                diff_pot(i) = diff_pot(i) + R1**(-l+1)*coeff(offset+l,1) * ( &
                                            l*r_mu(i)**(l-1)*spher_harm(l,1,cos(theta_mu(i)),phi_mu(i)) &
                                            +(-1.0_wp)**(l+1)*R*( l*r_mubar(i)**(l-1)*deriv_r_mubar* &
                                            spher_harm(l,1,cos(theta_mubar(i)),phi_mubar(i)) &
                                            + r_mubar(i)**(l)* &
                                            spher_harm_diff(l,1,cos(theta_mubar(i)),phi_mubar(i))* &
                                            deriv_theta_mubar ) &
                                            )
                !m=-1
                diff_pot(i) = diff_pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1) * ( &
                                            l*r_mu(i)**(l-1)*spher_harm(l,-1,cos(theta_mu(i)),phi_mu(i)) &
                                            +(-1.0_wp)**(l-1)*R*( l*r_mubar(i)**(l-1)*deriv_r_mubar* &
                                            spher_harm(l,-1,cos(theta_mubar(i)),phi_mubar(i)) &
                                            + r_mubar(i)**(l)* &
                                            spher_harm_diff(l,-1,cos(theta_mubar(i)),phi_mubar(i))* &
                                            deriv_theta_mubar ) &
                                            )
             end do
          end if
       end if
       
       !=============================================
       !
       ! Add incident and transmitted potentials
       If (iMed == Incident_Medium) Then  ! Ambient
          diff_pot(i) = diff_pot(i) - sqrt(2.0_wp*pi/3.0_wp)*( sqrt(2.0_wp)*costhetaE* &
                                      spher_harm(1,0,cos(theta_mu(i)),phi_mu(i)) &
                                      + sinthetaE*( expiPhiE*spher_harm(1,-1,cos(theta_mu(i)),phi_mu(i)) - &
                                      expMiniPhiE*spher_harm(1,1,cos(theta_mu(i)),phi_mu(i))) )

       Else if (iMed == 2) Then  ! Substrate
          diff_pot(i) = diff_pot(i) - sqrt(2.0_wp*pi/3.0_wp)*( epsfrac*sqrt(2.0_wp)*costhetaE* &
                                      spher_harm(1,0,cos(theta_mu(i)),phi_mu(i)) &
                                      + sinthetaE*( expiPhiE*spher_harm(1,-1,cos(theta_mu(i)),phi_mu(i)) - &
                                      expMiniPhiE*spher_harm(1,1,cos(theta_mu(i)),phi_mu(i))) )

       End if
       
       !=====================================================

       
       diff_pot(i) = diff_pot(i)*eps(iMed)

    end do


    ! --- Internal functions ------------------------------------
  contains
    ! -----------------------------------------------------------
    
    !--------------------------!
    function r_mubar_func(r,theta,z_mubar)
    !--------------------------!
      implicit none
      real(wp), intent(in)   :: r, theta, z_mubar
      real(wp)               :: r_mubar_func
      
      r_mubar_func = sqrt(r**2 - 2.0_wp*z_mubar*r*cos(theta) + z_mubar**2)
    !--------------------------!
    end function r_mubar_func
    !--------------------------!
    
    !--------------------------!
    function r_mubar_diff(r,theta,z_mubar)
    !--------------------------!
      implicit none
      real(wp), intent(in)   :: r, theta, z_mubar
      real(wp)               :: r_mubar_diff
      
      r_mubar_diff = (r - z_mubar*cos(theta))/r_mubar_func(r,theta,z_mubar)
    !--------------------------!
    end function r_mubar_diff
    !--------------------------!
    
    !--------------------------!
    function cosTheta_mubar_diff(r,theta,z_mubar)
    !--------------------------!
      implicit none
      real(wp), intent(in)   :: r, theta, z_mubar
      real(wp)               :: cosTheta_mubar_diff
      real(wp)               :: r_mubar

      r_mubar = r_mubar_func(r,theta,z_mubar)
      cosTheta_mubar_diff = ( r_mubar_diff(r,theta,z_mubar)*(r*cos(theta) - z_mubar) &
                            - r_mubar*cos(theta) )/r_mubar**2
    !--------------------------!
    end function cosTheta_mubar_diff
    !--------------------------!
    
    
  ! ----------------------------------------------------------------------------------------------- !
  End Subroutine Get_potential_derivatives_sphere
  ! ----------------------------------------------------------------------------------------------- !
    
  
  
! ====================================
! COMMON ROUTINES AND FUNCTIONS
! ====================================

  !-------------------------------------------------------------------!
  subroutine Cartesian_to_Spheroidal_Prolate(x,y,z,Npts,a,delta_z,xi,eta,phi)
  !-------------------------------------------------------------------!
    !delta_z: Position along the z-axis of the spheroidal coordinate system.
    !The cartesian coordinate system defines the origin.
    !a: The parameter "a" of the spheroidal coordinate system.
    !
    ! This routine is the same as the one in the potential_module. It has been
    ! copied here in order to separate the derivative calculation from the potential
    ! calculation
    !
    implicit none
    integer, intent(in)  :: Npts
    real(wp), intent(in) :: x(Npts),y(Npts),z(Npts),a,delta_z
    real(wp), intent(out):: xi(Npts),eta(Npts),phi(Npts)
    integer              :: i 
    real(wp)             :: rho1,rho2,z_prime

    do i=1, Npts
       phi(i) = atan2( y(i), x(i) )
       if ( phi(i) < 0.0_wp ) phi(i) = phi(i) + 2.0_wp*pi
       z_prime = z(i) - delta_z
       rho1 = sqrt( (z_prime + a)**2 + x(i)**2 + y(i)**2 )
       rho2 = sqrt( (z_prime - a)**2 + x(i)**2 + y(i)**2 )
       xi(i) = (rho1 + rho2)/(2.0_wp*a)
       eta(i) =  (rho1 - rho2)/(2.0_wp*a)
       if ( eta(i) < -1.0_wp ) eta(i) = -1.0_wp
       if ( eta(i) > 1.0_wp ) eta(i) = 1.0_wp
    end do

  end subroutine Cartesian_to_Spheroidal_Prolate
  !-----------------------------------------------------------!

  !-------------------------------------------------------------------!
  subroutine Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,delta_z,xi,eta,phi)
  !-------------------------------------------------------------------!
    !delta_z: Position along the z-axis of the spheroidal coordinate system.
    !The cartesian coordinate system defines the origin.
    !a: The parameter "a" of the spheroidal coordinate system.
    implicit none
    integer, intent(in)  :: Npts
    real(wp), intent(in) :: x(Npts),y(Npts),z(Npts),a,delta_z
    real(wp), intent(out):: xi(Npts),eta(Npts),phi(Npts)
    integer              :: i 
    real(wp)             :: rho1,rho2,z_prime

    do i=1,Npts
       phi(i) = atan2(y(i),x(i))
       if (phi(i) < 0.0_wp) phi(i) = phi(i) + 2.0_wp*pi
       z_prime = z(i) - delta_z
       rho1 = sqrt(z_prime**2 + (x(i)+a*cos(phi(i)))**2 + (y(i)+a*sin(phi(i)))**2)
       rho2 = sqrt(z_prime**2 + (x(i)-a*cos(phi(i)))**2 + (y(i)-a*sin(phi(i)))**2)
       xi(i) = sqrt(((rho1+rho2)/(2.0_wp*a))**2 - 1.0_wp)
       if (z_prime > 0.0_wp) then
          eta(i) = sqrt(1.0_wp - ((rho1-rho2)/(2.0_wp*a))**2)
       else 
          eta(i) = -sqrt(1.0_wp - ((rho1-rho2)/(2.0_wp*a))**2)
       end if
    end do
    
  end subroutine Cartesian_to_Spheroidal_Oblate
  !-----------------------------------------------------------!
  
  !-----------------------------------------------------------------!
  subroutine Cartesian_to_Spherical(x,y,z,Npts,delta_z,r,theta,phi)
  !-----------------------------------------------------------------!
    !delta_z: Position along the z-axis of the spherical coordinate system.
    !The cartesian coordinate system defines the origin.
    implicit none
    integer, intent(in)  :: Npts
    real(wp), intent(in) :: x(Npts),y(Npts),z(Npts),delta_z
    real(wp), intent(out):: r(Npts),theta(Npts),phi(Npts)
    integer              :: i 
    real(wp)             :: z_prime
    
    do i=1,Npts
       phi(i) = atan2(y(i),x(i))
       if (phi(i) < 0.0_wp) phi(i) = phi(i) + 2.0_wp*pi
       z_prime = z(i) - delta_z
       r(i) = sqrt(x(i)**2 + y(i)**2 + z_prime**2)
       theta(i) = acos(z_prime/r(i))
    end do

  end subroutine Cartesian_to_Spherical
  !--------------------------------------------------------------!

  
  !--------------------------------!
  function spher_harm(l,m,eta,phi)
  !--------------------------------!
    ! Spherical Harmonics
    ! ONLY m=-1,0,1 !
    Use Legendre_Module, only : Legendre_Plm
    implicit none
    complex(wp)           :: spher_harm
    integer, intent(in)   :: l, m
    real(wp), intent(in)  :: eta, phi
    
    if (m==0) then
       spher_harm = sqrt((2*l+1)/(4.0_wp*pi))*Legendre_Plm(l,0,eta)
    else if (m==1) then
       spher_harm = -sqrt((2*l+1)/(4.0_wp*pi*l*(l+1)))*Legendre_Plm(l,1,eta)&
            *exp(imu*phi)
    else if (m==(-1)) then
       spher_harm = sqrt((2*l+1)/(4.0_wp*pi*l*(l+1)))*Legendre_Plm(l,1,eta)&
            *exp(-imu*phi)
    else
       call Error_Failure('spher_harm','abs(m)>1 not supported!')
    end if
  end function spher_harm
  !--------------------------!
  
  !--------------------------------!
  function spher_harm_diff(l,m,eta,phi)
  !--------------------------------!
    Use Legendre_Module, only : Legendre_Plm_Diff
    implicit none
    complex(wp)           :: spher_harm_diff
    integer, intent(in)   :: l, m
    real(wp), intent(in)  :: eta, phi
    
    if (m==0) then
       spher_harm_diff = sqrt((2*l+1)/(4.0_wp*pi))*Legendre_Plm_Diff(l,0,eta)
    else if (m==1) then
       spher_harm_diff = -sqrt((2*l+1)/(4.0_wp*pi*l*(l+1)))*Legendre_Plm_Diff(l,1,eta)&
            *exp(imu*phi)
    else if (m==(-1)) then
       spher_harm_diff = sqrt((2*l+1)/(4.0_wp*pi*l*(l+1)))*Legendre_Plm_Diff(l,1,eta)&
            *exp(-imu*phi)
    else
       call Error_Failure('spher_harm_diff','abs(m)>1 not supported!')
    end if
    
  end function spher_harm_diff
  !--------------------------!
  
  ! ------------------------------------------------------------------------------------------------!
  subroutine write_derivatives_to_file(x,y,z,Matrix,iMedium, &
       Npts,cols,iEnergies,nMedia,eps)
  ! ------------------------------------------------------------------------------------------------!    
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    integer, intent(in)       :: Npts, cols, iMedium(Npts),iEnergies(cols),nMedia
    complex(wp), intent(in)   :: Matrix(Npts,cols), eps(cols,nMedia)
    real(wp), intent(in)      :: x(Npts),y(Npts),z(Npts)
    integer                   :: file_id,i,j
    
    ! Writing potential
    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
                                //"_potential_diff.dat")
    write(file_id,fmt='(A)',advance='no') "# x   y   z   Medium    eps*d(Potential)/dn(Re,Im) "
    do i=1,cols
       write(file_id,fmt='(F12.6)',advance='no') &
                              param%numerics%energy(iEnergies(i))
    end do
    write(file_id,*)
    do i=1,cols
       write(file_id,'(A8,F9.6,A3)',advance='no') "# eps(E=",param%numerics%energy(iEnergies(i)),"): "
       write(file_id,*) (eps(i,j), j=1,nMedia)
    end do
    do i=1,Npts
       write(file_id,*) x(i),y(i),z(i),iMedium(i),(real(Matrix(i,j)),aimag(Matrix(i,j)), j=1,cols)
    end do
    Close(unit=file_id)
  
  end subroutine write_derivatives_to_file
  ! -----------------------------------------------------------------------

!--------------------------------------!
End Module Potential_Derivatives_Module
!--------------------------------------!
