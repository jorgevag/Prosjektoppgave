! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!
! 
! --- AUTHOR : Eskil Aursand, Trondheim, 2012.                        
!
! --- Modified for prolate Spheroid support:
!        Sindre Stavseng, Trondheim, March 2013.
!
! ----------------------------------------------------------
!



!-----------------------------!
Module Potential_Module
!-----------------------------!

  ! --- The Use Statements global to the module
  Use Shared
  Use Error_Module,                     only : Error_Failure, Error_Warning

  ! --------------------------------------
  ! --- The Publicly available routines
  ! --------------------------------------
  Public :: Get_Potential
  ! --- Needed by Eigenmodes module:
  Public :: Get_points
  Public :: Get_closest_ienergy 
  Public :: Cartesian_to_Spherical
  Public :: Get_Medium_Sphere 
  Public :: Get_multipole_pot_sphere
  Public :: write_to_file
  Public :: Get_Medium_Spheroid
  Public :: Cartesian_to_Spheroidal_Oblate
  Public :: Cartesian_to_Spheroidal_Prolate
  Public :: Get_multipole_pot_spheroid_oblate
  Public :: Get_multipole_pot_spheroid_prolate
  Public :: Get_const_pot_sphere
  Public :: Get_const_pot_spheroid
  ! -------------------------------------------

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  ! --- Module-global data



!-------!
Contains
!-------!


  
  !------------------------------!
  Subroutine Get_Potential()
  !------------------------------!
    !
    ! --- This is the master routine for calculating the potential
    !     at coordinates as fiven by point-file (param%potential%points_file)
    !     and at energies as given by Param%Potential%Energy.
    !
    !
    Implicit None
    ! --- Logical
    Character(len=*), parameter     :: routine = "Get_Potential"
    Logical                         :: file_exists


    If (Param%Potential%Potential_Calculation) then
    !-----------------------------------------------------   
       !
       ! --- Potential Calculation
       !
    
       ! --- Does point-file exist?
       inquire(file = param%potential%points_file, exist = file_exists)
       
       if (file_exists) then
          ! --- Read the point-file
          Select case( size(param%geometry%radius) )
             Case(1) ! = Sphere
                call Get_Potential_Sphere()
             Case(2) ! = Spheroid
                call Get_Potential_Spheroid()
             Case default
                call Error_Failure(routine,"Internal Error; Unsupported size(param%geometry%radius)" )
          End select

          !if (size(param%geometry%radius) == 1) then 
          !   call Get_Potential_Sphere()
          !else if (size(param%geometry%radius) == 2) then
          !   call Get_Potential_Spheroid()
          !else
          !   call Error_Failure(routine,"(size(radius) /= 1) and (size(radius) /= 2)...")
          !end if

       else
          ! --- The point-file does not exist....
          call Error_Failure(routine,&
           "Potential Point-File ("//trim(adjustl(param%potential%points_file))//") does not exist.")
       end if
    !-------------   
    Else
    !-------------
       !
       ! --- NO Potential Calculation
       !
       
       ! --- Nothing to do ....
    End If
    !------------------------

  End subroutine Get_Potential
  !------------------------------!



  !
  !
  ! ====================================
  ! ==== LOCAL ROUTINES
  ! ====================================
  !
  !


  !------------------------------!
  Subroutine Get_Potential_Sphere()
  !------------------------------!
    Use Potential_Derivatives_Module, only : Get_potential_derivatives_sphere, &
                                             write_derivatives_to_file
    implicit none
    ! --- Logical 
    character(len=*), parameter     :: routine = "Get_Potential_Sphere"
    real(wp), allocatable           :: x(:),y(:),z(:)
    real(wp), allocatable           :: r(:),theta(:),phi(:)
    complex(wp), allocatable        :: inc_trans_pot(:), const_pot(:), &
         multipole_pot(:), output_matrix(:,:), &
         eps(:,:),coeff(:,:), diff_pot(:)
    integer, allocatable            :: iMedium(:),iEnergies(:)
    integer                         :: Npts, i,j, nEnergy_potential,nMedia, &
         system_dim, iEnergy

    ! --- DEBUGGING --------------------------------------------------------------------------
    ! Move this to Param%Potential?
    logical, parameter              :: potential_derivative_calculation = .false.
    !-----------------------------------------------------------------------------
    !!!!!!!!
    
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    
    nEnergy_potential = size(param%potential%energy)
    nMedia = 2*size(param%geometry%radius_ratios) + 2
    system_dim = 2 * param%numerics%multipole_order &
         * size(param%geometry%radius_ratios)
    allocate(eps(nEnergy_potential,nMedia),coeff(system_dim,0:1))
    call Get_Points(x,y,z,Npts)
    allocate(r(Npts),theta(Npts),phi(Npts),output_matrix(Npts,nEnergy_potential),&
         inc_trans_pot(Npts),const_pot(Npts),multipole_pot(Npts))
    allocate(iMedium(Npts),iEnergies(nEnergy_potential))
    
    ! === DEBUGGING ==============================================
    If (potential_derivative_calculation) allocate(diff_pot(Npts))
    ! ============================================================

    call Cartesian_to_Spherical(x,y,z,Npts,0.0_wp,r,theta,phi)
    call Get_Medium_Sphere(r,theta,phi,Npts,iMedium)
    call Get_closest_ienergy(nEnergy_potential,iEnergies)
    do i=1,nEnergy_potential
       iEnergy = iEnergies(i)
       if (param%inout%verbose) then
          write(*,'(A,F8.5)') ' ***Finding potential for E = ', &
               param%numerics%energy(iEnergy)
       end if
       do j=1,nMedia
          eps(i,j) = param%media(j)%epsilon(iEnergy)
       end do
       call Get_inc_trans_pot(x,y,z,eps(i,:),iMedium,Npts,inc_trans_pot)
       call Get_MP_coeff(iEnergy,system_dim,coeff)
       ! -----------------------------------------------------------------------------
       !call write_multipole_coeffs_to_file(coeff,system_dim) ! DEBUGGING: Remove this
       ! -----------------------------------------------------------------------------
       call Get_const_pot_sphere(iMedium,Npts,nMedia,system_dim,coeff,&
            eps(i,:),const_pot)
       call Get_multipole_pot_sphere(x,y,z,Npts,iMedium,nMedia, eps(i,:), &
            system_dim, coeff, multipole_pot)
       ! Not done
       output_matrix(:,i) = inc_trans_pot+const_pot+multipole_pot
       !output_matrix(:,i) = inc_trans_pot+multipole_pot
       !output_matrix(:,i) = multipole_pot !TESTING
       !output_matrix(:,i) = const_pot !TESTING
       !output_matrix(:,i) = inc_trans_pot !TESTING
       
       ! === DEBUGGING ======================
       If (potential_derivative_calculation) then
          call Get_potential_derivatives_sphere(x,y,z,Npts,iMedium,nMedia,eps(i,:),system_dim,coeff,diff_pot)
       End if
       ! =======================================
    end do
    
    call write_to_file(x,y,z,output_matrix,iMedium,Npts,&
         nEnergy_potential,iEnergies,nMedia,eps)
    
    ! === DEBUGGING =========
    If (potential_derivative_calculation) then
       call write_derivatives_to_file(x,y,z,diff_pot,iMedium,Npts,&
            nEnergy_potential,iEnergies,nMedia,eps)
       deallocate(diff_pot)
    End If
    ! =======================
    
    deallocate(x,y,z,r,theta,phi,iMedium,output_matrix,eps,inc_trans_pot, &
         const_pot,multipole_pot,iEnergies,coeff)
    
    
    
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine
    
  end subroutine Get_Potential_Sphere
  !--------------------------------------!




  !-----------------------------------!
  subroutine Get_Potential_Spheroid()
  !-----------------------------------!
    Use Potential_Derivatives_Module, only : Get_potential_derivatives_prolate, &
         Get_potential_derivatives_oblate, write_derivatives_to_file

    implicit none 
    character(len=*), parameter     :: routine = "Get_Potential_Spheroid"
    real(wp), allocatable           :: x(:),y(:),z(:)
    real(wp), allocatable           :: xi(:),eta(:),phi(:)
    complex(wp), allocatable        :: inc_trans_pot(:), const_pot(:), &
         multipole_pot(:), output_matrix(:,:), &
         eps(:,:),coeff(:,:),diff_pot(:)
    integer, allocatable            :: iMedium(:),iEnergies(:)
    real(wp)                        :: a
    integer                         :: Npts, i,j, nEnergy_potential,nMedia, &
         system_dim, iEnergy

    ! --- DEBUGGING --------------------------------------------------------------------------
    ! Move this to Param%Potential?
    logical, parameter              :: potential_derivative_calculation = .false.
    !-----------------------------------------------------------------------------
    
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    
    
    ! --- Common variables for oblate and prolate spheroids -------
    
    nEnergy_potential = size(param%potential%energy)
    nMedia = 2*size(param%geometry%radius_ratios) + 2
    system_dim = 2 * param%numerics%multipole_order &
                 * size(param%geometry%radius_ratios)
    allocate(eps(nEnergy_potential,nMedia),coeff(system_dim,0:1))
    call Get_Points(x,y,z,Npts)
    allocate(xi(Npts),eta(Npts),phi(Npts),output_matrix(Npts,nEnergy_potential),&
         inc_trans_pot(Npts),const_pot(Npts),multipole_pot(Npts))
    allocate(iMedium(Npts),iEnergies(nEnergy_potential))
    
    ! === DEBUGGING ==============================================
    If (potential_derivative_calculation) allocate(diff_pot(Npts))
    ! ============================================================
    
    ! -------------------------------------------------------------
    
    ! === OBLATE SPHEROIDS ========================================
    If ( Param%Geometry%isOblate ) Then
       a = sqrt(param%geometry%radius(2)**2-param%geometry%radius(1)**2)
       call Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,0.0_wp,xi,eta,phi)
       call Get_Medium_Spheroid(xi,eta,phi,Npts,iMedium)
       call Get_closest_ienergy(nEnergy_potential,iEnergies)
       do i=1,nEnergy_potential
          iEnergy = iEnergies(i)
          if (param%inout%verbose) then
             write(*,'(A,F8.5)') ' ***Finding potential for E = ', &
                  param%numerics%energy(iEnergy)
          end if
          do j=1,nMedia
             eps(i,j) = param%media(j)%epsilon(iEnergy)
          end do
          call Get_inc_trans_pot(x,y,z,eps(i,:),iMedium,Npts,inc_trans_pot)
          call Get_MP_coeff(iEnergy,system_dim,coeff)
          call Get_const_pot_spheroid(iMedium,Npts,nMedia,system_dim,coeff,&
               eps(i,:),const_pot)
          call Get_multipole_pot_spheroid_oblate(x,y,z,Npts,iMedium,nMedia, eps(i,:), &
               system_dim, coeff, multipole_pot)
          ! Not done
          output_matrix(:,i) = inc_trans_pot+const_pot+multipole_pot
          !output_matrix(:,i) = inc_trans_pot+multipole_pot
          !output_matrix(:,i) = multipole_pot !TESTING
          !output_matrix(:,i) = inc_trans_pot !TESTING
          
          ! === DEBUGGING ======================
          If (potential_derivative_calculation) then
             call Get_potential_derivatives_oblate(x,y,z,Npts,iMedium,nMedia,eps(i,:),system_dim,coeff,diff_pot)
          End if
          ! =======================================

       end do
       !write(*,*) "Oblate"
       ! =============================================================

       ! === PROLATE SPHEROIDS =======================================
    Else if ( Param%Geometry%isProlate ) Then
       a = sqrt(param%geometry%radius(1)**2-param%geometry%radius(2)**2)
       call Cartesian_to_Spheroidal_Prolate( x,y,z, Npts, a, 0.0_wp, xi,eta,phi )
       call Get_Medium_Spheroid( xi,eta,phi, Npts, iMedium )
       call Get_closest_iEnergy( nEnergy_potential, iEnergies )
       
       Do i=1,nEnergy_potential
          iEnergy = iEnergies(i)
          If (param%inout%verbose) then
             write(*,'(A,F8.5)') ' ***Finding potential for E = ', &
                  param%numerics%energy(iEnergy)
          End if
          Do j=1,nMedia
             eps(i,j) = param%media(j)%epsilon(iEnergy)
          End do
          
          call Get_Inc_Trans_Pot( x,y,z, eps(i,:), iMedium, Npts, inc_trans_pot )         !OK
          call Get_MP_coeff( iEnergy, system_dim, coeff ) !OK
          ! === DEBUGGING ====================================
          !write(*,*) "MP coeffs:"
          !write(*,*) coeff(:,1)
          ! ==================================================
          call Get_const_pot_Spheroid( iMedium,Npts,nMedia,system_dim,coeff,&
               eps(i,:),const_pot )                               !OK, but check again
          call Get_multipole_pot_spheroid_prolate( x,y,z,Npts,iMedium,nMedia, eps(i,:), &
               system_dim, coeff, multipole_pot )     !Should be OK
          
          output_matrix(:,i) = inc_trans_pot + const_pot + multipole_pot
          !output_matrix(:,i) = inc_trans_pot !TESTING
          !output_matrix(:,i) = const_pot !TESTING
          !output_matrix(:,i) = multipole_pot !TESTING
          
          ! === DEBUGGING ======================
          If (potential_derivative_calculation) then
             call Get_potential_derivatives_prolate(x,y,z,Npts,iMedium,nMedia,eps(i,:),system_dim,coeff,diff_pot)
          End if
          ! =======================================
       End do
       
       !write(*,*) "Prolate"
       ! =============================================================
       
    Else 
       ! Spheroid is neither Oblate nor Prolate...
       call Error_Failure( routine, "Internal errror: Spheroid type not specified." )
    Endif
    
    call write_to_file(x,y,z,output_matrix,iMedium,Npts,&
         nEnergy_potential,iEnergies,nMedia,eps)
    
    ! === DEBUGGING =========
    If (potential_derivative_calculation) then
       call write_derivatives_to_file(x,y,z,diff_pot,iMedium,Npts,&
            nEnergy_potential,iEnergies,nMedia,eps)
       deallocate(diff_pot)
    End If
    ! =======================
    
    deallocate(x,y,z,xi,eta,phi,iMedium,output_matrix,eps,inc_trans_pot, &
         const_pot,multipole_pot,iEnergies,coeff)
    
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine
    
  end subroutine Get_Potential_Spheroid
  !-----------------------------------!
  

  !---------------------------------------!
  Subroutine Get_MP_coeff(iEnergy,N,coeff)
  !---------------------------------------!
    ! Common routine for both sphere and spheroid.
    ! This is also common for both oblate and prolate spheroids.
    !
    ! OBS: The coefficients fetched here are dimensionless/normalized!!!!
    !
    Use Matrix_System_Sphere_Module,    only: Get_Matrix_System_Sphere
    Use Matrix_System_Spheroid_Module,  only: Get_Matrix_System_Spheroid
    Use Linear_Algebra_Module,          only : Solve_Linear_System 
    implicit none
    integer, intent(in)       :: iEnergy,N
    complex(wp), intent(out)  :: coeff(N,0:1)
    complex(wp), allocatable  :: A(:,:), b(:) 
    integer                   :: m
    Allocate(A(N,N),b(N))
    do m=0,1
       if (size(param%geometry%radius)==1) then !Sphere
          call Get_Matrix_System_Sphere(A,b,m,iEnergy)
       else                                     !Spheroid
          call Get_Matrix_System_Spheroid(A,b,m,iEnergy)
       end if
       if (param%inout%verbose) then
          write(*,'(A,I1)') " ***Solving linear system, m=",m
       end if
       call Solve_Linear_System(A,b)
       coeff(:,m) = b
    end do
    deallocate(A,b)
    
  End subroutine Get_MP_coeff
  !---------------------------------------!



  !----------------------------------------------------------!
  Subroutine Get_closest_ienergy(nEnergy_potential,iEnergies)
  !----------------------------------------------------------!
    implicit none
    character(len=*), parameter :: routine = "Get_closest_ienergy"
    integer, intent(in)         :: nEnergy_potential
    integer, intent(out)        :: iEnergies(NEnergy_potential)
    integer                     :: i,j
    real(wp)                    :: energy
    real(wp), parameter         :: warning_threshold = 0.05
    do i=1,nEnergy_potential
       energy = param%potential%energy(i)
       write(*,'(A,F10.7)') ' ***Finding match for E = ',energy
       if (energy < param%numerics%energy(1) .or. &
            energy > param%numerics%energy(size(param%numerics%energy))) then
          call Error_Failure(routine,"Energy for potential outside range.")
       end if
       do j=1,size(param%numerics%energy)
          if (param%numerics%energy(j)>energy) then
             iEnergies(i) = j
             write(*,'(A,F10.7)') " ***Found match: E = ",param%numerics%energy(j)
             if (abs(energy-param%numerics%energy(j)) > warning_threshold) then
                call Error_Warning(routine, "Matched energy may be far away from &
                     requested energy for potential calculation.")
             end if
             exit
          end if
       end do
    end do
    
  End Subroutine Get_closest_ienergy
  !----------------------------------------------------------!



  !-----------------------------------------------------------------------------------------------!
  subroutine Get_multipole_pot_spheroid_oblate(x,y,z,Npts,iMedium,nMedia,eps,system_dim,coeff,pot)
  !-----------------------------------------------------------------------------------------------!
    Use Tools_Module, only : deg2rad
    Implicit None
    Integer, intent(in)         :: Npts, iMedium(Npts), nMedia, system_dim
    Real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    Complex(wp), intent(in)     :: eps(nMedia), coeff(system_dim,0:1)
    Complex(wp), intent(out)    :: pot(Npts)
    Integer                     :: i,l,m,Mp,iMed,offset, Incident_Medium,         &
                                   Expansion_Medium, percent, percent_old
    Real(wp)                    :: a, R1, xi_mu(Npts),eta_mu(Npts),phi_mu(Npts),  &
                                   xi_mubar(Npts),eta_mubar(Npts),                &
                                   phi_mubar(Npts), mu_z, mubar_z
    Complex(wp)                 :: R,T,expi2phiE

    !TESTING
    complex(wp) :: test
    if (param%inout%verbose) then
       write(*,*) " ***Getting multipole potential."
    end if
    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    Mp = param%numerics%multipole_order
    a = sqrt(Param%Geometry%Radius(2)**2 - Param%Geometry%Radius(1)**2)
    R1 = param%geometry%radius(1)
    expi2phiE = exp(imu * 2.0_wp * param%source%PhiE * deg2rad())
    ! Getting Spheroidal coordinates at multipole and image multipole.
    mu_z = param%numerics%multipole_position_ratio*param%geometry%radius(1)
    mubar_z = 2.0_wp*param%geometry%distance(1) - mu_z
    call Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,mu_z,xi_mu,eta_mu,phi_mu) 
    call Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,mubar_z,xi_mubar,eta_mubar,phi_mubar) 
    
    percent_old = 0
    do i=1,Npts
       percent = int(100.0_wp*real(i,wp)/Npts)
       if (mod(percent,10)==0 .and. percent /= percent_old) then
          percent_old = percent
          write(*,'(I3,A3)',advance='no') percent,'%, ' 
          if (percent==100) write(*,*) ' '
       end if
       pot(i) = 0.0_wp
       iMed = iMedium(i)
       offset = Param%Numerics%MP_Storage_Offset(iMed)
       if (mod(iMed,2)==0) then            ! Below the substrate
          T = (2.0_wp*eps(iMed-1))/(eps(iMed-1)+eps(iMed))
          if ((iMed-1) /= Expansion_Medium) then
             ! A-TERMS
             do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,0)*Z_func(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))       
                !m=1
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,1)*Z_func(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                pot(i) = pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1)*Z_func(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))       
             end do
             offset = offset + Mp 
          end if
          ! B-TERMS
          if ((iMed-1) /= Incident_Medium) then
             do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,0)*X_func(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))       
                !m=1
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,1)*X_func(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                pot(i) = pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1)*X_func(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))       
             end do
          end if
          ! Final prefactor
          pot(i) = T*pot(i)
       else                                ! Above the substrate
          R = (eps(iMed)-eps(iMed+1))/(eps(iMed)+eps(iMed+1))
          if (iMed /= Expansion_Medium) then
             ! A-TERMS
             do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,0) * ( &
                                  Z_func(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                                + (-1.0_wp)**(l)*R*Z_func(l,0,xi_mubar(i),a)* &
                                    spher_harm(l,0,eta_mubar(i),phi_mubar(i)) & 
                                  )
                !m=1
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,1) * ( &
                                  Z_func(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                                +(-1.0_wp)**(l+1)*R*Z_func(l,1,xi_mubar(i),a)* &
                                  spher_harm(l,1,eta_mubar(i),phi_mubar(i))    &
                                )
                !m=-1
                pot(i) = pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1) * ( &
                                  Z_func(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                                + (-1.0_wp)**(l-1)*R*Z_func(l,-1,xi_mubar(i),a)* &
                                  spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) & 
                                )
             end do
             offset = offset + Mp 
          end if
          if (iMed /= Incident_Medium) then
             ! B-TERMS
             do l=1,Mp
                !m=0
                !write(*,*) offset+l
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,0) * ( &
                           X_func(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*X_func(l,0,xi_mubar(i),a) * &
                            spher_harm(l,0,eta_mubar(i),phi_mubar(i)) &
                            )
                !m=1
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,1) * ( &
                           X_func(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*X_func(l,1,xi_mubar(i),a) * &
                            spher_harm(l,1,eta_mubar(i),phi_mubar(i)) &
                            )
                !m=-1
                pot(i) = pot(i) - R1**(-l+1)*expi2phiE * coeff(offset+l,1) * ( &
                           X_func(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*X_func(l,-1,xi_mubar(i),a) * &
                            spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) &
                            )
             end do
          end if
       end if
    end do
    
    !------!
  contains
    !------!
   
    !--------------------------!
    function X_func(l,m,xi,a)
    !--------------------------!
    ! Function X with dimensions.
    ! ONLY m=-1,0,1 ! This is not checked, for speed.
      Use Integrand_Module_Spheroid_Oblate, only : Legendre_Plm_Aimag
      implicit none
      complex(wp)           :: X_func
      complex(wp)           :: Plm
      integer, intent(in)   :: l,m
      real(wp), intent(in)  :: xi,a
      integer               :: i
      
      X_func = 1.0_wp
      do i=1,l
         X_func = X_func*real(i,wp)/(2*i-1)
      end do
      X_func = X_func * imu**(abs(m)-l)*a**(l)*Legendre_Plm_Aimag(l,abs(m),xi)
      if (.not. m==0) then
         X_func = X_func/l
      end if
    end function X_func
    !--------------------------!
 
    !--------------------------!
    function Z_func(l,m,xi,a)
    !--------------------------!
      ! Function Z with dimensions.
      ! ONLY m=-1,0,1 ! This is not checked, for speed.
      Use Integrand_Module_Spheroid_Oblate, only : Legendre_Qlm_Aimag
      implicit none
      complex(wp)           :: Z_func
      integer, intent(in)   :: l, m
      real(wp), intent(in)  :: xi,a
      complex(wp)           :: Qlm
      integer               :: i
      
      Z_func = 1.0_wp
      do i=1,l
         Z_func = Z_func * real((2*i+1),wp)/i
      end do
      Z_func = Z_func * imu**(l+1) * a**(-l-1) * Legendre_Qlm_Aimag(l,abs(m),xi)
      if (.not. m==0) then
         Z_func = Z_func/(l+1)
      end if
    
    end function Z_func 
    !--------------------------!



    !--------------------------!
    function spher_harm(l,m,eta,phi)
    !--------------------------!
      ! Spherical Harmonics
      ! ONLY m=-1,0,1 !
      Use Legendre_Module, only : Legendre_Plm
      implicit none
      complex(wp)           :: spher_harm
      integer, intent(in)   :: l, m
      real(wp), intent(in)  :: eta, phi
      real(wp)              :: Plm
      
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
   

  end subroutine Get_multipole_pot_spheroid_oblate
  !-----------------------------------------------------------------------!


  !-----------------------------------------------------------------------------------------------!
  subroutine Get_multipole_pot_spheroid_prolate(x,y,z,Npts,iMedium,nMedia,eps,system_dim,coeff,pot)
  !-----------------------------------------------------------------------------------------------!
    ! --------------------------------------------------------------------------------------------!
    ! This routine is based on the similar routine for Oblate spheroids above.
    ! 
    ! Written by:
    ! Sindre Stavseng, Trondheim, March 2013
    ! --------------------------------------------------------------------------------------------!

    Use Tools_Module, only : deg2rad
    Implicit None
    Integer, intent(in)         :: Npts, iMedium(Npts), nMedia, system_dim
    Real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    Complex(wp), intent(in)     :: eps(nMedia), coeff(system_dim,0:1)
    Complex(wp), intent(out)    :: pot(Npts)
    ! --- Local variables ---------------
    Integer                     :: Incident_Medium, Expansion_Medium, Mp, percent, percent_old, &
                                   iMed, offset, i, l
    Real(wp)                    :: a, R1, mu_z, mubar_z, xi_mu(Npts), eta_mu(Npts), phi_mu(Npts),&
                                   xi_mubar(Npts), eta_mubar(Npts), phi_mubar(Npts)
    Complex(wp)                 :: expi2phiE, R, T
    
    ! ==== REMOVE LATER ==============
    !call Error_Failure("Get_multipole_pot_spheroid_prolate","Not yet implemented")
    ! ================================

    if (param%inout%verbose) then
       write(*,*) " ***Getting multipole potential."
    end if
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

    percent_old = 0
    Do i=1,Npts
       percent = int(100.0_wp*real(i,wp)/Npts)
       if (mod(percent,10)==0 .and. percent /= percent_old) then
          percent_old = percent
          write(*,'(I3,A3)',advance='no') percent,'%, ' 
          if (percent==100) write(*,*) ' '
       end if

       pot(i) = 0.0_wp
       iMed = iMedium(i)
       offset = Param%Numerics%MP_Storage_Offset(iMed)

       If (mod(iMed,2)==0) then            ! Below the substrate
          T = (2.0_wp*eps(iMed-1))/(eps(iMed-1)+eps(iMed))
          If ((iMed-1) /= Expansion_Medium) then
             ! A-terms (zero if expansion medium)
             Do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,0)*Z_func_prolate(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))       
                !m=1
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,1)*Z_func_prolate(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                pot(i) = pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1)*Z_func_prolate(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))       
             End do
             offset = offset + Mp 
          End if
          If ((iMed-1) /= Incident_Medium) then
             ! B-terms (zero if incident medium)
             Do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,0)*X_func_prolate(l,0,xi_mu(i),a) * &
                     spher_harm(l,0,eta_mu(i),phi_mu(i))       
                !m=1
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,1)*X_func_prolate(l,1,xi_mu(i),a) * &
                     spher_harm(l,1,eta_mu(i),phi_mu(i))       
                !m=-1
                pot(i) = pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1)*X_func_prolate(l,-1,xi_mu(i),a) * &
                     spher_harm(l,-1,eta_mu(i),phi_mu(i))       
             End do
          End if
          pot(i) = T*pot(i)   ! Final prefactor

       Else                                ! Above the substrate
          R = (eps(iMed)-eps(iMed+1))/(eps(iMed)+eps(iMed+1))
          If ((iMed) /= Expansion_Medium) then
             ! A-terms (zero if expansion medium)
             Do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,0) * ( &
                                  Z_func_prolate(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                                + (-1.0_wp)**(l)*R*Z_func_prolate(l,0,xi_mubar(i),a)* &
                                    spher_harm(l,0,eta_mubar(i),phi_mubar(i)) & 
                                  )
                !m=1
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,1) * ( &
                                  Z_func_prolate(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                                +(-1.0_wp)**(l+1)*R*Z_func_prolate(l,1,xi_mubar(i),a)* &
                                  spher_harm(l,1,eta_mubar(i),phi_mubar(i))    &
                                )
                !m=-1
                pot(i) = pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1) * ( &
                                  Z_func_prolate(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                                + (-1.0_wp)**(l-1)*R*Z_func_prolate(l,-1,xi_mubar(i),a)* &
                                  spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) & 
                                )
             End do
             offset = offset + Mp 
          End if
          If ((iMed) /= Incident_Medium) then
             ! B-terms (zero if incident medium)
             Do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,0) * ( &
                           X_func_prolate(l,0,xi_mu(i),a)*spher_harm(l,0,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*X_func_prolate(l,0,xi_mubar(i),a) * &
                            spher_harm(l,0,eta_mubar(i),phi_mubar(i)) &
                            )
                !m=1
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,1) * ( &
                           X_func_prolate(l,1,xi_mu(i),a)*spher_harm(l,1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*X_func_prolate(l,1,xi_mubar(i),a) * &
                            spher_harm(l,1,eta_mubar(i),phi_mubar(i)) &
                            )
                !m=-1
                pot(i) = pot(i) - R1**(-l+1)*expi2phiE * coeff(offset+l,1) * ( &
                           X_func_prolate(l,-1,xi_mu(i),a)*spher_harm(l,-1,eta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*X_func_prolate(l,-1,xi_mubar(i),a) * &
                            spher_harm(l,-1,eta_mubar(i),phi_mubar(i)) &
                            )
             End do
          End if
       End if
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
    function spher_harm(l,m,eta,phi)
    !--------------------------------!
      ! Spherical Harmonics
      ! ONLY m=-1,0,1 !
      Use Legendre_Module, only : Legendre_Plm
      implicit none
      complex(wp)           :: spher_harm
      integer, intent(in)   :: l, m
      real(wp), intent(in)  :: eta, phi
      !real(wp)              :: Plm
      
      ! -- DEBUG --
      if (abs(eta) > 1.0_wp) call Error_Failure('Prolate spher_harm', 'abs(eta) > 1')
      ! -- DEBUG --

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

  end subroutine Get_multipole_pot_spheroid_prolate
  !-----------------------------------------------------------------------!


  !--------------------------------------------------------------------------------------!
  subroutine Get_multipole_pot_sphere(x,y,z,Npts,iMedium,nMedia,eps,system_dim,coeff,pot)
  !--------------------------------------------------------------------------------------!
    Use Tools_Module, only : deg2rad
    implicit none
    integer, intent(in)         :: Npts, iMedium(Npts), nMedia, system_dim
    real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    complex(wp), intent(in)     :: eps(nMedia), coeff(system_dim,0:1)
    complex(wp), intent(out)    :: pot(Npts)
    integer                     :: i,l,m,Mp,iMed,offset, Incident_Medium, &
                                   Expansion_Medium, percent, percent_old
    real(wp)                    :: R1, r_mu(Npts),theta_mu(Npts),phi_mu(Npts),&
                                    r_mubar(Npts),theta_mubar(Npts), &
                                    phi_mubar(Npts), mu_z, mubar_z
    complex(wp)                 :: R,T,expi2phiE


    if (param%inout%verbose) then
       write(*,*) " ***Getting multipole potential."
    end if
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

    percent_old = 0
    do i=1,Npts
       percent = int(100.0_wp*real(i,wp)/Npts)
       if (mod(percent,10)==0 .and. percent /= percent_old) then
          percent_old = percent
          write(*,'(I3,A3)',advance='no') percent,'%, ' 
          if (percent==100) write(*,*) ' '
       end if
       pot(i) = 0.0_wp
       iMed = iMedium(i)
       offset = Param%Numerics%MP_Storage_Offset(iMed)
       if (mod(iMed,2)==0) then            ! Below the substrate
          T = (2.0_wp*eps(iMed-1))/(eps(iMed-1)+eps(iMed))
          if ((iMed-1) /= Expansion_Medium) then
             ! A-TERMS
             do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,0)*r_mu(i)**(-l-1) * &
                                  spher_harm(l,0,theta_mu(i),phi_mu(i))       
                !m=1
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,1)*r_mu(i)**(-l-1) * &
                                  spher_harm(l,1,theta_mu(i),phi_mu(i))       
                !m=-1
                pot(i) = pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1)*r_mu(i)**(-l-1) * &
                                  spher_harm(l,-1,theta_mu(i),phi_mu(i))       
             end do
             offset = offset + Mp 
          end if
          ! B-TERMS
          if ((iMed-1) /= Incident_Medium) then
             do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,0)*r_mu(i)**(l) * &
                                  spher_harm(l,0,theta_mu(i),phi_mu(i))       
                !m=1
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,1)*r_mu(i)**(l) * &
                                  spher_harm(l,1,theta_mu(i),phi_mu(i))       
                !m=-1
                pot(i) = pot(i) - R1**(-l+1)*expi2phiE*coeff(offset+l,1)*r_mu(i)**(l) * &
                                  spher_harm(l,-1,theta_mu(i),phi_mu(i))       
             end do
          end if
          ! Final prefactor
          pot(i) = T*pot(i)
       else                                ! Above the substrate
          R = (eps(iMed)-eps(iMed+1))/(eps(iMed)+eps(iMed+1))
          if (iMed /= Expansion_Medium) then
             ! A-TERMS
             do l=1,Mp
                !m=0
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,0) * ( &
                                  r_mu(i)**(-l-1)*spher_harm(l,0,theta_mu(i),phi_mu(i)) & 
                                +(-1.0_wp)**(l)*R*r_mubar(i)**(-l-1)* &
                                  spher_harm(l,0,theta_mubar(i),phi_mubar(i)) & 
                                                  )
                !m=1
                pot(i) = pot(i) + R1**(l+2)*coeff(offset+l,1) * ( &
                                  r_mu(i)**(-l-1)*spher_harm(l,1,theta_mu(i),phi_mu(i)) & 
                                +(-1.0_wp)**(l+1)*R*r_mubar(i)**(-l-1)* &
                                  spher_harm(l,1,theta_mubar(i),phi_mubar(i)) &
                                                  )
                !m=-1
                pot(i) = pot(i) - R1**(l+2)*expi2phiE*coeff(offset+l,1) * ( &
                                  r_mu(i)**(-l-1)*spher_harm(l,-1,theta_mu(i),phi_mu(i)) & 
                                +(-1.0_wp)**(l-1)*R*r_mubar(i)**(-l-1)* &
                                  spher_harm(l,-1,theta_mubar(i),phi_mubar(i)) & 
                                                  )
             end do
             offset = offset + Mp 
          end if
          if (iMed /= Incident_Medium) then
             ! B-TERMS
             do l=1,Mp
                !m=0
                !write(*,*) offset+l
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,0) * ( &
                           r_mu(i)**(l)*spher_harm(l,0,theta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l)*R*r_mubar(i)**(l) * &
                            spher_harm(l,0,theta_mubar(i),phi_mubar(i)) &
                            )
                !m=1
                pot(i) = pot(i) + R1**(-l+1)*coeff(offset+l,1) * ( &
                           r_mu(i)**(l)*spher_harm(l,1,theta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l+1)*R*r_mubar(i)**(l) * &
                            spher_harm(l,1,theta_mubar(i),phi_mubar(i)) &
                            )
                !m=-1
                pot(i) = pot(i) - R1**(-l+1)*expi2phiE * coeff(offset+l,1) * ( &
                           r_mu(i)**(l)*spher_harm(l,-1,theta_mu(i),phi_mu(i)) & 
                        +(-1.0_wp)**(l-1)*R*r_mubar(i)**(l) * &
                            spher_harm(l,-1,theta_mubar(i),phi_mubar(i)) &
                                                )
             end do
          end if
       end if
    end do
    
    !------!
  contains
    !------!
   

    !--------------------------!
    function spher_harm(l,m,theta,phi)
    !--------------------------!
      ! Spherical Harmonics
      ! ONLY m=-1,0,1 !
      Use Legendre_Module, only : Legendre_Plm
      !Use SFL_Precision, only : qp 
      implicit none
      complex(wp)           :: spher_harm
      integer, intent(in)   :: l, m
      real(wp), intent(in)  :: theta, phi
      !real(wp)              :: Plm
      if (m==0) then
         spher_harm = sqrt((2*l+1)/(4.0_wp*pi))*Legendre_Plm(l,0,cos(theta))
      else if (m==1) then
         spher_harm = -sqrt((2*l+1)/(4.0_wp*pi*l*(l+1)))*Legendre_Plm(l,1,cos(theta))&
              *exp(imu*phi)
      else if (m==(-1)) then
         spher_harm = sqrt((2*l+1)/(4.0_wp*pi*l*(l+1)))*Legendre_Plm(l,1,cos(theta))&
              *exp(-imu*phi)
      else
         call Error_Failure('spher_harm','abs(m)>1 not supported!')
      end if

    end function spher_harm
    !--------------------------!
   
  end subroutine Get_multipole_pot_sphere
  !----------------------------------------------------------------------------------!






  !-----------------------------------------------------------------------------!
  subroutine Get_const_pot_spheroid(iMediums,Npts,nMedia,system_dim,coeff,&
                                    eps,pot)
  !-----------------------------------------------------------------------------!
    ! --------------------------------------------------------------------------!
    ! Sindre: The constant potential terms are equal for the oblate and prolate 
    ! spheroidal case. The only exceptions is in the definition of xi_0. This
    ! routine is therefore also used for the prolate case.
    ! --------------------------------------------------------------------------!

    !Setting E_0 = 0
    Use Tools_Module, only : deg2rad
    implicit none
    integer, intent(in)         :: Npts, nMedia, system_dim, iMediums(Npts)
    complex(wp), intent(in)     :: coeff(system_dim,0:1), eps(nMedia)
    complex(wp), intent(out)    :: pot(Npts)
    complex(wp)                 :: pot_vals(nMedia), epsfrac, &
                                   I_terms(2), K_terms(2), R(2), fac(2)
    real(wp)                    :: chi,R1,tr1,xi_0_1,xi_0_s,costhetaE
    integer                     :: iMedium,i,s,l,iMP,iMPImage,iLimit_tr, &
                                   iLimit_One,incident_medium,expansion_medium, &
                                   offset(2),Mp

    R1 = param%geometry%radius(1)
    tr1 = param%geometry%truncation_ratio
    epsfrac = eps(1)/eps(2)
    costhetaE = cos(param%source%ThetaE * deg2rad())
    ! --- Oblate/Prolate ------------------------------------------------------------
    If (Param%Geometry%isOblate) Then       !OBLATE SPHEROID
       xi_0_1 = Param%Geometry%Radius(1) &
                /sqrt(Param%Geometry%Radius(2)**2 - Param%Geometry%Radius(1)**2)
    Else                                    !PROLATE SPHEROID
       xi_0_1 = Param%Geometry%Radius(1) &
                /sqrt(Param%Geometry%Radius(1)**2 - Param%Geometry%Radius(2)**2)
    Endif
    ! --------------------------------------------------------------------------------
    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    Mp = param%numerics%multipole_order
    pot_vals(1) = 0.0_wp
    pot_vals(2) = 0.0_wp
    do iMedium=3,nMedia,2
       s = (iMedium-1)/2
       iMP = Integrals(s)%MultiPole
       iMPImage = Integrals(s)%ImageMultiPole
       iLimit_tr = Integrals(s)%UpperLimit_tr
       iLimit_One = Integrals(s)%UpperLimit_One
       chi = param%geometry%radius_ratios(s)
       xi_0_s = chi*xi_0_1
       R(1) = (eps(2*s-1)-eps(2*s))/(eps(2*s-1)+eps(2*s))
       R(2) = (eps(2*s+1)-eps(2*s+2))/(eps(2*s+1)+eps(2*s+2))
       fac(1) = 2.0_wp*eps(2*s-1)/(eps(2*s-1)-eps(2*s))
       fac(2) = 2.0_wp*eps(2*s+1)/(eps(2*s+1)-eps(2*s+2))
       ! Sum:
       pot_vals(iMedium) = 0.0_wp
       do l=1,Mp
          I_terms(1) = xi_0_s**(l+1)*R(1)*( &
                                    -Integrals(s)%K(0,l,0,iMP,iLimit_tr) &
                       + (-1.0_wp)**(l)*Integrals(s)%K(0,l,0,iMPImage,iLimit_tr) &
                            + fac(1)*Integrals(s)%K(0,l,0,iMP,iLimit_One) &
                                    )
          I_terms(2) = xi_0_s**(l+1)*R(2)*( &
                                    -Integrals(s)%K(0,l,0,iMP,iLimit_tr) &
                    + (-1.0_wp)**(l)*Integrals(s)%K(0,l,0,iMPImage,iLimit_tr) &
                            + fac(2)*Integrals(s)%K(0,l,0,iMP,iLimit_One) &
                                      )
          K_terms(1) = xi_0_s**(-l)*R(1)*( &
                                    -Integrals(s)%M(0,l,0,iMP,iLimit_tr) &
                    + (-1.0_wp)**(l)*Integrals(s)%M(0,l,0,iMPImage,iLimit_tr) &
                            + fac(1)*Integrals(s)%M(0,l,0,iMP,iLimit_One) &
                                     )
          K_terms(2) = xi_0_s**(-l)*R(2)*( &
                                    -Integrals(s)%M(0,l,0,iMP,iLimit_tr) &
                    + (-1.0_wp)**(l)*Integrals(s)%M(0,l,0,iMPImage,iLimit_tr) &
                            + fac(2)*Integrals(s)%M(0,l,0,iMP,iLimit_One) &
                                     )
          offset(1) = param%numerics%MP_Storage_Offset(iMedium-2)
          offset(2) = param%numerics%MP_Storage_Offset(iMedium)
          if ((iMedium-2) /= Expansion_Medium) then
             ! A-term, outside interface s
             pot_vals(iMedium) = pot_vals(iMedium) + &  
                      param%numerics%zeta(0,l,0) * chi**(-l-2) * &
                      I_terms(1)*coeff(offset(1)+l,0)
             !write(*,*) "A(2s-1),s=",s,"l=",l,':',(offset(1)+l)
             offset(1) = offset(1) + Mp
          end if
          if (iMedium /= Expansion_Medium) then
             ! A-term, inside interface s
             pot_vals(iMedium) = pot_vals(iMedium) - &  
                  param%numerics%zeta(0,l,0) * chi**(-l-2) * &
                  I_terms(2)*coeff(offset(2)+l,0)
             !write(*,*) "A(2s+1),s=",s,"l=",l,':',(offset(2)+l)
             offset(2) = offset(2) + Mp
          end if
          
          if ((iMedium-2) /= Incident_Medium) then
             ! B-term, outside interface s
             pot_vals(iMedium) = pot_vals(iMedium) + &  
                      param%numerics%zeta(0,l,0) * chi**(l-1) * &
                      K_terms(1)*coeff(offset(1)+l,0)
             !write(*,*) "B(2s-1),s=",s,"l=",l,':',(offset(1)+l)
          end if
          if (iMedium /= Incident_Medium) then
             ! B-term, inside interface s
             pot_vals(iMedium) = pot_vals(iMedium)- &  
                      param%numerics%zeta(0,l,0) * chi**(l-1) * &
                      K_terms(2)*coeff(offset(2)+l,0)
             !write(*,*) "B(2s+1),s=",s,"l=",l,':',(offset(2)+l)
          end if

       end do
       ! Sum prefactor:
       pot_vals(iMedium) = pot_vals(iMedium)/(2.0_wp*sqrt(pi))
       
       ! \delta_{s,1} term:
       if (s==1) then
          pot_vals(iMedium) = pot_vals(iMedium) - &
               costhetaE*(epsfrac-1.0_wp) * (1.0_wp-tr1)**2/4.0_wp
       end if
       
       ! Multiply by prefactor. Add constant potential of previous medium, 
       ! since we really calculated the difference. 
       pot_vals(iMedium) = pot_vals(iMedium-2) + pot_vals(iMedium)*chi*R1
       ! The corresponding medium below the substrate has the same 
       ! constant potential.
       pot_vals(iMedium+1) = pot_vals(iMedium)
    end do
  
    if (param%inout%verbose) then
       write(*,*) " *** Constant terms of the potential: "
       do iMedium=1,nMedia
          write(*,'(A12,I3,A2,F15.8,F15.8,A1)') &
               "     Medium ",iMedium,": ",pot_vals(iMedium),'i'
       end do
    end if
    
    ! Setting constant potential for all points based on which medium they 
    ! belong to.
    do i=1,Npts
       pot(i) = pot_vals(iMediums(i))
    end do

  end subroutine Get_const_pot_spheroid
  !------------------------------------------!




  !-------------------------------------------------------------------------!
  subroutine Get_const_pot_sphere(iMediums,Npts,nMedia,system_dim,coeff,&
                                    eps,pot)
  !-------------------------------------------------------------------------!
    !Setting E_0 = 0
    Use Tools_Module, only : deg2rad
    implicit none
    integer, intent(in)         :: Npts, nMedia, system_dim, iMediums(Npts)
    complex(wp), intent(in)     :: coeff(system_dim,0:1), eps(nMedia)
    complex(wp), intent(out)    :: pot(Npts)
    complex(wp)                 :: pot_vals(nMedia), epsfrac, R(2), T(2),&
         I_terms(4)
    real(wp)                    :: chi,R1,Rs,tr1,costhetaE
    integer                     :: iMedium,i,s,l,iMP,iMPImage,iLimit_tr, &
                                   iLimit_One,incident_medium,expansion_medium, &
                                   offset(2),Mp

    R1 = param%geometry%radius(1)
    tr1 = param%geometry%truncation_ratio
    epsfrac = eps(1)/eps(2)
    costhetaE = cos(param%source%ThetaE * deg2rad())
    Incident_Medium    =  Param%Source%iambient
    Expansion_Medium   =  Param%Numerics%MP_Expansion_Medium
    Mp = param%numerics%multipole_order
    pot_vals(1) = 0.0_wp
    pot_vals(2) = 0.0_wp
    do iMedium=3,nMedia,2
       s = (iMedium-1)/2
       iMP = Integrals(s)%MultiPole
       iMPImage = Integrals(s)%ImageMultiPole
       iLimit_tr = Integrals(s)%UpperLimit_tr
       iLimit_One = Integrals(s)%UpperLimit_One
       chi = param%geometry%radius_ratios(s)
       Rs = chi*R1
       R(1) = (eps(2*s-1)-eps(2*s))/(eps(2*s-1)+eps(2*s))
       R(2) = (eps(2*s+1)-eps(2*s+2))/(eps(2*s+1)+eps(2*s+2))
       T(1) = 2.0_wp*eps(2*s-1)/(eps(2*s-1)+eps(2*s))
       T(2) = 2.0_wp*eps(2*s+1)/(eps(2*s+1)+eps(2*s+2))
       ! Sum:
       pot_vals(iMedium) = 0.0_wp
       do l=1,Mp
          !Outside
          I_terms(1) =    Integrals(s)%K(0,l,0,iMP,iLimit_tr)   &
                          + (-1.0_wp)**(l) * R(1) * Integrals(s)%K(0,l,0,iMPImage,iLimit_tr) &
                          + T(1) * (   Integrals(s)%K(0,l,0,iMP,iLimit_One)  &
                                            - Integrals(s)%K(0,l,0,iMP,iLimit_tr) )   

          !Inside
          I_terms(2) =    Integrals(s)%K(0,l,0,iMP,iLimit_tr)   &
                          + (-1.0_wp)**(l) * R(2) * Integrals(s)%K(0,l,0,iMPImage,iLimit_tr) &
                          + T(2) * (   Integrals(s)%K(0,l,0,iMP,iLimit_One)  &
                          - Integrals(s)%K(0,l,0,iMP,iLimit_tr) )   

          !Outside
          I_terms(3) =    Integrals(s)%M(0,l,0,iMP,iLimit_tr)   &
                          + (-1.0_wp)**(l) * R(1) * Integrals(s)%M(0,l,0,iMPImage,iLimit_tr) &
                          + T(1) * (   Integrals(s)%M(0,l,0,iMp,iLimit_One)  &
                                      - Integrals(s)%M(0,l,0,iMp,iLimit_tr) )   

          !Inside
          I_terms(4) =    Integrals(s)%M(0,l,0,iMp,iLimit_tr)   &
                          + (-1.0_wp)**(l) * R(2) * Integrals(s)%M(0,l,0,iMPImage,iLimit_tr)  &
                          + T(2) * (   Integrals(s)%M(0,l,0,iMp,iLimit_One)  &
                                      - Integrals(s)%M(0,l,0,iMp,iLimit_tr) )   

          offset(1) = param%numerics%MP_Storage_Offset(iMedium-2)
          offset(2) = param%numerics%MP_Storage_Offset(iMedium)
          if ((iMedium-2) /= Expansion_Medium) then
             ! A-term, outside interface s
             pot_vals(iMedium) = pot_vals(iMedium) + &  
                      param%numerics%zeta(0,l,0) * chi**(-l-2) * &
                      I_terms(1)*coeff(offset(1)+l,0)
             !write(*,*) "A(2s-1),s=",s,"l=",l,':',(offset(1)+l)
             offset(1) = offset(1) + Mp
          end if
          if (iMedium /= Expansion_Medium) then
             ! A-term, inside interface s
             pot_vals(iMedium) = pot_vals(iMedium) - &  
                      param%numerics%zeta(0,l,0) * chi**(-l-2) * &
                      I_terms(2)*coeff(offset(2)+l,0)
             !write(*,*) "A(2s+1),s=",s,"l=",l,':',(offset(2)+l)
             offset(2) = offset(2) + Mp
          end if

          if ((iMedium-2) /= Incident_Medium) then
             ! B-term, outside interface s
             pot_vals(iMedium) = pot_vals(iMedium) + &  
                      param%numerics%zeta(0,l,0) * chi**(l-1) * &
                      I_terms(3)*coeff(offset(1)+l,0)
             !write(*,*) "B(2s-1),s=",s,"l=",l,':',(offset(1)+l)
          end if
          if (iMedium /= Incident_Medium) then
             ! B-term, inside interface s
             pot_vals(iMedium) = pot_vals(iMedium)- &  
                      param%numerics%zeta(0,l,0) * chi**(l-1) * &
                      I_terms(4)*coeff(offset(2)+l,0)
             !write(*,*) "B(2s+1),s=",s,"l=",l,':',(offset(2)+l)
          end if

       end do
       ! Sum prefactor:
       pot_vals(iMedium) = pot_vals(iMedium)/(2.0_wp*sqrt(pi))

       ! \delta_{s,1} term:
       if (s==1) then
          pot_vals(iMedium) = pot_vals(iMedium) - &
                  costhetaE*(epsfrac-1.0_wp) * (1.0_wp-tr1)**2/4.0_wp
       end if

       ! Multiply by prefactor. Add constant potential of previous medium, 
       ! since we really calculated the difference. 
       pot_vals(iMedium) = pot_vals(iMedium-2) + pot_vals(iMedium)*chi*R1
       ! The corresponding medium below the substrate has the same 
       ! constant potential.
       pot_vals(iMedium+1) = pot_vals(iMedium)
    end do
  
    if (param%inout%verbose) then
       write(*,*) " *** Constant terms of the potential: "
       do iMedium=1,nMedia
          write(*,'(A12,I3,A2,F15.8,F15.8,A1)') &
                        "     Medium ",iMedium,": ",pot_vals(iMedium),'i'
       end do
    end if
  
    ! Setting constant potential for all points based on which medium they 
    ! belong to.
    do i=1,Npts
       pot(i) = pot_vals(iMediums(i))
    end do

  end subroutine Get_const_pot_sphere
  !----------------------------------------------------------------------------!




  !---------------------------------------------------------!
  subroutine Get_inc_trans_pot(x,y,z,eps,iMedium,Npts,pot)
  !---------------------------------------------------------!
    !Setting E_0 = 0
    Use Tools_Module, only : deg2rad
    implicit none
    integer, intent(in)         :: Npts, iMedium(Npts)
    real(wp), intent(in)        :: x(Npts),y(Npts),z(Npts)
    complex(wp), intent(in)     :: eps(:)
    complex(wp), intent(out)    :: pot(Npts)
    integer                     :: i
    real(wp)                    :: thetaE, phiE,d,sinthetaE,costhetaE, &
                                   sinphiE,cosphiE
    complex(wp)                 :: epsfrac
    
    if (param%inout%verbose) then
       write(*,*) "***Getting incoming and transmitted potential."
    end if
    epsfrac = eps(1)/eps(2)
    d = param%geometry%distance(1)
    thetaE = param%source%ThetaE * deg2rad()
    phiE = param%source%PhiE * deg2rad()
    sinthetaE = sin(thetaE)
    costhetaE = cos(thetaE)
    sinphiE = sin(phiE)
    cosphiE = cos(phiE)
  
    do i=1,Npts
       if (iMedium(i) == 1) then !Ambient
          pot(i) = -(sinthetaE*cosphiE*x(i) &
                   + sinthetaE*sinphiE*y(i) &
                   + costhetaE*z(i))
       else if (iMedium(i) == 2) then !Substrate
          pot(i) = (epsfrac-1.0_wp)*d*costhetaE &
                 - (epsfrac*costhetaE*z(i) + sinthetaE*sinphiE*y(i) &
                 + sinthetaE*cosphiE*x(i))
       else !Inside island
          pot(i) = 0.0_wp
       end if
    end do

  end subroutine Get_inc_trans_pot
  !----------------------------------------------------------------!



  !----------------------------------------------------------!
  subroutine Get_Medium_Spheroid(xi,eta,phi,Npts,iMedium)
  !----------------------------------------------------------!
    ! Get medium number of points (xi,eta,phi), assuming that the coordinate 
    ! system is centered at the main origin (center of spheroids / delta_z = 0).
    ! 
    ! Sindre: This routine takes in a list of (xi,eta,phi) coordinates and returns
    ! a list of equal length which contains the medium in which each coordinate
    ! set lies.
    ! This routine works for both oblate and prolate spheroids. The only difference
    ! lies in the definition of the parameter a.
    !
    implicit none
    integer, intent(in)           :: Npts
    real(wp), intent(in)          :: xi(Npts),eta(Npts),phi(Npts)
    integer, intent(out)          :: iMedium(Npts)
    integer                       :: i,j,s
    real(wp)                      :: xi_0_1, xi_0_s, a,z,d
    
    If ( Param%Geometry%isOblate ) Then   ! Oblate
       a = sqrt(param%geometry%radius(2)**2-param%geometry%radius(1)**2)
    Else                                  ! Prolate
       a = sqrt(param%geometry%radius(1)**2-param%geometry%radius(2)**2)
    End if
    xi_0_1 = param%geometry%radius(1)/a
    d = param%geometry%distance(1)
    
    do i=1,Npts
       ! Find which interface s it is INSIDE of. 
       ! s defaults to 0 if it is completely outside the particle..
       s = 0
       do j=1,size(param%geometry%radius_ratios)
          xi_0_s = param%geometry%radius_ratios(j)*xi_0_1
          if (xi(i)<xi_0_s) s = j
       end do
       z = a*xi(i)*eta(i)
       if (z>d) then     !Below the substrate
          iMedium(i) = 2*s+2
       else              !Above the substrate
          iMedium(i) = 2*s+1
       end if
    end do

  End Subroutine Get_Medium_Spheroid
  !-------------------------------------------------------------!



  !----------------------------------------------------------!
  subroutine Get_Medium_Sphere(r,theta,phi,Npts,iMedium)
  !----------------------------------------------------------!
    ! Get medium number of points (r,theta,phi), assuming that the coordinate 
    ! system is centered at the main origin (center of spheres / delta_z = 0).
    implicit none
    integer, intent(in)           :: Npts
    real(wp), intent(in)          :: r(Npts),theta(Npts),phi(Npts)
    integer, intent(out)          :: iMedium(Npts)
    real(wp)                      :: d,R1,Rs
    integer                       :: i,j,s
    
    d = param%geometry%distance(1)
    R1 = param%geometry%radius(1)
    do i=1,Npts
       ! Find which interface s it is INSIDE of. 
       ! s defaults to 0 if it is complecompletely outside the particle..
       s = 0
       do j=1,size(param%geometry%radius_ratios)
          Rs = param%geometry%radius_ratios(j)*R1
          if (r(i)<Rs) s = j
       end do
       !z = a*xi(i)*eta(i)
       if ((r(i)*cos(theta(i))) > d) then     !Below the substrate
          iMedium(i) = 2*s+2
       else                                   !Above the substrate
          iMedium(i) = 2*s+1
       end if
    end do

  end subroutine Get_Medium_Sphere
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


  !-------------------------------------------------------------------!
  subroutine Cartesian_to_Spheroidal_Prolate(x,y,z,Npts,a,delta_z,xi,eta,phi)
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



  !-----------------------------------!
  subroutine Get_Points(x,y,z,Npts)
  !-----------------------------------!
    ! Get the wanted points (x,y,z) to calculate the potential at. 
    ! Points are loaded from file Param%Potential%Points_File
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none 
    character(len=*), parameter         :: routine = "Get_Points"
    character(len=200)                  :: line
    integer                             :: file_id, iostatvar, i
    real(wp), intent(out), allocatable  :: x(:),y(:),z(:)
    integer, intent(out)                :: Npts
    
    Npts = 0
    Call SFL_Get_Free_Unit(file_id)
    open(unit=file_id, file = param%potential%points_file, status='old', action='read')
    do while (.true.)
       read(unit=file_id,fmt='(A)', iostat=iostatvar) line
       line = trim(adjustl(line))
       if (iostatvar == 0 .and. line /= '') then
          Npts = Npts + 1
       else if (iostatvar>0) then
          call Error_Failure(routine,("Input error from Points_File"))
       else if (iostatvar<0) then
          !End of file
          exit
       end if
    end do
    if (param%inout%verbose) then
       write(*,'(A,I7,A,A)') ' ***Found ',Npts,' points in ',&
            trim(adjustl(param%potential%points_file))
    end if
    rewind(unit=file_id)  
    allocate(x(Npts),y(Npts),z(Npts))
    
    do i=1,Npts 
       read(unit=file_id, fmt=*, iostat=iostatvar) x(i),y(i),z(i)
       if (iostatvar>0) then
          call Error_Failure(routine,"Input error from Points_File.")
       end if
    end do
    close(unit=file_id)
    
    !do i=1,Npts
    !  write(*,*) x(i),y(i),z(i)
    !end do
  end subroutine Get_Points
    !---------------------------------------------!





  !---------------------------------------------------------------------!
  subroutine write_to_file(x,y,z,Matrix,iMedium,Npts,cols,iEnergies, &
                                nMedia,eps)
  !---------------------------------------------------------------------!
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    integer, intent(in)       :: Npts, cols, iMedium(Npts),iEnergies(cols),nMedia
    complex(wp), intent(in)   :: Matrix(Npts,cols), eps(cols,nMedia)
    real(wp), intent(in)      :: x(Npts),y(Npts),z(Npts)
    integer                   :: file_id,i,j
    
  
    ! Writing potential
    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
                                //"_potential.dat")
    write(file_id,fmt='(A)',advance='no') "# x   y   z   Medium    Potential(Re,Im) "
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
  
    ! Writing medium integers. NEW: MEDIUM IS NOW WRITTEN IN "..._potential.dat"
    !call SFL_Get_Free_Unit( file_id )
    !Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
    !                          //"_media.dat")
    !do i=1,Npts
    !  write(file_id,*) iMedium(i)
    !end do 
    !Close(unit=file_id)

  end subroutine write_to_file
  !--------------------------------------------------------------------------!


  ! =========================================================================!
  ! DEBUGGING ROUTINES (May be removed or commented out later)
  ! -- Sindre Stavseng, Apr 2013
  ! =========================================================================!
  !
  ! -------------------------------------------------------------
  subroutine write_multipole_coeffs_to_file( coeffs, system_dim )
  ! -------------------------------------------------------------
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    integer, intent(in)            :: system_dim
    complex(wp), intent(in)        :: coeffs(system_dim,0:1)
    ! ------------
    integer                        :: file_id,i

    ! Writing coefficients
    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file="mult_coeffs.dat")
    !write(file_id,*) "Test!"
    do i=1,system_dim
       write(file_id,*) coeffs(i,0), coeffs(i,1)
    end do
    Close(unit=file_id)

  end subroutine write_multipole_coeffs_to_file
  ! --------------------------------------------------------------
  
  
  
  ! ===========================================================================!


!-------------------------------------------------------------------------------!
End Module Potential_Module
!-------------------------------------------------------------------------------!




