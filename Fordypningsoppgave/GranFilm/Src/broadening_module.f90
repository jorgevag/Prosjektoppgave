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
! --- AUTHOR : Eskil Aursand, Trondheim, 2011.
!
! ----------------------------------------------------------
!

!-----------------------------!
Module Broadening_Module
!-----------------------------!

  ! --- The Use Statements global to the module
  Use Shared
  Use Error_Module,  only : Error_Failure, Error_Warning

  ! --------------------------------------!
  ! --- The Publicly available routines
  ! --------------------------------------!
  Public :: Broaden_Alpha 

  ! --------------------------------------!
  ! --- Everyting is Private by default
  ! --------------------------------------!
  Private
  
Contains
 
  !-----------------------------!
  subroutine Broaden_Alpha()
  !-----------------------------!
  Use Special_functions_module,   only : Arth
  Use SFL_Interpolation,          only : SFL_AkimaSpline 
  implicit none

  ! alpha(Dipole/quadrupole(1/2), parallel/perpendicular(1/2), ienergy)
  complex(wp)                     :: alpha(2,2,param%numerics%No_Energy_Points)
  integer                         :: par,perp,k,m,i,nenergy,N_pts,ierr
  character(len=*), parameter     :: routine = "Broaden_Alpha" 
  real(wp)                        :: delta_E, delta_E_temp,broad_par,&
                                     broad_perp,&
                                     energy(param%numerics%No_Energy_Points)
  real(wp), allocatable           :: energy_temp(:) 
  complex(wp), allocatable        :: alpha_temp(:,:,:) 
  logical                         :: do_interpolation
 


  ! The limit for broadening where they are set to zero:
  real(wp), parameter             :: min_broadening = 0.001_wp

  !

  If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
 

  ! --- Warning
  call  Error_Warning( routine, "This routine should be checked and rewritten" ) 



  energy(:)           = param%numerics%energy(:)
  par                 = Results%par
  perp                = Results%perp 
  delta_E             = energy(2)-energy(1)
  nenergy             = param%numerics%No_Energy_Points
  broad_par           = Param%Geometry%Broadening_Par
  broad_perp          = Param%Geometry%Broadening_Perp
  
  ! Must limit how low the broadening is allowed to go during curve fitting,
  ! if not the interpolation procedure below will take infinite time and 
  ! memory as the broadening approaches zero.
  if (Param%InOut%Do_Curve_Fitting) then
    if (broad_par < Param%Curvefitting%broadening_zero) then 
      broad_par = 0.0_wp
    end if
    if (broad_perp < Param%Curvefitting%broadening_zero) then
      broad_perp = 0.0_wp
    end if
  end if

  ! For now Gaussian_Convolution is written for the GranFilm 1.0 
  ! data stuctures. This is an ugly hack to transform the current data 
  ! structures to the old one and back before and after calling the 
  ! subroutine. In the long run it should be rewritten instead...
  !
  !  
  ! For the convolution to work, Delta_E must be smaller than the 
  ! ad-hoc sigmas. 
  ! If this is not the case, increase the number of energy points in the 
  ! polarizability curves, do the convolution, and interpolate back to 
  ! the original number of points.
      
  do i=1,nenergy    
    alpha(1,1,i) = results%polarizabilities(i,1)%Dipole(par)
    alpha(1,2,i) = results%polarizabilities(i,1)%Dipole(perp)
    alpha(2,1,i) = results%polarizabilities(i,1)%Quadrupole(par)
    alpha(2,2,i) = results%polarizabilities(i,1)%Quadrupole(perp)
  end do
  
  do_interpolation = ((broad_par < delta_E .and. broad_par > 0.0_wp) & 
                      .or. (broad_perp < delta_E .and. broad_perp > 0.0_wp))

  if (do_interpolation) then
    If (Param%InOut%Verbose) then
      write(*,*) '--- Doing interpolation to represent the low broadening..'
    end if
    if (broad_par == 0.0_wp) then 
      delta_E_temp = broad_perp
    else if (broad_perp == 0.0_wp) then
      delta_E_temp = broad_par
    else
      delta_E_temp    = min(broad_par,broad_perp)
    end if

    N_pts           = ((energy(nenergy)-energy(1))/delta_E_temp) + 1
    allocate(energy_temp(N_pts),alpha_temp(2,2,N_pts))
    energy_temp(:)  = Arth(energy(1),delta_E_temp,N_pts)
    
    do m=1,2
      do k=1,2
        ! Interpolating to more energy points
        alpha_temp(m,k,1) = alpha(m,k,1)
        alpha_temp(m,k,N_pts) = alpha(m,k,nenergy)
        call SFL_AkimaSpline(energy(:),alpha(m,k,:),ierr,&
                            energy_temp(2:N_pts-1),alpha_temp(m,k,2:N_pts-1))

        if (ierr<0) then
          call Error_Failure(routine, "SFL_AkimaSpline reported an error") 
        end if
      end do
    end do 
  else
    N_pts           = param%numerics%No_Energy_Points
    allocate(energy_temp(N_pts),alpha_temp(2,2,N_pts))
    energy_temp(:)  = energy(:)
    alpha_temp(:,:,:) = alpha(:,:,:)
  end if



  call Gaussian_Convolution(energy_temp,alpha_temp,broad_par,broad_perp,N_pts)
  
  
  if (do_interpolation) then 
    do m=1,2
      do k=1,2
        ! Interpolating back to fewer energy points
        !write(*,*) "Interpolating to less points"
        !write(*,*) energy_temp(1),energy_temp(N_pts)
        alpha(m,k,1) = alpha_temp(m,k,1)
        alpha(m,k,nenergy) = alpha_temp(m,k,N_pts)
        call SFL_AkimaSpline(energy_temp(:),alpha_temp(m,k,:),ierr,&
                              energy(2:nenergy-1),alpha(m,k,2:nenergy-1))
        if (ierr<0) then
          call Error_Failure(routine, "SFL_AkimaSpline reported an error") 
        end if
      end do
    end do 
  else
    alpha(:,:,:) = alpha_temp(:,:,:)
  end if

  do i=1,nenergy    
    results%polarizabilities(i,1)%Dipole(par) = alpha(1,1,i)
    results%polarizabilities(i,1)%Dipole(perp) = alpha(1,2,i) 
    results%polarizabilities(i,1)%Quadrupole(par) = alpha(2,1,i)  
    results%polarizabilities(i,1)%Quadrupole(perp) = alpha(2,2,i) 
  end do

 
  If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine


  end subroutine Broaden_Alpha
  !-----------------------------!  


  !
  ! ---
  !



  !-------------------------------------------------------------------------!
  Subroutine Gaussian_Convolution(energy,alpha,sigma_par,sigma_perp,N_pts)
  !-------------------------------------------------------------------------!
    ! (Subroutine by Remi Lazzari, slightly rewritten to 
    ! fit GranFilm 2.0)
    !
    ! Convolution of the polarizabilities with a gaussian
    
    ! alpha(Dipole/quadrupole, parallel/perpendicular, ienergy)

    Use Special_functions_module, only : Arth
    
    implicit none
    Integer, intent(in)       :: N_pts
    Complex(wp), Intent(InOut):: alpha(2,2,N_pts)
    Complex(wp)               :: alpha_conv(2,2,N_pts)
    Real(wp)                  :: energy(N_pts)
    Real(wp), Allocatable     :: bound_energy(:)
    Real(wp)                  :: val,de,sigE(2),Emin,Emax,sigma_par,sigma_perp
    Integer                   :: nbound,m,ienergy,k
  
    !! Temporary prints
    !write(*,*) "No_Energy_Points= ", param%numerics%No_Energy_Points
    !write(*,*) "size(alpha)= ",size(alpha)
    !write(*,*) "shape(alpha)= ",shape(alpha)
    !!
    
    sigE(1)   = sigma_par
    sigE(2)   = sigma_perp

    ! Step in energy
    !nenergy                 = param%Nenergy
    !nenergy                 = param%numerics%No_Energy_Points
    !energy(:)              = param%energy(:)
    !energy(:)               = param%numerics%energy(:)
    Emin                    = energy(1)
    Emax                    = energy(N_pts)
    de                      = energy(2)-energy(1)
    !sigE(1)                 = param%sigEpar%v
    !sigE(2)                 = param%sigEperp%v
    nbound                  = Int(Max(3*Max(sigE(1),sigE(2))/de,1._wp))
    Allocate(bound_energy(2*nbound))
    bound_energy(1:nbound)  = Arth(Emin-de,-de,nbound)
    bound_energy(nbound+1:2*nbound)   = Arth(Emax+de,de,nbound)
    
    
    !write(*,*) "Starting loop"
    ! Parallel(k=1) and perpendicular(k=2) directions
    Do k=1,2
    
     If(sigE(k).ne.0._wp) Then
      val = 1._wp/sigE(k)/sqrt(2._wp*pi)*de
      ! The convolution
      Do ienergy = 1,N_pts
       Do m=1,2
        alpha_conv(m,k,ienergy) = Sum(alpha(m,k,:)*&
                      Gauss(energy(ienergy),sigE(k),energy(:))) + &
                      alpha(m,k,1)*Sum(Gauss(energy(ienergy),sigE(k),&
                      bound_energy(1:nbound))) + &
                      alpha(m,k,N_pts)*Sum(Gauss(energy(ienergy),&
                      sigE(k),bound_energy(nbound+1:2*nbound)))
       Enddo 
      Enddo
      alpha(:,k,:) = alpha_conv(:,k,:)*val  
     Endif
    Enddo

    ! Free space
    Deallocate(bound_energy)

  Contains

    !----------------------------------------!
    Function Gauss(y0,sig,y) Result(Gaussres)
    !----------------------------------------!
      ! Only exponential. No prefactor.
      Real(wp)    ::      sig, y0, y(:)
      Real(wp)    ::      Gaussres(size(y))
      Gaussres(:) = exp(-(y(:)-y0)**2/2._wp/sig**2)
    End Function Gauss
    !-----------------------------!

  End Subroutine Gaussian_Convolution
  !------------------------------------------------------------------------!




End Module Broadening_Module
!-----------------------------------------------------------------!  

