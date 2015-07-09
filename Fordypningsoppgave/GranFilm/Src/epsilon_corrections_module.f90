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
! --- AUTHOR  : Eskil Aursand, Trondheim, 2011.
!               Ingve Simonsen, Paris, Dec 2011
!              
! --- History : 
!
!       - Dec 2011, IS
!         Corrected an allocation error in Epsilon_Finite_Size_Correction
!
!
!
! ----------------------------------------------------------
!



!---------------------------------!
Module Epsilon_Corrections_Module
!---------------------------------!

  ! --- The Use Statements global to the module
  Use Shared

  ! --------------------------------------!
  ! --- The Publicly available routines
  ! --------------------------------------!
  Public :: Epsilon_Scaling
  Public :: Epsilon_Corrections



  ! --------------------------------------!
  ! --- Everyting is Private by default
  ! --------------------------------------!
  Private


  
Contains





  !-------------------------------------------!
  Subroutine Epsilon_Scaling( )
  !-------------------------------------------!
    ! 
    ! --- This routine scales the epsilon values according to the value of 
    !     param%Media(:)%Epsilon_Scale
    !     Only when  param%Media(:)%Epsilon_Scale /= (1._wp, 1._wp) is the scaling done
    !
    
    Implicit None
    ! --- Local
    Character(len=*), parameter     :: routine = "Epsilon_Scaling"
    Integer             :: i
    Real(wp), parameter :: two = 2._wp, tiny = 2*epsilon(1._wp)
    
    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    ! --- Loop over the media
    do i=1,size(Param%Media,1)

       ! --- Do the scaling (only if required..)
       if ( .not. all(  abs(Param%Media(i)%Epsilon_Scale-1._wp)< tiny )  ) then
          Param%Media(i)%Epsilon = Real(Param%Media(i)%Epsilon,wp) * Param%Media(i)%Epsilon_Scale(1) &
               + imu * Real(-imu*Param%Media(i)%Epsilon,wp) * Param%Media(i)%Epsilon_Scale(2) 
          write(*,*) "Performed epsilon scaling"
       endif
       
    end do
    
    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Epsilon_Scaling
  !-------------------------------------------!



  !----------------------------------!
  Subroutine Epsilon_Corrections()
  !----------------------------------!
    ! 
    ! --- Main routine for correcting the dielectric functions 
    !     finite-size and for temperature effects
    !
    implicit none
    ! --- Local
    character(len=*), parameter     :: routine = "Epsilon_Corrections"
    integer                         :: i
    integer                         :: ifile

    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
    
  
    ! --- Main work
    do i=1,size(Param%Geometry%Media,1)
  
       if (Param%Media(i)%Do_Temperature_Correction)  &
            call Epsilon_Temperature_Correction(i)

       if (Param%Media(i)%Do_Size_Correction)         &
            call Epsilon_Finite_Size_Correction(i)

    end do

    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
 
  end Subroutine Epsilon_Corrections
  !-----------------------------------!




  !------------------------------------------------!
  Subroutine Epsilon_Temperature_Correction(imedia)
  !------------------------------------------------!
    ! 
    ! --- Routine for correcting the dielectric function for temperature
    !
    !
    implicit none
    integer, intent(in)             :: imedia
    ! --- Local
    character(len=*), parameter     :: routine = "Epsilon_Temperature_Corrections"
  
    ! ---Verbose
    If (Param%InOut%Verbose)                    &
         Write(*,'(A17 A24 A9 I2 A2 A5 A1)')    &
          "  +++ Entering : ", routine,         &
          " (Media #", imedia,": ", adjustl(trim(param%geometry%media(imedia))),")"

    ! --- Main Work
    If (Param%InOut%Verbose)  Write(*,*) "     Routine doing nothing as for now!"

    ! ---Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine

  end subroutine Epsilon_Temperature_Correction
  !--------------------------------------------!






  !-------------------------------------------------!
  Subroutine Epsilon_Finite_Size_Correction(imedia)
  !-------------------------------------------------!
    Use Error_Module,               only : Error_Failure
    implicit none
    integer, intent(in)             :: imedia
    ! --- Local
    character(len=*), parameter     :: routine = "Epsilon_Finite_Size_Corrections"
    real(wp)                        :: Ep
    real(wp)                        :: hbar_inv_tau_bulk
    real(wp)                        :: hbar_inv_tau_corr
    real(wp)                        :: hbar_fermi_vel
    real(wp)                        :: meanfreepath
    real(wp)                        :: B_plasmonshift
    real(wp)                        :: surface_over_volume
    real(wp)                        :: sigma
    real(wp)                        :: R
    real(wp)                        :: t_r,a,xi_0
    integer                         :: nMedia,s
    !--Remove ---real(wp), allocatable           :: E(:)      


    ! --- Verbose
    If (Param%InOut%Verbose)                   &
           Write(*,'(A17,A24,A9,I2,A2,A5,A1)') &
            "  +++ Entering : ",               & 
            routine, " (Media #", imedia,": ", &
            adjustl(trim(param%geometry%media(imedia))),")"

    nmedia = size(Param%Geometry%Media,1)
    if (.not. (imedia == (nMedia-1))) then
      call Error_Failure(routine,"Finite-size corrections for coating not yet supported")
    end if
    s = (imedia-1)/2
  
    ! --- Input
    ! ---Remove----- E                       = param%numerics%energy
    Ep                      = param%media(imedia)%Ep
    hbar_inv_tau_bulk       = param%media(imedia)%hbar_inv_tau
    hbar_fermi_vel          = param%media(imedia)%hbar_fermi_velocity
    B_plasmonshift          = param%media(imedia)%B_plasmonshift

    R                       = param%geometry%radius(1)*param%geometry%radius_ratios(s)
    t_r                     = param%geometry%truncation_ratio/param%geometry%radius_ratios(s)
    
    ! --- Calculated quantities
    meanfreepath         =  0.5_wp * (1.0_wp + t_r) * R
    hbar_inv_tau_corr    =  hbar_inv_tau_bulk + hbar_fermi_vel/meanfreepath

    if (size(Param%Geometry%Radius)==1) then ! Sphere
      surface_over_volume  =  3.0_wp*(t_r-3.0_wp) / ((t_r-2.0_wp)*(t_r+1.0_wp)*R)
    else if (size(Param%Geometry%Radius)==2) then !Spheroid
      a = sqrt(param%geometry%radius(2)**2 - param%geometry%radius(1)**2)
      xi_0 = R/a
      surface_over_volume = ( &
        3.0_wp * (sqrt(xi_0**2+1.0_wp)*(2.0_wp-t_r**2) &
            + t_r*sqrt(t_r**2+xi_0**2) &
            + xi_0**2*log((sqrt(t_r**2+xi_0**2)+t_r)/ &
                      (sqrt(xi_0**2+1.0_wp)-1.0_wp))) &
        / &
          (a*xi_0*sqrt(xi_0**2+1.0_wp)*(2.0_wp+3.0_wp*t_r-t_r**3))&
        )
 
    else
      call Error_Failure(routine,"Unsupported length of radius vector.")
    end if


    sigma                =  B_plasmonshift*surface_over_volume

    ! --- Correcting
    param%media(imedia)%epsilon = param%media(imedia)%epsilon                  &
                      + Ep**2/(param%numerics%energy**2                        &
                                + imu*param%numerics%energy*hbar_inv_tau_bulk) &
                      - Ep**2/(param%numerics%energy**2                        &
                                + sigma + imu*param%numerics%energy*hbar_inv_tau_corr) 
    !
    ! --- Eskil's org kode (to be removed)
    !param%media(imedia)%epsilon = param%media(imedia)%epsilon  &
    !                  + Ep**2/(E**2 + imu*E*hbar_inv_tau_bulk) &
    !                  - Ep**2/(E**2 + sigma + imu*E*hbar_inv_tau_corr) 


    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine
 
  End Subroutine Epsilon_Finite_Size_Correction
  !------------------------------------------------------!










!--------------------------------------!
End Module Epsilon_Corrections_Module
!--------------------------------------!

