! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!       In this module the physical observables are calculated
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!---------------------------------!
Module Physical_Observables_Module
!---------------------------------!

  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Results	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Allocate_Results
  Public :: Get_Physical_Observables


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!



  !-------------------------------------------!
  Subroutine Allocate_Results()
  !-------------------------------------------!
    Implicit None
    ! --- Local 
    Character(len=*), parameter :: routine = "Allocate_Results"
    Integer :: Nenergy, no_island_type, istat

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine



    ! --- Some constants
    Nenergy        =  size(param%Numerics%Energy,1)
    no_island_type =  1

    ! --- Polarizabilities
    ! ...........................
    allocate( Results%Polarizabilities(  Nenergy, no_island_type ), stat=istat  )
    call Error_Allocation( istat )
    
    ! --- Susceptibilities
    ! ............................
    allocate( Results % Susceptibilities ( Nenergy ), stat=istat  )
    call Error_Allocation( istat )

    !
    ! --------------------------------------------------------------
    ! ---- Optional Observable to calculate below this line....
    ! --------------------------------------------------------------
    !

    ! --- Delta_R_over_R
    ! ............................
    if (Param%Should_Calc%Delta_R_over_R) then
       allocate( Results % Observables % Delta_R_over_R( Nenergy, 2 ), stat=istat  )
       call Error_Allocation( istat )
    endif


    ! --- Reflectivity
    ! ............................
    if (Param%Should_Calc%Reflectivity) then
       allocate( Results % Reflection_Amplitudes( Nenergy ), stat=istat  )
       call Error_Allocation( istat )
       allocate( Results % Observables % Reflectivity( Nenergy, 2 ), stat=istat  )
       call Error_Allocation( istat )
    endif


    ! --- Transmissivity
    ! ............................
    if (Param%Should_Calc%Transmissivity) then
       allocate( Results % Transmission_Amplitudes( Nenergy ), stat=istat  )
       call Error_Allocation( istat )
       allocate( Results % Observables % Transmissivity( Nenergy, 2 ), stat=istat  )
       call Error_Allocation( istat )
    endif


    ! --- EELS
    ! ............................
    if (Param%EELS%EELS_Calculation) then
       allocate( Results % Observables % EELS( Nenergy ), stat=istat  )
       call Error_Allocation( istat )
    endif


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine


  !-------!  
  contains
  !-------!  
    

    Subroutine Error_Allocation( istat )
      Use Error_Module, only : Error_Failure
      Implicit None
      Integer, intent(In) :: istat
      
      if ( istat /= 0) then
         ! --- Allocation error occured....
         Call Error_Failure( routine, "Internal allocation error!")
      endif
      

    End Subroutine Error_Allocation

  End Subroutine Allocate_Results
  !-------------------------------------------!






  !-------------------------------------------!
  Subroutine Get_Physical_Observables( )
  !-------------------------------------------!
    !Use Susceptibilities_Module,      only : Get_Surface_Susceptabilities     
    !!!!!Use Ref_Trans_Amplitudes_Module,  only : Get_Reflection_Transmission_Amplitudes
    !Use EELS_Module,  only : get_EELS_spectrum
    Implicit None
    ! --- Local 
    Character(len=*), parameter :: routine = "Get_Physical_Observables"

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    
    ! -------------------------------------------------
    ! --- Surface suceptibilities 
    ! -------------------------------------------------
    !     This should always be calculated...
    ! ..........................................
    !call Get_Surface_Susceptabilities( ) 



    ! -------------------------------------------------
    ! --- Surface reflection/Transmission Amplitudes
    ! -------------------------------------------------
    !     This should always be calculated...
    !     NOTE : also calculates the Fresness amplitudes 
    ! ..................................................
    !!!!!! Moved to the main program....
    !!!!!!call Get_Reflection_Transmission_Amplitudes( )




    ! -----------------------------
    ! --- Delta R over R
    ! -----------------------------
    if (allocated( Results%Observables%Delta_R_over_R ) ) then
       call Get_Delta_R_over_R()
    endif


    ! -----------------------------
    ! --- Reflectivity
    ! -----------------------------
    if (allocated( Results%Observables%Reflectivity ) ) then
       call Get_Reflectivity()
    endif


    ! -----------------------------
    ! --- Transmissivity
    ! -----------------------------
    if (allocated( Results%Observables%Transmissivity ) ) then
       call Get_Transmissivity()
    endif


    ! ----------------------------
    ! --- EELS
    ! ----------------------------
    if (allocated( Results%Observables%EELS ) ) then
       call get_EELS_spectrum()
    endif


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Get_Physical_Observables
  !--------------------------------------!



  ! ======================================
  ! ==== Local Routines ==================
  ! ======================================



  !---------------------------------------------!
  Subroutine Observable_Not_Yey_Defined( string )
  !---------------------------------------------!
    Use Error_Module,   only : Error_Warning
    Implicit None
    character(len=*) :: string

    call Error_Warning( string, "Routine for this physical observable is not yet defined/implemented")

  End Subroutine Observable_Not_Yey_Defined
  !------------------------------------------!





  !------------------------------------------!
  Subroutine Get_Reflectivity (  ) 
  !------------------------------------------!
    Use Error_Module,                only : Error_Failure
    Use SFL_Logical_Units,           only : SFL_Get_Free_Unit
    Use Tools_Module,                only : Get_Phase_in_Degrees
    Use Optics_Module,               only : Reflection_Coefficient
    Use Ref_Trans_Amplitudes_Module, only : Get_Amplitude, Get_Fresnel_Amplitude 
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Reflectivity"
    Integer                                                  :: ienergy, file_id
    Real(wp),    dimension(size(param%Numerics%energy,1),2)  :: Fresnel
    Complex(wp), dimension(size(param%Numerics%energy,1))    :: Amplitudes

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some error checking
    if ( .not.allocated( Results%Reflection_Amplitudes) )  then
       call Error_Failure( routine, "Internal allocation error!" )
    end if
    !
    if ( size(Results%Reflection_Amplitudes,1) /= size(Results%Observables%Reflectivity,1) ) then
       call Error_Failure( routine, "Internal error: Inconsistent dimensions!" )
    End if


    ! --- Get the amplitudes (for the correct polarization.....)
    Amplitudes =  Get_Amplitude( Results%Reflection_Amplitudes, param%Source%Polarization )

    ! --- Calculats the Reflection amplitudes for the differetne polarizations
    !
    ! ... Amplitude
    Results%Observables%Reflectivity(:,1) = Reflection_Coefficient( Amplitudes )
    ! ... Phase
    Results%Observables%Reflectivity(:,2) = Get_Phase_in_Degrees(   Amplitudes )





    ! --------------------------------------------------------------------
    ! --- Writing the Reflection from the island-film and the substrate
    ! ---------------------------------------------------------------------
    if (Param%InOut%Debug) then       
    
       ! --- Get the amplitudes (for the correct polarization.....)
       Amplitudes    =  Get_Fresnel_Amplitude( Results%Reflection_Amplitudes, param%Source%Polarization )
       ! --- Fresnel Reflectance...       
       Fresnel(:,1)  =  Reflection_Coefficient( Amplitudes )
       Fresnel(:,2)  =  Get_Phase_in_Degrees(   Amplitudes )

       !-- Open the file
       call SFL_Get_Free_Unit( file_id )
       Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_Reflectance.dat")
       Write(file_id,"(a)") "# Format: energy, Ref, Fresnel_ref, Phase_Ref, Phase_Fresnel"
       ! --- Energy loop
       do ienergy=1,size(Param%Numerics%Energy,1)
          ! --- Write the result to file....
          Write(file_id,'(f10.5,5x,4(2f13.5,8x))')           &
               Param%Numerics%Energy(ienergy),               &
               ! --- Amplitudes
               Results%Observables%Reflectivity(ienergy,1), Fresnel(ienergy,1), &     
               ! --- Phases
               Results%Observables%Reflectivity(ienergy,2), Fresnel(ienergy,2)       
       enddo
       ! --- Close the file
       Close( file_id )
       
    end if
    ! --- End writeout



    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Get_Reflectivity
  !-----------------------------------------!




  !------------------------------------------!
  Subroutine Get_Transmissivity (  ) 
  !------------------------------------------!
    Use Error_Module,                only : Error_Failure, Error_Warning
    Use SFL_Logical_Units,           only : SFL_Get_Free_Unit
    Use Tools_Module,                only : Get_Phase_in_Degrees
    Use Optics_Module,               only : Transmission_Coefficient
    Use Ref_Trans_Amplitudes_Module, only : Get_Amplitude, Get_Fresnel_Amplitude
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Transmissivity"
    Integer                                                  :: ienergy, file_id, iambient, isubstrate
    Real(wp),    dimension(size(param%Numerics%energy,1),2)  :: Fresnel
    Complex(wp), dimension(size(param%Numerics%energy,1))    :: Amplitudes


   
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some abbreviations
    iambient   = param%Source%iambient
    isubstrate = param%Source%isubstrate

    ! --- Some error checking
    if ( .not.allocated( Results%Transmission_Amplitudes) )  then
       call Error_Failure( routine, "Internal allocation error!" )
    end if
    !
    if ( size(Results%Transmission_Amplitudes,1) /= size(Results%Observables%Transmissivity,1) ) then
       call Error_Failure( routine, "Internal error: Inconsistent dimensions!" )
    End if

    ! --- Get the amplitude (for the correct polarization.....)
    Amplitudes =  Get_Amplitude( Results%Transmission_Amplitudes, param%Source%Polarization )

    ! --- Calculats the Reflection amplitudes for the differetne polarizations
    !
    ! ... Amplitude
    Results%Observables%Transmissivity(:,1) = Transmission_Coefficient( Amplitudes, &
         Param%Media(iambient)%Epsilon, Param%Media(isubstrate)%epsilon )
    ! ... Phase
    Results%Observables%Transmissivity(:,2) = Get_Phase_in_Degrees(     Amplitudes )



    ! --------------------------------------------------------------------
    ! --- Writing the Reflection from the island-film and the substrate
    ! ---------------------------------------------------------------------
    if (Param%InOut%Debug) then       
    
       ! --- Get the amplitudes (for the correct polarization.....)
       Amplitudes    =  Get_Fresnel_Amplitude( Results%Transmission_Amplitudes, param%Source%Polarization )
       ! --- Fresnel Reflectance...       
       Fresnel(:,1)  =  Transmission_Coefficient( Amplitudes, &
            Param%Media(iambient)%Epsilon, Param%Media(isubstrate)%Epsilon )
       Fresnel(:,2)  =  Get_Phase_in_Degrees(     Amplitudes )

       !-- Open the file
       call SFL_Get_Free_Unit( file_id )
       Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_Transmittance.dat")
       Write(file_id,"(a)") "# Format: energy, Trans, Fresnel_trans, Phase_Trans, Phase_Fresnel"
       ! --- Energy loop
       do ienergy=1,size(Param%Numerics%Energy,1)
          ! --- Write the result to file....
          Write(file_id,'(f10.5,5x,4(2f13.5,8x))')           &
               Param%Numerics%Energy(ienergy),               &
               ! --- Amplitudes
               Results%Observables%Transmissivity(ienergy,1), Fresnel(ienergy,1), &     
               ! --- Phases
               Results%Observables%Transmissivity(ienergy,2), Fresnel(ienergy,2)       
       enddo
       ! --- Close the file
       Close( file_id )
       
    end if
    ! --- End writeout

    
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Get_Transmissivity
  !-----------------------------------------!




  !------------------------------------------!
  Subroutine Get_Delta_R_over_R( ) 
  !------------------------------------------!
    !
    ! --- Calculates the differential reflectivity of the 
    !     island film
    !
    !      \Delta R / R = (R-R_0)/R
    !
    !
    ! --- Ingve Simonsen, Paris, Aug 2010
    !
    Use Error_Module,                only : Error_Failure, Error_Warning
    Use Optics_Module,               only : Reflection_Coefficient
    Use Tools_Module,                only : Get_Phase_in_Degrees
    Use Ref_Trans_Amplitudes_Module, only : Get_Amplitude, Get_Fresnel_Amplitude
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Delta_R_over_R"
    Integer :: ienergy
    Complex(wp), dimension(size(param%Numerics%energy,1))    :: Amplitudes
    Complex(wp), dimension(size(param%Numerics%energy,1))    :: Fresnel_Amplitudes


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some error checking
    ! -----------------------------
    if ( .not.allocated( Results%Observables%Delta_R_over_R) ) then
       call Error_Failure( routine, "Internal allocation error!") 
    end if
    !
    if ( size(Results%Reflection_Amplitudes,1) /= size(Results%Observables%Delta_R_over_R,1) ) then
       call Error_Failure( routine, "Internal error : Dimensions not consistent!" )
    endif

    
    ! --------------------------------------
    ! --- Calcutaion of Delta R over R
    ! --------------------------------------
    !
    ! --- Get the amplitudes (for the correct polarization.....)
    Amplitudes         =  Get_Amplitude(         Results%Reflection_Amplitudes, param%Source%Polarization )
    Fresnel_Amplitudes =  Get_Fresnel_Amplitude( Results%Reflection_Amplitudes, param%Source%Polarization )
    ! --- Do the Delta_R_over_R calculation
    ! ... the amplitude
    Results%Observables%Delta_R_over_R(:,1)  =    & 
         Reflection_Coefficient( Amplitudes ) / Reflection_Coefficient( Fresnel_Amplitudes ) - 1._wp
    ! ... the phase
    Results%Observables%Delta_R_over_R(:,2)  =  Get_Phase_in_Degrees( Amplitudes / Fresnel_Amplitudes )

    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Get_Delta_R_over_R
  !-----------------------------------------!




  !------------------------------------------!
  Subroutine get_EELS_spectrum( ) 
  !------------------------------------------!
    !
    ! PURPOSE
    !    Calculates the EELS spectrum for the island film
    !
    !          dP / d(\hbar\omega)
    !  
    !    based on the notes from Remi.
    !
    !
    ! AUTHOR
    !    Ingve Simonsen, Paris, Jan 2014
    !
    Use Error_Module,                only : Error_Failure, Error_Warning
    Implicit None
    ! --- Local
    character(len=*), parameter :: routine = "get_EELS_spectrum"
    integer          :: ienergy, iambient, isubstrate 
    real(wp)         :: Qpar, Prob, prefactor
    complex(wp)      :: eps1, eps2, gamma, beta, delta, Det
    complex(wp)      :: k_gamma, k_beta, k2_gamma_beta, k2_delta
    complex(wp)      :: cfactor1, cfactor2, cfactor3


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some error checking
    ! -----------------------------
    if ( .not.allocated( Results % Observables % EELS) ) then
       call Error_Failure( routine, "Internal allocation error!") 
    end if
    !
    if ( .not.allocated( Results % Susceptibilities) ) then
       call Error_Failure( routine, "Internal allocation error!") 
    end if


    ! --- Some abbreviations
    iambient   = param%Source%iambient
    isubstrate = param%Source%isubstrate
    !
    Qpar       = Param%EELS%Wavevector_Transfer
 

    ! --------------------------------------
    ! --- Calcutaion of the EELS spectrum
    ! --------------------------------------
    !

    ! --- Energy independent prefactor
    prefactor = 1._wp ! = Qpar * Delta0**2 / ( 2*pi**3 *hbar**2 )



    do ienergy = 1, size( Results%Observables%EELS, 1 )

       ! ... dielectric functions
       !
       eps1   = Param%Media(iambient)%epsilon(ienergy)
       eps2   = Param%Media(isubstrate)%epsilon(ienergy)
       !
       ! ... Susceptibilities
       gamma = Results % Susceptibilities(ienergy) % gamma
       beta  = Results % Susceptibilities(ienergy) % beta
       delta = Results % Susceptibilities(ienergy) % delta
       !
       ! ... shorthands
       k_gamma = Qpar *  gamma
       k_beta  = Qpar *  beta
       k2_gamma_beta = k_gamma * k_beta
       k2_delta      = Qpar**2 * delta
       !
       ! ... common factors
       cfactor1 = 1._wp  + 0.25_wp* k2_gamma_beta
       cfactor2 = eps2   + 0.50_wp*       k_gamma  + 0.5_wp* eps2* k2_delta
       cfactor3 = 1._wp  - 0.50_wp* eps2* k_beta   - 0.5_wp*       k2_delta
       !
       ! ... Determinant (can this be zero?)
       Det   =  eps1 + eps2 + k_gamma - eps1*eps2*k_beta                         &
               - 0.25_wp * (eps1+eps2) * k2_gamma_beta - (eps1-eps2) *k2_delta    
       !Det   =  eps1 + eps2 + Qpar*gamma -eps1*eps2*Qpar*beta   &
       !        - 0.25_wp * (eps1+eps2)*Qpar**2*gamma*beta       &
       !        -           (eps1-eps2)*Qpar**2*delta    
       !
       !
       ! ... 1st term
       Prob = Im(eps2)* abs( cfactor1 )**2
       ! ... 2nd term
       Prob = Prob + Im(beta) * Qpar * abs( cfactor2 )**2
       ! ... 3rd term
       Prob = Prob + Im(gamma) * Qpar * abs( cfactor3 )**2
       ! ... 4th term
       Prob = Prob + 2*real(delta,wp) * Qpar**2 * Im(cfactor3* cfactor2 )    
       !
       !
       ! ... Prefactor
       Prob  =  Prob * prefactor * eps1**2 / abs( Det )**2
    
    enddo

    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine


  contains

    function Im( x )  Result( Im_x )
      ! Take the imaginary part of the argument....
      implicit none
      complex(wp), intent(in) :: x
      real(wp)                :: Im_x
      ! -- Local
       Im_x =  real( - (0._wp, 1._wp) * x, wp) 
    end function Im

  End Subroutine get_EELS_spectrum
  !-----------------------------------------!






End Module Physical_Observables_Module
!------------------------------------!













! ===========================================================
! ========== OLD CODE FROM GRANFILM VERSION 1.1 =============
! ===========================================================

!!$
!!$
!!$Module Optics_mod
!!$    
!!$  ! ---------------------------
!!$  ! --- The use statement
!!$  ! ---------------------------
!!$  Use Share_Parameters_Mod
!!$  Use Interaction_mod
!!$
!!$  ! --------------------------------------
!!$  ! --- The Publicly avaiable quantities
!!$  ! --------------------------------------
!!$  Public :: surf_const_coef_island
!!$  Public :: surf_const_coef_rough_substrate
!!$  Public :: surf_const_coef_film_above
!!$  Public :: surf_const_coef_cap
!!$  Public :: surf_const_coef_island
!!$  Public :: fresnel_calc
!!$
!!$
!!$
!!$  
!!$  ! --------------------------------------
!!$  ! --- Everyting is Private by default
!!$  ! --------------------------------------
!!$  Private
!!$
!!$
!!$Contains
!!$
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_rough_substrate(surf_const_coef)
!!$    !
!!$    ! Calculates the surface constitutive coefficients of rough substrate
!!$    ! with a gaussian roughness
!!$    !
!!$    ! SEE : Vlieger/Bedeaux book pages 382
!!$    !
!!$    Implicit None
!!$    Type(surface_constitutive_type)    :: surf_const_coef(:)
!!$    ! Local
!!$    integer                            :: Ienergy
!!$    complex(wp )                       :: e1,e2
!!$    Real(wp)                           :: t,xi
!!$
!!$    t  = param%Film%Thickness/param%Island%Radius
!!$    xi = param%Island%Coating_Thickness/param%Island%Radius
!!$    e1 =  eps_vacuum    ! Vacuum
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e2                              =   param%Materials%Epsilon%Substrate_file(ienergy)
!!$       surf_const_coef(ienergy)%gamma  =   - Sqrt(pi)/8._wp*(e1+e2)/(e1*e2)*(e2-e1)**2*t**2/xi
!!$       surf_const_coef(ienergy)%beta   =   Sqrt(pi)/4._wp*(e1+e2)/((e1*e2)**2)*(e2-e1)**2*t**2/xi
!!$       surf_const_coef(ienergy)%tau    =   (e2-e1)*t**2/2._wp
!!$       surf_const_coef(ienergy)%delta  =   (e2**2-e1**2)*t**2/(e1*e2)/2._wp
!!$    Enddo
!!$
!!$  End Subroutine surf_const_coef_rough_substrate
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_film_above(surf_const_coef,thickness,eps_film)
!!$    !
!!$    ! Calculates the surface constitutive coefficients of a thin contiguous film 
!!$    ! shifted just above the substrate
!!$    !
!!$    Implicit None
!!$    Type(surface_constitutive_type)    :: surf_const_coef(:)
!!$    Real(wp)                           :: thickness
!!$    complex(wp )                       :: eps_film(:)
!!$    ! Local
!!$    integer                            :: Ienergy
!!$    complex(wp )                       :: e1,e3
!!$
!!$    e1     =  eps_vacuum    ! Vacuum
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e3                              = eps_film(ienergy)
!!$       surf_const_coef(ienergy)%gamma  = (e3-e1)*thickness/param%Island%Radius    
!!$       surf_const_coef(ienergy)%beta   = (1._wp/e1-1._wp/e3) * thickness/param%Island%Radius
!!$       surf_const_coef(ienergy)%tau    = (thickness/param%Island%Radius)**2*(e3-e1)/2 
!!$       surf_const_coef(ienergy)%delta  = (thickness/param%Island%Radius)**2*(e3/e1-e1/e3)/2
!!$    Enddo
!!$
!!$  End Subroutine surf_const_coef_film_above
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_film_below(surf_const_coef,thickness,eps_film,eps_substrate)
!!$    ! Calculates the surface constitutive coefficients of a thin contiuous film
!!$    ! shifted just below the substrate
!!$    Implicit None
!!$    Type(surface_constitutive_type), intent(out)    :: surf_const_coef(:)
!!$    Real(wp),                        intent(in)     :: thickness
!!$    complex(wp ),                    intent(in)     :: eps_film(:),eps_substrate(:)
!!$    ! Local 
!!$    integer                            :: Ienergy
!!$    complex(wp )                       :: e2,e3
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e2                              = eps_substrate(ienergy)
!!$       e3                              = eps_film(ienergy)
!!$       surf_const_coef(ienergy)%gamma  = (e3-e2)*thickness/param%Island%Radius    
!!$       surf_const_coef(ienergy)%beta   = (1._wp/e2 - 1._wp/e3)*thickness/param%Island%Radius
!!$       surf_const_coef(ienergy)%tau    = -(thickness/param%Island%Radius)**2*(e3-e2)/2 
!!$       surf_const_coef(ienergy)%delta  = (thickness/param%Island%Radius)**2*(e2/e3-e3/e2)/2
!!$    Enddo
!!$
!!$  End Subroutine surf_const_coef_film_below
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine add_2fims_surf_const_coef(SCC1,SCC2,SCC)
!!$    !
!!$    ! Add the surface constitutive coefficients (formulas 3.63 of Bedeaux's book)
!!$    !
!!$    Implicit None
!!$    Type(surface_constitutive_type), intent(in)    :: SCC1(:),SCC2(:)
!!$    Type(surface_constitutive_type), intent(out)   :: SCC(:)
!!$
!!$    ! Add the layers (formulas 3.63 of Bedeaux's book)
!!$    SCC(:)%gamma =  SCC1(:)%gamma + SCC2(:)%gamma
!!$    SCC(:)%beta  =  SCC1(:)%beta  + SCC2(:)%beta
!!$    SCC(:)%tau   =  SCC1(:)%tau   + SCC2(:)%tau
!!$    SCC(:)%delta =  SCC1(:)%delta + SCC2(:)%delta + &
!!$         (SCC2(:)%gamma*SCC1(:)%beta-SCC1(:)%gamma*SCC2(:)%beta)/2._wp
!!$
!!$  End Subroutine add_2fims_surf_const_coef
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_cap(surf_const_coef)
!!$    !
!!$    ! Calculates the surface constitutive coefficients of a thin 
!!$    ! spherical cap 
!!$    !
!!$    ! SEE : Vlieger/Bedeaux book Section 8.5
!!$    !
!!$    Implicit None    
!!$    Type(surface_constitutive_type)     :: surf_const_coef(:)
!!$    ! --- Local 
!!$    complex(wp )                        :: alphad(2,2,param%Numerics%NEnergy)
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt=*) '-----------------------------------------------------------'
!!$    Write(unit=6,fmt=*) ' Warning (Subroutine : surf_const_coef_cap) !'
!!$    Write(unit=6,fmt=*) ' The formulas for a thin cap are valid Only for '
!!$    Write(unit=6,fmt=*) ' a small effective height tr~-1 !'
!!$    Write(unit=6,fmt=*) '-----------------------------------------------------------'
!!$
!!$    ! Calculate the (dimensionless) surface constitutive coefficients for all energies
!!$    !NICK   Select Case(TRAL(param%Numerics%Island_Island_Interaction))
!!$    Select Case(TRIM(param%Numerics%Island_Island_Interaction))
!!$    Case('none')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies 
!!$       Call polarizabilities_cap(alphad)
!!$       Call surf_const_coef_nointeract(surf_const_coef,alphad)
!!$    Case('dipole')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the dipolar situation
!!$       Call polarizabilities_cap(alphad)
!!$       Call surf_const_coef_dipole(surf_const_coef,alphad)
!!$    Case default
!!$       !NICK     Write(unit=6,fmt=*) 'ERROR (surf_const_coef_cap): Parameter ', &
!!$       !NICK       TRAL(param%Numerics%Island_Island_Interaction),' not supported (yet)'
!!$       Write(unit=6,fmt=*) 'ERROR (surf_const_coef_cap): Parameter ', &
!!$            TRIM(param%Numerics%Island_Island_Interaction),' not supported (yet)'
!!$       Pause
!!$       Stop
!!$    End Select
!!$
!!$  End Subroutine surf_const_coef_cap
!!$  
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_island(surf_const_coef,Ad,Aq)
!!$    !
!!$    ! Calculate the surface constitutive coefficients for the island geometry
!!$    ! Generic routine
!!$    !
!!$
!!$    !    Use Size_mod,  Only    :       Size_distribution_main                       
!!$    Implicit None    
!!$    Type(surface_constitutive_type)     :: surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                        :: Ad(2,0:1,param%Numerics%NEnergy)
!!$    complex(wp )                        :: Aq(3,param%Numerics%NEnergy)
!!$    complex(wp )                        :: alphad(2,2,param%Numerics%NEnergy)
!!$    complex(wp )                        :: alphaq(3,param%Numerics%NEnergy)
!!$    Real(wp)                            :: c0,s0,sqr1,sqr2,e1
!!$    complex(wp )                        :: exp0
!!$
!!$    ! Ad : results of the calculations with a constant field
!!$    ! Aq : results for the calculations for a quadrupolar field    
!!$
!!$    ! Calculate the (dimensionless) surface constitutive coefficients for all energies
!!$    !NICK   Select Case(TRAL(param%Numerics%Island_Island_Interaction))
!!$    Select Case(TRIM(param%Numerics%Island_Island_Interaction))
!!$    Case('none')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies 
!!$       Call polarizabilities(alphad,Ad)
!!$       Call surf_const_coef_nointeract(surf_const_coef,alphad)
!!$    Case('dipole')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the dipolar situation
!!$       Call polarizabilities(alphad,Ad)
!!$       Call surf_const_coef_dipole(surf_const_coef,alphad)
!!$       ! Correct to first order the potential development
!!$       e1          = eps_vacuum
!!$       c0          = Cos(param%Source%Derived%Theta0_calc)
!!$       s0          = Sin(param%Source%Derived%Theta0_calc)
!!$       exp0        = Exp(-imu*param%Source%Derived%Phi0_calc)
!!$       sqr1        = Sqrt(pi/3._wp)
!!$       sqr2        = Sqrt(pi/5._wp)
!!$       Ad(1,0,:)   = alphad(1,2,:)*sqr1*c0/(2*pi*e1)
!!$       Ad(2,0,:)   = alphad(2,2,:)*sqr2*c0/(pi*e1)
!!$       Ad(1,1,:)   = -alphad(1,1,:)*Sqrt(2._wp)*sqr1*s0*exp0/(4*pi*e1)
!!$       Ad(2,1,:)   = -alphad(2,1,:)*Sqrt(6._wp)*sqr1*s0*exp0/(4*pi*e1)
!!$    Case('quadrupole')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the quadrupolar situation
!!$       Call polarizabilities(alphad,Ad)
!!$       Call polarizabilities_quadrupole(alphaq,Aq)
!!$       Call surf_const_coef_quadrupole(surf_const_coef,alphad,alphaq)
!!$       !           Case('Size')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the dipolar situation taking into account
!!$       ! a Size distribution
!!$       !                  Call polarizabilities(alphad,Ad)
!!$       !                  e1            = eps_vacuum
!!$       !                  Call Size_distribution_main(alphad)
!!$       !                  density       = param%Interaction%Derived%Density*param%Island%Radius**2
!!$       !                  Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Only the particle polarizabities are taken into account and 
!!$       ! the second order coefficient are set equal to zero
!!$       !                        surf_const_coef(ienergy)%gamma  =   density*alphad(1,1,ienergy) 
!!$       !                        surf_const_coef(ienergy)%beta   =   density*alphad(1,2,ienergy)/(e1**2)   
!!$       !                        surf_const_coef(ienergy)%delta  =   0._wp
!!$       !                        surf_const_coef(ienergy)%tau    =       0._wp   
!!$       !                  Enddo
!!$    End Select
!!$
!!$  End Subroutine surf_const_coef_island
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine polarizabilities(alpha,A)
!!$    !
!!$    !-----------------------------------------------------------
!!$    !--- The polarizabilites (alpha) are calculated from 
!!$    !--- (Bedeaux's book) Eq. (5.6.43) (cf. Eq. (8.2.29))
!!$    !--- when the MP are above the subsrate and from
!!$    !--- Eq. (8.3.7) when thay are below 
!!$    !--- Notice that dimensionless quanities are used.
!!$    !--- Here A is the (dimensionless) multipole coefficients
!!$    !------------------------------------------------------------
!!$    !
!!$    Implicit None    
!!$    complex(wp ), intent(out)           :: alpha(2,2,param%Numerics%NEnergy)
!!$    complex(wp ), intent(in)           :: A(2,0:1,param%Numerics%NEnergy)
!!$    ! -- Local 
!!$    integer                :: ienergy
!!$    Real(wp)               :: c0,s0,vol
!!$    complex(wp )           :: e1,e2
!!$    complex(wp )           :: exp0,A10,A11,A20,A21
!!$
!!$    ! Some energy independent abbreviations
!!$    e1    =  eps_vacuum
!!$    c0    =  Cos(param%Source%Derived%Theta0_calc)
!!$    s0    =  Sin(param%Source%Derived%Theta0_calc) 
!!$    exp0  =  Exp(-imu*param%Source%Derived%Phi0_calc)
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       !----------------------------------------
!!$       !---  MP above the subsrate; Eq. (8.2.29)
!!$       !----------------------------------------
!!$       Do ienergy = 1, param%Numerics%NEnergy
!!$          ! Some energy dependent abbreviations
!!$          A10 = A(1,0,ienergy)
!!$          A11 = A(1,1,ienergy)
!!$          A20 = A(2,0,ienergy)
!!$          A21 = A(2,1,ienergy)
!!$          ! Calculates the (dimensionless) polarizabilities 
!!$          ! 1st index : 1 : dipole order
!!$          !             2 : quadropole order
!!$          ! 2nd index : 1 : parallel
!!$          !             2 : perpendicular
!!$          ! Dipole order 
!!$          alpha(1,1,ienergy) = -4*pi*e1*A11/(Sqrt(2*pi/3)*s0*exp0)
!!$          alpha(1,2,ienergy) =  2*pi*e1*A10/(Sqrt(pi/3)*c0)
!!$          ! Quadropole order
!!$          alpha(2,1,ienergy) = -4*pi*e1*A21/(Sqrt(6*pi/5)*s0*exp0)
!!$          alpha(2,2,ienergy) =    pi*e1*A20/(Sqrt(pi/5)*c0)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             Open(unit=31,file='Polarizability.dat',status='unknown')
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),alpha(1,1,ienergy)/vol,alpha(1,2,ienergy)/vol
!!$          Endif
!!$       Enddo
!!$    Else
!!$       !----------------------------------------
!!$       !---  MP below the subsrate; Eq. (8.3.7)
!!$       !----------------------------------------
!!$       Do ienergy = 1, param%Numerics%NEnergy
!!$          ! Some energy dependent abbreviations
!!$          e2 = param%Materials%Epsilon%Substrate(ienergy)   ! Misc%Eps_Substrate(ienergy)
!!$          A10 = A(1,0,ienergy)
!!$          A11 = A(1,1,ienergy)
!!$          A20 = A(2,0,ienergy)
!!$          A21 = A(2,1,ienergy)
!!$          ! Calculates the (dimensionless) polarizabilities 
!!$          ! 1st index : 1 : dipole order
!!$          !             2 : quadropole order
!!$          ! 2nd index : 1 : parallel
!!$          !             2 : perpendicular
!!$          ! Dipole order 
!!$          alpha(1,1,ienergy) = -4*pi*e2*A11/(Sqrt(2*pi/3)*s0*exp0)
!!$          alpha(1,2,ienergy) =  2*pi*e2*A10/((e1/e2)*Sqrt(pi/3)*c0)
!!$          ! Quadropole order
!!$          alpha(2,1,ienergy) = -4*pi*e2*A21/(Sqrt(6*pi/5)*s0*exp0)
!!$          alpha(2,2,ienergy) =    pi*e2*A20/((e1/e2)*Sqrt(pi/5)*c0)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             Open(unit=31,file='Polarizability.dat',status='unknown')
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),alpha(1,1,ienergy)/vol,(e1/e2)**2*alpha(1,2,ienergy)/vol
!!$          Endif
!!$       Enddo
!!$    Endif
!!$    Close(unit=31)
!!$
!!$    ! Convolution of the polarizability with a gaussian
!!$    Call Gaussian_Convolution(alpha)
!!$
!!$  End Subroutine polarizabilities
!!$
!!$  !--------------------------------------------------------------------------------------------
!!$  Subroutine Gaussian_Convolution(alpha)
!!$    !
!!$    ! Convolution of the polarizabilities with a Gaussian
!!$    !
!!$
!!$    Use Special_functions_mod, only : Arth
!!$    Implicit None    
!!$    complex(wp ), Intent(InOut)            :: alpha(2,2,param%Numerics%NEnergy)
!!$    ! -- Local
!!$    complex(wp )                           :: alpha_conv(2,2,param%Numerics%NEnergy)
!!$    Real(wp)                               :: energy(param%Numerics%NEnergy)
!!$    Real(wp), Allocatable                  :: bound_energy(:)
!!$    Real(wp)                               :: val,de,sigE(2),Emin,Emax
!!$    Integer                                :: nbound,nenergy,m,ienergy,k
!!$
!!$    ! Step in energy
!!$    nenergy                                = param%Numerics%NEnergy
!!$    energy(:)                              = param%Numerics%Energy(:)
!!$    Emin                                   = energy(1)
!!$    Emax                                   = energy(nenergy)
!!$    de                                     = energy(2)-energy(1)
!!$    sigE(1)                                = param%Misc%sigEpar
!!$    sigE(2)                                = param%Misc%sigEperp
!!$    nbound                                 = Int(Max(3*Max(sigE(1),sigE(2))/de,1._wp))
!!$    Allocate(bound_energy(2*nbound))
!!$    bound_energy(1:nbound)                 = Arth(Emin-de,-de,nbound)
!!$    bound_energy(nbound+1:2*nbound)        = Arth(Emax+de,de,nbound)
!!$
!!$    ! Parallel(k=1) and perpendicular(k=2) directions
!!$    Do k=1,2
!!$       If(sigE(k).ne.0._wp) Then
!!$          val = 1._wp/sigE(k)/sqrt(2._wp*pi)*de
!!$          ! The convolution
!!$          Do ienergy = 1,nenergy
!!$             Do m=1,2
!!$                alpha_conv(m,k,ienergy) = Sum(alpha(m,k,:)*Gauss(energy(ienergy),sigE(k),energy(:))) + &
!!$                     alpha(m,k,1)*Sum(Gauss(energy(ienergy),sigE(k),bound_energy(1:nbound))) + &
!!$                     alpha(m,k,nenergy)*Sum(Gauss(energy(ienergy),sigE(k),bound_energy(nbound+1:2*nbound)))
!!$             Enddo
!!$          Enddo
!!$          alpha(:,k,:) = alpha_conv(:,k,:)*val    
!!$       Endif
!!$    Enddo
!!$
!!$    ! Free space
!!$    Deallocate(bound_energy)
!!$
!!$  Contains
!!$
!!$    Function Gauss(y0,sig,y)    Result(Gaussres)
!!$      Implicit None        
!!$      Real(wp)                  ::      sig,y0,y(:)
!!$      Real(wp)                  ::      Gaussres(size(y))
!!$      Gaussres(:) = exp(-(y(:)-y0)**2/2._wp/sig**2)
!!$    End Function Gauss
!!$
!!$  End Subroutine Gaussian_Convolution
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$  Subroutine  polarizabilities_quadrupole(alpha,A)
!!$
!!$    !-------------------------------------------------------
!!$    !--- The polarizabilites (alpha) are calculated from 
!!$    !--- (Bedeaux's book) Eq. (10.88) 
!!$    !--- when the MP are above the subsrate and from
!!$    !--- Eq. (10.88) modified by the coordinates transformations
!!$    !--- x<->-x y<-->y z<-->-z e1<-->e2 e3<-->e4 when thay are below 
!!$    !--- Notice that dimensionless quanities are used.
!!$    !--- Here Aq is the (dimensionless) multipole coefficients
!!$    !-------------------------------------------------------
!!$    Implicit None    
!!$    complex(wp )                ::      alpha(3,param%Numerics%NEnergy)
!!$    complex(wp )                ::      A(3,param%Numerics%NEnergy)
!!$    Real(wp)                    ::      sqr
!!$    complex(wp )                ::      e1,e2,A20,A21,A22
!!$    integer                     ::      ienergy
!!$
!!$    ! --- Some energy independent abbreviations
!!$    e1          =       eps_vacuum
!!$    sqr         =       Sqrt(6._wp*pi/5._wp)
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       Do ienergy=1,param%Numerics%NEnergy
!!$          e2    = param%Materials%Epsilon%Substrate_file(ienergy)   ! Misc%Eps_Substrate(ienergy)
!!$          A20   =       A(1,ienergy)
!!$          A21   =       A(2,ienergy)
!!$          A22   =       A(3,ienergy)
!!$          !----------------------------------------
!!$          !---  MP above the subsrate; Eq. (10.88)
!!$          !----------------------------------------
!!$          alpha(1,ienergy) = -pi*e1*A20/(sqr*Sqrt(2._wp/3._wp) ) + &
!!$               3*pi*e1*A22/(imu*sqr)
!!$          alpha(2,ienergy) = -4*pi*e1*A21/sqr/(1._wp-imu) + &
!!$               4*pi*e1*A22/(imu*sqr)
!!$          alpha(3,ienergy) = -2*pi*e1*A22/(imu*sqr)
!!$       Enddo
!!$    Else
!!$       Do ienergy=1,param%Numerics%NEnergy
!!$          ! Doubt on the conversion 
!!$          !------------------------------------------------
!!$          !---  MP below the subsrate; Eq. (10.88) modified
!!$          !------------------------------------------------
!!$          e2    =       param%Materials%Epsilon%Substrate_file(ienergy)   ! Misc%Eps_Substrate(ienergy)
!!$          A20   =       A(1,ienergy)
!!$          A21   =       A(2,ienergy)
!!$          A22   =       A(3,ienergy)
!!$          alpha(1,ienergy) = -pi*e2*A20/(sqr*Sqrt(2._wp/3._wp) ) - &
!!$               3*pi*e2*A22/(imu*sqr)
!!$          alpha(2,ienergy) = -4*pi*e2*A21/sqr/(1._wp+imu) - &
!!$               4*pi*e2*A22/(imu*sqr)
!!$          alpha(3,ienergy) =  2*pi*e2*A22/(imu*sqr)
!!$       Enddo
!!$    Endif
!!$
!!$  End Subroutine polarizabilities_quadrupole
!!$
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine polarizabilities_cap(alpha)
!!$    ! Calculates the polarizabilities in the Case of a thin spherical
!!$    ! cap using the limited development 
!!$    ! Formulas 8.104 of Bedeaux's book
!!$    Implicit None    
!!$    complex(wp )                  ::      alpha(2,2,param%Numerics%NEnergy)
!!$    Real(wp)                      ::      h,volume,e1,vol         
!!$    complex(wp )                  ::      e2,e3
!!$    integer                       ::      ienergy
!!$
!!$    ! --- Some abreviations
!!$    e1                     =       eps_vacuum
!!$    h                      =       1._wp - Abs(param%Island%Truncation_Ratio)
!!$    volume                 =       pi*h**2*(1._wp-h/3._wp)
!!$    ! Values necessary for keeping constant the order
!!$    ! of development in the surface constitutive coefficient
!!$    param%Island%Truncation_Ratio          =       1._wp
!!$    param%Numerics%Multole_Position      =       0._wp
!!$    param%Island%Derived%Div_Surface       =       param%Island%Truncation_Ratio*param%Island%Radius
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt='(a,f8.4)') ' Effective height : ',h
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e2   = param%Materials%Epsilon%Substrate(ienergy)
!!$       e3   = param%Materials%Epsilon%Island(ienergy)
!!$       ! Calculates the (dimensionless) polarizabilities 
!!$       ! 1st index : 1 : dipole order
!!$       !             2 : quadropole order
!!$       ! 2nd index : 1 : parallel
!!$       !             2 : perpendicular
!!$       ! Dipole order 
!!$       alpha(1,1,ienergy) =  volume*(e3-e1)
!!$       alpha(1,2,ienergy) =  volume*e2**2*(e3-e1)/(e1*e3)
!!$       ! Quadropole order
!!$       alpha(2,1,ienergy) =  -volume*(e3-e1)
!!$       alpha(2,2,ienergy) =  -volume*e2**2*(e3-e1)/(e1*e3) 
!!$       ! Output for the polarizability
!!$       If(param%Misc%Output_Polarizability) Then
!!$          Open(unit=31,file='Polarizability.dat',status='unknown')
!!$          If(param%Misc%Normalization) Then
!!$             vol = volume
!!$          Else
!!$             vol = 1._wp
!!$          Endif
!!$          Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$               param%Numerics%Energy(ienergy),alpha(1,1,ienergy)/vol,(e1/e2)**2*alpha(1,2,ienergy)/vol
!!$       Endif
!!$    Enddo
!!$
!!$
!!$  End Subroutine polarizabilities_cap
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_nointeract(surf_const_coef,alpha)
!!$    ! Calculate the surface costitutive coefficient from Eq. (8,2.30),
!!$    ! when the MP are located above the subsrate and by Eq. (8.3.12)
!!$    ! when they are located below.
!!$    ! NOTE : All these formulae asSumes LOW COVERAGE
!!$    !        No inter-island interaction is taken into account
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                     :: alpha(2,2,param%Numerics%NEnergy)
!!$    integer                          :: ienergy
!!$    Real(wp)                         :: density,d
!!$    complex(wp )                     :: e1,e2
!!$
!!$    ! Some energy independent abbreviations
!!$    e1       = eps_vacuum      
!!$    ! Dimensionless d and density
!!$    ! d= Absolute distance between surface and the location of multipoles
!!$    d        = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position )
!!$    density  = param%Interaction%Derived%Density * param%Island%Radius**2
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       ! MP above the subsrate; Eq. (8.2.30)
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          surf_const_coef(ienergy)%gamma  =   density*alpha(1,1,ienergy) 
!!$          surf_const_coef(ienergy)%beta   =   density*alpha(1,2,ienergy)/(e1**2)   
!!$          surf_const_coef(ienergy)%delta  = - density/e1 *                          &
!!$               Sum(alpha(2,:,ienergy) - d*alpha(1,:,ienergy))
!!$          surf_const_coef(ienergy)%tau    = - density *                             & 
!!$               ( alpha(2,1,ienergy) - d*alpha(1,1,ienergy) )   
!!$       Enddo
!!$    Else
!!$       ! MP below the subsrate; Eq. (8.3.12)
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          e2 = param%Materials%Epsilon%Substrate(ienergy)   !     Misc%Eps_Substrate(ienergy)
!!$          surf_const_coef(ienergy)%gamma  =   density*alpha(1,1,ienergy) 
!!$          surf_const_coef(ienergy)%beta   =   density*alpha(1,2,ienergy)/(e2**2)   
!!$          surf_const_coef(ienergy)%delta  = - density/e2 *                          &
!!$               Sum(alpha(2,:,ienergy) + d*alpha(1,:,ienergy))
!!$          surf_const_coef(ienergy)%tau    = - density * (e1/e2) *                   & 
!!$               ( alpha(2,1,ienergy) + d*alpha(1,1,ienergy) )
!!$       Enddo
!!$    Endif
!!$  End Subroutine surf_const_coef_nointeract
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_dipole(surf_const_coef,alpha)
!!$    ! Calculate the surface costitutive coefficient
!!$    ! NOTE : All these formulae includes dipole interatction between the 
!!$    !        various islands
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                     :: alpha(2,2,param%Numerics%NEnergy)
!!$    integer                          :: ienergy
!!$    Real(wp)                         :: S_mp,S_imp,L,factor,density,d,vol
!!$    complex(wp )                     :: e1,e2,eps
!!$    Complex(wp)                      :: paral,perp
!!$    Real(wp)                         :: sqr
!!$
!!$    ! --- Some energy independent abbreviations
!!$    e1           = eps_vacuum
!!$    sqr          = Sqrt(4._wp*pi/5._wp)
!!$    ! --- Dimensionless density,d,L
!!$    !     d= Absolute distance between surface and the location of multipoles
!!$    d            = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position )
!!$    density      = param%Interaction%Derived%Density*param%Island%Radius**2
!!$    L            = param%Interaction%Lattice_Constant / param%Island%Radius
!!$    ! --- Special value for the dimensionlesslattice constant
!!$    !      (P269 Bedeaux's book) for a random network
!!$    If(param%Interaction%Lattice_Type=='MFT'.or.param%Interaction%Lattice_Type=='RPT') &
!!$         L       =       1._wp/Sqrt(density)  
!!$    ! --- Lattice Sums for the multipoles (MP) and image multipoles (IMP)
!!$    S_mp         = lattice_Sum(0._wp,2)
!!$    S_imp        = lattice_Sum(d,2)
!!$
!!$    ! --- Renormalization of the polarisabilities in the Barrera model
!!$    !     Phys. Rev. B 43 (17) P13819 (1991)
!!$    If(param%Interaction%Lattice_Type=='RPT') Then
!!$       Call Renorm_Polarizability(d,alpha(1,:,:))       
!!$    Endif
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$
!!$       !------------------------------------------------
!!$       !--- MP above the substrate Eqs. 10.29 - 10.28
!!$       !------------------------------------------------
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          e2  =  param%Materials%Epsilon%Substrate(ienergy)    !       Misc%Eps_Substrate(ienergy)    
!!$          eps =  (e1-e2)/(e2+e1)
!!$          ! Uses the formulas 5-67 for conversion betwenn various polarizbilities
!!$          ! Parallel component (10-29)
!!$          factor = S_mp + eps*S_imp
!!$          paral  = alpha(1,1,ienergy)/(4*pi*e1)
!!$          paral  =  4*pi*e1*paral/(1._wp + paral*sqr/(L**3)*factor)
!!$          ! Perpendicular component (10-28)
!!$          factor = S_mp - eps*S_imp
!!$          perp   = alpha(1,2,ienergy)/(4*pi*e1)
!!$          perp   = 4*pi*e1*perp / (1._wp - 2._wp*perp*sqr/(L**3)*factor)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Open(unit=31,file='Polarizability_Int.dat',status='unknown')
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),paral/vol,perp/vol
!!$          Endif
!!$          ! Fills the surface suceptibilities (10-30)
!!$          surf_const_coef(ienergy)%beta   =  density*perp/(e1**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*paral
!!$          surf_const_coef(ienergy)%delta  =  density*d*(paral + perp)/e1
!!$          surf_const_coef(ienergy)%tau    =  density*d*paral
!!$       Enddo
!!$
!!$    Else
!!$
!!$       !------------------------------------------------
!!$       !---  MP below the substrate Eqs. 10-33 10-34
!!$       !------------------------------------------------
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          e2      =  param%Materials%Epsilon%Substrate(ienergy)     !      Misc%Eps_Substrate(ienergy)    
!!$          eps     = (e2-e1)/(e2+e1)
!!$          ! Uses the formulas (5-67) for conversion betwenn various polarizbilities
!!$          ! Parallel component (10-34)
!!$          factor  =  S_mp + eps*S_imp
!!$          paral   =  alpha(1,1,ienergy)/(4*pi*e2)
!!$          paral   =  4*pi*e2*paral/(1._wp + paral*sqr/(L**3)*factor)
!!$          ! Perpendicular component (10-33)
!!$          factor  = S_mp - eps*S_imp
!!$          perp    = alpha(1,2,ienergy)/(4*pi*e2)
!!$          perp    = 4*pi*e2*perp / (1._wp - 2._wp*perp*sqr/(L**3)*factor)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Open(unit=31,file='Polarizability_Int.dat',status='unknown')
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),paral/vol,perp/vol
!!$          Endif
!!$          ! Fills the surface suceptebilities (10-30)
!!$          surf_const_coef(ienergy)%beta   =  density*perp/(e2**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*paral
!!$          surf_const_coef(ienergy)%delta  =  -density*d*(paral + perp)/e2
!!$          surf_const_coef(ienergy)%tau    =  -density*d*paral*e1/e2
!!$       Enddo
!!$
!!$    Endif
!!$
!!$  End Subroutine surf_const_coef_dipole
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_quadrupole(surf_const_coef,alphad,alphaq)
!!$    ! Calculate the surface costitutive coefficients 
!!$    ! NOTE : All these formulae include quadrupole interactions between the 
!!$    !        various islands
!!$    ! Formulas 10-36 --> 10.55
!!$    Implicit None    
!!$    Type(surface_constitutive_type)       ::      surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                          ::      alphad(2,2,param%Numerics%NEnergy)
!!$    complex(wp )                          ::      alphaq(3,param%Numerics%NEnergy) 
!!$    complex(wp )                          ::      A1010,A1111,A2111,A2010,A2121,A2020,A2222,A1020,A1121
!!$    complex(wp )                          ::      Az,Az10,Ap,Ap10 
!!$    complex(wp )                          ::      e1,e2,eps,Dz,Dp
!!$    integer                               ::      ienergy
!!$    Real(wp)                              ::      S2_mp,S2_imp,S3_mp,S3_imp,S4_mp,S4_imp
!!$    complex(wp )                          ::      S2p,S3p,S4p,S2m,S3m,S4m
!!$    Real(wp)                              ::      sq1,sq2,sq3,sq4,sq5,sq6
!!$    Real(wp)                              ::      L,density,d
!!$
!!$    ! Some energy independent abbreviations
!!$    e1       =    eps_vacuum
!!$    sq1      =    Sqrt(15._wp*pi/7._wp)
!!$    sq2      =    Sqrt(pi)
!!$    sq3      =    Sqrt(pi/5._wp)
!!$    sq4      =    Sqrt(3._wp*pi/35._wp)
!!$    sq5      =    Sqrt(pi/35._wp)
!!$    sq6      =    Sqrt(5._wp*pi/7._wp)
!!$
!!$    ! Dimensionless density,d,L
!!$    ! d= Absolute distance between surface and the location of multipoles
!!$    d        = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position ) 
!!$    density  = param%Interaction%Derived%Density*param%Island%Radius**2
!!$    L        = param%Interaction%Lattice_Constant / param%Island%Radius
!!$    ! Special value for the dimensionlesslattice constant
!!$    ! (P269 Bedeaux's book) for a random network
!!$    If(param%Interaction%Lattice_Type=='MFT'.or.param%Interaction%Lattice_Type=='RPT') &
!!$         L   =  1._wp/Sqrt(density)
!!$
!!$    ! Evaluation of the lattice Sums
!!$    S2_mp    =  lattice_Sum(0._wp,2)
!!$    S2_imp   =  lattice_Sum(d,2)
!!$    S3_mp    =  lattice_Sum(0._wp,3)
!!$    S3_imp   =  lattice_Sum(d,3)
!!$    S4_mp    =  lattice_Sum(0._wp,4)
!!$    S4_imp   =  lattice_Sum(d,4)
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       !------------------------
!!$       !- MP above the substrate 
!!$       !------------------------        
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          !Some abreviations
!!$          e2    =    param%Materials%Epsilon%Substrate(ienergy)    !    Misc%Eps_Substrate(ienergy)    
!!$          eps   =    (e1-e2)/(e2+e1)
!!$          S2p   =    S2_mp + eps*S2_imp
!!$          S2m   =    S2_mp - eps*S2_imp      
!!$          S3p   =    S3_mp + eps*S3_imp
!!$          S3m   =    S3_mp - eps*S3_imp
!!$          S4p   =    S4_mp + eps*S4_imp
!!$          S4m   =    S4_mp - eps*S4_imp      
!!$
!!$          ! Polarizabilities of clusters interacting with the substrate
!!$          ! Eqs. (5-67) Bedeaux's book
!!$          A1010 =    alphad(1,2,ienergy)/(4*pi*e1)
!!$          A1111 =    alphad(1,1,ienergy)/(4*pi*e1)
!!$          A2111 =    3*alphad(2,1,ienergy)/(4*pi*e1*Sqrt(5._wp))
!!$          A2010 =    alphad(2,2,ienergy)/(2*pi*e1*Sqrt(5._wp/3._wp))
!!$          A2121 =    3*(alphaq(2,ienergy)+2*alphaq(3,ienergy))/(4*pi*e1)
!!$          A2020 =   (2*alphaq(1,ienergy)+3*alphaq(3,ienergy))/(2*pi*e1)
!!$          A2222 =    3*alphaq(3,ienergy)/(2*pi*e1)
!!$          A1020 =    5._wp/3._wp*A2010
!!$          A1121 =    5._wp/3._wp*A2111
!!$
!!$          ! Equations 10-42 10-45
!!$          Dz = (1._wp-4*A1010*S2m/(L**3)*sq3 - 6*A1020*S3m/(L**4)*sq4)* &
!!$               (1._wp+2*A2010*S3p/(L**4)*sq1 + 4*A2020*S4p/(L**5)*sq2)+ &
!!$               (4*A2010*S2m/(L**3)*sq3 + 6*A2020*S3m/(L**4)*sq4)* &
!!$               (2*A1010*S3p/(L**4)*sq1 + 4*A1020*S4p/(L**5)*sq2)
!!$
!!$          Dp = (1._wp+2*A1111*S2p/(L**3)*sq3 + 6*A1121*S3p/(L**4)*sq5)* &
!!$               (1._wp-2*A2111*S3m/(L**4)*sq6 - 8*A2121*S4m/(L**5)*sq2/3)+ &
!!$               (2*A2111*S2p/(L**3)*sq3 + 6*A2121*S3p/(L**4)*sq5)* &
!!$               (2*A1111*S3m/(L**4)*sq6 + 8*A1121*S4m/(L**5)*sq2/3)
!!$
!!$          ! Equations 10-41 10-42
!!$          Az            =       4*pi*e1/(Dz)* &
!!$               ( A1010*(1._wp+2*A2010*S3p/(L**4)*sq1+4*A2020*S4p/(L**5)*sq2) - &
!!$               A2010*(2*A1010*S3p/(L**4)*sq1+4*A1020*S4p/(L**5)*sq2) ) 
!!$
!!$          Az10  =       2*pi*e1*Sqrt(5._wp/3._wp)/(Dz)* &
!!$               ( A2010*(1._wp-4*A1010*S2m/(L**3)*sq3-6*A1020*S3m/(L**4)*sq4) + &
!!$               A1010*(4*A2010*S2m/(L**3)*sq3+6*A2020*S3m/(L**4)*sq4) )
!!$
!!$          Ap            =       4*pi/(Dp)* &
!!$               ( A1111*(1._wp-2*A2111*S3m/(L**4)*sq6-8*A2121*S4m/(L**5)*sq2/3) + &
!!$               A2111*(2*A1111*S3m/(L**4)*sq6+8*A1121*S4m/(L**5)*sq2/3) )
!!$
!!$          Ap10  =       4*pi*e1*Sqrt(5._wp)/3/(Dp)* &
!!$               ( A2111*(1._wp+2*A1111*S2p/(L**3)*sq3+6*A1121*S3p/(L**4)*sq5) - &
!!$               A1111*(2*A2111*S2p/(L**3)*sq3+6*A2121*S3p/(L**4)*sq5) )
!!$
!!$          ! Fills the surface suceptebilities (10-47)
!!$          surf_const_coef(ienergy)%beta   =  density*Az/(e1**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*Ap
!!$          surf_const_coef(ienergy)%delta  =  -density*(Az10+Ap10-d*Az-d*Ap)/e1
!!$          surf_const_coef(ienergy)%tau    =  -density*(Ap10-d*Ap)
!!$
!!$       Enddo
!!$
!!$    Else
!!$       !------------------------
!!$       !- MP below the substrate 
!!$       !------------------------        
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          !Some abreviations
!!$          e2    =     param%Materials%Epsilon%Substrate(ienergy)   !  Misc%Eps_Substrate(ienergy)    
!!$          eps   =     (e1-e2)/(e2+e1)
!!$          S2p   =     S2_mp + eps*S2_imp
!!$          S2m   =     S2_mp - eps*S2_imp      
!!$          S3p   =     S3_mp + eps*S3_imp
!!$          S3m   =     S3_mp - eps*S3_imp
!!$          S4p   =     S4_mp + eps*S4_imp
!!$          S4m   =     S4_mp - eps*S4_imp      
!!$
!!$          ! Polarizabilities of clusters interacting with the substrate
!!$          ! Eqs. (5-67) Bedeaux's book
!!$          A1010 =     alphad(1,2,ienergy)/(4*pi*e2)
!!$          A1111 =     alphad(1,1,ienergy)/(4*pi*e2)
!!$          A2111 =     3*alphad(2,1,ienergy)/(4*pi*e2*Sqrt(5._wp))
!!$          A2010 =     alphad(2,2,ienergy)/(2*pi*e2*Sqrt(5._wp/3._wp))
!!$          A2121 =     3*(alphaq(2,ienergy)+2*alphaq(3,ienergy))/(4*pi*e2)
!!$          A2020 =     (2*alphaq(1,ienergy)+3*alphaq(3,ienergy))/(2*pi*e2)
!!$          A2222 =     3*alphaq(3,ienergy)/(2*pi*e2)
!!$
!!$          ! Equations 10-42 10-45
!!$          Dz = (1._wp-4*A1010*S2p/(L**3)*sq3 - 6*A1020*S3p/(L**4)*sq4)* &
!!$               (1._wp+2*A2010*S3m/(L**4)*sq1 + 4*A2020*S4m/(L**5)*sq2)+ &
!!$               (4*A2010*S2p/(L**3)*sq3 + 6*A2020*S3p/(L**4)*sq4)* &
!!$               (2*A1010*S3m/(L**4)*sq1 + 4*A1020*S4m/(L**5)*sq2)
!!$
!!$          Dp = (1._wp+2*A1111*S2m/(L**3)*sq3 + 6*A1121*S3m/(L**4)*sq5)* &
!!$               (1._wp-2*A2111*S3p/(L**4)*sq6 - 8*A2121*S4p/(L**5)*sq2/3)+ &
!!$               (2*A2111*S2m/(L**3)*sq3 + 6*A2121*S3m/(L**4)*sq5)* &
!!$               (2*A1111*S3p/(L**4)*sq6 + 8*A1121*S4p/(L**5)*sq2/3)
!!$
!!$          ! Equations 10-41 10-42
!!$          Az            =       4*pi*e2/(Dz)* &
!!$               ( A1010*(1._wp+2*A2010*S3m/(L**4)*sq1+4*A2020*S4m/(L**5)*sq2) - &
!!$               A2010*(2*A1010*S3m/(L**4)*sq1+4*A1020*S4m/(L**5)*sq2) ) 
!!$
!!$          Az10  =       2*pi*e2*Sqrt(5._wp/3._wp)/(Dz)* &
!!$               ( A2010*(1._wp-4*A1010*S2p/(L**3)*sq3-6*A1020*S3p/(L**4)*sq4) + &
!!$               A1010*(4*A2010*S2p/(L**3)*sq3+6*A2020*S3p/(L**4)*sq4) )
!!$
!!$          Ap            =       4*pi*e2/(Dp)* &
!!$               ( A1111*(1._wp-2*A2111*S3p/(L**4)*sq6-8*A2121*S4p/(L**5)*sq2/3) + &
!!$               A2111*(2*A1111*S3p/(L**4)*sq6+8*A1121*S4p/(L**5)*sq2/3) )
!!$
!!$          Ap10  =       4*pi*e2*Sqrt(5._wp)/3/(Dp)* &
!!$               ( A2111*(1._wp+2*A1111*S2m/(L**3)*sq3+6*A1121*S3m/(L**4)*sq5) - &
!!$               A1111*(2*A2111*S2m/(L**3)*sq3+6*A2121*S3m/(L**4)*sq5) )
!!$
!!$          ! Fills the surface suceptebilities (10-47)
!!$          surf_const_coef(ienergy)%beta   =  density*Az/(e2**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*Ap
!!$          surf_const_coef(ienergy)%delta  =  -density*(Az10+Ap10+d*Az+d*Ap)/e2
!!$          surf_const_coef(ienergy)%tau    =  -density*(Ap10+d*Ap)*(e1/e2)
!!$
!!$       Enddo
!!$
!!$    Endif
!!$
!!$  End Subroutine  surf_const_coef_quadrupole
!!$
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine Fresnel_calc(Fresnel_coeff,Fresnel_phase,surf_const_coef)
!!$    Implicit None
!!$    Type(surface_constitutive_type)     :: surf_const_coef(param%Numerics%NEnergy)
!!$    Real(wp)                            :: Fresnel_coeff(param%Numerics%NEnergy)
!!$    Real(wp)                            :: Fresnel_phase(param%Numerics%NEnergy)
!!$    ! Local 
!!$    Type(Invariants_type),Allocatable   :: Invar(:)
!!$    Integer                             :: istat
!!$
!!$    !NICK   SelectCase(TRAL(param%Misc%Fresnel_DR_Formulae))
!!$    SelectCase(TRIM(param%Misc%Fresnel_DR_Formulae))
!!$
!!$    Case('constitutive')
!!$       ! ------------------------------------------------------
!!$       ! --- Surface constitutive coefficients
!!$       ! ------------------------------------------------------
!!$       Select Case(param%Misc%Output_Type)
!!$       Case('DR2','R2')
!!$          ! Calculate the differential reflectivity of a stacking of 2films with a reference on substrate+first film
!!$          Call diff_ref_coef_2film_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,param%Misc%Output_Type)                          
!!$       Case('DR','R')
!!$          ! Calculate the differential reflectivity or reflectivity
!!$          Call diff_ref_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,param%Misc%Output_Type)
!!$       Case('DT','T')
!!$          ! Calculate the differential transmittance or trasmittance
!!$          Call diff_tran_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,param%Misc%Output_Type)
!!$       Case('A')
!!$          ! Calculate the Absolute coefficient of Absorption
!!$          Call absorp_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef)
!!$       Case('EELS')
!!$          ! Calculate the EELS cross-section
!!$          Call EELS_Loss_Function(Fresnel_coeff,surf_const_coef)
!!$          Fresnel_phase(:) = 0._wp
!!$       Case Default
!!$          Write(unit=6,fmt=*) 'Error in Fresnel_calc for param%Misc%Output_Type :',param%Misc%Output_Type
!!$       End Select
!!$
!!$    Case('constitutive_all')
!!$       ! -------------------------------------------------------------
!!$       ! --- Surface constitutive coefficients with second order terms
!!$       ! -------------------------------------------------------------
!!$       Select Case(param%Misc%Output_Type)
!!$       Case('DR2','R2')
!!$          ! Calculate the differential reflectivity of a stacking of 2films with a reference on substrate+first film
!!$          Call diff_ref_coef_2film_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,param%Misc%Output_Type)
!!$       Case('DR','R')
!!$          ! Calculate the differential reflectivity or reflectivity
!!$          Call diff_ref_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,param%Misc%Output_Type)
!!$       Case('DT','T')
!!$          ! Calculate the differential transmittance or trasmittance
!!$          Call diff_tran_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,param%Misc%Output_Type)
!!$       Case('A')
!!$          ! Calculate the Absolute coefficient of Absorption
!!$          Call absorp_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef)
!!$       Case Default
!!$          Write(unit=6,fmt=*) 'Error in Fresnel_calc for param%Misc%Output_Type :',param%Misc%Output_Type
!!$       End Select
!!$
!!$    Case('invariants')
!!$       ! ------------------------------------------------------
!!$       ! --- Surface Invariants
!!$       ! ------------------------------------------------------
!!$       ! Calculate the (dimensionless) invariants (invar) for all energies     
!!$       Allocate(Invar(param%Numerics%NEnergy))
!!$       Call Invariants_calc(Invar,surf_const_coef)
!!$       Select Case(param%Misc%Output_Type)
!!$       Case('DR','R')
!!$          ! Calculate the differential reflectivity or reflectivity      
!!$          Call diff_ref_coef_invariants(Fresnel_coeff,Invar,param%Misc%Output_Type)
!!$       Case('DT','T')
!!$          ! Calculate the differential transmittance or trasmittance
!!$          Call diff_tran_coef_invariants(Fresnel_coeff,Invar,param%Misc%Output_Type)
!!$       Case('A')
!!$          ! Calculate the Absolute coefficient of Absorption
!!$          Call Absorp_coef_invariants(Fresnel_coeff,Invar)
!!$       Case Default
!!$          Write(unit=6,fmt=*) 'Error in Fresnel_calc for param%Misc%Output_Type :',param%Misc%Output_Type
!!$       End Select
!!$       Deallocate(Invar,stat=ISTAT)
!!$       Fresnel_phase(:) = 0._wp
!!$
!!$    Case('aspnes')
!!$       ! --------------------------------------------------------------
!!$       ! --- Mac Intyre and Aspnes formulae modified by Bagchi
!!$       ! --- cf YB,FA Thin Solid Film 125 (1985) 129   formulae 6-7
!!$       ! --------------------------------------------------------------
!!$       Write(unit=6,fmt=*)
!!$       Write(unit=6,fmt=*) '-----------------------------------------------------------'
!!$       Write(unit=6,fmt=*) ' Warning (Subroutine : diff_ref_coef_aspnes) !'
!!$       Write(unit=6,fmt=*) ' Apsnes formulas Only valid for differential reflectivity !'
!!$       Write(unit=6,fmt=*) '-----------------------------------------------------------'
!!$       Call diff_ref_coef_aspnes(Fresnel_coeff,surf_const_coef)
!!$       Fresnel_phase(:) = 0._wp
!!$
!!$    Case default
!!$       !NICK     Write(unit=6,fmt=*) 'ERROR (Fresnel_calc): Parameter ', &
!!$       !NICK     TRAL(param%Misc%Fresnel_DR_Formulae),' not supported (yet)'
!!$       Write(unit=6,fmt=*) 'ERROR (Fresnel_calc): Parameter ', &
!!$            TRIM(param%Misc%Fresnel_DR_Formulae),' not supported (yet)'
!!$       Pause
!!$       Stop
!!$    End Select
!!$
!!$  End Subroutine Fresnel_calc
!!$
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_ref_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,output)
!!$    ! Calculates the differential reflectivity from knowledge of the
!!$    ! surface constitutive coefficients.
!!$    ! This method is described in detail in Haarmans and Bedeaux,
!!$    ! Thin Solid Films 224, 117 (1992).
!!$    ! See Eq. (34)--(37)
!!$    Implicit None    
!!$    Type(surface_constitutive_type)    :: surf_const_coef(:), chi
!!$    Real(wp)                           :: Fresnel_coeff(:)
!!$    Real(wp)                           :: Fresnel_phase(:)
!!$    Character(len=*)                   :: output
!!$    integer                            :: ienergy
!!$    Real(wp)                           :: c0,s0,ooc,hc
!!$    complex(wp )                       :: e1,n1,e2,n2,st,ct,Reflec,fresnel
!!$    complex(wp )                       :: factor(2),tmp(4)
!!$
!!$
!!$    ! Some energy independent Parameters
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Omega over c (dimensionless)
!!$       ooc            =  param%Island%Radius * param%Numerics%Energy(ienergy) / hc     ! / (HBAR*Speed_of_Light)
!!$       chi            =  surf_const_coef(ienergy)
!!$       e2             =  param%Materials%Epsilon%Substrate_file(ienergy)   !    Misc%Eps_Substrate_file(ienergy)
!!$       n2             =  Sqrt(e2) 
!!$       st             =  n1/n2*s0
!!$       ct             =  Sqrt(1._wp - st**2) 
!!$
!!$       !NICK   Select Case (TRAL(param%Source%Polarization))
!!$       Select Case (TRIM(param%Source%Polarization))
!!$       Case('p')
!!$          tmp(1)         =  imu*ooc                                              
!!$          tmp(2)         =  -tmp(1)**2/4._wp*e1*chi%beta*chi%gamma*s0**2         
!!$          tmp(3)         =  chi%gamma*c0*ct + n1*n2*chi%beta*e1*s0**2            
!!$          tmp(4)         =  chi%gamma*c0*ct - n1*n2*chi%beta*e1*s0**2    
!!$          factor         =  (/ n2*c0 + n1*ct , n2*c0 - n1*ct /) 
!!$          fresnel        =  factor(2)/factor(1)  
!!$          Reflec         =  (factor(2) - tmp(1)*tmp(4) -tmp(2)*factor(2))/   &
!!$               (factor(1) -tmp(1)*tmp(3) -tmp(2)*factor(1))
!!$       Case('s')
!!$          tmp(1)         =  imu*ooc 
!!$          factor         =  (/ n1*c0 + n2*ct , n1*c0 - n2*ct /) 
!!$          fresnel        =  factor(2)/factor(1) 
!!$          Reflec         =  ( factor(2) + tmp(1)*chi%gamma )   /   &
!!$               ( factor(1) - tmp(1)*chi%gamma )  
!!$       End Select
!!$
!!$       Select Case(output)
!!$       Case('DR')
!!$          ! The differential Reflection 
!!$          Fresnel_coeff(ienergy)  = ( (Abs(Reflec))**2 ) / ( (Abs(fresnel))**2 ) - 1._wp
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Reflec/fresnel),Real(Reflec/fresnel,wp)) * 180._wp /pi
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Reflec/fresnel),Real(Reflec/fresnel,wp)) 
!!$       Case('R')
!!$          ! Absolute coefficient
!!$          Fresnel_coeff(ienergy)  = (Abs(Reflec))**2
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Reflec),Real(Reflec,wp))* 180._wp/pi
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Reflec),Real(Reflec,wp))
!!$       End Select
!!$
!!$    Enddo
!!$
!!$  End Subroutine diff_ref_coef_constitutive
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_tran_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,output)
!!$    ! Calculates the differential Transmittance from knowledge of the
!!$    ! surface constitutive coefficients.
!!$    ! This method is described in detail in Haarmans and Bedeaux,
!!$    ! Thin Solid Films 224, 117 (1992).
!!$    ! See Eq. (34)--(37)
!!$    ! or in the parper of Bedeaux Physica 67 (1973) 55-73
!!$
!!$    Type(surface_constitutive_type)   :: surf_const_coef(:), chi
!!$    Real(wp)                          :: Fresnel_coeff(:)
!!$    Real(wp)                          :: Fresnel_phase(:)
!!$    Character(len=*)                  :: output
!!$    integer                           :: ienergy
!!$    Real(wp)                          :: c0,s0,ooc,hc
!!$    complex(wp )                      :: e1,n1,e2,n2,st,ct
!!$    complex(wp )                      :: Transmi,fresnel
!!$    complex(wp )                      :: factor,tmp(3)
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt=*) '----------------------------------------------------'
!!$    Write(unit=6,fmt=*) ' Warning (Subroutine : diff_tran_coef_constitutive) !'
!!$    Write(unit=6,fmt=*) ' The transmission coefficient is Only valid for'
!!$    Write(unit=6,fmt=*) ' non-Absorbing substrate above the total reflection'
!!$    Write(unit=6,fmt=*) '----------------------------------------------------'
!!$
!!$    ! Some energy independent Parameters
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Omega over c (dimensionless)
!!$       ooc            =  param%Island%Radius * param%Numerics%Energy(ienergy)  / hc  ! / (HBAR*Speed_of_Light)
!!$       chi            =  surf_const_coef(ienergy)
!!$       e2             =  param%Materials%Epsilon%Substrate_file(ienergy)    ! Misc%Eps_Substrate_file(ienergy)
!!$       n2             =  Sqrt(e2) 
!!$       st             =  n1/n2*s0
!!$       If(Real(st)>1._wp) Then
!!$          Write(unit=6,fmt=*) 'Total reflection at : ',param%Numerics%Energy(ienergy),' eV !'
!!$          Write(unit=6,fmt=*) '(Subroutine : diff_tran_coef_constitutive)'
!!$       Endif
!!$       ct             =  Sqrt(1._wp - st**2) 
!!$       tmp(1)         =  imu*ooc                                              
!!$       tmp(2)         =  -tmp(1)**2/4._wp*e1*chi%beta*chi%gamma*s0**2         
!!$       tmp(3)         =  chi%gamma*c0*ct + n1*n2*chi%beta*e1*s0**2    
!!$
!!$       !NICK   Select Case (TRAL(param%Source%Polarization))
!!$       Select Case (TRIM(param%Source%Polarization))
!!$       Case('p')
!!$          factor         =  n2*c0 + n1*ct 
!!$          fresnel        =  2*n1*c0 / factor  
!!$          Transmi        =  2*n1*c0* (1+tmp(2)) / (factor -tmp(1)*tmp(3) -tmp(2)*factor)
!!$       Case('s')
!!$          factor         =  n1*c0 + n2*ct 
!!$          fresnel        =  2*n1*c0 / factor 
!!$          Transmi        =  2*n1*c0   /  ( factor - tmp(1)*chi%gamma )  
!!$       End Select
!!$
!!$       Select Case(output)
!!$       Case('DT')
!!$          ! The differential Transmission 
!!$          Fresnel_coeff(ienergy)  = ( (Abs(Transmi))**2)/( (Abs(fresnel))**2)-1._wp
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Transmi/fresnel),Real(Transmi/fresnel,wp)) * 180._wp/pi
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Transmi/fresnel),Real(Transmi/fresnel,wp)) 
!!$       Case('T')
!!$          ! The Absolute coefficient 
!!$          Fresnel_coeff(ienergy)  =  Real(n2*ct/n1/c0)*Abs(Transmi)**2
!!$          Fresnel_phase(ienergy)  =  atan2(Aimag(Transmi),Real(Transmi,wp)) * 180._wp/pi 
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Transmi),Real(Transmi,wp)) 
!!$       End Select
!!$
!!$    Enddo
!!$  End Subroutine diff_tran_coef_constitutive
!!$
!!$
!!$  !
!!$  !-------------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$  Subroutine absorp_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef)
!!$
!!$    ! Calculates the absorption coefficient with constitutive coefficients up to first order
!!$
!!$    Implicit None    
!!$    Type(surface_constitutive_type) :: surf_const_coef(:)
!!$    Real(wp)                        :: Fresnel_coeff(:)
!!$    Real(wp)                        :: Fresnel_phase(:)
!!$    Real(wp),Allocatable            :: Fresnel_coeff_tmp(:)
!!$    Integer                         :: istat
!!$    
!!$
!!$    Allocate(Fresnel_coeff_tmp(param%Numerics%NEnergy))
!!$    Call diff_ref_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,'R')
!!$    Fresnel_coeff_tmp(:) = Fresnel_coeff(:)
!!$    Call diff_tran_coef_constitutive(Fresnel_coeff,Fresnel_phase,surf_const_coef,'T')
!!$    Fresnel_coeff(:) = 1._wp - Fresnel_coeff_tmp(:) - Fresnel_coeff(:)
!!$    Deallocate(Fresnel_coeff_tmp,stat=ISTAT)
!!$    Fresnel_phase(:) = 0._wp
!!$
!!$
!!$  End Subroutine absorp_coef_constitutive
!!$
!!$
!!$  !
!!$  !-------------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$  Subroutine diff_ref_coef_2film_constitutive(Fresnel_coeff,Fresnel_phase,SCC1,output)
!!$    Implicit None    
!!$    Type(surface_constitutive_Type) :: SCC1(:)
!!$    Real(wp)                        :: Fresnel_coeff(:)
!!$    Real(wp)                        :: Fresnel_phase(:)
!!$    Character(len=*)                :: output
!!$    Type(surface_constitutive_type) :: SCC2(param%Numerics%NEnergy),SSCstack(param%Numerics%NEnergy)
!!$    Real(wp)                        :: FCS(param%Numerics%NEnergy), FPS(param%Numerics%NEnergy)
!!$    Real(wp)                        :: FCC(param%Numerics%NEnergy), FPC(param%Numerics%NEnergy)
!!$
!!$    ! Get the constitutive coefficients of the coating layer (dividing surface = surface of the coating)
!!$!    Call surf_const_coef_film_below(SCC2,param%Island%Coating_Thickness,param%Misc%Eps_Coating,param%Misc%Eps_Substrate_file)
!!$    Call surf_const_coef_film_below(SCC2,param%Island%Coating_Thickness,   &
!!$              param%Materials%Epsilon%Coating,param%Materials%Epsilon%Substrate_file)
!!$    ! Get the total surface constitutive coefficients
!!$    Call add_2fims_surf_const_coef(SCC1,SCC2,SSCstack)
!!$    ! Get the reflectivity of the whole stacking
!!$    Call diff_ref_coef_constitutive(FCS,FPS,SSCstack,'R')
!!$
!!$    Select Case(output)
!!$    Case('R2')
!!$       ! The output for the stack reflectivity
!!$       Fresnel_coeff(:) = FCS(:)
!!$       Fresnel_phase(:) = FPS(:)
!!$    Case('DR2')
!!$       ! Get the Fresnel reflectivity of the substrate plus the coating film
!!$       Call diff_ref_coef_constitutive(FCC,FPC,SCC2,'R')
!!$       ! The differential reflectivity (reference=subtrate+coating)
!!$       Fresnel_coeff(:)  =  FCS(:)/FCC(:)  - 1._wp
!!$       Fresnel_phase(:)  =  FPS(:)-FPC(:)
!!$    End Select
!!$
!!$  End Subroutine diff_ref_coef_2film_constitutive
!!$
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$  Subroutine diff_ref_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,output)
!!$    ! Calculates the differential reflectivity from knowledge of the
!!$    ! surface constitutive coefficients up to second order
!!$    ! See Eq. 4.18 Bedeaux's book
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: surf_const_coef(:), chi
!!$    Real(wp)                         :: Fresnel_coeff(:)
!!$    Real(wp)                         :: Fresnel_phase(:)
!!$    Character(len=*)                 :: output
!!$    integer                          :: ienergy
!!$    Real(wp)                         :: c0,s0,ooc, hc
!!$    complex(wp )                     :: e1,n1,e2,n2,st,ct,Reflec,fresnel
!!$    complex(wp )                     :: factor(2),tmp(4)
!!$
!!$    ! Some energy independent Parameters
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Omega over c (dimensionless)
!!$       ooc            =  param%Island%Radius * param%Numerics%Energy(ienergy) / hc !/ (HBAR*Speed_of_Light)
!!$       chi            =  surf_const_coef(ienergy)
!!$       e2             =  param%Materials%Epsilon%Substrate_file(ienergy)   !  Misc%Eps_Substrate_file(ienergy)
!!$       n2             =  Sqrt(e2) 
!!$       st             =  n1/n2*s0
!!$       ct             =  Sqrt(1._wp - st**2) 
!!$
!!$       !NICK   Select Case (TRAL(param%Source%Polarization))
!!$       Select Case (TRIM(param%Source%Polarization))
!!$       Case('p')
!!$          tmp(1)         =  imu*ooc                                                       
!!$          tmp(2)         =  -tmp(1)**2/4._wp*e1*chi%beta*chi%gamma*s0**2                          
!!$          tmp(3)         =  chi%gamma*c0*ct + n1*n2*chi%beta*e1*s0**2            
!!$          tmp(4)         =  chi%gamma*c0*ct - n1*n2*chi%beta*e1*s0**2    
!!$          factor(1)      =  n2*c0*(1._wp-ooc**2*chi%tau) + n1*ct*(1._wp+ooc**2*chi%tau)
!!$          factor(2)              =  n2*c0*(1._wp-ooc**2*chi%tau) - n1*ct*(1._wp+ooc**2*chi%tau)
!!$          fresnel        =  (n2*c0-n1*ct)/(n2*c0+n1*ct)  
!!$          Reflec         =  (factor(2) - tmp(1)*tmp(4) -tmp(2)*(n2*c0-n1*ct))/   &
!!$               (factor(1) - tmp(1)*tmp(3) -tmp(2)*(n2*c0+n1*ct))
!!$       Case('s')
!!$          tmp(1)         =  imu*ooc 
!!$          factor(1)      =  n1*c0*(1._wp+ooc**2*chi%tau) + n2*ct*(1._wp-ooc**2*chi%tau)
!!$          factor(2)               = n1*c0*(1._wp+ooc**2*chi%tau) - n2*ct*(1._wp-ooc**2*chi%tau)
!!$          fresnel        =  (n1*c0-n2*ct)/(n1*c0+n2*ct) 
!!$          Reflec         =  ( factor(2) + tmp(1)*chi%gamma )   /   &
!!$               ( factor(1) - tmp(1)*chi%gamma )  
!!$       End Select
!!$
!!$       Select Case(output)
!!$       Case('DR')
!!$          ! The differential Reflection 
!!$          Fresnel_coeff(ienergy)  = (Abs(Reflec/fresnel))**2-1._wp
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Reflec/fresnel),Real(Reflec/fresnel,wp)) * 180._wp/pi
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Reflec/fresnel),Real(Reflec/fresnel,wp)) 
!!$       Case('R')
!!$          ! Absolute coefficient
!!$          Fresnel_coeff(ienergy)  = (Abs(Reflec))**2
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Reflec),Real(Reflec,wp)) 
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Reflec),Real(Reflec,wp)) 
!!$       End Select
!!$
!!$    Enddo
!!$
!!$  End Subroutine diff_ref_coef_constitutive_all
!!$
!!$  
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_tran_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,output)
!!$    ! Calculates the differential Transmittance from knowledge of the
!!$    ! surface constitutive coefficients up to second order
!!$
!!$    Type(surface_constitutive_type)  :: surf_const_coef(:), chi
!!$    Real(wp)                         :: Fresnel_coeff(:)
!!$    Real(wp)                         :: Fresnel_phase(:)
!!$    Character(len=*)                 :: output
!!$    integer                          :: ienergy
!!$    Real(wp)                         :: c0,s0,ooc,hc
!!$    complex(wp )                     :: e1,n1,e2,n2,st,ct
!!$    complex(wp )                     :: Transmi,fresnel
!!$    complex(wp )                     :: factor,tmp(3)
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt=*) '---------------------------------------------------------'
!!$    Write(unit=6,fmt=*) ' Warning (Subroutine : diff_tran_coef_constitutive_all) !'
!!$    Write(unit=6,fmt=*) ' The transmission coefficient is Only valid for'
!!$    Write(unit=6,fmt=*) ' non-Absorbing substrate above the total reflection'
!!$    Write(unit=6,fmt=*) '---------------------------------------------------------'
!!$
!!$    ! Some energy independent Parameters
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Omega over c (dimensionless)
!!$       ooc            =  param%Island%Radius * param%Numerics%Energy(ienergy) / hc  !/ (HBAR*Speed_of_Light)
!!$       chi            =  surf_const_coef(ienergy)
!!$       e2             =  param%Materials%Epsilon%Substrate_file(ienergy)!    Misc%Eps_Substrate_file(ienergy)
!!$       n2             =  Sqrt(e2) 
!!$       st             =  n1/n2*s0
!!$       If(Real(st)>1._wp) Then
!!$          Write(unit=6,fmt=*) 'Total reflection at : ',param%Numerics%Energy(ienergy),' eV !'
!!$          Write(unit=6,fmt=*) '(Subroutine : diff_tran_coef_constitutive)'
!!$       Endif
!!$       ct             =  Sqrt(1._wp - st**2) 
!!$       tmp(1)         =  imu*ooc                                              
!!$       tmp(2)         =  -tmp(1)**2/4._wp*e1*chi%beta*chi%gamma*s0**2         
!!$       tmp(3)         =  chi%gamma*c0*ct + n1*n2*chi%beta*e1*s0**2    
!!$
!!$       !NICK   Select Case (TRAL(param%Source%Polarization))
!!$       Select Case (TRIM(param%Source%Polarization))
!!$       Case('p')
!!$          factor         =  n2*c0*(1._wp-ooc**2*chi%tau)+n1*ct*(1._wp+ooc**2*chi%tau) 
!!$          fresnel        =  2*n1*c0 / (n2*c0 + n1*ct)  
!!$          Transmi        =  2*n1*c0* (1+tmp(2)) /   &
!!$               (factor -tmp(1)*tmp(3) -tmp(2)*(n2*c0 + n1*ct))
!!$       Case('s')
!!$          factor         =  n1*c0*(1._wp+ooc**2*chi%tau)+n2*ct*(1._wp-ooc**2*chi%tau) 
!!$          fresnel        =  2*n1*c0 / (n1*c0 + n2*ct) 
!!$          Transmi        =  2*n1*c0   /   &
!!$               ( factor - tmp(1)*chi%gamma )  
!!$       End Select
!!$
!!$       Select Case(output)
!!$       Case('DT')
!!$          ! The differential Transmission 
!!$          Fresnel_coeff(ienergy)  = Abs(Transmi/fresnel)**2-1._wp
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Transmi/fresnel),Real(Transmi/fresnel,wp)) * 180._wp/pi
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Transmi/fresnel),Real(Transmi/fresnel,wp)) 
!!$       Case('T')
!!$          ! The Absolute coefficient 
!!$          Fresnel_coeff(ienergy)  =  Real(n2*ct/n1/c0)*Abs(Transmi)**2
!!$          Fresnel_phase(ienergy)  = atan2(Aimag(Transmi),Real(Transmi,wp)) * 180._wp/pi
!!$          !Fresnel_phase(ienergy)  = Atan2d(Aimag(Transmi),Real(Transmi,wp)) 
!!$       End Select
!!$
!!$    Enddo
!!$  End Subroutine diff_tran_coef_constitutive_all
!!$
!!$
!!$  !
!!$  !-------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine absorp_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef)
!!$    !
!!$    ! Calculates the absorption coefficient with constitutive coefficients up to first order
!!$    !
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: surf_const_coef(:)
!!$    Real(wp)                         :: Fresnel_coeff(:)
!!$    Real(wp)                         :: Fresnel_phase(:)
!!$    Real(wp),Allocatable             :: Fresnel_coeff_tmp(:)
!!$    Integer                          :: istat
!!$
!!$
!!$    Allocate(Fresnel_coeff_tmp(param%Numerics%NEnergy))
!!$    Call diff_ref_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,'R')
!!$    Fresnel_coeff_tmp(:) = Fresnel_coeff(:)
!!$    Call diff_tran_coef_constitutive_all(Fresnel_coeff,Fresnel_phase,surf_const_coef,'T')
!!$    Fresnel_coeff(:) = 1._wp - Fresnel_coeff_tmp(:) - Fresnel_coeff(:)
!!$    Deallocate(Fresnel_coeff_tmp,stat=ISTAT)
!!$    Fresnel_phase(:) = 0._wp
!!$
!!$  End Subroutine absorp_coef_constitutive_all
!!$
!!$  !
!!$  !-------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_ref_coef_2film_constitutive_all(Fresnel_coeff,Fresnel_phase,SCC1,output)
!!$    
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: SCC1(:)
!!$    Real(wp)                         :: Fresnel_coeff(:)
!!$    Real(wp)                         :: Fresnel_phase(:)
!!$    Character(len=*)                 :: output
!!$    Type(surface_constitutive_type)  :: SCC2(param%Numerics%NEnergy),SSCstack(param%Numerics%NEnergy)
!!$    Real(wp)                         :: FCS(param%Numerics%NEnergy), FPS(param%Numerics%NEnergy)
!!$    Real(wp)                         :: FCC(param%Numerics%NEnergy), FPC(param%Numerics%NEnergy)
!!$
!!$    ! Get the constitutive coefficients of the coating layer (dividing surface = surface of the coating)
!!$    !Call surf_const_coef_film_below(SCC2,param%Island%Coating_Thickness,param%Misc%Eps_Coating,param%Misc%Eps_Substrate_file)
!!$    Call surf_const_coef_film_below(SCC2,param%Island%Coating_Thickness,   &
!!$         param%Materials%Epsilon%Coating, param%Materials%Epsilon%Substrate_file)
!!$    ! Get the total surface constitutive coefficients
!!$    Call add_2fims_surf_const_coef(SCC1,SCC2,SSCstack)
!!$    ! Get the reflectivity of the whole stacking
!!$    Call diff_ref_coef_constitutive_all(FCS,FPS,SSCstack,'R')
!!$
!!$    Select Case(output)
!!$    Case('R2')
!!$       ! The output for the stack reflectivity
!!$       Fresnel_coeff(:) = FCS(:)
!!$       Fresnel_phase(:) = FPS(:)
!!$    Case('DR2')
!!$       ! Get the Fresnel reflectivity of the substrate plus the coating film
!!$       Call diff_ref_coef_constitutive_all(FCC,FPC,SCC2,'R')
!!$       ! The differential reflectivity (reference=subtrate+coating)
!!$       Fresnel_coeff(:)  =  FCS(:)/FCC(:)  - 1._wp
!!$       Fresnel_phase(:)  =  FPS(:)-FPC(:)
!!$    End Select
!!$
!!$
!!$  End Subroutine diff_ref_coef_2film_constitutive_all
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine Invariants_calc(Invar,surf_const_coef) 
!!$    ! Calculates The (diemsionless) invariants (asSuming non-magnetic materials)
!!$    ! See Bedeaux book Eq. (3.9.4)
!!$    Implicit None
!!$    Type(surface_constitutive_type) :: surf_const_coef(:), chi
!!$    Type(Invariants_type)           :: Invar(:)
!!$    ! Local 
!!$    integer                         :: ienergy
!!$    Real(wp)                        :: ooc,hc
!!$    complex(wp )                    :: e1,e2
!!$
!!$    ! Some energy independent abbreviations
!!$    e1       = eps_vacuum      
!!$    hc       = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$    ! Main loop over energy
!!$    Do ienergy = 1,param%Numerics%NEnergy    
!!$       ooc    =   param%Island%Radius * param%Numerics%Energy(ienergy) / hc  !/ (HBAR*Speed_of_Light)
!!$       chi    =   surf_const_coef(ienergy)
!!$       e2     =   param%Materials%Epsilon%Substrate(ienergy)    ! Misc%Eps_Substrate(ienergy)
!!$       Invar(ienergy)%e        = chi%gamma  - e2*e1*chi%beta
!!$       Invar(ienergy)%delta_e  = chi%delta -  0.5_wp*(e2+e1)/(e2-e1)*chi%gamma*chi%beta
!!$       Invar(ienergy)%tau      = chi%tau - 0.5_wp*chi%gamma**2/(e2-e1)
!!$       Invar(ienergy)%c        = Aimag(chi%gamma/(e2-e1))  
!!$    Enddo
!!$
!!$  End Subroutine Invariants_calc
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_ref_coef_invariants(Fresnel_coeff,Invar,output) 
!!$    ! Calculates the differential reflectivity from knowledge of the
!!$    ! invariants.
!!$    ! This method is described in detail in Bedeaux book
!!$    ! See in particular Eq. (4.58) (4.62)      
!!$    Implicit None
!!$    Real(wp)                  :: Fresnel_coeff(:)
!!$    Type(Invariants_type)     :: Invar(:), I
!!$    Character(len=*)          :: output
!!$    ! Local 
!!$    integer                   :: ienergy
!!$    Real(wp)                  :: c0,s0,ooc,hc
!!$    Real(wp)                  :: fresnel, R
!!$    complex(wp )              :: prefactor,nominator,denominator,tmp(2)
!!$    complex(wp )              :: ct, st, n2, n1, e2, e1 
!!$
!!$    ! Some energy independent abbreviations
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Some energy dependent abbreviations
!!$       e2        = param%Materials%Epsilon%Substrate_file(ienergy)   ! Misc%Eps_Substrate_file(ienergy)
!!$       n2        = Sqrt(e2) 
!!$       st        = n1/n2*s0                    ! Snell's law
!!$       ct        = Sqrt(1._wp - st**2)
!!$       I         = Invar(ienergy)
!!$       ! Omega over c (dimensionless)
!!$       ooc       = param%Island%Radius * param%Numerics%Energy(ienergy) / hc  !/ (HBAR*Speed_of_Light)
!!$
!!$       !NICK   Select Case(TRAL(param%Source%Polarization))
!!$       Select Case(TRIM(param%Source%Polarization))
!!$
!!$       Case('p')
!!$          ! The Fresnel reflectivity
!!$          fresnel =  Abs( (n2*c0-n1*ct)/(n2*c0+n1*ct) )**2  
!!$          !---------------------------------------------------------
!!$          !--- Calculates the Reflection coefficient; Eq. (4.62) 
!!$          !---------------------------------------------------------
!!$          tmp(1) = 2*ooc**2*( Real(I%tau)-Real(I%delta_e)*(n1*s0)**2 )     &
!!$               *( Abs(n2)**2*c0**2-n1**2*Abs(ct)**2 )
!!$          tmp(2) = Aimag(n1*Conjg(n2)*c0*ct)  *  &
!!$               4*ooc**2*( Aimag(I%tau)-Aimag(I%delta_e)*(n1*s0)**2 )
!!$          prefactor   = Exp( 4*ooc*I%c*n1*c0 )
!!$          nominator   = Abs( n2*c0-n1*ct-imu*ooc*(n1/n2)*I%e*s0**2 )**2  &
!!$               - tmp(1) + tmp(2)   
!!$          denominator = Abs( n2*c0+n1*ct+imu*ooc*(n1/n2)*I%e*s0**2 )**2  &
!!$               - tmp(1) - tmp(2)   
!!$          ! The reflection coefficient for p-polarization
!!$          R         =  Real(prefactor * nominator / denominator)
!!$
!!$       Case('s')
!!$          ! The Fresnel reflectivity
!!$          fresnel =  Abs( (n1*c0-n2*ct)/(n1*c0+n2*ct) )**2  
!!$          !---------------------------------------------------------
!!$          !--- Calculates the Reflection coefficient; Eq. (4.56) 
!!$          !---------------------------------------------------------
!!$          prefactor   = Exp( 4*ooc*I%c*n1*c0 )
!!$          tmp(1) = 2*ooc**2*Real(I%tau)*( (n1*c0)**2-Abs(n2*ct)**2)
!!$          tmp(2) = 4*ooc**2*Aimag(I%tau)*n1*c0*Aimag(n2*ct)
!!$          denominator = Abs(n1*c0+n2*ct)**2 + tmp(1) + tmp(2)
!!$          nominator   = Abs(n1*c0-n2*ct)**2 + tmp(1) - tmp(2)
!!$          ! The reflection coefficient for s-polarization
!!$          R         =  Real(prefactor * nominator / denominator)
!!$
!!$       End Select
!!$
!!$       Select Case(output)
!!$          ! The differential reflection
!!$       Case('DR')
!!$          Fresnel_coeff(ienergy) =   R/fresnel -1._wp
!!$          ! The reflection coefficient
!!$       Case('R')
!!$          Fresnel_coeff(ienergy) =   R
!!$       End Select
!!$    Enddo
!!$
!!$  End Subroutine diff_ref_coef_invariants
!!$
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_tran_coef_invariants(Fresnel_coeff,Invar,output) 
!!$
!!$    ! Calculates the differential trasmittance from knowledge of the
!!$    ! invariants.
!!$    ! This method is described in detail in Bedeaux book
!!$    ! See in particular Eq. (4.60) (4.66)
!!$    Implicit None
!!$    Real(wp)                  :: Fresnel_coeff(:)
!!$    Type(Invariants_type)     :: Invar(:), I
!!$    Character(len=*)          :: output
!!$    ! Local 
!!$    integer                   :: ienergy
!!$    Real(wp)                  :: c0,s0,ooc,hc
!!$    Real(wp)                  :: fresnel, T
!!$    complex(wp )              :: prefactor,nominator,denominator,tmp(2)
!!$    complex(wp )              :: ct, st, n2, n1, e2, e1 
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt=*) '----------------------------------------------------'
!!$    Write(unit=6,fmt=*) ' Warning (Subroutine : diff_tran_coef_invariants) !'
!!$    Write(unit=6,fmt=*) ' The transmission coefficient is Only valid for'
!!$    Write(unit=6,fmt=*) ' non-Absorbing substrate above the total reflection'
!!$    Write(unit=6,fmt=*) '----------------------------------------------------'
!!$
!!$    ! Some energy independent abbreviations
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Some energy dependent abbreviations
!!$       e2        = param%Materials%Epsilon%Substrate_file(ienergy)   !  Misc%Eps_Substrate_file(ienergy)
!!$       n2        = Sqrt(e2) 
!!$       st        = n1/n2*s0                    ! Snell's law
!!$       If(Real(st)>1._wp) Then
!!$          Write(unit=6,fmt=*) 'Total reflection at : ',param%Numerics%Energy(ienergy),' eV !'
!!$          Write(unit=6,fmt=*) '(Subroutine : diff_tran_coef_invariants)'
!!$       Endif
!!$       ct        = Sqrt(1._wp - st**2)
!!$       I         = Invar(ienergy)
!!$       ! Omega over c (dimensionless)
!!$       ooc       = param%Island%Radius * param%Numerics%Energy(ienergy) / hc  !/ (HBAR*Speed_of_Light)
!!$
!!$       !NIC    Select Case(TRAL(param%Source%Polarization))
!!$       Select Case(TRIM(param%Source%Polarization))
!!$
!!$       Case('p')
!!$          ! The Fresnel transmission coefficient
!!$          fresnel =  Real(4*n1*n2*c0*ct)/( Abs(n2*c0+n1*ct)**2 )  
!!$          !---------------------------------------------------------
!!$          !--- Calculates the transmission coefficient; Eq. (4.66)
!!$          !---------------------------------------------------------
!!$          prefactor     = exp(2*ooc*I%c*(n1*c0-n2*ct))
!!$          nominator     = 4*n1*n2*c0*ct
!!$          tmp(1)        = n2*c0+n1*ct
!!$          tmp(2)        = e1/e2*Abs(I%e)**2*s0**4-2*(Real(I%tau)-Real(I%delta_e)*&
!!$               e1*s0**2)*(e2*c0**2-e1*ct**2)
!!$          denominator   = tmp(1)**2-2*ooc*n1/n2*Aimag(I%e)*s0**2*tmp(1)+ooc**2*tmp(2)
!!$          ! The transmission coefficient for p-polarization
!!$          T             =  Real(prefactor * nominator / denominator)
!!$
!!$       Case('s')
!!$          ! The Fresnel transmission coefficient
!!$          fresnel =  Real(4*n1*n2*c0*ct)/( Abs(n1*c0+n2*ct)**2 )  
!!$          !---------------------------------------------------------
!!$          !--- Calculates the transmission coefficient; Eq. (4.60)
!!$          !---------------------------------------------------------
!!$          tmp(1)        = n1*c0+n2*ct
!!$          tmp(2)        = n1*c0-n2*ct
!!$          prefactor     = exp(2*ooc*I%c*tmp(2))
!!$          nominator     = 4*n1*n2*c0*ct
!!$          denominator   = tmp(1)**2-2*ooc**2*Real(I%tau)*(e2-e1)
!!$          ! The transmission coefficient for s-polarization
!!$          T             =  Real(prefactor * nominator / denominator)
!!$
!!$       End Select
!!$
!!$       Select Case(output)
!!$          ! The differential reflection
!!$       Case('DT')
!!$          Fresnel_coeff(ienergy) =   T/fresnel -1._wp 
!!$          ! The reflection coefficient
!!$       Case('T')
!!$          Fresnel_coeff(ienergy) =   T
!!$       End Select
!!$
!!$    Enddo
!!$
!!$  End Subroutine diff_tran_coef_invariants
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine Absorp_coef_invariants(Fresnel_coeff,Invar)
!!$
!!$    ! Calculates the Absorption from knowledge of the
!!$    ! invariants.
!!$    ! This method is described in detail in Bedeaux book
!!$    ! See in particular Eq. (4.61) (4.67)
!!$    Implicit None
!!$    Real(wp)             :: Fresnel_coeff(:)
!!$    Type(Invariants_type):: Invar(:), I
!!$    ! Local
!!$    integer              :: ienergy
!!$    Real(wp)             :: c0,s0,ooc,hc
!!$    Real(wp)             :: A
!!$    complex(wp )         :: prefactor,nominator,denominator,tmp(2)
!!$    complex(wp )         :: ct, st, n2, n1, e2, e1 
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt=*) '----------------------------------------------------'
!!$    Write(unit=6,fmt=*) ' Warning (Subroutine : Absorp_coef_invariants) !'
!!$    Write(unit=6,fmt=*) ' The Absorption coefficient is Only valid for'
!!$    Write(unit=6,fmt=*) ' non-Absorbing substrate above the total reflection'
!!$    Write(unit=6,fmt=*) '----------------------------------------------------'
!!$
!!$    ! Some energy independent abbreviations
!!$    e1 = Real(eps_vacuum)
!!$    n1 = Sqrt(e1)
!!$    c0 = Cos(param%Source%Theta0)
!!$    s0 = Sin(param%Source%Theta0)
!!$    hc = param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$
!!$       ! Some energy dependent abbreviations
!!$       e2        = param%Materials%Epsilon%Substrate_file(ienergy)   !   Misc%Eps_Substrate_file(ienergy)
!!$       n2        = Sqrt(e2) 
!!$       st        = n1/n2*s0                    ! Snell's law
!!$       If(Real(st)>1._wp) Then
!!$          Write(unit=6,fmt=*) 'Total reflection at : ',param%Numerics%Energy(ienergy),' eV !'
!!$          Write(unit=6,fmt=*) '(Subroutine : Absorp_coef_invariants)'
!!$       Endif
!!$       ct        = Sqrt(1._wp - st**2)
!!$       I         = Invar(ienergy)
!!$       ! Omega over c (dimensionless)
!!$       ooc       = param%Island%Radius * param%Numerics%Energy(ienergy) /  hc  !/ (HBAR*Speed_Of_Light)
!!$
!!$       !NICK   Select Case(TRAL(param%Source%Polarization))
!!$       Select Case(TRIM(param%Source%Polarization))
!!$
!!$       Case('p')
!!$          !---------------------------------------------------------
!!$          !--- Calculates the Absorption coefficient; Eq. (4.67) 
!!$          !---------------------------------------------------------
!!$          prefactor   = exp(2*ooc*I%c*(n1*c0-n2*ct))
!!$          nominator   = 4*ooc*n1*c0* &
!!$               ( I%c*(e2-e1)*(s0**2+ct**2)-Aimag(I%e)*s0**2+2*ooc/n2*s0**2*ct**2* &
!!$               (I%c**2*(e2-e1)**2-I%c*Aimag(I%e)*(e2-e1)) )
!!$          tmp(1)                = n2*c0+n1*ct
!!$          tmp(2)                = e1/e2*Abs(I%e)**2*s0**4-2*(Real(I%tau)-Real(I%delta_e)*&
!!$               e1*s0**2)*(e2*c0**2-e1*ct**2)
!!$          denominator = tmp(1)**2-2*ooc*n1/n2*Aimag(I%e)*s0**2*tmp(1)+ooc**2*tmp(2)
!!$          A         =  prefactor * nominator / denominator
!!$
!!$       Case('s')
!!$          !---------------------------------------------------------
!!$          !--- Calculates the Absorption coefficient; Eq. (4.61) 
!!$          !---------------------------------------------------------
!!$          tmp(1)          = n1*c0-n2*ct
!!$          tmp(2)          = ooc*Real(I%tau)*(e2-e1)
!!$          prefactor       = exp(2*ooc*I%c*tmp(1))
!!$          nominator       = 4*n1*c0*tmp(2)
!!$          denominator     = tmp(1)**2-2*ooc*tmp(2)
!!$          A               = prefactor * nominator / denominator
!!$
!!$       End Select
!!$
!!$       Fresnel_coeff(ienergy) = A
!!$
!!$    Enddo
!!$
!!$  End Subroutine Absorp_coef_invariants
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine diff_ref_coef_aspnes(Fresnel_coeff,surf_const_coef) 
!!$    ! Calculates the differential reflectivity from knowledge of the
!!$    ! surafce constitutive coefficients.
!!$    ! This method is described in the article of Yves Borensztein
!!$    ! Thin Solid Films 125 (1985) 129   6-7
!!$    Implicit None
!!$    Real(wp)                            :: Fresnel_coeff(:)
!!$    Type(surface_constitutive_type)     :: surf_const_coef(:),chi
!!$    ! Local 
!!$    integer                             :: ienergy
!!$    Real(wp)                            :: c0,s0,ooc,hbar,Speed_of_Light
!!$    complex(wp )                        :: e1,e2,n1,y1,y2,prefactor
!!$
!!$
!!$    ! Some energy independent Parameters
!!$    e1   = eps_vacuum
!!$    n1   = Sqrt(e1)
!!$    c0   = Cos(param%Source%Theta0)
!!$    s0   = Sin(param%Source%Theta0)
!!$    hbar           = param%PhysConst%Planck_over_2_pi 
!!$    Speed_of_Light = param%PhysConst%Speed_of_Light
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Some energy dependent abbreviations
!!$       e2        =  param%Materials%Epsilon%Substrate_file(ienergy)   ! Misc%Eps_Substrate_file(ienergy)
!!$       ! 1/lambda dimensionless
!!$       ooc                =   param%Island%Radius * param%Numerics%Energy(ienergy) / &
!!$            (2*pi * HBAR * Speed_Of_Light)
!!$       chi                =  surf_const_coef(ienergy)
!!$       prefactor =  8*pi*c0*n1*ooc
!!$       ! Differential reflectivity
!!$       !NICK   Select Case(TRAL(param%Source%Polarization))
!!$       Select Case(TRIM(param%Source%Polarization))
!!$       Case('s')
!!$          Fresnel_coeff(ienergy) =  Aimag(prefactor*chi%gamma/(e2-e1))
!!$       Case('p')
!!$          y1 = (e2-e1*s0**2)*chi%gamma - e2**2*e1*s0**2*chi%beta
!!$          y2 = (e1-e2)*(e1*s0**2 - e2*c0**2)
!!$          Fresnel_coeff(ienergy) =  Aimag(prefactor*y1/y2) 
!!$       End Select
!!$    Enddo
!!$
!!$  End Subroutine diff_ref_coef_aspnes
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine EELS_Loss_Function(Loss,surf_const_coef)
!!$
!!$    ! Computes the Energy Electron Cross Section Remi 15/07/01
!!$    Use SFL_Logical_Units, only : SFL_Get_Free_Unit
!!$    Implicit None
!!$    Real(wp)                           ::      Loss(:)
!!$    Type(surface_constitutive_type)    ::      surf_const_coef(:)
!!$    ! Local
!!$    Real(wp)                           ::      e1
!!$    complex(wp )                       ::      e2,gamma,beta
!!$    integer                            ::      ienergy, io_unit
!!$    Real(wp)                           ::      energy,kpar,E,tmp,tmp0
!!$    Real(wp)                           ::      cost,sint,vpar,vper, term(3) 
!!$    Logical                            ::      lfile
!!$
!!$    Write(unit=6,fmt=*)
!!$20  Write(unit=6,fmt='(a\)') ' Enter the energy of the incident beam E(eV) : '
!!$    Read(unit=5,fmt=*,err=20) E
!!$30  Write(unit=6,fmt='(a\)') ' Enter the parallel momentum transfer kpar(nm-1) : '
!!$    Read(unit=5,fmt=*,err=30) kpar
!!$40  Write(unit=6,fmt='(a\)') ' File for various terms in EELS cross-section (T-F) ? : '
!!$    Read(unit=5,fmt=*,err=40) lfile
!!$
!!$
!!$    ! some abbreviations
!!$    e1    = eps_vacuum
!!$    cost  = cos(param%Source%Theta0)
!!$    sint  = sin(param%Source%Theta0)
!!$    vpar  = Sqrt(2*E/param%PhysConst%Mass_Electron)*sint*param%PhysConst%Planck_over_2_pi
!!$    vper  = Sqrt(2*E/param%PhysConst%Mass_Electron)*cost*param%PhysConst%Planck_over_2_pi
!!$    tmp0  = cost*E**2*kpar ! forget the constant term *1e9*8/(pi**2)
!!$
!!$    ! Open the file If necessary
!!$    If(lfile) Then 
!!$       call SFL_Get_Free_Unit( io_unit )
!!$       Open(unit=io_unit,file='EELS.dat',status='unknown')
!!$       Write(unit=io_unit,fmt='(a)') '# EELS cross section'
!!$       Write(unit=io_unit,fmt='(a,10a20)') '#', 'Energy(eV)', 'TOTAL LOSSES', 'PREFACTOR', &
!!$            'SUBSTRATE TERM','BETA TERM','GAMMA TERM','Im(BETA)','Im(GAMMA)', 'Real(BETA)','Real(GAMMA)'
!!$    Endif
!!$
!!$    Do ienergy=1,param%Numerics%NEnergy          
!!$       e2       = param%Materials%Epsilon%Substrate_file(ienergy)   !     Misc%Eps_Substrate_file(ienergy)
!!$       energy   = param%Numerics%Energy(ienergy)
!!$       gamma    =       surf_const_coef(ienergy)%gamma
!!$       beta     =       surf_const_coef(ienergy)%beta
!!$       tmp      =       tmp0/( (vper**2*kpar**2+ (kpar*vpar - energy)**2)**2 )
!!$       ! Do not forget to convert in unit of the radius
!!$       term(1)  = -Aimag(1._wp/(e1+e2))*( 1._wp - kpar*param%Island%Radius*Real( (gamma-beta*e1*e2)/(e1+e2),wp ) )
!!$       term(2)  = kpar*param%Island%Radius*Aimag(beta)*Abs(e2/(e1+e2))**2
!!$       term(3)  = kpar*param%Island%Radius*Aimag(gamma)*Abs(1._wp/(e1+e2))**2
!!$       ! Multiply by the prefactor
!!$       term(:)  = term(:) * tmp
!!$       ! The total enrgy loss
!!$       Loss(ienergy) = term(1) + term(2) + term(3)
!!$       If(lfile) Then
!!$          Write(unit=io_unit,fmt='(a,10e20.10)') ' ',energy, Loss(ienergy), tmp, &
!!$               term(1), term(2), term(3) , Aimag(beta), Aimag(gamma), Real(beta,wp), Real(gamma,wp)
!!$       Endif
!!$    Enddo
!!$
!!$    If(lfile) Close(unit=io_unit,status='keep')
!!$
!!$  End Subroutine EELS_Loss_Function
!!$
!!$
!!$
!!$  !--------------------------------------------------------------------------------------------
!!$
!!$
!!$End Module Optics_mod
