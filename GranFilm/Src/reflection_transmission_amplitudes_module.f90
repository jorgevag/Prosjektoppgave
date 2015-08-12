! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!        Calculating the reflection and/or the transmission 
!        amplitudes for the film.
!
!        Some service routines are also provided.
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!----------------------------------!
Module Ref_Trans_Amplitudes_Module
!----------------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp, param, Results	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Reflection_Transmission_Amplitudes
  Public :: Get_Amplitude
  Public :: Get_Fresnel_Amplitude
  !Public :: Fresnel_Reflection_Amplitude
  !Public :: Fresnel_Transmission_Amplitude


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!


  !--------------------------------------------------!
  Subroutine Get_Reflection_Transmission_Amplitudes()
  !--------------------------------------------------!
    Implicit None
    Character(len=*), parameter :: routine = "Get_Reflection_Transmission_Amplitudes"
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine



    ! --- Reflection amplitudes
    if ( allocated( Results%Reflection_Amplitudes) )  then
       call Get_Reflection_Amplitudes( )
    endif

    ! --- Transmission Amplitudes if (
    if ( allocated(Results%Transmission_Amplitudes) )  then
       call Get_Transmission_Amplitudes( )
    endif

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Get_Reflection_Transmission_Amplitudes
  !----------------------------------------------------!




  ! ============================
  ! === Local Routines =========
  ! ============================





  !------------------------------------------!
  Subroutine Get_Reflection_Amplitudes(  ) 
  !------------------------------------------!
    !
    ! PURPOSE   
    !      To calculate the reflection amplitues for the film, 
    !      as well as the transmission amplitude of the substrate.
    !      It is here assumed, but not checked, that the ambient
    !      medium is transparent
    ! 
    !
    ! NOTE    
    !      This routine has some overhead since it calculates the 
    !      the reflection amplitudes for bit p and spolarization
    !      even if only one of them will be needed later.
    !      However, the overhead shiuld not be significent.
    !
    !
    ! REFERENCE:
    !      The implemented formulaes are taken from
    !
    !          M.T Haarmans and D. Bedeuax, Thin Solid Films 224, 117 (1993).
    !      
    !          Equations (34) and (36)
    !
    !
    ! AUTHOR  : Ingve Simonsen, Paris, Jul 2010
    !
    !
    Use Shared,                     only : Physical_Constants, pi, imu
    Use Error_Module,               only : Error_Failure, Error_Warning
    Use Supported_Options_Module,   only : POLARIZATION_OPTION_TABEL
    Use Tools_Module,               only : deg2rad
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Reflection_Amplitudes"
    Integer      :: ienergy
    Real(wp)     :: ooc, oocR, hbar_over_c
    Real(wp)     :: cos_theta_0, sin_theta_0
    Complex(wp)  :: cos_theta_trans, sin_theta_trans 
    Complex(wp)  :: n1, n2, nominator, denominator
    Complex(wp)  :: ooc_gamma, ooc_beta, ooc_delta, ooc_tau
    Complex(wp)  :: term(3)

    ! --- TESTING
    !call Error_Warning( routine, "SHOULD Check is ambient is transparent!" )
    !call Error_Warning( routine, "CHECK THIS ROUTINE!" )
    !call Error_Warning( routine, "FIX the renomalization of the susceptibilities!" )
    ! --- TESTING


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some error checking
    if ( .not.allocated( Results%Susceptibilities ) )   then
       call Error_Failure( routine, "Internal allocation error!" )
    end if

    if ( size(Results%Susceptibilities,1) /= size(Results%Reflection_Amplitudes,1) ) then
       call Error_Failure( routine, "Dimensions are not consistent!" )
    end if
    

    
    ! --- Calculats the Reflection amplitudes for the differetne polarizations
    ! ---------------------------------------------------------------------------

    ! --- Some common energy independet constants
    cos_theta_0 = Cos( param%Source%Theta0 * deg2rad() )
    sin_theta_0 = Sin( param%Source%Theta0 * deg2rad() )
    hbar_over_c = Physical_Constants%Planck_Const_over_2_pi * Physical_Constants%Speed_of_Light_in_vacuum


    ! --- The energy loop
    energy_loop : do ienergy = 1, size(Results%Susceptibilities,1) 
       
       
       ! --- Refractive indices for ambient (index 1) and substrate (index 2)
       n1    =   sqrt( param% Media(Param%Source%iambient)   % Epsilon(ienergy) )
       n2    =   sqrt( param% Media(Param%Source%isubstrate) % Epsilon(ienergy) )
       
       ! --- w/c in dimensionless units
       !ooc   =  param%Island%Radius * param%Numerics%Energy(ienergy) / hbar_over_c
       ooc   =  param%Numerics%Energy(ienergy) / hbar_over_c
     

       ! --- Snells law used to determine cos/sin of the transmission angle
       sin_theta_trans   =  (n1/n2) * sin_theta_0
       cos_theta_trans   =  Sqrt(1._wp - sin_theta_trans**2) 


       ! --- Renormalization  
       !
       !     RECALL : Results%Susceptibilities(ienergy) are dimmensionless
       !              and scaled with proper powers of the outer radius 
       !              (and the amplitude of the electric field)
       !  
       !              First order suscept. (gamma and beta) are scaled with the out R
       !              Second order suscept. (delta and tau) are scaled with the out R**2
       !
       oocR       =   ooc * Param%Geometry%Radius(1)  ! in order to renomalize
       ! --- Susceptebilities
       ooc_gamma  =   oocR * Results%Susceptibilities( ienergy )% gamma   
       ooc_beta   =   oocR * Results%Susceptibilities( ienergy )% beta    


       !
       ! ----------------------
       ! --- S-polarization
       ! ----------------------
       ! 
       ! --- Common factors
       nominator    =   n1 * cos_theta_0  -  n2 * cos_theta_trans
       denominator  =   n1 * cos_theta_0  +  n2 * cos_theta_trans
       !
       ! --- Fresnel amplitude for the substrate
       Results%Reflection_Amplitudes(ienergy)%Fresnel_S  =   nominator / denominator 
       !
       ! --- Reflection amplitude for the film
       ! ----------------------------------------------------------
       !     Reference : Thin Solid Films 224, 117 (1993), Eq. (34)
       Results%Reflection_Amplitudes(ienergy)%S            &
            =    ( nominator    +  imu * ooc_gamma   )     &
              /  ( denominator  -  imu * ooc_gamma   )         

      !    tmp(1)         =  imu*ooc 
      !    factor         =  (/ n1*c0 + n2*ct , n1*c0 - n2*ct /) 
      !    fresnel        =  factor(2)/factor(1) 
      !    Reflec         =  ( factor(2) + tmp(1)*chi%gamma )   /   &
      !         ( factor(1) - tmp(1)*chi%gamma )  


       !        
       ! ----------------------
       ! --- P-polarization
       ! ----------------------
       !
       ! --- Common factors
       nominator    =   n2 * cos_theta_0  -  n1 * cos_theta_trans
       denominator  =   n2 * cos_theta_0  +  n1 * cos_theta_trans
       !
       ! --- Fresnel amplitude for the substrate
       Results%Reflection_Amplitudes(ienergy)%Fresnel_P  =   nominator / denominator 
       !
       ! --- Reflection amplitude for the film
       ! ----------------------------------------------------------
       !     Reference : Thin Solid Films 224, 117 (1993), Eq. (36)
       term(1)      =  1._wp - 0.25_wp * (n1**2) * ooc_gamma * ooc_beta * sin_theta_0**2 
       term(2)      =  imu *  ooc_gamma * cos_theta_0 * cos_theta_trans 
       term(3)      =  imu * (n1**3) * n2 * ooc_beta * sin_theta_0**2
       Results%Reflection_Amplitudes(ienergy)%P                   &
            =    ( nominator   * term(1) - term(2) + term(3) )    &
              /  ( denominator * term(1) - term(2) - term(3) )     

!          tmp(1)         =  imu*ooc                                              
!          tmp(2)         =  -tmp(1)**2/4._wp*e1*chi%beta*chi%gamma*s0**2         
!          tmp(3)         =  chi%gamma*c0*ct + n1*n2*chi%beta*e1*s0**2            
!          tmp(4)         =  chi%gamma*c0*ct - n1*n2*chi%beta*e1*s0**2    
!          factor         =  (/ n2*c0 + n1*ct , n2*c0 - n1*ct /) 
!          fresnel        =  factor(2)/factor(1)  
!          Reflec         =  (factor(2) - tmp(1)*tmp(4) -tmp(2)*factor(2))/   &
!               (factor(1) -tmp(1)*tmp(3) -tmp(2)*factor(1))

    enddo energy_loop

    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
    

  End Subroutine Get_Reflection_Amplitudes
  !-----------------------------------------!







  !------------------------------------------!
  Subroutine Get_Transmission_Amplitudes(  ) 
  !------------------------------------------!
    !
    ! PURPOSE   
    !      To calculate the transmission amplitues for the film
    !      as well as the fresnel transmission amplitude.
    !      It is here assumed, but not checked, that the ambient
    !      medium is transparent
    ! 
    !
    ! NOTE    
    !      This routine has some overhead since it calculates the 
    !      the reflection amplitudes for bit p and spolarization
    !      even if only one of them will be needed later.
    !      However, the overhead shiuld not be significent.
    !
    !
    ! REFERENCE:
    !      The implemented formulaes are taken from
    !
    !          M.T Haarmans and D. Bedeuax, Thin Solid Films 224, 117 (1993).
    !      
    !          Equations (35) and (37)
    !
    !
    ! AUTHOR  : Ingve Simonsen, Paris, Jul 2010
    !
    !
    Use Shared,                     only : Physical_Constants, pi, imu
    Use Error_Module,               only : Error_Failure, Error_Warning
    Use Supported_Options_Module,   only : POLARIZATION_OPTION_TABEL
    Use Tools_Module,               only : deg2rad
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Transmission_Amplitudes"
    Integer      :: ienergy
    Real(wp)     :: ooc, oocR, hbar_over_c
    Real(wp)     :: cos_theta_0, sin_theta_0
    Complex(wp)  :: cos_theta_trans, sin_theta_trans 
    Complex(wp)  :: n1, n2, nominator, denominator
    Complex(wp)  :: ooc_gamma, ooc_beta, ooc_delta, ooc_tau
    Complex(wp)  :: term(3)

    ! --- TESTING
    !call Error_Warning( routine, "SHOULD Check if the substarete is transparent!" )
    !call Error_Warning( routine, "CHECK THIS ROUTINE!" )
    !call Error_Warning( routine, "FIX the renomalization of the susceptibilities!" )
    ! --- TESTING


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some error checking
    if ( .not.allocated( Results%Susceptibilities ) )   then
       call Error_Failure( routine, "Internal allocation error!" )
    end if

    if ( size(Results%Susceptibilities,1) /= size(Results%Reflection_Amplitudes,1) ) then
       call Error_Failure( routine, "Dimensions are not consistent!" )
    end if
    

    
    ! --- Calculats the Reflection amplitudes for the differetne polarizations
    ! ---------------------------------------------------------------------------

    ! --- Some common energy independet constants
    cos_theta_0 = Cos( param%Source%Theta0 * deg2rad() )
    sin_theta_0 = Sin( param%Source%Theta0 * deg2rad() )
    hbar_over_c =  Physical_Constants%Planck_Const_over_2_pi * Physical_Constants%Speed_of_Light_in_vacuum


    ! --- The energy loop
    energy_loop : do ienergy = 1, size(Results%Susceptibilities,1) 
       
       
       ! --- Refractive indices for ambient and substrate 
       n1    =   sqrt( param% Media(Param%Source%iambient)   % Epsilon(ienergy) )
       n2    =   sqrt( param% Media(Param%Source%isubstrate) % Epsilon(ienergy) )
       
       ! --- w/c in dimensionless units
       !ooc   =  param%Island%Radius * param%Numerics%Energy(ienergy) / hbar_over_c
       ooc   =  param%Numerics%Energy(ienergy) / hbar_over_c
     

       ! --- Snells law used to determine cos/sin of the transmission angle
       sin_theta_trans   =  (n1/n2) * sin_theta_0
       cos_theta_trans   =  Sqrt(1._wp - sin_theta_trans**2) 


       ! --- Renormalization  
       !
       !     RECALL : Results%Susceptibilities(ienergy) are dimmensionless
       !              and scaled with proper powers of the outer radius
       !              (and the amplitude of the electric field)
       !  
       !              First order suscept. (gamma and beta) are scaled with the out R
       !              Second order suscept. (delta and tau) are scaled with the out R**2
       !
       oocR       =   ooc * Param%Geometry%Radius(1)  ! in order to renomalize
       ! --- Susceptebilities
       ooc_gamma  =   oocR * Results%Susceptibilities( ienergy )% gamma   
       ooc_beta   =   oocR * Results%Susceptibilities( ienergy )% beta    


       !
       ! ----------------------
       ! --- S-polarization
       ! ----------------------
       ! 
       ! --- Common factors
       nominator    =   2 * n1 * cos_theta_0  
       denominator  =   n1 * cos_theta_0  +  n2 * cos_theta_trans
       !
       ! --- Fresnel amplitude for the substrate
       Results%Transmission_Amplitudes(ienergy)%Fresnel_S  =   nominator / denominator 
       !
       ! --- Reflection amplitude for the film
       ! ----------------------------------------------------------
       !     Reference : Thin Solid Films 224, 117 (1993), Eq. (35)
       Results%Transmission_Amplitudes(ienergy)%S          &
            =    ( nominator                         )     &
              /  ( denominator  -  imu * ooc_gamma   )         


       !        
       ! ----------------------
       ! --- P-polarization
       ! ----------------------
       ! 
       ! --- Common factors
       nominator    =   2 * n1 * cos_theta_0
       denominator  =  n2 * cos_theta_0  +  n1 * cos_theta_trans
       !
       ! --- Fresnel amplitude for the substrate
       Results%Transmission_Amplitudes(ienergy)%Fresnel_P  =   nominator / denominator 
       !
       ! --- Reflection amplitude for the film
       ! ----------------------------------------------------------
       !     Reference : Thin Solid Films 224, 117 (1993), Eq. (37)
       term(1)      =  0.25_wp * (n1**2) * ooc_gamma * ooc_beta * sin_theta_0**2 
       term(2)      =  imu *  ooc_gamma * cos_theta_0 * cos_theta_trans 
       term(3)      =  imu * n1 * (n2**3) * ooc_beta * sin_theta_0**2
       Results%Transmission_Amplitudes(ienergy)%P                         &
            =    ( nominator   * (1._wp+term(1))                     )    &
              /  ( denominator * (1._wp-term(1)) - term(2) - term(3) )     


    enddo energy_loop


    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
    

  End Subroutine Get_Transmission_Amplitudes
  !-----------------------------------------!






  !----------------------------------------------------------------!
  Elemental Function Get_Amplitude( Data, polarization ) Result(res)
  !----------------------------------------------------------------!
    !
    ! --- Service routine for returning the reflection of transmission
    !     amplitudes for a a given polarization.
    !
    !     By using such a fucntion, one may realativly easily adjust
    !     the internal storage scheme.
    !
    !     Ingve Simonsen, Paris, Jul, 2010
    !
    Use Error_Module,               only : Error_Failure
    Use Supported_Options_Module,   only : POLARIZATION_OPTION_TABEL, List_Supported_Options 
    Use Derived_Type_Module,        only : Ref_Trans_Amp_Type
    Implicit None
    Type(Ref_Trans_Amp_Type), Intent(In)  :: Data
    Character(len=*),         Intent(In)  :: polarization
    Complex(wp)                           :: res 
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Amplitude"


    ! -- Copy the Fresnel Relfection amplitude for given polarization
    select Case( Polarization )
       
    Case( POLARIZATION_OPTION_TABEL(1) )
       ! --- P-polarization
       Res = Data%P

    Case( POLARIZATION_OPTION_TABEL(2) )
       ! --- S-polarization
       Res = Data%S

    Case( POLARIZATION_OPTION_TABEL(3), POLARIZATION_OPTION_TABEL(4) )
       ! --- PS-polarization (aka unpolarized)
       Res = 0.5_wp * ( Data%P + Data%S )       
 
    !Case Default
    !   ! This should never happen
    !   call List_Supported_Options( Polarization, POLARIZATION_OPTION_TABEL, quit=.true. )

    End select

  End Function Get_Amplitude
  !--------------------------------------------!



  !----------------------------------------------------------------!
  Elemental Function Get_Fresnel_Amplitude( Data, polarization ) Result(res)
  !----------------------------------------------------------------!
    !
    ! --- Service routine for returning the Frenel reflection of
    !     transmission amplitudes for a a given polarization.
    !
    !     By using such a fucntion, one may realativly easily adjust
    !     the internal storage scheme.
    !
    !     Ingve Simonsen, Paris, Jul, 2010
    !
    Use Error_Module,               only : Error_Failure
    Use Supported_Options_Module,   only : POLARIZATION_OPTION_TABEL, List_Supported_Options 
    Use Derived_Type_Module,        only : Ref_Trans_Amp_Type
    Implicit None
    Type(Ref_Trans_Amp_Type), Intent(In)  :: Data
    Character(len=*),         Intent(In)  :: polarization
    Complex(wp)                           :: res 
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Fresnel_Amplitude"


    ! -- Copy the Fresnel Relfection amplitude for given polarization
    select Case( Polarization )
       
    Case( Trim(adjustl(POLARIZATION_OPTION_TABEL(1))) )
       ! --- P-polarization
       Res = Data%Fresnel_P

    Case( POLARIZATION_OPTION_TABEL(2) )
       ! --- S-polarization
       Res = Data%Fresnel_S
       
    Case( POLARIZATION_OPTION_TABEL(3), POLARIZATION_OPTION_TABEL(4) )
       ! --- PS-polarization (aka unpolarized)
       Res = 0.5_wp * ( Data%Fresnel_P + Data%Fresnel_S )       
 
    !Case Default
    !   ! This should never happen
    !   call List_Supported_Options( Polarization, POLARIZATION_OPTION_TABEL, quit=.true. )

    End select
    

  End Function Get_Fresnel_Amplitude
  !--------------------------------------------!




!!$
!!$  !----------------------------------------------------------------!
!!$  Function Fresnel_Reflection_Amplitude( polarization ) Result(res)
!!$  !----------------------------------------------------------------!
!!$    !
!!$    ! --- Service routine for returning the Fresnel refelction
!!$    !     amplitude for the flat substrate (i.e. without the island
!!$    !     film).
!!$    !
!!$    !     Ingve Simonsen, Paris, Jul, 2010
!!$    !
!!$    Use Error_Module,               only : Error_Failure
!!$    Use Supported_Options_Module,   only : POLARIZATION_OPTION_TABEL, List_Supported_Options 
!!$    Implicit None
!!$    Character(len=*), Intent(In) :: polarization
!!$    Complex(wp), dimension( size(Param%Numerics%Energy,1) )  :: res 
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Fresnel_Refelection_Amplitude"
!!$
!!$
!!$    ! --- Error checking
!!$    if ( .not. allocated( Results%Reflection_Amplitudes ) ) then
!!$       call Error_Failure( routine, & 
!!$             "Internal Failure (Results%Reflection_Amplitudes not allocated)!" )
!!$    endif
!!$
!!$
!!$    ! -- Copy the Fresnel Relfection amplitude for given polarization
!!$    select Case( Polarization )
!!$       
!!$    Case( POLARIZATION_OPTION_TABEL(1) )
!!$       ! --- P-polarization
!!$       Res = Results%Reflection_Amplitudes(:)%Fresnel_P
!!$
!!$    Case( POLARIZATION_OPTION_TABEL(2) )
!!$       ! --- S-polarization
!!$       Res = Results%Reflection_Amplitudes(:)%Fresnel_S
!!$
!!$    Case( POLARIZATION_OPTION_TABEL(3), POLARIZATION_OPTION_TABEL(4) )
!!$       ! --- PS-polarization (aka unpolarized)
!!$       Res = Results%Reflection_Amplitudes(:)%Fresnel_P + Results%Reflection_Amplitudes(:)%Fresnel_S       
!!$       Res = Res * 0.5_wp
!!$
!!$    Case Default
!!$       ! This should never happen
!!$       call List_Supported_Options( Polarization, POLARIZATION_OPTION_TABEL, quit=.true. )
!!$
!!$    End select
!!$
!!$  End Function Fresnel_Reflection_Amplitude
!!$  !--------------------------------------------!
!!$
!!$
!!$
!!$  !----------------------------------------------------------------!
!!$  Function Fresnel_Transmission_Amplitude( polarization ) Result(res)
!!$  !----------------------------------------------------------------!
!!$    !
!!$    ! --- Service routine for returning the Fresnel Transmission
!!$    !     amplitude for the flat substrate (i.e. without the island
!!$    !     film).
!!$    !
!!$    !     Ingve Simonsen, Paris, Jul, 2010
!!$    !
!!$    Use Error_Module,               only : Error_Failure
!!$    Use Supported_Options_Module,   only : POLARIZATION_OPTION_TABEL, List_Supported_Options 
!!$    Implicit None
!!$    Character(len=*), Intent(In) :: polarization
!!$    Complex(wp), dimension( size(Param%Numerics%Energy,1) )  :: res 
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Fresnel_Transmission_Amplitude"
!!$
!!$
!!$    ! --- Error checking
!!$    if ( .not. allocated( Results%Transmission_Amplitudes ) ) then
!!$       call Error_Failure( routine, & 
!!$             "Internal Failure (Results%Trnasmission_Amplitudes not allocated)!" )
!!$    endif
!!$
!!$
!!$    ! -- Copy the Fresnel Relfection amplitude for given polarization
!!$    select Case( Polarization )
!!$       
!!$    Case( POLARIZATION_OPTION_TABEL(1) )
!!$       ! --- P-polarization
!!$       Res = Results%Transmission_Amplitudes(:)%Fresnel_P
!!$
!!$    Case( POLARIZATION_OPTION_TABEL(2) )
!!$       ! --- S-polarization
!!$       Res = Results%Transmission_Amplitudes(:)%Fresnel_S
!!$
!!$    Case( POLARIZATION_OPTION_TABEL(3), POLARIZATION_OPTION_TABEL(4) )
!!$       ! --- PS-polarization (aka unpolarized)
!!$       Res = Results%Transmission_Amplitudes(:)%Fresnel_P + Results%Transmission_Amplitudes(:)%Fresnel_S       
!!$       Res = Res * 0.5_wp
!!$
!!$    Case Default
!!$       ! This should never happen
!!$       call List_Supported_Options( Polarization, POLARIZATION_OPTION_TABEL, quit=.true. )
!!$
!!$    End select
!!$
!!$  End Function Fresnel_Transmission_Amplitude
!!$  !--------------------------------------------!



  

End Module Ref_Trans_Amplitudes_Module
!--------------------------------------------------!

























!!$
!!$
!!$    
!!$    ! --- Calculats the Reflection amplitudes for the differetne polarizations
!!$    select case ( trim(adjustl( Param % Source % Polarization )) ) 
!!$                                                
!!$    case( POLARIZATION_OPTION_TABEL(1) )
!!$       ! --------------------------
!!$       ! --- p-polarization
!!$       ! --------------------------
!!$       call Reflection_Amplitude_P() 
!!$       ! --- the other polarization
!!$       Results % Relfection_Amplitudes (:, Results%S_pol ) =   0._wp
!!$
!!$    case( POLARIZATION_OPTION_TABEL(2) )
!!$       ! ---------------------
!!$       ! --- s-polarization
!!$       ! ---------------------
!!$       call Reflection_Amplitude_S()
!!$       ! --- the other polarization 
!!$       Results % Relfection_Amplitudes (:, Results%P_pol ) =   0._wp
!!$
!!$    case( POLARIZATION_OPTION_TABEL(3), POLARIZATION_OPTION_TABEL(4) )
!!$       ! --- ps or sp (i.e. unpolarized)
!!$       call Reflection_Amplitude_P() 
!!$       call Reflection_Amplitude_S() 
!!$
!!$    case Default
!!$       ! --- Should ever happen
!!$       call Error_Failure( routine, "Polarization not supporte!" )          
!!$    end select
!!$    
!!$    
!!$    ! --- If verbose
!!$    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
!!$
!!$  !-------!  
!!$  Contains
!!$  !-------!  
!!$
!!$    !----------------------------------!
!!$    Subroutine Reflection_Amplitude_P() 
!!$    !----------------------------------!
!!$      ! --- P-polarization
!!$      Implicit None
!!$      Integer :: ienergy
!!$      ! --- Loop over energy
!!$      do ienergy = 1, size(Results%Susceptibilities,1)
!!$         call get_common_factors()
!!$         
!!$         ! --- Set the p-pol refelection amplitude 
!!$         Results % Relfection_Amplitudes ( Results%P_pol ) =
!!$         
!!$      enddo
!!$
!!$      
!!$    End Subroutine Reflection_Amplitude_P
!!$    !-------------------------------------!
!!$    
!!$
!!$    !----------------------------------!
!!$    Subroutine Reflection_Amplitude_S() 
!!$      ! --- P-polarization
!!$      Implicit None
!!$      
!!$      ! --- Loop over energy
!!$      
!!$      
!!$    End Subroutine Reflection_Amplitude_S
!!$
!!$    
!!$
!!$  End Subroutine Get_Reflection_Amplitudes
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
!!$
