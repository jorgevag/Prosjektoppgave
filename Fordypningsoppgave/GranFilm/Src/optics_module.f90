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


!-------------------!
Module Optics_Module
!-------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Reflection_Coefficient
  Public :: Transmission_Coefficient


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!



  !-----------------------------------------------------------------!
  Elemental Function Reflection_Coefficient( Amplitude ) Result(Res)
  !-----------------------------------------------------------------!
    !
    !  This routine that calculates the reflection coefficient (or reflectance) 
    !  from the knowledge fof the corresponding amplitude.
    !  
    !  Strictly speaking it should be cheacked that the medium is transparent, 
    !  but this is not done here due to the reason of preformence.
    !
    Implicit None
    Complex(wp), Intent(In)  :: Amplitude
    Real(wp)                 :: Res
    ! --- Local
    Character(len=*), parameter :: routine = "Reflection_Coefficient"

    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    Res = abs(Amplitude)**2

    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Function Reflection_Coefficient
  !------------------------------------------------------------------!





  !-----------------------------------------------------------------!
  Elemental Function Transmission_Coefficient( Amplitude ,    &
       eps_ambient, eps_substrate )      Result(Res)
  !-----------------------------------------------------------------!  
    !
    !  This routine calculates the transmission coefficient (or transmittance) 
    !  from the knowledge fof the corresponding amplitude (and angle of incidence).
    !
    !  It is assumed that both the ambient and substarte are
    !  trasnparent (i.e. the imaginary part of their epsilon is zero
    !  or neglectable). This is, however, not chack in the routine (in
    !  order to not create too much overhead). In this routine we
    !  simply neglect the imaginary parts.
    !
    !  Moreover, it is assumed that the madia are non-magneti. if not
    !  the result has to be multiplied by the ratio
    !  mu_substrate/mu_ambient.
    !
    !  The angle of incidence needed in these expressions is taken from the shared
    !  paramters of this program
    !  
    !  
    !  Ingve Simonsen, Paris, Jun 2010
    !
    !
    Use Shared,        only : param
    Use Tools_Module,  only : deg2rad
    Implicit None
    Complex(wp), Intent(In)  :: Amplitude
    Complex(wp), Intent(In)  :: eps_ambient
    Complex(wp), Intent(In)  :: eps_substrate
    Real(wp)                 :: Res
    ! --- Local
    Character(len=*), parameter :: routine = "Transmission_Coefficient"
    Real(wp)     :: cos_theta_0, sin_theta_0
    Complex(wp)  :: cos_theta_trans, sin_theta_trans 
    Complex(wp)  :: n1, n2 
 
    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    ! --- Some common energy independet constants
    cos_theta_0 = cos( param%Source%Theta0 * deg2rad() )
    sin_theta_0 = sin( param%Source%Theta0 * deg2rad() )
       
    ! --- Refractive indices for ambient and substrate 
    n1    =   sqrt( eps_ambient    )
    n2    =   sqrt( eps_substrate  )
  
    ! --- Snells law used to determine cos/sin of the transmission angle
    sin_theta_trans   =  ( n1 / n2 ) * sin_theta_0

    ! --- Check if transmission is allowed from the ambient into the substrate
    !     under these conditions.
    if ( (abs(aimag(sin_theta_trans))>0.001_wp) .or. (abs(Real(sin_theta_trans))>1._wp) ) then
       Res = 0._wp
       Return
    Else
       cos_theta_trans   =  Sqrt(1._wp - sin_theta_trans**2) 
       Res = abs(Amplitude)**2
       Res =  Res * Real ( n2 * cos_theta_trans / ( n1 * cos_theta_0 ) , wp )
    Endif

    ! --- If verbose
    !If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Function Transmission_Coefficient
  !------------------------------------------------------------------!





End Module Optics_Module
!-----------------------!
