! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     Contains the main data that should be easily accessable
!     throughout the code
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!------------!
Module Shared
!------------!


  ! ----------------------------
  ! --- The Use Statments
  ! ----------------------------
  !Use SFL_Precision,               only : sp,dp      ! Defines the default precission
  Use Working_Precision,               only : wp         ! Defines the working precission
  Use Physical_Constants_Module,       only : Physical_Constants
  Use Software_Info_Mod,               only : Software_Info_Type 
  Use Derived_Type_Module,             only : GranFilm_Parameter_Type, &
                                              GranFilm_Integral_Type,  &
                                              GranFilm_Results_Type
                                                  


!  Use GrandFilm_Error_Handling_Module, only : &
            
  ! --------------------------------------
  ! --- The Publicly avaiable variables
  ! --------------------------------------
  Public :: wp!, sp !, dp
  Public :: pi
  Public :: imu

  ! --- Shared derived types
  Public :: GranFilm_Results_Type
  Public :: GranFilm_Integral_Type

  ! --- Shared derived type variables
  Public :: Physical_Constants
  Public :: Software_Information  
  Public :: Param
  Public :: Integrals
  Public :: Results


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private






  ! --------------------------------------------------------
  ! --- Declare and define the Parameter variable to share
  ! --------------------------------------------------------
  !
  !Real(wp) :: pi = 4._wp*atan(1._wp)
  Real(wp),    parameter :: pi =3.14159265358979323846264338327950288419716939937510582097494459230781_wp
  Complex(wp), Parameter :: imu = (0._wp, 1._wp)



  Type(Software_Info_Type)              :: Software_Information

  ! --- The main parameter type 
  Type(GranFilm_Parameter_Type), save   :: Param

  ! --- Integral type
  !       index  : No. of interfaces (coatings)
  Type(GranFilm_Integral_Type),       &
          dimension(:), allocatable     :: Integrals   


  ! --- The main storage for the results of the simulation.....
  Type(GranFilm_Results_Type), save     :: Results

  






End Module Shared
!---------------!
