
!-------------------------------!
Module Physical_Constants_Module  
!-------------------------------!


  ! --- The Use Statements global to this module
  Use Working_Precision, only : wp	   
  

 
  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Physical_Constant_Type, &
            Physical_Constants,     &
            Set_Physical_Constants 
            



  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private




  Type :: Physical_Constant_Type
     ! Set some needed physical constants...
     !Real(wp)   ::  epsilon_of_vacuum
     Real(wp)   ::  Planck_Const_over_2_pi     !=  6.582122E-16                               
     Real(wp)   ::  Speed_of_Light_in_vacuum   !=  2.99792458E17
     Real(wp)   ::  Electron_Mass              !=  0.511e6/(Speed_of_Light_in_vacuum**2)   
  End Type Physical_Constant_Type



  ! --- Declare the variable
  Type(Physical_Constant_Type) :: Physical_Constants


!-------!
Contains
!-------!


  Subroutine Set_Physical_Constants()
    ! 
    ! --- This routine should be called once in 
    !     the initialization phase
    !
    ! Ingve Simonsen, Paris Jul, 2010.
    Implicit None
    

    !Physical_Constants % epsilon_of_vacuum     

    ! --- Plancks constant over 2pi [eV s]
    Physical_Constants % Planck_Const_over_2_pi      &
         = real(6.582119282D-16,wp)                           

    ! --- Speed of light in vacuum [nm / s]
    Physical_Constants % Speed_of_Light_in_vacuum    &
         = real(2.99792458D17,wp)

    ! --- Mass of the electron [eV.nm-2.s2] 
    Physical_Constants % Electron_Mass               &
         = real(0.511D6,wp)/(Physical_Constants % Speed_of_Light_in_vacuum**2)  
  

  End Subroutine Set_Physical_Constants


End Module Physical_Constants_Module
!----------------------------------!
