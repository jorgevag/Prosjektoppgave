!----------------------------------!
Module GranFilm_Supported_Options
!----------------------------------!
  !
  ! PURPOSE 
  !   Defining and setting the options used in the code
  !
  ! AUTHOR
  !   Ingve Simonsen, Trondheim, Dec. 21.12.2013
  !
  ! --------------------------------------------------------



  ! --- The Use Statements global to the module
  Use Shared, only : Param, Software_Information


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: GranFilm_Option_T
  

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private 



  

  Type GranFilm_Option_T
     character(len=20)  :: options
     character(len=80)  :: text
     integer            :: narg = 0
  End type GranFilm_Option_T

  
  Type(GranFilm_Option_T), dimension(5) :: GF_Option_Table


  
!-------!  
contains
!-------!


  Subroutine GF_set_option_Table


  End Module GranFilm_Options
!----------------------------------!
