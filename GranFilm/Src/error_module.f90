! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     Handles the error reporting of GranFilm
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!------------------!
Module Error_Module
!-------------------!


  ! -------------------------------------------------
  ! --- The Publicly avaiable functions and routines
  ! -------------------------------------------------
  Public :: Error_Initialize,        &
            Error_Fatal,             &
            Error_Failure,           &
            Error_Warning


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private




!-------!
Contains
!-------!
  

  !------------------------------------!
  Subroutine Error_Initialize()
  !------------------------------------!
    Use SFL_Error_Handling, only : SFL_Error_Init, SFL_Error_Type  
    Implicit None
    Type(SFL_Error_Type) :: error
    
    ! --- Initializing with default parameters
    call SFL_Error_Init(error)
    
  End Subroutine Error_Initialize
  !---------------------------------------!


  

  !-----------------------------------------!
  Subroutine Error_Fatal( routine, message )
  !-----------------------------------------!
    Use SFL_Error_Handling, only : SFL_Error_Fatal       
    Implicit None
    Character(len=*),  Intent(in)   :: routine
    Character(len=*),  Intent(in)   :: message
    ! --- Local
    Character(len=500)  :: local_message

    local_message = Common_Error_String() // trim(adjustl(message))

    call SFL_Error_Fatal( "GRANFILM : "//Trim(routine), local_message )
    
  End Subroutine Error_Fatal
  !-----------------------------------------!



  !-------------------------------------------!
  Subroutine Error_Failure( routine, message )
  !-------------------------------------------!
    Use SFL_Error_Handling, only : SFL_Error_Failure
    Implicit None
    Character(len=*),  Intent(in)   :: routine
    Character(len=*),  Intent(in)   :: message
    ! --- Local
    Character(len=500)  :: local_message

    local_message = Common_Error_String() // trim(adjustl(message))

    call SFL_Error_Failure( "GRANFILM : "//Trim(routine), local_message )

  End Subroutine Error_Failure
  !------------------------------------------!




  !-------------------------------------------!
  Subroutine Error_Warning( routine, message )
  !-------------------------------------------!
    Use SFL_Error_Handling, only : SFL_Error_Warning
    Implicit None
    Character(len=*),  Intent(in)   :: routine
    Character(len=*),  Intent(in)   :: message
    ! --- Local
    Character(len=500)  :: local_message

    local_message = Common_Error_String() // trim(adjustl(message))
    call Error_Initialize()
    call SFL_Error_Warning( "GRANFILM : "//Trim(routine), local_message )

  End Subroutine Error_Warning
  !-------------------------------------------!



  !-----------------------------------------!
  Function Common_Error_String() Result(res)
  !-----------------------------------------!
    Implicit None
    Character(len=21) :: res
    
    !res = "GRANFILM reported : "
    res = ""
    return

  End Function Common_Error_String
  !-----------------------------------------!





End Module Error_Module
!----------------------!
