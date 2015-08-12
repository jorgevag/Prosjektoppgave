Module F2003_Wrapper_Mod

  ! --------------------------------------------------------
  ! --- This Module defefines some of the new intrinsic 
  !     function of F2003. It will later simplify to the 
  !     conversion to F2003 once compilers are ready to 
  !     be released....
  !
  !     The wrapper assums that appropriate non-standard
  !     alternative like iarg etc eists.....

  ! --- Ingve Simonen, Mar 2007
  !
  ! --------------------------------------------------------

  ! --------------------------------------
  ! --- The Publicly avaiable quantities
  ! --------------------------------------
  Public :: Command_Argument_Count
  Public :: Get_Command
  Public :: Get_Command_Argument
  Public :: Get_Enviornment_Variable


  
  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private 


  ! --------------------------------------------
  ! --- Define default parameter settings
  ! --------------------------------------------
  Integer, parameter :: NO_ERROR=0


 
Contains



  Function Command_Argument_Count() result(res)
    ! --- Returns the number of command arguments.....
    Implicit None
    Integer  :: res
    ! Local
    !****************! integer, external :: iargc
    integer      :: iargc
    ! --- The Wrapper
    res = iargc()
  End Function Command_Argument_Count

  !
  ! -----
  !

  Subroutine Get_Command( Command, Length, Status )
    ! --- Gets the FULL command line
    Implicit None
    Character(len=*), intent(out), optional :: Command
    Integer,          intent(out), optional :: Length
    Integer,          intent(out), optional :: Status
    ! Local
    !*****************! Integer, external :: iargc
    Integer            :: iargc
    Integer            :: i
    Character(len=200) :: str, CommandLine=""


    ! --- The Wrapper
    !     Note that this will not handle the spaces correctly....
    CommandLine=""   ! Initialize the string....
    do i=0,iargc() 
       call getarg (i, str)
       CommandLine = trim(adjustl(CommandLine)) //" "// trim(Adjustl(Str)) 
    end do
    
    ! The optinal arguments....
    If (present(Command)) Command = Trim( Adjustl(CommandLine) ) 
    if (present(Length))  Length  = len( Trim(Adjustl(CommandLine)) )
    if (present(Status))  Status  = NO_ERROR
     
  End Subroutine Get_Command

  !
  ! -----
  !

  Subroutine  Get_Command_Argument( Number, Value, Length, Status )
    Implicit None
    Integer,          intent(in)            :: Number
    Character(len=*), intent(out), optional :: Value
    Integer,          intent(out), optional :: Length
    Integer,          intent(out), optional :: Status
    ! Local
    Character(len=500) :: str
    
    ! --- The Wrapper
    !     Non-standard F90 extension (e.g. Portland, lahey, Intel etc.)
    CALL GETARG (Number, str)       
    
    ! The optinal arguments....
    if (present(Value))  Value  = Trim(Adjustl(str))
    if (present(Length)) Length = len(Trim(Adjustl(str)))
    if (present(Status)) Status = NO_ERROR


  End Subroutine Get_Command_Argument


  !
  ! -----
  !

  Subroutine  Get_Enviornment_Variable( Name, Value, Length, Status, Trim_Name )
    Implicit None
    Character(len=*), intent(in)            :: Name
    Character(len=*), intent(out), optional :: Value
    Integer,          intent(out), optional :: Length
    Integer,          intent(out), optional :: Status
    Logical,          intent(in),  optional :: Trim_Name 
    ! Local
    Character(len=500) :: str

    ! --- The Wrapper
    if (present(Trim_Name)) then
       if (Trim_Name) then
          CALL GETENV(Name,str)
       else
          CALL GETENV(Trim(Adjustl(Name)),str)
       end if
    else
       CALL GETENV(Trim(Adjustl(Name)),str)
    end if

    ! The optinal arguments....
    if (present(Value))  Value  = Trim(Adjustl(str))
    if (present(Length)) Length = len(Trim(Adjustl(str)))
    if (present(Status)) Status = NO_ERROR

  End Subroutine Get_Enviornment_Variable



End Module F2003_Wrapper_Mod
