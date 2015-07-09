!
!  $Id:$
!
! ------------------------------------------------------------------


!
! ------------------------------------------------------------------
!
! TODO
!
!   * Facilitate the output of messages in warning and error messages 
!   * Add some standaradized error messages with corresponding 
!       error codes, e.g.  SFL_Error_Message(303)
!       E.g. incorporate the code for sixpack ny Norris for reporting
!       standard messages....  
!       Examples of this can be found in the library SIXPACK....!
!   * Only adjust the CURRENT module settings from global 
!       error instances
!   * Remove the automatic setting of the error%code
!   * Automatically setting the error%level from the error%code
!   * QUESTION : Should the error unit defined here also control the 
!                default StdErr unit? 
!                Here we do implement such a solution
!
! ------------------------------------------------------------------
!


!------------------------!
Module SFL_Error_Handling
!------------------------!
  !
  ! ---------------------------------------------------------------
  ! 
  ! This module is modeled after the the NAG fl90 library 
  ! error routine : nag_error_handling  
  ! Additional documentation can be found in the file 
  ! NAG_ErrorHandling.pdf 
  !
  ! I. Simonsen, Paris, Nov, 2006
  !
  ! ---------------------------------------------------------------
  !



  ! ----------------------------------------
  ! --- The public Interfaces
  ! --------------------------------------
  Public :: SFL_Error_Type          ! Base type
  !
  Public :: SFL_Error_Fatal         ! Black box routine for reporting a Fatal error
  Public :: SFL_Error_Failure       ! Black box routine for reporting a Failure
  Public :: SFL_Error_Warning       ! Black box routine for reporting a Warning

  !
  Public :: SFL_Error_Init          ! Initialization subroutine (alsways required to be run once) 
  Public :: SFL_Error_Set           ! Setting SFL_Error_Type entries
  Public :: SFL_Error_Get           ! Getting SFL_Error_Type entries
  Public :: SFL_Error_Detected      ! Inquiry function for errors
  Public :: SFL_Error_Report        ! Default Error reporting subroutine
  Public :: SFL_Error_Monitor_On    ! Turns error monitoring on  (should be used first in any sub-program)
  Public :: SFL_Error_Monitor_Off   ! Turns error monitoring off (should be used last in any sub-program)
  Public :: SFL_Error_Internal      ! Simple Internal Error routine (to raise design errors...)


  ! ----------------------------------------
  ! --- Everytinhg is private by default
  ! --------------------------------------
  Private
  



  ! -----------------------------------------
  ! --- Some private constants and variable
  ! -----------------------------------------
  !
  ! --- Default settings
  Integer,            Parameter, Private :: DEFAULT_Halt_Level      = 2
  Integer,            Parameter, Private :: DEFAULT_Print_Level     = 1
  Integer,            Parameter, Private :: DEFAULT_Level_Code      = 0
  Logical,            Parameter, Private :: DEFAULT_Show_Call_Tree = .true. 
  Character(len=10),  Parameter, Private :: DEFAULT_Top_Tree       = "Main"

  !
  ! --- Variables  used to communicate current settings to local
  !     instances of error variables. 
  !     NOTE : that NOT all settings of the error variable is inherited. 
  Integer,            Private :: Current_Halt_Level       =  DEFAULT_Halt_Level
  Integer,            Private :: Current_Print_Level      =  DEFAULT_Print_Level
  Logical,            Private :: Current_Show_Call_Tree   =  DEFAULT_Show_Call_Tree
  !Integer,           Private :: Current_Unit             =  DEFAULT_Unit   = StdErr = 6   ! Replaced by StdErr
  !
  Logical,            Private :: SFL_Error_Module_Initialized   =  .false.



  ! ------------------------------------------
  ! --- The Error Type (with defaults)
  ! ------------------------------------------
  Type SFL_Error_Type
     Private                   ! All entries in the derived type are private... 
     !                           and can thus not be modified directly
     Integer           :: halt_level             =  2    ! Current_Halt_Level
     Integer           :: print_level            =  1    ! Current_Print_Level
     integer           :: unit
     integer           :: code                   =  0    ! No error value
     integer           :: level                  =  0    ! No error value
     character(len=31) :: generic_name
     ! For the error message
     integer           :: message_length         =  0
     character(len=79) :: message(25)
     ! For the call tree
     logical           :: show_call_tree         = .true.
     integer           :: call_tree_pos          =  0
     character(len=79) :: call_tree(25) 
     ! Some help variables (are all of them needed?)
     Logical           :: catch_error            = .true.
     Logical           :: local_copy             = .true.
     Logical           :: local_init             = .false.

  End Type SFL_Error_Type


  ! --- Interfaces
  Interface SFL_Error_Internal
     MODULE PROCEDURE Error_Internal_vector, Error_Internal_Scalar
 End Interface




contains

  !-----------------------------------------!
  Subroutine SFL_Error_Fatal(routine,message)
  !-----------------------------------------!
    !
    ! Black box routine for reporting a Fatal error.
    !
    ! Simple interface routine to the more general SFL_Error_Report routine.
    ! Here one reports a fatal error....
    ! 
    ! Note that the routine reporting the error does not need to know about the 
    ! error type at all.....
    !
    ! Ingve Simonsen, Paris, Mar, 2007.
    !
    ! --------------------------------------------------------------------------
    !
    Implicit None
    Character(len=*),  Intent(in)   :: routine
    Character(len=*),  Intent(in)   :: message
    ! Local
    Type(SFL_Error_Type)   :: error
    Integer :: i

    Call SFL_Error_Set(error,name=routine,level=3,code=399,show_call_tree=.false., message=message)    
    Call SFL_Error_Report(error)

  End Subroutine SFL_Error_Fatal
  !-----------------------------!

  !-------------------------------------------!
  Subroutine SFL_Error_Failure(routine,message)
  !-------------------------------------------!
    !
    ! Black box routine for reporting a Failure error.
    !
    ! Simple interface routine to the more general SFL_Error_Report routine.
    ! Here one reports a failure error....
    ! 
    ! Note that the routine reporting the error does not need to know about the 
    ! error type at all.....
    !
    ! Ingve Simonsen, Paris, Mar, 2007.
    !
    ! --------------------------------------------------------------------------
    !
    Implicit None
    Character(len=*),  Intent(in)   :: routine
    Character(len=*),  Intent(in)   :: message
    ! Local
    Type(SFL_Error_Type)   :: error
    Integer :: i

    Call SFL_Error_Set(error,name=routine,level=2,code=299,show_call_tree=.false., message=message)    
    Call SFL_Error_Report(error)

  End Subroutine SFL_Error_Failure
  !-------------------------------!


  !-------------------------------------------!
  Subroutine SFL_Error_Warning(routine,message)
  !-------------------------------------------!
    !
    ! Black box routine for reporting a Warning.
    !
    ! Simple interface routine to the more general SFL_Error_Report routine.
    ! Here one reports a warning....
    ! 
    ! Note that the routine reporting the error does not need to know about the 
    ! error type at all.....
    !
    ! Ingve Simonsen, Paris, Mar, 2007.
    !
    ! --------------------------------------------------------------------------
    !
    Implicit None
    Character(len=*),  Intent(in)   :: routine
    Character(len=*),  Intent(in)   :: message
    ! Local
    Type(SFL_Error_Type)   :: error
    Integer :: i

    Call SFL_Error_Set(error,name=routine,level=1,code=199,show_call_tree=.false., message=message)    
    Call SFL_Error_Report(error)

  End Subroutine SFL_Error_Warning
  !-------------------------------!




  !----------------------------------------------------------------------------------!
  Subroutine SFL_Error_Init(error,halt_level,print_level,unit,show_call_tree,top_tree)
  !----------------------------------------------------------------------------------!
    !
    ! Routien used to initialize the SFL_Error_Handling module.
    ! This rputine must be call at least once, preferably in the main program.
    ! Not doing so, will be flagged as an error.
    !
    ! Ingve Simonsen, Paris, Nov, 2006.
    !
    ! --------------------------------------------------------------------------
    !
    Use SFL_Logical_Units, only : SFL_StdErr, SFL_Set_StdErr
    Implicit None
    Type(SFL_Error_Type),           intent(out)  :: error
    Integer,           optional,    intent(in)   :: halt_level
    Integer,           optional,    intent(in)   :: print_level
    Integer,           optional,    intent(in)   :: unit
    Logical,           Optional,    Intent(in)   :: show_call_tree
    Character(len=*),  Optional,    Intent(in)   :: top_tree
    ! Local
    Integer :: i

    ! --- The halt_level
    If (Present(halt_level)) Then
       error%halt_level = halt_level
    Else
       error%halt_level = DEFAULT_Halt_Level
    Endif
    Current_Halt_Level  = error%halt_level 
    
    ! --- The print_level
    If (Present(print_level)) Then
       error%print_level = print_level
    Else
       error%print_level = DEFAULT_Print_Level
    Endif
    Current_Print_Level  = error%print_level 
    
    ! --- The unit
    If (Present(unit)) Then
       error%unit = unit
       call SFL_Set_StdErr(error%unit)    ! Redefine the standard error unit.... 
    Else
       error%unit = SFL_StdErr() ! DEFAULT_Unit
    Endif
    !Current_Unit  = error%unit
    
    ! --- The call tree
    If (Present(show_call_tree)) Then
       error%show_call_tree = show_call_tree
    Else
       error%show_call_tree = DEFAULT_Show_Call_Tree
    End If
    If (error%show_call_tree) Then
       error%call_tree_pos = error%call_tree_pos + 1
       If (Present(top_tree)) Then
          error%call_tree(error%call_tree_pos) = Trim(Adjustl(top_tree))
       Else
          error%call_tree(error%call_tree_pos) = Trim(Adjustl(DEFAULT_Top_Tree))
       End If
    Endif

    ! --- Level and code
    error%level = DEFAULT_Level_Code
    error%code  = DEFAULT_Level_Code

    ! --- Set the error variable to initialized...
    error%Local_copy              =  .False.
    SFL_Error_Module_Initialized  =  .True.

  End Subroutine SFL_Error_Init
  !---------------------------!



  !---------------------------------------!
  Subroutine _SFL_Error_Init_Local_(error)        ! IS this routine needed...?
  !---------------------------------------!
    !
    ! This routine is used to initialize local error variables.
    ! 
    ! Ingve Simonsen, Paris¸ Nov. 2006.
    !
    ! --------------------------------------------------------------
    !
    Use SFL_Logical_Units, only : SFL_StdErr
    Implicit None
    Type(SFL_Error_Type), intent(out)  :: error
    ! Local
    ! --- Set the error variable to local copy
    error%local_copy     = .True.                ! Shold one use this?
    ! --- Inherit the current global settings
    error%halt_level      =  Current_Halt_Level 
    error%print_level     =  Current_Print_Level 
    error%unit            =  SFL_StdErr()           !Current_Unit
    error%show_call_tree  =  Current_show_call_tree
    ! --- Turn of call tree for local instances of error
    error%show_call_tree  = .false.
    ! -- defaultcall tree settings if (if used)
    error%call_tree_pos   = 0
    ! -- indicate the local copy has been initialized
    error%local_init      = .true.
    !error%call_tree(error%call_tree_pos) =  trim(adjustl( procedure ))

  End Subroutine _SFL_Error_Init_Local_
  !------------------------------------!





  !-------------------------------------------------------------------------!
  Subroutine SFL_Error_Set(error,name,halt_level,print_level,unit,&
                              show_call_tree,catch_error,level,code,message)
  !-------------------------------------------------------------------------!
    !
    ! This routine is used to change the private entries of the 
    ! derived type SFL_Error_Type.
    !
    ! Ingve Simonsen, Paris, Nov. 2006.
    !
    ! --------------------------------------------------------------------
    !
    !  The code 199, 299 and 399 indictae user a specified message....
    !
    Use SFL_Logical_Units, only : SFL_Set_StdErr
    Implicit None
    Type(SFL_Error_Type), Intent(inout)  :: error
    Character(len=*),  Optional,    Intent(in)   :: name
    Integer,           Optional,    Intent(in)   :: halt_level
    Integer,           Optional,    Intent(in)   :: print_level
    Integer,           Optional,    Intent(in)   :: unit
    Logical,           Optional,    Intent(in)   :: show_call_tree
    Logical,           Optional,    Intent(in)   :: catch_error
    Integer,           Optional,    Intent(in)   :: level
    Integer,           Optional,    Intent(in)   :: code
    Character(len=*),  Optional,    Intent(in)   :: message


    If (.Not. SFL_Error_Module_Initialized ) Then
       Write(error%unit,*)  &
            "ERROR : SFL_Error_Handling not initialized with the required call to SFL_Error_Init !"
       stop ""
    End If
    
    ! Update the local derived type variables
    !

    ! - Routine
    If (Present(name))  Then
       error%generic_name    =  Trim(adjustl(name))
    Endif
    ! - Halt level
    If (Present(halt_level))  Then
       error%halt_level    =  halt_level
       Current_Halt_Level  =  error%halt_level 
    Endif
    ! - Print level
    If (Present(print_level)) Then
       error%print_level    =  print_level
       Current_Print_Level  =  error%print_level
    End If
    ! - unit
    If (Present(unit))   Then
       error%unit        =  unit
       call SFL_Set_StdErr(error%unit)   ! Redefines the StdErr 
       !Current_Unit      =  error%unit
    Endif
    ! - Show call tree
    If (Present(show_call_tree)) Then
       error%show_call_tree     =  show_call_tree
       Current_Show_Call_Tree   =  error%show_call_tree
    End If
    ! - The rest
    If (Present(catch_error))     error%catch_error     =  catch_error
    if (present(level))           error%level           =  level
    if (present(code))            error%code            =  code
    If (Present(message))  Then
       error%message_length      =  error%message_length + 1 
       error%message(error%message_length)   =  trim(adjustl( message ))
    Endif

  End Subroutine SFL_Error_Set
  !---------------------------!




  !-------------------------------------------------------------------------!
  Subroutine SFL_Error_Get(error,name, halt_level,print_level,unit,&
                              show_call_tree,catch_error,level,code,message)
  !-------------------------------------------------------------------------!
    !
    ! This routine is used to get the private entries of the 
    ! derived type SFL_Error_Type. It is smimilar to SFL_Error_Set except 
    ! that the optional arguments are retuned a value instad of setting one.
    !
    ! Ingve Simonsen, Paris, Nov. 2006.
    !
    ! --------------------------------------------------------------------
    Implicit None
    Type(SFL_Error_Type), Intent(inout)  :: error
    Character(len=*),  Optional,    Intent(out)   :: name
    Integer,           Optional,    Intent(out)   :: halt_level
    Integer,           Optional,    Intent(out)   :: print_level
    Integer,           Optional,    Intent(out)   :: unit
    Logical,           Optional,    Intent(out)   :: show_call_tree
    Logical,           Optional,    Intent(out)   :: catch_error
    Integer,           Optional,    Intent(out)   :: level
    Integer,           Optional,    Intent(out)   :: code
    Character(len=*),  Optional,    Intent(out)   :: message


    If (.Not. SFL_Error_Module_Initialized ) Then
       Write(error%unit,*)  &
            "ERROR : SFL_Error_Handling not initialized with the required call to SFL_Error_Init !"
       stop ""
    End If
    
    ! Report the local derived type variables
    !
    ! - Routine Name
    If (Present(name))            name           =   error%generic_name  
    ! - Halt level
    If (Present(halt_level))      halt_level      =   error%halt_level   
    ! - Print level
    If (Present(print_level))     print_level     =   error%print_level    
    ! - Unit
    If (Present(unit))            unit            =   error%unit        
    ! - Show call tree
    If (Present(show_call_tree)) show_call_tree =   error%show_call_tree  
    ! - The rest
    If (Present(catch_error))     catch_error     =   error%catch_error    
    if (present(level))           level           =   error%level           
    if (present(code))            code            =   error%code           
    If (Present(message))         message         =   error%message(error%message_length)     
 
  End Subroutine SFL_Error_Get
  !--------------------------!





  !----------------------------------------------!
  Function SFL_Error_Detected(error)  result(res)
  !----------------------------------------------!
    !
    ! This routine is an iquiring function to check if error%level>=1.
    ! Such a function is nedded since the fields of the derived type 
    ! SFL_Error_Type only contains privat entries.
    !
    ! Ingve Simonsen, Paris, Nov. 2006.
    !
    ! --------------------------------------------------------------------
    Implicit None
    Type(SFL_Error_Type), Intent(in)  :: error
    Logical                           :: res
    ! Local
    
    !!!!! If  (error%level >= 1)  then  
    If ( (error%level >= 1) .or. (error%code >= 100) )  then   
       ! ??? Should one also check for error%code here ?
      res = .True.
    else
       res = .False.
    End If

  End Function SFL_Error_Detected
  !------------------------------!




  !----------------------------------------------!
  Subroutine SFL_Error_Monitor_On(error,Procedure)
  !----------------------------------------------!
    Implicit None
    Type(SFL_Error_Type), intent(inout) :: error
    Character(len=*),     intent(in)    :: procedure
    ! Local
    ! --- If not initialized.....
    If ( (.not.error%Local_init) .and. (error%Local_Copy) )   &
         call _SFL_Error_Init_Local_(error)
    ! Local definitions
    error%generic_name    =   Trim( Adjustl( procedure) )
    error%call_tree_pos  =   error%call_tree_pos  + 1
    error%call_tree(error%call_tree_pos) = Trim(Adjustl( error%generic_name ))

    ! --- Write a message
    if (error%print_level <= 0)& 
         Write(error%unit,*) " +++ Procedure ",Trim(Adjustl(_uppercase_(error%generic_name))), &
         " entered!"

  end Subroutine SFL_Error_Monitor_On
  !----------------------------------!





  !-------------------------------------!
  Subroutine SFL_Error_Monitor_Off(error)
  !-------------------------------------!
    Implicit None
    Type(SFL_Error_Type), intent(inout) :: error

    if (error%level <= 0) then
       ! --- No Error found
       If (error%print_level <= 0) &
            Write(error%unit,*) " --- Procedure ",Trim(Adjustl(_uppercase_(error%generic_name))), &
                      " compleded succesfully!"
       error%call_tree_pos  =   error%call_tree_pos  - 1
    else
       ! --- Report an Error
       !if (error%catch_error) then
       !   ! -- Report an error
       !   ! call SFL_Error_Report(error)
       !else
          ! -- Retun to the calling routine
          !    NOTE : potensial errors must me handled separately
          If (error%print_level <= 0) &
               Write(error%unit,*) " --- Procedure ",Trim(Adjustl(_uppercase_(error%generic_name))), &
                  " compleded with reported errors!"
          error%call_tree_pos  =   error%call_tree_pos  - 1
      !    Return
      ! End if
    End if

  end Subroutine SFL_Error_Monitor_Off
  !-----------------------------------!




  !--------------------------------!
  Subroutine SFL_Error_Report(error)
  !--------------------------------!
    Implicit None
    Type(SFL_Error_Type), intent(inout) :: error
    ! Local
    Integer            :: icall, imessage
    Character(len=3), parameter :: offset= "."
    Character(len=100)          :: spacing
    Character(len=40)           :: str
 
    ! ----------------------------------------
    ! --- No Error
    ! ----------------------------------------
    if (error%level==0) Return


    ! ----------------------------------------
    ! --- Do not flag errors or warnings
    !      (This may be dangerous)
    ! ----------------------------------------
    if (.not.error%catch_error) then
       ! Reset the message index variable
       error%message_length=0
       Return
    end if


    ! ----------------------------------------
    ! --- Do reportWarning, Failure or Fatal Error 
    ! ----------------------------------------
    !

    ! --- Prepare the error code
    ! ?????????????????  Should this be in here....????????????
    !if (error%level > 0) error%code =  error%level * 100 + 1 
    ! ?????????????????  Should this be in here....????????????

    !    The print level
    if (error%level >= error%print_level) then
       Write(error%unit,*)
       Write(error%unit,*)
       if (error%level == 1) &
            Write(error%unit,*)  "***************************** WARNING Reported ******************************"
       if (error%level == 2) &
            Write(error%unit,*)  "***************************** FAILURE Reported ******************************"
       if (error%level >= 3) &
            Write(error%unit,*)  "*************************** FATAL ERROR Reported ****************************"
       ! --- Report the routine name etc.
       write(str,*) Trim(AdjustL(_uppercase_(error%generic_name)))
       Write(error%unit,*)
       Write(error%unit,'("  Procedure: ", a," Level = ", i1, ", Code = ",i4)')    &
            AdjustL(str), error%level,  error%code
       ! --- Print the error massage here and
       !      reset the array index variable
       Write(error%unit,*)
       do imessage = 1, error%message_length
          Write(error%unit,*) " "//error%message(imessage)
       enddo
       error%message_length = 0

       ! --- Print call tree 
       if (error%show_call_tree) then
          Write(error%unit,*)
          Write(error%unit,*) "............................................................................."
          Write(error%unit,*)
          Write(error%unit,*) "The Call Tree :"
          spacing = trim(adjustl(offset))      
          do icall=1,error%call_tree_pos
             spacing  = Trim(Adjustl(spacing))//Trim(Adjustl(offset))      
             Write(error%unit,*) "   "//Trim(Adjustl(spacing))//" ", Trim(Adjustl( error%call_tree(icall) ))
          enddo
       End If
       If (error%level<error%halt_level) then
          Write(error%unit,*)
          Write(error%unit,*) "**************************** Execution Continued ****************************"
          Write(error%unit,*)
       endif
    endif
       
    ! --- The HALT LEVEL
    if (error%level>=error%halt_level) then
       Write(error%unit,*)
       Write(error%unit,*) "***************************** Execution Halted ******************************"
       Write(error%unit,*)
       call exit(-1)  !stop ""
    endif

  End Subroutine SFL_Error_Report
  !------------------------------!




  !------------------------------------------------!
  Subroutine Error_Internal_vector(routine, message)
  !------------------------------------------------!   
    ! --- Simple Internal Error raising due to internal inconsistencies. 
    !     The interface is simple, and not sofisticated.
    !     It is ment to be able to trow simple errors that in principle 
    !     should not never occure.... and reflects more a deficiency 
    !     in the design than any user problems with input.
    Implicit None
    character(len=*),               intent(in)  :: routine
    character(len=*), dimension(:), intent(in)  :: message
    ! Local
    Integer :: i
    Write(*,*) "INTERNAL ERROR : "// trim(adjustl(_uppercase_(routine)))
    do i=1,size(message,1)
       Write(*,*) "               ... "// trim(adjustl(message(i)))
    end do
    STOP "Execution Halted"

  End Subroutine Error_Internal_vector
  !-----------------------------------!
 
  !------------------------------------------------!
  Subroutine Error_Internal_scalar(routine, message)
  !------------------------------------------------!
    Implicit None
    character(len=*),               intent(in)  :: routine
    character(len=*),               intent(in)  :: message
    ! Local
    
    call Error_Internal_vector(routine, (/ message/) )

  End Subroutine Error_Internal_scalar
  !-----------------------------------!

  !------------------------------------------------!
  function _uppercase_(in_string) result(out_string)
  !------------------------------------------------!
     !---------------------------------------------------------------------
     ! ROUTINE   :  Upper2lower 
     ! TYPE      :  Function
     ! SYSTEM    :  Fortran 90
     !
     ! PURPOSE   :  Tranlate a character variable to all uppercase letters.
     !              Non-alphabetic characters are not affected.  
     !
     ! SYNTAX    :  lower2upper
     !---------------------------------------------------------------------
     ! USES  Lib :  
     !       Mod : 
     !---------------------------------------------------------------------
     ! DATE      : 98.08.18
     ! LAST MOD  :  
     ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
     ! E-mail    : ingves@phys.ntnu.no
     !---------------------------------------------------------------------
     implicit none
     character(len=*)               :: in_string
     character(len=len(in_string))  :: out_string
     integer                        :: i,ascii
     do i=1,len(in_string)
        ascii = ichar(in_string(i:i))
        if ((ascii>96).and.(ascii<123)) THEN
           out_string(i:i) = char(ascii-32)
        else
           out_string(i:i) = in_string(i:i)
        endif
     enddo
     return
   end function _uppercase_
  !------------------------!

End Module SFL_Error_Handling




!!$
!!$! ========================================================
!!$! === TESTING
!!$! ========================================================
!!$
!!$Module test_mod
!!$
!!$  Use SFL_Error_Handling
!!$
!!$contains
!!$
!!$  Subroutine Sub1(error)
!!$    Implicit none
!!$    Type(SFL_Error_Type), intent(inout) :: error
!!$    character(len=10):: routine="Sub1"
!!$    
!!$    Call SFL_Error_Monitor_On(error,routine)
!!$
!!$    
!!$    Call SFL_Error_Set(error,level=1,code=102,message="testing")    
!!$    Call SFL_Error_Report(error)
!!$
!!$    call sub2(error)
!!$  !  call SFL_Error_Report(error)
!!$
!!$    call SFL_Error_Monitor_Off(error)
!!$
!!$  End Subroutine Sub1
!!$
!!$  !
!!$  ! ---
!!$  !
!!$
!!$  Subroutine Sub2(error)
!!$    Implicit none
!!$    Type(SFL_Error_Type), intent(inout) :: error
!!$    character(len=10):: routine="Sub2"
!!$
!!$    Call SFL_Error_Monitor_On(error,routine)
!!$
!!$    call sub3(error)
!!$ !   call SFL_Error_Report(error)
!!$
!!$    call SFL_Error_Monitor_Off(error)
!!$
!!$  End Subroutine Sub2
!!$
!!$  !
!!$  ! ---
!!$  !
!!$
!!$  Subroutine Sub3(error)
!!$    Implicit none
!!$    Type(SFL_Error_Type), intent(inout) :: error
!!$    character(len=10):: routine="Sub3"
!!$    Integer          :: i
!!$
!!$    Call SFL_Error_Monitor_On(error,routine)
!!$
!!$    ! Raise an error
!!$    Call SFL_Error_Set(error,level=3,code=202)
!!$    Call SFL_Error_Set(error,message="Problems with the input ....")
!!$    Call SFL_Error_Set(error,message="Problems with the input .... 2")
!!$
!!$    call SFL_Error_Report(error)
!!$
!!$    call SFL_Error_Monitor_Off(error)
!!$    Return
!!$
!!$  End Subroutine Sub3
!!$
!!$  !
!!$  ! ---
!!$  !
!!$
!!$  Subroutine Sub_NoError(error)
!!$    Implicit none
!!$    Type(SFL_Error_Type), intent(inout) :: error
!!$    character(len=25):: routine="Sub_NoError"
!!$
!!$    Call SFL_Error_Monitor_On(error,routine)
!!$    
!!$    ! the main part of the routine
!!$    call SFL_Error_Monitor_Off(error)
!!$
!!$
!!$  End Subroutine Sub_NoError
!!$
!!$  !
!!$  ! ---
!!$  !
!!$
!!$  Subroutine Sub_Warning(error)
!!$    Implicit none
!!$    Type(SFL_Error_Type), intent(inout) :: error
!!$    character(len=25):: routine="Sub_Warning"
!!$
!!$    Call SFL_Error_Monitor_On(error,routine)
!!$
!!$    Call SFL_Error_Set(error,level=1,message="This is a Warning from level 2!")
!!$    call SFL_Error_Report(error)
!!$
!!$    ! the main part of the routine
!!$    call SFL_Error_Monitor_Off(error)
!!$
!!$
!!$  End Subroutine Sub_Warning
!!$
!!$  !
!!$  ! ---
!!$  !
!!$
!!$  Subroutine Sub_Local_Warning()
!!$    Implicit None
!!$    ! Local
!!$    Type(SFL_Error_Type)  :: error_local
!!$    Character(len=25)     :: routine = "Sub_Local_Warning"
!!$
!!$        
!!$    Call SFL_Error_Monitor_On(error_local,routine)
!!$    
!!$    Call SFL_Error_Set(error_local,level=1,message="This is a Warning!",show_call_tree=.true.)
!!$    call SFL_Error_Report(error_local)
!!$
!!$    call SFL_Error_Monitor_Off(error_local)
!!$
!!$  end Subroutine Sub_Local_Warning
!!$
!!$  !
!!$  ! ---
!!$  !
!!$
!!$  Subroutine Sub_Local_Warning_Level_2()
!!$    Implicit None
!!$    ! Local
!!$    Type(SFL_Error_Type)  :: error_local
!!$    Character(len=25)     :: routine = "Sub_Local_Warning_Level_2"
!!$
!!$        
!!$    Call SFL_Error_Monitor_On(error_local,routine)
!!$    
!!$!    call SFL_Error_Set(error_local,show_call_tree=.true.)
!!$    call SFL_Error_Set(error_local)
!!$    call Sub_Warning(error_local)
!!$
!!$    call SFL_Error_Monitor_Off(error_local)
!!$
!!$  end Subroutine Sub_Local_Warning_Level_2
!!$
!!$
!!$
!!$End Module test_mod
!!$
!!$
!!$
!!$
!!$! ------------------------------------------
!!$! --- Main Program
!!$! ------------------------------------------
!!$
!!$
!!$Program Main
!!$  Use SFL_Error_Handling
!!$  Use SFL_Logical_Units
!!$  Use test_mod
!!$  Implicit None
!!$  type(SFL_Error_Type) :: error
!!$  
!!$
!!$
!!$  Call SFL_Error_Init(error,print_level=0,unit=10)
!!$  
!!$write(SFL_StdErr(),*) "##########################################"
!!$
!!$  WRITE(*,*) "==== NO Error ===="
!!$  call Sub_NoError(error)
!!$
!!$  
!!$  ! Report a local warning
!!$  WRITE(*,*) "===== Local Warning ===="
!!$  call Sub_Local_Warning()
!!$
!!$
!!$  ! Report a local warning from level 2
!!$  WRITE(*,*) "==== Local Warning Level 2 ====="
!!$  call Sub_Local_Warning_Level_2()
!!$
!!$  
!!$  ! Continue without catching an error crated....(dangerous)
!!$  WRITE(*,*) "===== Do not stop because of errors ====="
!!$  Call SFL_Error_Set(error,catch_error=.False.)
!!$  call sub1(error)
!!$  Write(*,*) " Program continued even with errors"
!!$  
!!$  
!!$
!!$  ! Here Sub1 calls sub2 which calls sub3 which raises an error.
!!$  ! here the Call Tree should be generated. 
!!$  WRITE(*,*) "==== Warning then Fatal Error ==== "
!!$  Call SFL_Error_Set(error,catch_error=.true.)
!!$  call sub1(error)
!!$
!!$
!!$end Program Main
!!$
