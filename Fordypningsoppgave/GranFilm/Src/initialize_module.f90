! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!    
!     This module provides the main initialization of the 
!     
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!

!----------------------!
Module Initialize_Module
!----------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp, Param  


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Initialize_GranFilm


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains
!-------!



  !-------------------------------!
  Subroutine Initialize_GranFilm()
  !-------------------------------!  
    Use Physical_Constants_Module,        only : Set_Physical_Constants
    Use Error_Module,                     only : Error_Initialize
    Use Command_Line_Handling_Module,     only : Read_Command_Line
    Use Read_Input_File_Module,           only : Read_Input_Parameter_File
    Use Check_Required_Parameter_Module,  only : Check_for_Required_Parameters
    Use Default_Parameter_Module,         only : Set_Default_Parameters, Set_Default_Internal_Parameters
    Use Post_Process_Parameters_Module,   only : Post_Process_Parameters
    Use Check_Parameters_Module,          only : Check_Input_Parameters, Check_Derived_Parameters
    Use Print_Parameters_Module,          only : Print_Parameters
    Implicit None
    Character(len=*), parameter :: routine = "Initialize_GranFilm"



    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Initialize the Physical Constants
    call Set_Default_Internal_Parameters



    ! --- Initialize the Physical Constants
    call Set_Physical_Constants()


    ! --- Initialize the Error handling routines....    
    call Error_Initialize()


    ! --- Handle the command line
    call Read_Command_Line()


    ! --- Check Required Parameter 
    call Check_for_Required_Parameters()


    ! --- Set Parameter Defaults
    call Set_Default_Parameters()


    ! --- Read and process the  Input File
    Call Read_Input_Parameter_File()


    ! --- Check and make sure that the given parameters are consistent
    Call Check_Input_Parameters()

       
    ! --- Post process parameters()
    !     QUESTION :  Can one move this routine to before Check_Input_Parameters
    !                 and do all of the cheacking at once?
    Call Post_Process_Parameters()
    

    ! --- Check the derived parameters
    call Check_Derived_Parameters()


    ! --- Print parameters to screen (and exit)
    if (Param%InOut%Print_Parameters) then
       call Print_Parameters()
       stop 
    endif

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine



  End Subroutine Initialize_GranFilm
  !---------------------------------!







End Module Initialize_Module
!--------------------------!
