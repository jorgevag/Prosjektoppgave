! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module contains the main routine for checking if the input
!     file in the sif format ("Scientific Input Format") contains the
!     user-defined parameters REQURIED by GranFilm in the sif format
!     ("Scientific Input Format")
! 
!     Only parameters where are not given default values should be checked for here.
!
! --- AUTHOR : Ingve Simosnen, Paris, May 2012.
!
! ----------------------------------------------------------
!


!------------------------------------------!
Module Check_Required_Parameter_Module
!------------------------------------------!



  ! --- The Use Statements global to the module
  !Use Shared, only : wp, Param	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Check_for_Required_Parameters


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  


Contains



  !-----------------------------------------!
  Subroutine Check_for_Required_Parameters()
  !-----------------------------------------!
    !
    !
    !  --- This routine checks if the input file 
    !      contains the required parameters.
    !
    !      Ingve Simonsen, Paris, May 2012.
    !
    !
    Use Shared,              only : Param
    Use SFL_Logical_Units,   only : SFL_Get_Free_Unit
    Use Error_Module
    Use Tools_Read_Input_File_Module
    Implicit None
    ! --- Local
    character(len=*),parameter :: routine="Check_for_Required_Parameters"
    Character(len=100)         :: NML, Field 
    Integer :: ifile, iostat
    Logical :: file_exists
    


    ! ---  Verbose
    If (Param%InOut%Verbose) then
       Write(*,*) " +++ Entering : ", routine
       !Write(*,*) "      ... Reading Input Parameter File : ", trim(adjustl(Param%InOut%Input_File_Name))
    End If

    ! -----------------------------------------
    ! --- Open the Input Parameter File
    ! -----------------------------------------
    !
    ! --- Get free unit
    call SFL_Get_Free_Unit( ifile ) 
    ! --- Does the input parameter file exist?
    inquire(file = Trim(Adjustl(param%InOut%Input_File_Name)), exist = file_exists)
    if ( file_exists) then
       ! --- Open the input parameter file
       open(unit=ifile, file=Trim(Adjustl(param%InOut%Input_File_Name)))    
    else
       ! Give an error message
       call Error_Fatal( routine, &
          "Input Parameter File ("//Trim(Adjustl(param%InOut%Input_File_Name))//") does not exist!")       
    End if

    !
    ! ------------------------------------------------------
    ! --- Check for the REQUIRED (non-default) parameters
    ! ------------------------------------------------------
    !



    !
    ! ----------------------------------------
    ! --- The Required NameLists
    ! ----------------------------------------
    !



    ! --- Global
    ! ----------------------------
    NML="Global"


    ! --- Geomtry
    ! ----------------------------
    NML="Geomtry"


    ! --- Source
    ! ----------------------------
    NML="Source"

 
    ! --- Intercation
    ! ----------------------------
    NML="Interaction"


    ! --- Numerics
    ! ----------------------------
    NML="Numerics"


    ! --- Media
    ! ----------------------------
    NML="Meida"

    !
    ! .......................................


    !
    ! ----------------------------------------
    ! --- The optional NameLists
    ! ----------------------------------------
    !



    ! --- Curvefitting
    ! ----------------------------
    NML="Curvefitting"


    ! --- Potential
    ! ----------------------------
    NML="Potential"
    if ( NameList_Exists(ifile,NML) ) then

       ! ... Points-File
       Field ="Points_File"
       call Field_in_Namelist_Present( NML, Field)

       ! ... Energy
       Field ="Energy"
       call Field_in_Namelist_Present( NML, Field )
       
    Endif



    !
    ! .......................................









    ! --- Closing the input parameter file
    close( unit = ifile)


    ! ---  Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

                                                           

  Contains


    ! -----------------------------------------------!
    Subroutine Field_in_Namelist_Present( NML, Field)
    ! -----------------------------------------------!
      !
      ! Give an error if the field is not present in the Namelist
      !
      Implicit None
      Character(len=*), Intent(In) :: NML
      Character(len=*), Intent(In) :: Field
      ! --- Local
      Character(len=100) :: Error_String
      
      if (.not. Field_in_NameList_Exists(ifile, NML, Field) ) then
         Error_String = Trim(adjustl(Field))//"@"//trim(adjustl(NML))// " is not present, but required!"
         call Error_Fatal( routine, trim(adjustl( Error_String)) )
       endif

    End Subroutine Field_in_Namelist_Present
    ! ---------------------------------------!


  End Subroutine Check_for_Required_Parameters
  !-------------------------------------------------------!




End Module Check_Required_Parameter_Module
!---------------------------------------------!
