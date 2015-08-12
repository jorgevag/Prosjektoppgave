! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!-----------------------------!
Module Supported_Options_Module
!-----------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  !
  Public :: List_Supported_Options
  Public :: Is_Option_Supported
  Public :: Not_A_Supported_Option
  Public :: Check_for_Supported_Option
  !
  Public :: ARRANGEMENT_OPTION_TABEL  
  Public :: ISLAND_ISLAND_INTERACTION_OPTION_TABEL
  Public :: LATTICE_TYPE_OPTION_TABEL 
  Public :: POLARIZATION_OPTION_TABEL 


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private






  ! ---------------------------------------------------
  ! ---------------------------------------------------
  ! --- The Supported Option Tables
  !
  !     WARNING:
  !
  !         DO NOT CHANGE THE ORDER OF SUPPORTED 
  !         OPTIONS IN AN OPTION TABLE SINCE THIS 
  !         MAY BE USED EXPLICITLY LATER IN THE CODE
  !           
  ! ---------------------------------------------------
  ! ---------------------------------------------------




  ! --- The Island-Island Interaction
  Character(len=10), dimension(3), parameter ::  ISLAND_ISLAND_INTERACTION_OPTION_TABEL   &
             = (/   "None      ",       &
                    "Dipole    ",       &
                    "Quadrupole"            /)


  ! --- The island Arrangement
  Character(len=7), dimension(1), parameter ::  ARRANGEMENT_OPTION_TABEL    &
             = (/   "Lattice"           &
                                            /)


  ! --- The Lattice that the islands form
  Character(len=9), dimension(2), parameter ::  LATTICE_TYPE_OPTION_TABEL    &
             = (/   "Square   ",        &
                    "Hexagonal"             /)



  ! --- Polarization of the incident light
  Character(len=2), dimension(4), parameter ::  POLARIZATION_OPTION_TABEL   &
             = (/   "p ",               &
                    "s ",               &
                    "ps",               &
                    "sp"                    /)






!-------!
Contains
!-------!




  !--------------------------------------------------------------!
  Subroutine List_Supported_Options( Option_Name, Option_Table ) 
  !--------------------------------------------------------------!
    !
    ! --- List the supported options for option "OptionName"
    !
    !     Ingve Simonsen, Paris, Jun 2010
    !
    Use Error_Module, only : Error_Warning 
    Implicit None
    Character(len=*),               Intent(In) :: Option_Name
    Character(len=*), dimension(:), Intent(In) :: Option_Table
    ! --- Local
    Character(len=*), parameter :: routine = "Check_Parameters"
    Integer :: i


    ! --- List possible options....
    Write(*,*)
    Write(*,*) "   --- Supported options for : " // Trim(Adjustl(Option_Name))
    Do i=1,size(Option_Table,1)
       Write(*,*) "         *   ", trim(adjustl( Option_Table(i) ))
    enddo

  End Subroutine List_Supported_Options
  !------------------------------------!







  !--------------------------------------------------------------------!
  Subroutine Not_A_Supported_Option( Option_Name, Option_Table ) 
  !--------------------------------------------------------------------!
    !
    ! --- The given value for Option_Name is not in Option_Table so we
    !     list the supported options and give an error message.
    !
    !     Ingve Simonsen, Paris, Jun 2010
    !
    Use Error_Module, only : Error_Failure 
    Implicit None
    Character(len=*),               Intent(In) :: Option_Name
    Character(len=*), dimension(:), Intent(In) :: Option_Table
    ! --- Local
    Character(len=*), parameter :: routine = "Check_Parameters"


    ! -- List the supported Options
    call List_Supported_Options( Option_Name, Option_Table ) 

    ! --- Give the error message
    call Error_Failure( "", "Unsupported input option for " // Trim(Adjustl(Option_Name)) )

  End Subroutine Not_A_Supported_Option
  !-------------------------------------!






  !--------------------------------------------------------------------!
  Function Is_Option_Supported( Option, Option_Table) Result(res)
  !--------------------------------------------------------------------!
    !
    ! --- Check if the option "Option" is supported,
    !     i.e. in present in Option_Table
    !
    !     Ingve Simonsen, Paris, Jun 2010
    !
    Implicit None
    Character(len=*),               Intent(In) :: Option
    Character(len=*), dimension(:), Intent(In) :: Option_Table
    Logical                                    :: Res
    ! --- Local
    Integer :: i
    ! --- Default value
    
    ! --- Default is not supported
    Res = .false.

    ! -- Loop over the options 
    do i=1,size(Option_Table,1)
       if ( trim(adjustl(Option)) == trim(adjustl(Option_Table(i))) ) then
          Res = .true.
          exit
       endif
    enddo

  End Function Is_Option_Supported
  !-------------------------------------!






  !-----------------------------------------------------------------------!
  Subroutine Check_for_Supported_Option( Option_Name, Option, Option_Table ) 
  !-----------------------------------------------------------------------!
    !
    ! --- Check if Option is supported, i.e. is in Option_Table.
    !
    !     If it is not, the suppoted options for option name,
    !     Option_Name, are listed, and further executuion is halted.
    !
    !     This is a canned routine to simplify error checking of input
    !     parameters.
    !
    !     Ingve Simonsen, Paris, Jun 2010
    !
    Implicit None
    Character(len=*),               Intent(In) :: Option_Name
    Character(len=*),               Intent(In) :: Option
    Character(len=*), dimension(:), Intent(In) :: Option_Table
    ! --- Local

    if ( .not. Is_Option_Supported( Option, Option_Table ) ) then
       ! --- End further execuation
       call Not_A_Supported_Option( Option_Name, Option_Table ) 
    End if


  End Subroutine Check_for_Supported_Option
  !-------------------------------------!






End Module Supported_Options_Module
!---------------------------------!

