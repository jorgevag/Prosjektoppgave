! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!     To allocate the storage for the variables 
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!

Module Allocation_Module



  ! --- The Use Statements global to the module
  Use Shared, only : wp	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: .....


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!



  Subroutine XXXX

    Character(len=*), parameter :: routine = "Initialize_GranFilm"
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine

  End Subroutine XXXX


End Module Allocation_Module
!---------------------------!
