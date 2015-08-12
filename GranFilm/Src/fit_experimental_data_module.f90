! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE  : To fit experiemtnal data
!
!
! 
! --- AUTHOR : Ingve Simonsen, Paris, Apr 2012.
!
! ----------------------------------------------------------
!



!-----------------------------------!
Module Fit_Experimental_Data_Module
!------------------------------------!


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Fit_Experimental_Data


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private



!--------!  
Contains
!--------!


  !----------------------------------!
  Subroutine Fit_Experimental_Data()
  !----------------------------------!
    Use Shared
    Use Curvefit_Module,          only : CurveFit
    !
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Fit_Experimental_Data" 

    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Entering : ", routine


    if (Param%InOut%Do_Curve_Fitting) then
       !
       ! --- Do the fit of the experimental data
       !
       Param%InOut%Verbose     =   .false.
       Param%InOut%debug       =   .false.
       call Curvefit()
       ! --- Stop further execution....
       call exit
    else
       !
       ! --- Do Nothing
       !
    End if

    ! --- Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Fit_Experimental_Data
  !----------------------------------!



End Module Fit_Experimental_Data_Module
!----------------------------------------!

