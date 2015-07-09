Module SFL_Logical_Units
  
  ! 
  ! This module defines the standard input/output 
  !  and error logical units used in the lirary
  !
  !  With Fortran 90, an additional three unit numbers are also preconnected:
  !
  !      Standard input is logical units 5 and 100 
  !      Standard output is logical units 6 and 101 
  !      Standard error is logical units 0 and 102



  ! +++++++++++++++++++++++++++++++++++++++++++++++++++
  ! +++     TODO       ++++++++++++++++++++++++++++++++
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !  ** Check in SFL_Set_XXX if unit is within range
  !  ** Check if e.g. unit 5 can be used for output after 
  !     redefinittion...? (or should we just avoid it)
  !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++


  
  ! The what will be the intrinsic F2003 module.....
  Use ISO_FORTRAN_ENV, only : Input_Unit, Output_Unit, Error_Unit 


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: SFL_StdErr        ! Function retuning StdErr logical unit
  Public :: SFL_StdIn         ! Function retuning StdIn  logical unit
  Public :: SFL_StdOut        ! Function retuning StdOut logical unit
  Public :: SFL_Set_StdErr    ! Redefining StdErr logical unit
  Public :: SFL_Set_StdIn     ! Redefining StdIn  logical unit
  Public :: SFL_Set_StdOut    ! Redefining StdOut logical unit
  Public :: SFL_Get_Free_Unit ! Get an available Logical Unit


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private








  ! --- The defaults
  Integer, Parameter   :: DEFAULT_StdErr = Error_Unit   !0
  Integer, Parameter   :: DEFAULT_StdIn  = Input_Unit   !5
  Integer, Parameter   :: DEFAULT_StdOut = Output_Unit  !6

  ! --- The current settings
  Integer, Private, Save   :: Std_Err = DEFAULT_StdErr 
  Integer, Private, Save   :: Std_In  = DEFAULT_StdIn 
  Integer, Private, Save   :: Std_Out = DEFAULT_StdOut
  

Contains



  Function SFL_StdErr(unit)  Result(res)
    ! Getting Standard Error
    Implicit None
    Integer, optional, intent(out) :: unit
    Integer                        :: res
    ! Local
    res = Std_Err  
    If (present(unit)) unit = res
  end Function SFL_StdErr

  !
  ! ------
  !

  Function SFL_StdIn(unit)  Result(res)
    ! Getting Standard Input
    Implicit None
    Integer, optional, intent(out) :: unit
    Integer                        :: res
    ! Local
    res = Std_In  
    If (present(unit)) unit = res
  end Function SFL_StdIn

  !
  ! ------
  !

  Function SFL_StdOut(unit)  Result(res)
    ! Getting Standard Output
    Implicit None
    Integer, optional, intent(out) :: unit
    Integer                        :: res
    ! Local
    res = Std_Out
    If (present(unit)) unit = res
  end Function SFL_StdOut

  !
  ! ------
  !


  ! -----------------------------------
  ! --- For redefining the defaults
  ! -----------------------------------

  Subroutine SFL_Set_StdErr(unit) 
    ! Setting Standard Error
    !   Avoid the defaults
    Implicit None
    Integer :: unit
    ! Local
    Std_Err = unit
  end Subroutine SFL_Set_StdErr
  
  !
  ! -----
  !

  Subroutine SFL_Set_StdIn(unit)
    ! Setting Standard Input 
    Implicit None
    Integer :: unit
    ! Local
    Std_In = unit
  end Subroutine SFL_Set_StdIn

  !
  ! -----
  !

  Subroutine SFL_Set_StdOut(unit)
    ! Setting Standard Output
    Implicit None
    Integer :: unit
    ! Local
    Std_Out = unit
  end Subroutine SFL_Set_StdOut


  !
  ! ------
  !


  subroutine SFL_Get_Free_Unit ( iunit )
    !
    ! --- SFL_GET_FREE_UNIT returns a free FORTRAN unit number.
    !------------------------------------------------------------
    !
    ! Adopted after John Burkardt's GET_UNIT 
    !
    !  Discussion:
    !
    !    A "free" FORTRAN unit number is an integer between 1 and 99 which
    !    is not currently associated with an I/O device.  A free FORTRAN unit
    !    number is needed in order to open a file with the OPEN command.
    !
    !  Parameters:
    !
    !    Output, integer IUNIT.
    !
    !    If IUNIT = 0, then no free FORTRAN unit could be found, although
    !    all 99 units were checked (except for units 5 and 6).
    !
    !    Otherwise, IUNIT is an integer between 1 and 99, representing a
    !    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
    !    are special, and will never return those values.
    !
    implicit none
    Integer, intent(out) :: iunit
    ! Local
    integer :: i
    integer :: ios
    logical :: lopen
   
    iunit = 0
    do i = 1,99
       if ( i/=DEFAULT_StdIn .and. i/=DEFAULT_StdOut .and. &
            i/=Std_Err .and. i/=Std_In .and. i/=Std_Out       ) then
          inquire ( unit = i, opened = lopen, iostat = ios )
          if ( ios == 0 ) then
             if ( .not. lopen ) then
                iunit = i
                return
             end if
          end if
       end if
    end do
    return
  end subroutine SFL_Get_Free_Unit




 
End Module SFL_Logical_Units




!!$
!!$
!!$
!!$! =============================================
!!$! === Testing Program =========================
!!$! =============================================
!!$
!!$Program LogicalUnitTest
!!$  Use SFL_Logical_Units
!!$  Implicit none
!!$  Integer :: itmp, iunit 
!!$
!!$
!!$  ! -- Write results to file
!!$  itmp = SFL_StdErr(iunit)
!!$  Write(SFL_StdOut(),*) "StdErr Logical unit is : ", iunit
!!$
!!$  itmp = SFL_StdOut(iunit)
!!$  Write(SFL_StdOut(),*) "StdOut Logical unit is : ", iunit
!!$
!!$  itmp = SFL_StdIn(iunit)
!!$  Write(SFL_StdOut(),*) "StdIn Logical unit is : ", iunit
!!$
!!$
!!$
!!$  ! -- Write results to file
!!$  call SFL_Get_Free_Unit(iunit)
!!$  call SFL_Set_StdOut(iunit)
!!$  
!!$  itmp = SFL_StdErr(iunit)
!!$  Write(SFL_StdOut(),*) "StdErr Logical unit is : ", iunit
!!$
!!$  itmp = SFL_StdOut(iunit)
!!$  Write(SFL_StdOut(),*) "StdOut Logical unit is : ", iunit
!!$
!!$  itmp = SFL_StdIn(iunit)
!!$  Write(SFL_StdOut(),*) "StdIn Logical unit is : ", iunit
!!$
!!$
!!$End Program LogicalUnitTest
