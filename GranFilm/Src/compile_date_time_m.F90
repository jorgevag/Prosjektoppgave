module compile_date_time_m


  ! --------------------------------------
  ! --- The Publicly avaiable quantities
  ! --------------------------------------
  Public :: get_compile_time
  Public :: get_compile_date

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  
contains



  Function get_compile_time() Result(res)
    !
    !  Returns the time of the compile
    !
    Implicit None
    Character(len=8) :: Res

    res = __TIME__
    
  End Function get_compile_time





  Function get_compile_date() Result(res)
    !
    !  Returns the time of the compile
    !
    Implicit None
    Character(len=11) :: Res

    res = __DATE__
    
  End Function get_compile_date



end module compile_date_time_m

