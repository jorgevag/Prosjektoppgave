!
!  $Id:$
! -------------------------------------------------------------------------
!

Module SFL_Util

  !
  ! This module contains some utility routies 
  ! used throughout the code
  !
  ! It also used some a nonintrinsic F90 version of the
  ! to be intrinsic ISO_FORTRAN_ENV module. It is used for stdout 
  ! and error units...
  !

  ! TODO 
  !
  ! 1. Optimise SFL_Reverse for memory consumption.....
  !    One may successfully use some of the F90 built in functions 
  !    for this purpose....
  !


  ! --------------------------------------
  ! --- Use statments
  ! --------------------------------------
  Use SFL_NR_Util, only :                          &
       SFL_Assert       => assert,                 &
       SFL_Assert_Eq    => assert_eq,              &
       SFL_CumSum       => cumsum          !,                 &
  !!SFL_Reallocate   => reallocate



  ! Error flags
  ! They should be defined in  SFL_COMMON
  Integer,          parameter   :: ERROR_FOUND    = -1         ! Std Error flag
  Integer,          parameter   :: NO_ERROR_FOUND =  0         ! Std Error flag




  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: SFL_Assert
  Public :: SFL_Assert_Eq
  Public :: SFL_CumSum
  !!Public :: SFL_Reallocate
  !
  Public :: SFL_LowerCase          ! Converts strings to uppercase
  Public :: SFL_UpperCase          ! Converts strings to lowercase
  Public :: SFL_TimeString         ! Create a timeString
  Public :: SFL_TimeStamp          ! Write a timestanmp on a output unit
  Public :: SFL_Sequence           ! Makes a (lin/log) sequence..... (partly clones arth from NR...)
  Public :: SFL_Reverse            ! Reverse the order of a 1D (int/real/complex) vector 
                                   !     ( TODO : could be optimized for memory consumption...)

  Public :: SFL_Count_Substrings   ! Count the number of substrings in string

  Public :: SFL_BaseName           ! extract teh base name from an input path

  Public :: SFL_String_Replace_All ! Replace all occurances of a substring withanother one in string

  Public :: SFL_LinSpace           ! Makes a linear profression.....(same as  SFL_Sequence...!)
                                   !     do we need both?



  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  ! --------------------------------------
  ! --- Interfaces
  ! --------------------------------------
  Interface SFL_LowerCase
     MODULE PROCEDURE LowerCase_Scalar, LowerCase_Vector
  End Interface
  !
  Interface SFL_UpperCase
     MODULE PROCEDURE UpperCase_Scalar, UpperCase_Vector
  End Interface
  !
  Interface SFL_Sequence
     MODULE PROCEDURE seq_sp, seq_dp
  End Interface
  !
  Interface SFL_Reverse
     MODULE PROCEDURE &
          mirror_integer, mirror_real_sp, mirror_real_dp,   &
          mirror_complex_sp, mirror_complex_dp
  end interface
  !
  Interface SFL_LinSpace
     MODULE PROCEDURE &
          LinSpace_sp, LinSpace_dp
  end interface

contains


  !--------------------------------------------------------------!
  Function SFL_BaseName(Full_Path, Suffix, Path)  Result(FileName)
  !--------------------------------------------------------------!
    !
    ! --- This routine removes UNIX path of the input character sting.
    !     If the optional argument Suffix is present, this will be  
    !     removed from the end of the input string if match is found.
    !     The optional argument path restuns the removed path if present.
    !     Recall to include the "." in the extension.....
    !
    !     Trailing and leading  blanks are ignored.... 
    !    
    !     This function MIMICS the PHP function of the same name....
    !
    !     Ingve Simonsen, UCI, Irvine, Aug. 2007.
    !
    !     SEE ALSO : PATH
    !
    implicit none
    character(len=*),                        Intent(In)  :: Full_Path
    character(len=*),              Optional, Intent(In)  :: Suffix
    character(len=len(Full_Path)), Optional, Intent(Out) :: Path
    ! Local 
    Character(len=*), parameter    :: UNIX="/"
    character(len=len(Full_Path))  :: FileName        
    Integer                        :: ipos, length

    FileName = trim(adjustl(Full_Path))
    length   = len(FileName)

    ! --- Remove the path
    ipos = INDEX(FileName, UNIX , BACK=.true.)
    if ( (ipos /= 0) .and. (ipos<length) )  then
       if (present(path))  path = FileName(1:ipos)
       FileName = FileName(ipos+1:length)
    endif


    ! --- Remove the Suffix if present
    if (present(Suffix)) then
       ipos = INDEX(FileName,Trim(Adjustl(Suffix)), BACK=.true.)
       if ( (ipos /= 0) .and. (ipos<=length) ) FileName = FileName(1:ipos-1)
    endif

    Return

  End Function SFL_BaseName
  !--------------------!



  function LowerCase_Scalar(in_string) result(out_string)
    !---------------------------------------------------------------------
    ! ROUTINE   :  LowerCase
    ! TYPE      :  Function
    ! SYSTEM    :  Fortran 90
    !
    ! PURPOSE   :  Tranlate a character variable to all lowercase letters.
    !              Non-alphabetic characters are not affected.  
    !
    ! SYNTAX    :  LowerCase
    !---------------------------------------------------------------------
    ! USES  Lib :  
    !       Mod : 
    !---------------------------------------------------------------------
    ! DATE      : 98.08.18
    ! LAST MOD  :  
    ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
    ! E-mail    : ingves@phys.ntnu.no
    !---------------------------------------------------------------------
    Implicit None
    Character(len=*),              intent(in)  :: in_string
    Character(len=len(in_string))              :: out_string
    ! Local
    integer                        :: i,ascii
    do i=1,len(in_string)
       ascii = ichar(in_string(i:i))
       if ((ascii>64).and.(ascii<91)) THEN
          out_string(i:i) = char(ascii+32)
       else
          out_string(i:i) = in_string(i:i)
       endif
    enddo
    return
  end function LowerCase_Scalar

  ! ----

  function LowerCase_Vector(in_string) result(out_string)
    Implicit None
    Character(len=*),              dimension(:),               intent(in)  :: in_string
    Character(len=len(in_string)), dimension(size(in_string,1))            :: out_string
    ! Local
    Integer :: i

    do i=1,size(in_string,1)
       out_string(i) = LowerCase_Scalar( out_string(i) )
    enddo

  End function LowerCase_Vector


  !
  ! ------
  !


  function UpperCase_Scalar(in_string) result(out_string)
    !---------------------------------------------------------------------
    ! ROUTINE   :  UpperCase
    ! TYPE      :  Function
    ! SYSTEM    :  Fortran 90
    !
    ! PURPOSE   :  Convert a character variable to all uppercase letters.
    !              Non-alphabetic characters are not affected.  
    !
    ! SYNTAX    :  UpperCase
    !---------------------------------------------------------------------
    ! USES  Lib :  
    !       Mod : 
    !---------------------------------------------------------------------
    ! DATE      : 98.08.18
    ! LAST MOD  :  
    ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
    ! E-mail    : ingves@phys.ntnu.no
    !---------------------------------------------------------------------
    Implicit none
    Character(len=*),               intent(in) :: in_string
    Character(len=len(in_string))              :: out_string
    ! Local
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
  end function UpperCase_Scalar

  ! ---

  function UpperCase_Vector(in_string) result(out_string)
    Implicit None
    Character(len=*),              dimension(:),               intent(in)  :: in_string
    Character(len=len(in_string)), dimension(size(in_string,1))            :: out_string
    ! Local
    Integer :: i

    do i=1,size(in_string,1)
       out_string(i) = UpperCase_Scalar( out_string(i) )
    enddo

  End function UpperCase_Vector

  !
  ! -----
  !


  recursive Function SFL_Count_Substrings(string,substring) result(occur)
    !
    ! The function returns the total number of occurances 
    ! of substring within string.  
    !
    ! I. Simonsen, Paris, Oct 2006
    !
    Implicit None
    character(len=*), intent(in)   :: string
    character(len=*), intent(in)   :: substring
    Integer                        :: occur
    ! Local
    Integer       :: ipos
    
    ! Default start value
    occur = 0 
    ! Handle the zero length substring case
    if (len(substring)==0)  return
    ! Recursice call....
    ipos = index(string,substring)
    if (ipos>0)        &
         occur = 1 +   &
         SFL_Count_Substrings(string(ipos+len(substring):len(string)),substring)
    return
  End Function SFL_Count_Substrings

  !
  ! ----
  !

  Function LinSpace_sp(a,b,n,ierr) result(res)
    Use SFL_Precision, only : wp=>sp
    ! Generates a vector of linearly spaced elements....
    Implicit None
    real(wp),          intent(in)  :: a,b
    integer,           intent(in)  :: n
    integer, optional, intent(out) :: ierr
    real(wp)             :: res(n)
    !Local
    real(wp)             :: delta
    integer              :: i
    if ((a<b) .and. (n>1) ) then
       delta = (b-a)/(n-1)
       do i=1,n
          res(i) = a+(i-1)*delta
       enddo
       if (present(ierr)) ierr = NO_ERROR_FOUND
    else
       if (present(ierr)) ierr = ERROR_FOUND
    end if
  End Function LinSpace_sp

  !
  ! ---
  !

  Function LinSpace_dp(a,b,n,ierr) result(res)
    Use SFL_Precision, only : wp=>dp
    ! Generates a vector of linearly spaced elements....
    Implicit None
    real(wp),          intent(in)   :: a,b
    integer,           intent(in)   :: n
    integer, optional, intent(out)  :: ierr
    real(wp)             :: res(n)
    !Local
    real(wp)             :: delta
    integer              :: i
    if ((a<b) .and. (n>1) ) then
       delta = (b-a)/(n-1)
       do i=1,n
          res(i) = a+(i-1)*delta
       enddo
       if (present(ierr)) ierr = NO_ERROR_FOUND
    else
       if (present(ierr)) ierr = ERROR_FOUND
    end if
  End Function LinSpace_dp
  !
  ! ---
  !

  subroutine SFL_String_Replace_All(string,find,substitute)
    !
    ! The routine replaces a substring (find) with another (subsitute) 
    ! one in the mother string. The length of "find" and "substitute" 
    ! must be the same and this length must be smaller or equal to that 
    ! of "string".
    !
    ! I. Simonsen, Dresden, April 2006
    !
    Integer, parameter :: Not_Found = 0
    character(len=*)   :: string, find, substitute
    Integer            :: ipos

    ! --- Do some checking
    if ( (len(find)/=len(substitute)) .or. (len(string)<len(find)) ) then
       Write(*,*) "WARNING : Strings not consistent in form"
    endif
   
    ! --- Main loop
    do 
       ipos = index(string,find)
       if (ipos==Not_Found) then
          ! Exit the loop
          EXIT
       else
          ! Do the replacment
          string(ipos:ipos+len(find)-1) = substitute
       endif
    enddo

  End subroutine SFL_String_Replace_All

  !
  ! ------
  !

  subroutine SFL_TimeString ( string )
    !
    ! This routine is modeled after timestring by John Burkardt.
    !   http://www.scs.fsu.edu/~burkardt/f_src/timestamp/timestamp.html
    !
    ! I. Simosnen has removed the AM/PM time and introduced 
    !    abriviations for the months
    ! 
    ! -----------------------------------------
    !  Example:
    !
    !    STRING = '11 Nov 2006  23:43:03.737'
    !
    !  Parameters:
    !
    !    Output, character ( len = * ) STRING, contains the date information.
    !    A character length of 30 should always be sufficient.
    !
    implicit none
    character(len=*), intent(out) ::  string
    ! Local
    character(len=3), parameter, dimension(12) :: month = (/     &
         'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', &
         'Sep', 'Oct', 'Nov', 'Dec' /)
    Logical, parameter    :: use_24hour_time=.true.   ! Use 24 hour clock?
    character(len=8)      :: ampm
    integer               :: d, h, m, mm
    integer               :: n,s, values(8), y

    ! --- call the f90 dat and time routine
    call date_and_time ( values = values )

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)
    mm = values(8)

    if (.not.use_24hour_time) call AMPM_Format(h,ampm)

    if (use_24hour_time ) then
       ! Use 24 hour time
       write ( string, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3)' ) &
            d, trim(month(m)), y, h, ':', n, ':', s, '.', mm
    else
       ! AM/PM adjusted
       write ( string, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
            d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )
    end if
    return

  contains

    Subroutine AMPM_Format(h,ampm)
      Implicit None
      Integer,          intent(inout) :: h
      Character(len=*), intent(out) :: ampm
      ! Local
      if ( h < 12 ) then
         ampm = 'AM'
      else if ( h == 12 ) then
         if ( n == 0 .and. s == 0 ) then
            ampm = 'Noon'
         else
            ampm = 'PM'
         end if
      else
         h = h - 12
         if ( h < 12 ) then
            ampm = 'PM'
         else if ( h == 12 ) then
            if ( n == 0 .and. s == 0 ) then
               ampm = 'Midnight'
            else
               ampm = 'AM'
            end if
         end if
      end if
    end Subroutine AMPM_Format

  End subroutine SFL_TimeString

  !
  ! ------
  !

  Subroutine SFL_TimeStamp(unit)
    !
    ! Writes a timestamp on output unit UNIT (default StdOut)
    !
    !Use SFL_Logical_Units, only : SFL_StdOut
    Use ISO_FORTRAN_ENV, only : Output_Unit
    Implicit None
    Integer, optional, intent(in) :: unit
    ! Local
    Integer           :: unit_local
    character(len=30) :: string

    call  SFL_Timestring ( string )
    ! Get the unit
    if (present(unit)) then
       unit_local = unit
    else
       unit_local = Output_Unit    ! 6    ! ??? SFL_StdOut
    end if
    ! Write out the timestamp
    write(unit_local,*) Trim( Adjustl( string ))

  End Subroutine SFL_TimeStamp

  !
  ! -----
  !

  !================================================================
  !      Sequence
  !================================================================

  !---------------------------------------------------------------------
  !DF90* SEQ -- Sequence
  !DF90+
  !
  ! ROUTINE   :  SEQ   (Generic)
  ! TYPE      :  Function
  !
  ! PURPOSE   :  Generate a sequence of data evenly spaced on a
  !              linear (type='lin') og logarithmic (type='log')
  !              scale
  !              This is a clone and extension of NR arth
  !
  ! SYNTAX    :  seq(a,b,N,[type])
  !   a           R   I      lower limit
  !   b           R   I      upper limit
  !   N           I   I      # elments
  !   type        S   I      Optional : 'lin' (default)  or 'log' 
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib :  Routines from package NMS
  !       Mod :
  !---------------------------------------------------------------------
  ! DATE      : 01/24/07
  ! LAST MOD  :
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------          


  !----------------------
  ! Single precision
  !----------------------
  function  seq_sp(a,b,N,type_opt) result(res)
    Use SFL_Precision, only : sp,dp
    implicit none
    real(sp), intent(in)  ::  a,b
    real(sp)              ::  res(N)
    integer               ::  N
    character*3, optional ::  type_opt 
    ! Local
    integer     :: i
    real(sp)    :: delta,base 
    character*3 :: type 

    ! Find type
    if (present(type_opt)) then
       type= type_opt
    else
       type ='lin'
    end if
    ! do the real work
    select case (type)
    case('lin')
       ! Linear scale
       delta = (b-a)/(N-1)
       res = (/ (a+delta*(i-1),i=1,N) /)
    case('log')
       ! Logarithmic (base 10) scale
       delta = (log10(b)-log10(a))/(N-1)
       res   = (/ (log10(a)+delta*(i-1),i=1,N) /)
       res   = 10._sp**res
    case default
       write(*,*) 'ERROR: SEQ : Type not supported'
       stop
    end select
  End function seq_sp


  !----------------------
  ! Double precision
  !----------------------
  function  seq_dp(a,b,N,type_opt) result(res)
    Use SFL_Precision, only : sp,dp
    implicit none
    real(dp), intent(in)  ::  a,b
    real(dp)              ::  res(N)
    integer               ::  N
    character*3, optional ::  type_opt 
    ! Local
    integer     :: i
    real(dp)    :: delta,base 
    character*3 :: type 

    ! Find type
    if (present(type_opt)) then
       type= type_opt
    else
       type ='lin'
    end if
    ! do the real work
    select case (type)
    case('lin')
       ! Linear scale
       delta = (b-a)/(N-1)
       res = (/ (a+delta*(i-1),i=1,N) /)
    case('log')
       ! Logarithmic (base 10) scale
       delta = (log10(b)-log10(a))/(N-1)
       res   = (/ (log10(a)+delta*(i-1),i=1,N) /)
       res   = 10._dp**res
    case default
       write(*,*) 'ERROR: SEQ : Type not supported'
       stop
    end select
  End function seq_dp


  !================================================================
  !      MIRROR
  !================================================================
  !---------------------------------------------------------------------
  !DF90* mirror -- reverse the order of an array
  !DF90+
  !
  ! ROUTINE   :  mirror   (Generic)
  ! TYPE      :  Function
  !
  ! PURPOSE   :  Reverse the order of an array

  !
  ! SYNTAX    :  mirror(a) result(b)
  !   a(:)        G   I     the input data
  !   direction   G   O     the output data
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib : 
  !       Mod :
  !---------------------------------------------------------------------
  ! DATE      : 01//08/12
  ! LAST MOD  :
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------     


  ! ---------- Integer ----------------------------
  function mirror_integer(a) result(b)
    implicit none
    integer, intent(in)     :: a(:)
    integer                 :: b(size(a,1))
    integer                 :: i,N
    N = size(a,1)
    do i=1,N
       b(N-i+1) = a(i)
    end do
  end function mirror_integer

  ! ---------- Real single prec. ------------------
  function mirror_real_sp(a) result(b)
    Use SFL_Precision, only : wp=>sp
    implicit none
    real(wp), intent(in)     :: a(:)
    real(wp)                 :: b(size(a,1))
    integer                  :: i,N
    N = size(a,1)
    do i=1,N
       b(N-i+1) = a(i)
    end do
  end function mirror_real_sp

  ! ---------- Real double prec. ------------------
  function mirror_real_dp(a) result(b)
    Use SFL_Precision, only : wp=>dp
    implicit none
    real(wp), intent(in)     :: a(:)
    real(wp)                 :: b(size(a,1))
    integer                  :: i,N
    N = size(a,1)
    do i=1,N
       b(N-i+1) = a(i)
    end do
  end function mirror_real_dp

  ! ---------- Complex single prec. ------------------
  function mirror_complex_sp(a) result(b)
    Use SFL_Precision, only : wp=>sp
    implicit none
    complex(wp), intent(in)     :: a(:)
    complex(wp)                 :: b(size(a,1))
    integer                     :: i,N
    N = size(a,1)
    do i=1,N
       b(N-i+1) = a(i)
    end do
  end function mirror_complex_sp

  ! ---------- Complex double prec. ------------------
  function mirror_complex_dp(a) result(b)
    Use SFL_Precision, only : wp=>dp
    implicit none
    complex(wp), intent(in)     :: a(:)
    complex(wp)                 :: b(size(a,1))
    integer                     :: i,N
    N = size(a,1)
    do i=1,N
       b(N-i+1) = a(i)
    end do
  end function mirror_complex_dp


End Module SFL_Util







!!$Program Seq_test
!!$  Use PM_Type, only : wp=>dp
!!$  Use PM_Div_Util
!!$  integer  :: i
!!$  integer, parameter :: N=25
!!$  real(wp) :: x(n)
!!$
!!$
!!$  x = seq(10._wp,1000._wp,N,'log')
!!$
!!$  do i=1,N
!!$     write(*,*) x(i),i
!!$  enddo
!!$
!!$  pause
!!$
!!$
!!$  call check(1<2,3>2,'tullball 1')
!!$!  call check(1<2,3<2,'tullball 2')
!!$  
!!$  pause
!!$
!!$  i=check_eq(1,1,1,1,'equal 1')
!!$  write(*,*) " N = ", i
!!$  i=check_eq(1,2,1,1,'not-equal 2')
!!$  write(*,*) " N = ", i
!!$
!!$End Program Seq_test
