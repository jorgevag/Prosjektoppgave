
!  $Id:$



! PROBLEM : See routine FirstSeparator
!           The problem seems to be that the F90 index-routine
!           called in eg. GetOption_Int_Vec ALWAYS return the last 
!           accurance (and not the first as is should when back=.false.)
!           This seems to be a problme when getoption is part of the 
!           PM-library. Why?
!           IS 05/12/05

! NOTE    :  This routine is converted from get_option.f90 of PMLib

!
! TODO    : -- should the option takevalues be renamed to "flag"?
!           -- add an option for determining the size of an input vector
!

Module SFL_Get_Options


  ! Private  
  USE SFL_Precision, only : sp, dp



  !=========== Derived Type Declaration ====================== 


  !---------------------------------------------------------------------
  !DF90* GetOption -- Getting option values
  !DF90+
  !
  ! ROUTINE   :  GetOption (Generic)
  ! TYPE      :  Subroutine
  !
  ! PURPOSE   :  GetOption is a genric routine for handling
  !              command line options in Fortran 90.
  !              It reads the appropriate option specified via the 
  !              AddOption routine. There is built in consistency checks.
  !              In order to use this function, the Option module has 
  !              to be initialised via the "ParseOption" routine. 
  !            
  !              The module by default supports the option "-help", 
  !              that prints out the usage information about the program.
  !              Depending on what is set via AddOption, an option may have a 
  !              default vaule or not, or it may, or may not, take values.
  !              GetOption can ONLY be used for options that take values 
  !              (and error will be produced otherwise). For an option that is 
  !              not specified on the command line, but has a default value set 
  !              by AddOption, the GetOption routine will return this 
  !              default value.
  !              
  ! SYNTAX    :  GetOption(value,optionID)
  !
  !   value       G   O     the value to be read
  !   OptionID    C   I     the option tag (without "-")
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib :  
  !       Mod : 
  !---------------------------------------------------------------------
  ! DATE      : 03/11/10
  ! LAST MOD  :  
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------
  interface GetOption
     module procedure &
          GetOption_int, GetOption_real_sp, GetOption_real_dp,   &
          GetOption_complex_sp, GetOption_complex_dp,            &
          GetOption_char, GetOption_logical,                     &
          GetOption_int_vec,                                     &
          GetOption_Real_Vec_sp, GetOption_Real_Vec_dp
  end interface

  Public  :: GetOption

 
  !---------------------------------------------------------------------
  !DF90* ParseOption -- The initialization of the Option-Module
  !DF90+
  !
  ! ROUTINE   :  InitOption 
  ! TYPE      :  Subroutine
  !
  ! PURPOSE   :  ParseOption is a subroutine that must be called AFTER
  !              all AddOption calls have been done, and before the Option
  !              Module can be used.
  !              derived type
  !              
  ! SYNTAX    :  InitOption(usagetext)
  !
  !   UsageText   C    I    Usage help string Excluding the prog. name
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib :  
  !       Mod : 
  !---------------------------------------------------------------------
  ! DATE      : 04/11/10
  ! LAST MOD  :  
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------
  Public  :: ParseOption


  !---------------------------------------------------------------------
  !DF90* PresentOption -- Check if an option (or flag) is present 
  !DF90+
  !
  ! ROUTINE   :  PresentOption
  ! TYPE      :  Function
  !
  ! PURPOSE   :  Check if an option (or flag) is present on the command line
  !              
  !
  !  function OptionPresent(optionID) result(res)
  !    character(len=*) :: optionID
  !    logical          :: res
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib :  
  !       Mod : 
  !---------------------------------------------------------------------
  ! DATE      : 04/11/01
  ! LAST MOD  :  
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------
  Public  :: PresentOption, PresentFlag    


  !---------------------------------------------------------------------
  !DF90* AddOption -- Set an option to be used by the calling program 
  !DF90+
  !
  ! ROUTINE   :  AddOption
  ! TYPE      :  Subroutine
  !
  ! PURPOSE   :  Set an option to be used by the calling program 
  !              
  !
  !   Subroutine AddOption(tag,text,takevalues,default)
  !      character(len=*)             :: tag
  !      character(len=*)             :: text
  !      logical,          optional   :: takevalues
  !      character(len=*), optional   :: default
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib :  
  !       Mod : 
  !---------------------------------------------------------------------
  ! DATE      : 03/01/12
  ! LAST MOD  :  
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------
  Public  :: AddOption


  !---------------------------------------------------------------------
  !DF90* AddFlag -- Set a flag to be used by the calling program 
  !DF90+
  !
  ! ROUTINE   :  AddFlag
  ! TYPE      :  Subroutine
  !
  ! PURPOSE   :  Set an flag to be used by the calling program 
  !              A flag is similar to an option, but takes NO values.
  !              This is indicated by takevalues being default set to
  !              false. Addflag is essentially equal to AddOption (for 
  !              a charater variable) where the take values is set to false.
  !
  !   Subroutine AddFlag(tag,text)
  !      character(len=*)             :: tag
  !      character(len=*)             :: text
  !
  !DF90-
  !---------------------------------------------------------------------
  ! SYSTEM    :  Fortran 90
  ! USES  Lib :  
  !       Mod : 
  !---------------------------------------------------------------------
  ! DATE      : 05/12/05
  ! LAST MOD  :  
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.ntnu.no
  !---------------------------------------------------------------------
  Public  :: AddFlag



  



  !--------------------------------
  ! Local ModuleGolobal varaibales
  !--------------------------------


  !=========== Derived Type Declaration ====================== 

  Integer, parameter :: TagCharLength     = 25  
  Integer, parameter :: TextCharLength    = 150
  Integer, parameter :: DefaultCharLength = 25
  Character(len=DefaultCharLength), parameter :: Not_Defined="No_Default"

  ! The derived type
  Type OptionListType
     Character(len=TagCharLength)       :: tag           ! Option tag
     Character(len=TextCharLength)      :: text          ! Option text 
     Character(len=DefaultCharLength)   :: default       ! Default value (if any)
     logical                            :: TakeValues    ! Does the option take values
  End Type OptionListType

  !--------------------

  Type(OptionListType),allocatable :: optionlist_local(:)
  character(len=1), parameter      :: optionprefix='-'
  character(len=1), parameter      :: valueseparator =','
  character(len=100),allocatable   :: sysargv_local(:)
  character(len=100)               :: usage_local
  character(len=100)               :: helptag="help"
  Integer                          :: Not_Found   = -99
  Integer                          :: ASCII_Start =  48           ! Ascii code for  0 
  Integer                          :: ASCII_Stop  =  57           ! Ascii code for  9 
  logical                          :: Option_Initialized=.false.


  
  !--------------------------------
  ! Private
  !--------------------------------
  private :: TagCharLength,TextCharLength,DefaultCharLength, Not_Defined
  Private :: optionlist_local, optionprefix, sysargv_local, usage_local, helptag
  Private :: Not_Found, ASCII_Start, ASCII_Stop, Option_Initialized


  Private ::  No_Valid_Option, OptionListNumber


  Contains

    !-------------------
    ! A HELP Routine
    !-------------------
    Function FirstSeparator(str,substr) result(res)
      ! Implements   index(str,substr,back=.false.)
      ! There seems to be some problem (unknow for what reason) with
      ! the above calling sequence when called from
      ! e.g. GetOption_Int_Vec et.al.
      
      Implicit none
      Integer            :: res
      character(len=*)   :: str
      character(len=1)  :: substr 
      ! Local
      integer            :: i, length, found

      ! If the substring has zero length
      length = len(substr)
      if (length==0) then
         res=1
         return
      end if
      
      found = 0
      do i=1,len(str)
         if (str(i:i+length-1)==substr) then
            found = i
            exit
         endif
      enddo
      res = found
    End Function FirstSeparator



 
    Subroutine AddOption(tag,text,takevalues,default)
      ! PURPOSE : To set up an option table to be initialized via 
      !           ParseOption
      Implicit None
      character(len=*)             :: tag
      character(len=*)             :: text
      logical,          optional   :: takevalues
      character(len=*), optional   :: default
      ! Local
      Type(OptionListType),allocatable :: optionlist_tmp(:)
      Integer                          :: N

      ! Add the help option the first time
      if (.not.allocated(optionlist_local)) then
         allocate(optionlist_local(1))
         optionlist_local(1)%tag         = "help"
         optionlist_local(1)%text        = "display this help and exit"
         optionlist_local(1)%default     = Not_Defined
         optionlist_local(1)%takevalues  = .false.
      endif

      ! Add the new option
      N=size( optionlist_local,1 ) 
      allocate( optionlist_tmp(N+1) )
      optionlist_tmp(1:N) =  optionlist_local
      optionlist_tmp(N+1)%tag=tag
      optionlist_tmp(N+1)%text=text
      ! -- The default field
      if (present(default)) then
         optionlist_tmp(N+1)%default = default
      else
         optionlist_tmp(N+1)%default = Not_Defined
      endif
      ! -- The takevalues field
      if (present(takevalues)) then
         optionlist_tmp(N+1)%takevalues = takevalues
      else
         optionlist_tmp(N+1)%takevalues = .false.   ! Default value
         if (present(default)) then
            if (default/=Not_Defined) then
               optionlist_tmp(N+1)%takevalues = .true.
            endif
         endif
      endif
      
      ! ERROR checking (default set and takevalues=.false. throw an ERROR)
      if ( (optionlist_tmp(N+1)%default /= Not_Defined) .and. (.not.optionlist_tmp(N+1)%takevalues) ) then
         ! Generate an ERROR
         Write(*,*)
         Write(*,*) " ERROR : Options to AddOption are inconsistent ! "
         Write(*,*)
         stop ""
      end if
 
      ! Copy the result to optionlist_local
      deallocate(optionlist_local)
      allocate(optionlist_local(N+1))
      optionlist_local = optionlist_tmp
      deallocate(optionlist_tmp)
               
    End Subroutine AddOption

    !--------------------------------------------------------
            
    Subroutine AddFlag(tag,text)
      ! PURPOSE : To set up an option table to be initialized via 
      !           ParseOption. Thnis routine adds a flag to the 
      !           option list.

      Implicit None
      character(len=*)             :: tag
      character(len=*)             :: text
      ! Local 
      logical                      :: takevalues


      ! A flag takes NO values
      takevalues   =  .false.

      call AddOption(tag,text,takevalues)

    End Subroutine AddFlag


    !--------------------------------------------------------


    Subroutine ParseOption( UsageText, Init_String )
      !
      ! UsageText      : Test pr be printed to informa bout program usage
      ! Init_String    : String to be used for initialize the Get_Option moduel
      !                  Useful when restarting calculations
      !
      implicit none         
      Character(len=*), intent(in)            :: usagetext
      Character(len=*), intent(in), optional  :: Init_String
      ! Local               
      Integer, external    :: iargc
      Integer              :: i, Noptions, Narg, iostat
      character(len=len(Init_String)) :: str


      ! Copies the usage statment
      usage_local     =  usagetext

      if  (.not. (Present(Init_String))) then
         ! Reads the command line arguments
         Narg = iargc()
         allocate( sysargv_local(0:Narg) )
         do i=0,Narg
            call getarg(i,sysargv_local(i) )
         enddo
      else
         ! Initialize from Init_String
         ! Useful when reinitialize a simulation, or initializing from file .....
         Narg   = iargc_fromstring( Init_String )
         allocate( sysargv_local(0:Narg) )
         do i=0,Narg
            call getarg_fromstring(i,sysargv_local(i), Init_String ) 
         enddo
      End if


      ! The option module initialized 
      Option_Initialized = .true.

      ! If no arguments given or if help is a command line argument, then write usage()
      If  ((Narg==0).or.(PresentOption(helptag)) ) call usage()

      ! Check if the supplied options are consntent
      call ConsistencyCheck()


    contains

            
      Subroutine getarg_fromstring(iarg, arg, source)
        !
        ! --- Like the Fortran built in function GETARG, 
        !     but this one obtains the input from the 
        !     provided string, source
        !  
        !     Ingve Simonsen, Paris, Oct. 2007
        !
        Implicit None
        Integer,          intent(in)  :: iarg
        Character(len=*), intent(out) :: arg
        Character(len=*), intent(in)  :: source
        ! Local
        Integer                       :: i, iblank
        Character(len=len(source))    :: stmp
        
        ! --- Initialize the output argument 
        !     (affect the behavior when iarg<0)
        arg = repeat(" ", len(arg) )
        
        ! --- Make a local copy 
        stmp = trim(adjustl( source ))
        
        do i=0,iarg
           
           ! Find the first blank in the string...
           iblank = scan( stmp, " " ) 
           
           if (i==iarg) then
              ! Argument found
              arg = stmp(1:iblank)
              exit
           else
              ! Reduce the string ....
              stmp = trim(adjustl( stmp(iblank:len(stmp)) ))
           end if
        End do
        
      End Subroutine getarg_fromstring

      ! ----
      

      Function iargc_fromstring(source) result(iarg)
        !
        ! --- Like the Fortran built in function IARGC, 
        !     but this one obtains the input from the 
        !     provided string, source
        !  
        !     Ingve Simonsen, Paris, Oct. 2007
        !
        Implicit None
        Character(len=*), intent(in)  :: source
        Integer                       :: iarg
        ! Local
        Integer                       :: i
        Character(len=len(source))    :: stmp
        
        i=0
        do       
           ! Get the argument.....
           call getarg_fromstring(i, stmp, source)
           
           if ( len(trim(adjustl(stmp))) /= 0) then
              ! Next argument
              i=i+1
           else
              ! No more arguments....
              exit
           end if
           
        enddo
        
        ! Function return value ....
        iarg = i-1
        
      End Function iargc_fromstring
      
    End Subroutine ParseOption


!!$    Subroutine ParseOption(usagetext)
!!$      implicit none         
!!$      character(len=*)     :: usagetext
!!$      ! Local               
!!$      Integer, external    :: iargc
!!$      Integer              :: i, Noptions, Narg
!!$
!!$      ! Copies the usage statment
!!$      usage_local     =  usagetext
!!$
!!$      ! Reads the command line arguments
!!$      Narg = iargc()
!!$      allocate( sysargv_local(0:Narg) )
!!$      do i=0,Narg
!!$         call getarg(i,sysargv_local(i) )
!!$      enddo
!!$
!!$      ! The option module initialized 
!!$      Option_Initialized = .true.
!!$
!!$      ! If no arguments given or if help is a command line argument, then write usage()
!!$      If  ((Narg==0).or.(PresentOption(helptag)) ) call usage()
!!$
!!$      ! Check if the supplied options are consntent
!!$      call ConsistencyCheck()
!!$
!!$    End Subroutine ParseOption

    !--------------------------------------------------------

    function PresentOption(optionID) result(res)
      ! PURPOSE : Check if the option is present
      !           Note that the option prefix is NOT included
      implicit none
      character(len=*) :: optionID
      logical          :: res
      ! Local
      Integer          :: i
      character(len=101) :: str
      res=.false.
      if (Option_Initialized) then
         do i=1,ubound(sysargv_local,1)
            str = optionprefix//trim(adjustl(optionID))
            if ( trim(adjustl(sysargv_local(i)))==trim(adjustl(str)) ) then
               res =.true.
            endif
         enddo
      else
         Write(*,*)
         Write(*,*) " Option Module has NOT been initialized via ParseOption !"
         Write(*,*)
         stop ""
      endif

    End function PresentOption


    !--------------------------------------------------------

    function PresentFlag(optionID) result(res)
      ! PURPOSE : Check if the flag is present
      !           Note that the option prefix is NOT included
      ! NOTe    : This routine is just a warpper to PrenstOption
      implicit none
      character(len=*) :: optionID
      logical          :: res

      res = PresentOption(optionID)

    End function PresentFlag


    !--------------------------------------------------------

    function OptionListNumber(optionID) result(res)
      ! PURPOSE : Returns the number of an option in the option list
      !           If the option is leagal is not cheacked for.
      !
      implicit none
      character(len=*) :: optionID
      integer          :: res
      ! Local
      Integer            :: i
      character(len=101) :: str

      res = -99
      if (Option_Initialized) then
         do i=1,size(optionlist_local)
            if ( trim(adjustl(optionID))==trim(adjustl(optionlist_local(i)%tag)) )  then
               res = i
               return 
            end if
         enddo
         ! Do this if no optionID found in the option list
         Write(*,*)
         Write(*,*) " Option ", trim(adjustl(optionID)), " not in OptionList !"
         Write(*,*)
         stop "" 
      else
         Write(*,*)
         Write(*,*) " Option Module not initialized via InitOption !"
         Write(*,*)
         stop "" 
      endif

    End function OptionListNumber



    !--------------------------------------------------------
    !   For Generic Subroutines/Functions
    !--------------------------------------------------------



    Subroutine GetOption_Char(value,optionID)
      ! PURPOSE : The main routine for getting options
      Implicit none
      character(len=100) :: value
      character(len=*)   :: optionID
      ! Local
      Integer            :: i,ClaPos,ListPos
      character(len=100) :: str
      Logical            :: Found
      !
      ! Before calling this routine, one should have checked the following
      ! (done in the routine ConsistencyCheck)
      !    1. that optionID is a legal option
      !    2. an value is provided for tags that are present and takes values
      !    3. tags should not appear multiple times
      !

      ! First check if the optionList has been properly initialized
      if (.not. Option_Initialized ) then
         Write(*,*) 
         Write(*,*) "   ERROR : ParseOption must be called before calling GetOption ! "
         Write(*,*)
         stop "" 
      endif

      ! Check if the optionId provided is valid
      Found = .false.
      do i=1,size(optionlist_local)
         if ( trim(adjustl(optionlist_local(i)%tag))==trim(adjustl(optionID)) ) Found=.true.
      enddo
      if (.not.Found) then
         Write(*,*) 
         Write(*,*) "   ERROR : Option ", optionprefix//trim(adjustl(optionID)), &
                           " provided in the call to GetOption "
         Write(*,*) "           is not consistent with what is set by AddOption !"
         Write(*,*)
         stop "" 
      endif

      ! Lets go ahead and "get the option"
      call FindPositionOption(optionID,ClaPos,ListPos) 

      ! If optionID is given as a command line option
      if (ClaPos/=Not_Found) then
         if (OptionList_local(ListPos)%TakeValues) then
            ! If the tag takes values, read it
            value = Trim(adjustl( sysargv_local(ClaPos+1) ))
         else 
            ! One should NOT try to get values for an option that takes NO values
            Write(*,*) 
            Write(*,*) "   ERROR : Option (or flag) ", optionprefix//trim(adjustl(optionID)), &
                 " takes no values"
            Write(*,*) "           GetOption should thus NOT be used on this option (or flag)! "
            Write(*,*)
            stop ""
         endif
      endif

      ! If optionID is NOT given as a command line option
      if (ClaPos==Not_Found) then
         if (OptionList_local(ListPos)%default/=Not_Defined) then
            ! Get default value
            value = Trim(adjustl( OptionList_local(ListPos)%default ))
         else
            ! If option takes values write an error message and stop
            Write(*,*)
            Write(*,*) " ERROR : Option ",optionprefix//trim(adjustl(optionID)), &
                        " is given no value or default setting  !  " 
            Write(*,*)
            stop ""
         endif
      endif
 
    End Subroutine GetOption_Char


    !===========================================================


    Subroutine GetOption_logical(value,optionID)
      ! PURPOSE : The main routine for getting options
      Implicit none
      logical            :: value
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char

      call GetOption_Char(value_char,optionID)
      value = char2logical( Trim(Adjustl(value_char)) )

    contains

      ! COPY OF : Use PM_Util,  only : char2logical
      function char2logical(string) result(logic)
        Implicit None
        character*(*)     :: string 
        logical           :: logic
        string = trim(adjustl(string))
        if (string(1:7)=='.false.') then
           logic =.false.
        elseif (string(1:6)=='.true.') then
           logic =.true.
        else
           write(*,*) "EROOR in the subroutine char2logical !!!"
           write(*,*) "The input string does not contain any logical expression "
           stop ""
       endif
     end function char2logical

    End Subroutine GetOption_logical

    !------------------------------------------------------

    Subroutine GetOption_Int(value,optionID)
      ! PURPOSE : The main routine for getting options
      Implicit none
      Integer            :: value
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char

      call GetOption_Char(value_char,optionID)
      value = char2int( Trim(Adjustl(value_char)) )

    contains

      ! COPY OF : Use PM_Util,  only : char2int
      function char2int(string) result(number)
        Implicit None
        character*(*)      :: string
        character(len=10)  :: new_string
        integer            :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2int

    End Subroutine GetOption_Int

    !------------------------------------------------------


    Subroutine GetOption_Int_Vec(value,optionID)
      ! PURPOSE : The main routine for getting options
      Implicit none
      Integer            :: value(:)
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char
      Integer            :: pos,i,N,last

      call GetOption_Char(value_char,optionID)
      value_char = trim(adjustl(value_char))
      last = len(trim(adjustl(value_char)))
      N = size(value)
      do i=1,N-1 
         !pos=index(value_char,valueseparator)  ! Is not working propperly
         pos = FirstSeparator(value_char,valueseparator)
         if ( (pos==0).or.(value_char(last:last)==valueseparator) ) then
            ! Not enough values found (or last character is ',')
            Write(*,*) " ERROR : Too few values given for option ", trim(adjustl(optionID))
            Write(*,*) "         You should have provided :",size(value)
            Stop ""
         Endif
         value(i) = char2int( Trim(Adjustl( value_char(:pos) ) ) )
         if (i==N-1) then
            ! Read the last argument
            value(N) = char2int( Trim(Adjustl( value_char(pos+1:) ) ) )
         else
            value_char = value_char(pos+1:)
         endif
      enddo
    contains
      ! COPY OF : Use PM_Util,  only : char2int
      function char2int(string) result(number)
        Implicit None
        character*(*)      :: string
        character(len=10)  :: new_string
        integer            :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2int

    End Subroutine GetOption_Int_Vec

    !------------------------------------------------------


    Subroutine GetOption_real_sp(value,optionID)
      ! PURPOSE : The main routine for getting options
      !Use SFL_Precision,  only : wp=>sp
      Implicit none
      Integer, parameter :: wp=sp
      real(wp)           :: value
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char

      call GetOption_Char(value_char,optionID)
      value = char2sreal( Trim(Adjustl(value_char)) )

    contains

      ! COPY OF : Use PM_Util,  only : char2sreal
      function char2sreal(string) result(number)
        ! USE SFL_Precision, only : sp
        Implicit None
        character*(*)      :: string 
        character(len=20)  :: new_string
        real(sp)           :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2sreal
      
    End Subroutine GetOption_real_sp

    !------------------------------------------------------

    Subroutine GetOption_Real_Vec_sp(value,optionID)
      ! PURPOSE : The main routine for getting options
      ! Use PM_Type,  only : wp=>sp
      Implicit none
      Integer, parameter :: wp=sp
      Real(wp)           :: value(:)
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char
      Integer            :: pos,i,N,last

      call GetOption_Char(value_char,optionID)
      value_char = trim(adjustl(value_char))
      last = len(trim(adjustl(value_char)))
      N = size(value)
      do i=1,N-1 
         !pos=index(value_char,valueseparator)  ! Is not working propperly
         pos = FirstSeparator(value_char,valueseparator)
         if ( (pos==0).or.(value_char(last:last)==valueseparator) ) then
            ! Not enough values found (or last character is ',')
            Write(*,*) " ERROR : Too few values given for option ", trim(adjustl(optionID))
            Write(*,*) "         You should have provided :",size(value)
            Stop ""
         Endif
         value(i) = char2sreal( Trim(Adjustl( value_char(:pos) ) ) )
         if (i==N-1) then
            ! Read the last argument
            value(N) = char2sreal( Trim(Adjustl( value_char(pos+1:) ) ) )
         else
            value_char = value_char(pos+1:)
         endif
      enddo

    contains

      ! COPY OF : Use PM_Util,  only : char2sreal
      function char2sreal(string) result(number)
        ! USE SFL_Precision, only : sp
        Implicit None
        character*(*)      :: string 
        character(len=20)  :: new_string
        real(sp)           :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2sreal

    End Subroutine GetOption_Real_Vec_sp

    !------------------------------------------------------

    Subroutine GetOption_real_dp(value,optionID)
      ! PURPOSE : The main routine for getting options
      ! Use PM_Type,  only : wp=>dp
      Implicit none
      Integer, parameter :: wp=dp
      real(wp)           :: value
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char

      call GetOption_Char(value_char,optionID)
      value = char2dreal( Trim(Adjustl(value_char)) )

    contains

      ! COPY OF : Use PM_Util,  only : char2dreal
      function char2dreal(string) result(number)
        ! USE SFL_Precision, only : dp
        Implicit None
        character*(*)      :: string 
        character(len=50)  :: new_string
        real(dp)           :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2dreal

    End Subroutine GetOption_real_dp

    !------------------------------------------------------



   Subroutine GetOption_Real_Vec_dp(value,optionID)
      ! PURPOSE : The main routine for getting options
      ! Use SFL_Precision,  only : wp=>dp
      Implicit none
      Integer, parameter :: wp=dp
      Real(wp)           :: value(:)
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char
      Integer            :: pos,i,N,last

      call GetOption_Char(value_char,optionID)
      value_char = trim(adjustl(value_char))
      last = len(trim(adjustl(value_char)))
      N = size(value)
      do i=1,N-1 
         !pos=index(value_char,valueseparator)  ! Is not working propperly
         pos = FirstSeparator(value_char,valueseparator)
         if ( (pos==0).or.(value_char(last:last)==valueseparator) ) then
            ! Not enough values found (or last character is ',')
            Write(*,*) " ERROR : Too few values given for option ", trim(adjustl(optionID))
            Write(*,*) "         You should have provided :",size(value)
            Stop ""
         Endif
         value(i) = char2dreal( Trim(Adjustl( value_char(:pos) ) ) )
         if (i==N-1) then
            ! Read the last argument
            value(N) = char2dreal( Trim(Adjustl( value_char(pos+1:) ) ) )
         else
            value_char = value_char(pos+1:)
         endif
      enddo

    contains

      ! COPY OF : Use PM_Util,  only : char2dreal
      function char2dreal(string) result(number)
        ! USE SFL_Precision, only : dp
        Implicit None
        character*(*)      :: string 
        character(len=50)  :: new_string
        real(dp)           :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2dreal


    End Subroutine GetOption_Real_Vec_dp



    !------------------------------------------------------

    Subroutine GetOption_complex_sp(value,optionID)
      ! PURPOSE : The main routine for getting options
      ! Use SFL_Precision,  only : wp=>sp
      Implicit none
      Integer, parameter :: wp=sp
      complex(wp)        :: value
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char

      call GetOption_Char(value_char,optionID)
      value = char2scomplex( Trim(Adjustl(value_char)) )

    contains

      ! COPY OF : Use PM_Util,  only : char2scomplex
      function char2scomplex(string) result(number)
        ! USE SFL_Precision, only : sp
        Implicit None
        character*(*)      :: string 
        character(len=40)  :: new_string
        complex(sp)        :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2scomplex

    End Subroutine GetOption_complex_sp

    !------------------------------------------------------

    Subroutine GetOption_complex_dp(value,optionID)
      ! PURPOSE : The main routine for getting options
      !Use SFL_Precision,  only : wp=>dp
      Implicit none
      Integer, parameter :: wp=dp
      complex(wp)        :: value
      character(len=*)   :: optionID
      ! Local
      Character(len=100) :: value_char

      call GetOption_Char(value_char,optionID)
      value = char2dcomplex( Trim(Adjustl(value_char)) )

    contains

      ! COPY OF : Use PM_Util,  only : char2dcomplex
      function char2dcomplex(string) result(number)
        ! USE SFL_Precision, only : dp
        Implicit None
        character*(*)      :: string 
        character(len=40)  :: new_string
        complex(dp)        :: number
        new_string = trim(adjustl(string))
        read(new_string,*) number
      end function char2dcomplex

    End Subroutine GetOption_complex_dp


    !--------------------------------------------------------
    !  UTIL Subroutines/Functions
    !--------------------------------------------------------


    Subroutine ConsistencyCheck()
      ! PURPOSE : To check if the supplied options are consistent
      !    1. that the optionID is a legal option
      !    2. a value is provided for tags that are present and takes values
      !    3. tags should not appear multiple times
      !    4. if option "default" present, then  WRITE the default values
      !    5. if option "help" present, then  WRITE the Usage()
      !    6. check if ALL required options are present (NOT IMPLEMENTED)
      !
      implicit none
      ! Local
      Integer, parameter      :: CharSize=100
      Integer                 :: i, j, length, dim, iOptionPos
      character(len=CharSize) :: str, next, optionID
      logical                 :: Error

      dim = size(sysargv_local)-1  ! we use -1 here since the vect. starts at index 0

      ! Check that the same option appears only once
      do i=1, dim
         str = trim(adjustl(sysargv_local(i)))
         if ( (str(1:1)==optionprefix).and.(.not.IsIntegerChar(str(2:2))) ) then
            ! Option Found
            optionID = trim(adjustl(str))
            do j=i+1,dim
               ! Loop through the rest of the command line arguments
               next = trim(adjustl(sysargv_local(j)))
               if ( (next(1:1)==optionprefix).and.(.not.IsIntegerChar(next(2:2))) ) then
                  if ( trim(adjustl(next)) == trim(adjustl(optionID)) ) then
                     ! Error
                     Write(*,*)
                     Write(*,*) " ERROR : Option ", trim(adjustl(sysargv_local(i))), &
                          " appears more then once ! "
                     Write(*,*)
                     stop ""
                  end if
               endif
            enddo
         end if
      enddo

      ! Check that all options are legal
      ! Options are here detected as the option prefix (e.g. "-") 
      !   followed by a non-numeric character
      do i=1, dim
         str = trim(adjustl(sysargv_local(i)))
         if ( (str(1:1)==optionprefix).and.(.not.IsIntegerChar(str(2:2))) ) then
            ! Check that option is legal 
            length=len(str)
            str=trim(str(2:length))
            if (.not.ValidOption(str)) then
               Write(*,*)
               Write(*,*) " ERROR : Option ", optionprefix//trim(str(2:length)), " is NOT valid !"
               Write(*,*)
               stop ""
            endif
         endif
      enddo


      ! Check that a value is provided if the option takes values
      !    We check if the next command line argument is a valid Option
      Error = .false.
      do i=1,dim     ! NOTE : i is NOT a loop index for OptionList.                                        
         str = trim(adjustl(sysargv_local(i)))
         if ( (str(1:1)==optionprefix).and.(.not.IsIntegerChar(str(2:2))) ) then
            ! For an option tag, check if it takes values
            iOptionPos = OptionListNumber(str(2:CharSize))            
            if (optionlist_local(iOptionPos)%takevalues) then
               if (i==dim) then
                  ! At the end of the command line argument list => error
                  Error = .true.
               else
                  ! If the next command line option is a valid option => error
                  next   = trim(adjustl(sysargv_local(i+1)))  ! next argument
                  if ( (next(1:1)==optionprefix).and.(.not.IsIntegerChar(next(2:2))) )  then
                     length = len(next)
                     next   = trim(next(2:length))
                     if (ValidOption(next)) Error = .true.
                  endif
               endif
               ! An error is detected
               If (Error) then
                  Write(*,*) 
                  Write(*,*) " ERROR : Option ", trim(adjustl(sysargv_local(i))), " is given no value !"
                  Write(*,*) 
                  stop ""
               endif
            endif
         endif
      enddo

    End Subroutine ConsistencyCheck
    
    !-----------------------------------------

    Function  ValidOption(optionID) result(res) 
      ! PURPOSE : Check if the optionID is valid 
      !           Note that the option prefix is NOT included
      implicit none
      character(len=*) :: optionID
      logical          :: res
      ! Local
      Integer            :: i

      res = .false.
      if (Option_Initialized) then    ! Probably not needed, but just in case
         do i=1,size(optionlist_local)
            if ( ( trim(adjustl(optionlist_local(i)%tag))==trim(adjustl(optionID)) ) .or.  &
                 ( trim(adjustl(optionID)) == optionprefix//trim(adjustl(helptag)) ) ) then
               ! Option found (or optionID = helptag)
               res = .true.
               return
            endif
         enddo
         ! If this part of the function is reached, optionID is NOT valid => Error
         Write(*,*)
         Write(*,*) " ERROR : Option ", optionprefix//trim(adjustl(optionID)), " is NOT valid (or initialized) !"
         Write(*,*)
         stop ""
      endif
    End Function ValidOption

    !-----------------------------------------

    Subroutine Usage()
      Implicit none
      Integer :: i
      character(len=15) :: str
      character(len=3)  :: offset="  "
      Write(*,*) 
      Write(*,*) " USAGE : ", trim(adjustl(sysargv_local(0))),  " ", trim(adjustl(usage_local)) 
      Write(*,*) 
      Write(*,*) " Options:"
      do i=1,size(optionlist_local)
         str=offset//optionprefix//trim(adjustl(optionlist_local(i)%tag))
         if (optionlist_local(i)%default==Not_Defined) then
            Write(*,'(a15,G)')  str, trim(adjustl(optionlist_local(i)%text))
         else
!            Write(*,'(a15,G)')  str, trim(adjustl(optionlist_local(i)%text)) 
!            Write(*,'(a15,G)') " ", '  Default : '//trim(adjustl(optionlist_local(i)%default))    
            Write(*,'(a15,G)')  str, trim(adjustl(optionlist_local(i)%text))//"   ["//trim(adjustl(optionlist_local(i)%default))//"]"
         end if
      enddo
      Write(*,*) 
      stop ""
    End Subroutine Usage

    !-----------------------------------------

    Subroutine FindPositionOption(optionID,ClaPos,ListPos) 
      ! PURPOSE : Check the position of an option both in the 
      !           command Line (ClaPos) and option list (ListPos)
      !           -99 is returned if options not found
      implicit none
      character(len=*) :: optionID
      integer          :: ClaPos    ! Command Line pos
      Integer          :: ListPos   ! Option List position   
      ! Local
      Integer            :: i
      character(len=101) :: str

      ClaPos  = Not_Found
      ListPos = Not_Found
      ! Find the position in the ListOption
      Do i=1,size(OptionList_local)
         if ( trim(adjustl(OptionList_local(i)%tag)) == trim(adjustl(optionID)) ) then
             ListPos = i
             exit
          endif
      End Do
      if (ListPos == Not_Found ) then
         Write(*,*)
         Write(*,*) " Option ", trim(adjustl(optionID)), " is not a valid option !"
         Write(*,*)
         stop ""
         !call Usage()
      endif
      ! Find the position in the Command line argument list      
      do i=1,ubound(sysargv_local,1)
         str = optionprefix//optionID
         if ( trim(adjustl(sysargv_local(i)))==trim(adjustl(str)) ) then
            ClaPos = i
            exit
         endif
      enddo
    End Subroutine FindPositionOption

    !-----------------------------------------

    Function IsIntegerChar(str) result(res)
      ! PURPOSE : To check if the string str is a valid number 
      Implicit None
      character(len=1)   :: str 
      logical            :: res
      ! Local
      Integer, parameter :: ASCII_Start = 48         ! Ascii code for  0 
      Integer, parameter :: ASCII_Stop  = 57         ! Ascii code for  9 
      Integer            :: AsciiCode   
      
      res        = .false.
      AsciiCode  = IACHAR(str) 
      ! str is in the range "0-9"
      if ( (ASCII_Start<=AsciiCode) .and. (AsciiCode<=ASCII_Stop))  then
         res = .true.
      endif
     
    End Function IsIntegerChar


End Module SFL_Get_Options



!======================================================


!!$
!!$Program MainGetOptions
!!$  Use SFL_Precision, only : wp=>dp
!!$  Use PM_Util
!!$  Use SFL_Get_Options
!!$  Implicit none
!!$
!!$  Type(OptionListType) :: OptionList(5)
!!$
!!$  Integer             :: i, N=3,ivec(3)
!!$  character(len=100)  :: UsageStr
!!$  real(wp)            :: freq
!!$  real(wp)            :: vec(3)
!!$  complex(wp)         :: ctmp
!!$  character(len=100) :: file="empty"
!!$
!!$
!!$
!!$  Write(*,*) " Before : "
!!$  Write(*,*) "---------"
!!$  Write(*,*) " f    : " , freq
!!$  Write(*,*) " N    : " , N
!!$  Write(*,*) " if   : " , Trim(adjustl(file))
!!$  Write(*,*) " c    : " , ctmp
!!$
!!$
!!$  ! Should be called AddOption
!!$  call AddOption("f","Frequency",takevalues=.true.)
!!$  call AddOption("N","# samples",default="200")
!!$  call AddOption("if","Input File", default="TestFile.nc")
!!$  call AddOption("p","Print results...")
!!$  call AddOption("c","A complex number",default="(3.,4.)")
!!$
!!$  call AddOption("iv","Int Vector",takevalues=.true.)
!!$  call AddOption("v","Real Vector",takevalues=.true.)
!!$
!!$  call ParseOption("[option] < datafile")
!!$
!!$
!!$  call GetOption(freq,"f")
!!$  call GetOption(N,"N")
!!$  call GetOption(file,"if")
!!$  call GetOption(ctmp,"c")
!!$  call GetOption(ivec,"iv")
!!$  call GetOption(vec,"v")
!!$
!!$
!!$  if (PresentOption("p")) then
!!$     Write(*,*) "Option -p is present "
!!$  endif
!!$
!!$
!!$  Write(*,*) " After : "
!!$  Write(*,*) "---------"
!!$  Write(*,*) " f    : " , freq
!!$  Write(*,*) " N    : " , N
!!$  Write(*,*) " if   : " , Trim(adjustl(file))
!!$  Write(*,*) " c    : " , ctmp
!!$  Write(*,*) " iv    : " , ivec
!!$  Write(*,*) " v    : " , vec
!!$
!!$
!!$End Program MainGetOptions


