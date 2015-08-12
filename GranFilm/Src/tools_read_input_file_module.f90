! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module contains various tools for checking various 
!     properties of the input parameter file used by GranFilm in the 
!     sif format ("Scientific Input Format")
! 
! --- AUTHOR : Ingve Simosnen, Paris, Mai 2012.
!
! ----------------------------------------------------------
!


!-----------------------------------!
Module Tools_Read_Input_File_Module
!-----------------------------------!



  ! --- The Use Statements global to the module
  !Use Shared, only : wp, Param


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: String_Value_Length
  Public :: NameList_Exists
  Public :: Field_in_NameList_Exists
  Public :: Get_StringValue_for_NameList_Field


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private



  ! --- Some constants
  Integer, parameter :: String_Value_Length = 300   ! The maximum length of the string values
  Integer, parameter :: Line_Length = 1000          ! The maximum length of a line of the parameter file




Contains


  ! --------------------------------------------------!
  Function NameList_Exists(fid, NML)   Result(Res) 
  ! --------------------------------------------------!
    ! 
    ! --- This function checks if a NameList exists 
    !     in an already opened (parameter) file of 
    !     fileID 
    !
    !     Ingve Simonsen, Paris, Apr 2012
    !
    Implicit None
    Integer,          Intent(In) :: fid
    Character(len=*), Intent(In) :: NML   ! = NameList
    Logical                      :: Res
    ! --- Local
    Integer :: iostat
    Character(len=Line_Length) :: line

    ! --- Default Value
    Res = .false.

    ! --- Rewind the file
    rewind(fid)
    
    !--- Read the file line by line and process it
    iostat = 0
    line_loop : do while (iostat == 0 )

       ! --- Read a line
       read(fid,"(A)",iostat=iostat) line

       ! --- Process line......
       if ( trim(adjustl(line)) == "&"//trim(adjustl(NML)) ) then
          Res = .true.
          exit
       End if

    enddo line_loop
    
  End Function NameList_Exists
  ! --------------------------------------------------!


  !
  ! ---
  !



  ! ------------------------------------------------------------------!
  Function Field_in_NameList_Exists( fid, NML, Field)   Result(Res) 
  ! ------------------------------------------------------------------!
    ! 
    ! --- This function checks if a field (Field) exists in 
    !     a NameList (NML) in an already opened (parameter) file (fid)
    !
    !     Note to find a field means that the line should start by
    !     Field and the next non-blank charcter being "=".
    !
    !     Ingve Simonsen, Paris, Apr 2012
    !
    Implicit None
    Integer,          Intent(In) :: fid
    Character(len=*), Intent(In) :: NML   ! = NameList
    Character(len=*), Intent(In) :: Field ! = Field
    Logical                      :: Res
    ! --- Local
    Integer                     :: iostat, Field_Length, i
    Character(len=Line_Length)  :: line
    Character(len=1)            :: ch
    Character(len=len(Field)+1) :: Target_Str
    Logical                     :: Found_NML


    ! --- Initialize
    Field_Length = len( Trim(adjustl(Field)) )

    ! --- Default Value
    Res = .false.

    ! --- Rewind the file
    rewind(fid)

 
    ! --- Find the namelist (NML)
    ! ----------------------------------
    iostat = 0
    ! ... Read the file line by line and process it
    do while (iostat == 0 )

       ! --- Read a line
       read(fid,"(A)",iostat=iostat) line

       ! --- Process line......
       if ( trim(adjustl(line)) == "&"//trim(adjustl(NML)) ) then
          Found_NML = .true.
          exit
       End if

    enddo


    ! --- Find the field (Field)
    ! ---------------------------------
    If (Found_NML) then
 
      ! ... Read the file line by line and process it
       do while (iostat == 0 )
       
          ! --- Read a line
          read(fid,"(A)",iostat=iostat) line
                  
          ! --- Process line......
          ! -----------------------------------
          ! ... Return .false. if end-of-namelist is found
          If ( trim(adjustl(line)) == "/" ) then
             Res = .false.
             Return
          End if
          !
          ! Find the first Field_Length+1 non-blank charcters of line
          Target_Str = " "
          do i=1,len(line)
             ch =line(i:i)
             if (ch /= " ") then
                Target_Str = trim(Target_Str) // ch
             endif
             ! Are all characters of Target_Str non-blanks?
             if (len(trim(adjustl(Target_Str))) == Field_Length+1 ) exit
          enddo
          !
          ! ... Is the field found? 
          if ( Target_Str == trim(adjustl(Field))//"=" ) then
              Res=.true.
             exit
          End if

       enddo
      
    End If

    
  End Function Field_in_NameList_Exists
  ! --------------------------------------------------!




  ! -----------------------------------------------------------------------------!
  Subroutine Get_StringValue_for_NameList_Field( fid, String_Value, NML, Field )  
  ! ------------------------------------------------------------------------------!
    ! 
    ! --- This subroutine returns in String_Value the text field
    !     provided for a field (Field) in a NameList (NML) as given in
    !     an already opened (parameter) file (of sif-format).
    !
    !     Note to find a field means that the line should start by
    !     Field and the next non-blank charcter being "=".
    !
    !     Ingve Simonsen, Paris, Apr 2012
    !
    Implicit None
    Integer,                            Intent(In) :: fid
    Character(len=*),                   Intent(In)  :: NML   ! = NameList
    Character(len=*),                   Intent(In)  :: Field ! = Field
    Character(len=String_Value_Length), Intent(Out) :: String_Value
    ! --- Local
    Integer                     :: iostat, Field_Length, i
    Character(len=1000)         :: line
    Character(len=1)            :: ch
    Character(len=len(Field)+1) :: Target_Str
    Logical                     :: Found_NML


    ! --- Initialize
    Field_Length = len( Trim(adjustl(Field)) )

    ! --- Default Value
    String_Value = " " 


    ! --- Rewind the file
    rewind(fid)

 
    ! --- Find the namelist (NML)
    ! ----------------------------------
    iostat = 0
    ! ... Read the file line by line and process it
    do while (iostat == 0 )

       ! --- Read a line
       read(fid,"(A)",iostat=iostat) line

       ! --- Process line......
       if ( trim(adjustl(line)) == "&"//trim(adjustl(NML)) ) then
          Found_NML = .true.
          exit
       End if

    enddo


    ! --- Find the field (Field)
    ! ---------------------------------
    If (Found_NML) then
 
      ! ... Read the file line by line and process it
       do while (iostat == 0 )
       
          ! --- Read a line
          read(fid,"(A)",iostat=iostat) line
                  
          ! --- Process line......
          ! -----------------------------------
          ! ... Exit if end-of-namelist is found
          If ( trim(adjustl(line)) == "/" ) then
             String_Value = " "
             Return
          End if
          !
          ! Find the first Field_Length+1 non-blank charcters of line
          Target_Str = " "
          do i=1,len(line)
             ch =line(i:i)
             if (ch /= " ") then
                Target_Str = trim(Target_Str) // ch
             endif
             ! Are all characters of Target_Str non-blanks?
             if (len(trim(adjustl(Target_Str))) == Field_Length+1 ) exit
          enddo
          !
          ! ... Is the field found? 
          if ( Target_Str == trim(adjustl(Field))//"=" ) then

             i =  scan(line, "=")
             if ( (i/=0) .and. (i<len(line)) ) then
                ! --- Field value found
                ! 
                ! ... If strings : remove traling " and 's
                line = trim(adjustl(line(i+1:len(line))))
                if ( line(1:1) == '"' .or. line(1:1) == "'" )  then
                   i    = len(line)
                   line = Trim(Adjustr(line(2:i)))
                endif
                line = Trim(Adjustr(line))
                i    = len(line)
                if ( line(i:i) == '"' .or. line(i:i) == "'" )  then
                   line = Trim(adjustl(line(1:i-1)))
                end if
                String_Value = Trim(adjustl(line))
                Return
             else
                ! --- Field value NOT found
                String_Value = " "
                Return
             End if

          End if

       enddo
      
    End If

    
  End Subroutine  Get_StringValue_for_NameList_Field
  ! --------------------------------------------------!




End Module Tools_Read_Input_File_Module
!---------------------------------------!
