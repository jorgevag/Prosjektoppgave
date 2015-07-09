Module Read_Unknown_NML_Module


Contains

  Subroutine Read_Unknown_Named_NML(fid,NML)
    Implicit None
    Integer,           Intent(In) :: fid
    Character(len=*),  Intent(In) :: NML
    ! --- Local
    Integer            :: iostat
    Character(len=250) :: line
    Character(len=3000):: Internal_File
    Logical            :: in_nml

    Integer    :: Delta
    Real       :: Value
    Character(len=20) :: String
    
    namelist /Material/ Value, Delta, String

    
    ! --- Rewind the file
    rewind(fid)
    

    !--- Read the file line by line and process it
    iostat = 0
    in_nml = .false.
    line_loop : do while (iostat >= 0 )
       
       ! --- Read a line
       read(fid,"(A)",iostat=iostat) line

       ! --- Left Adjust the line
       line = adjustl(line)


       ! ------------------------
       ! --- The NML start-field
       ! ------------------------

       !Write(*,*) "Comparing : ", line(1:len(trim(adjustl(nml)))+1) , "#&"//trim(adjustl(nml))  

       ! --- Are we in the NML?
       if ( line(1:len(trim(adjustl(nml)))+1) == "&"//trim(adjustl(nml)) ) then
          ! --- The start of the NML
          in_nml = .true.
          ! ---The first line of the Internal File
          Internal_File =  "&Material "
          ! --- Process the rest of the line
          Internal_File =  trim(Adjustl(Internal_File)) // " " &
               // Trim(Adjustl(line( len(trim(adjustl(nml)))+2:) )) 
          ! --- Are we at the end of the NML?
          if (at_the_end_of_nml()) exit

       elseif (in_nml) then           
          ! --- if we are in the actual namelist
          !     generate the rest of the Internal File.....
          Internal_file =  trim(adjustl(Internal_File)) // " " // Trim(Adjustl(line)) 
             
          ! --- Are we at the end of the NML?
          if (at_the_end_of_nml()) exit
       Endif

    enddo line_loop


    ! --- After reading part of or the whole file
    if (iostat<0) then
       ! --------------------------------
       ! --- NML Not Found
       ! --------------------------------
       Write(*,*) " ERROR : NML not found!"
       stop
    else
       ! --------------------------------
       ! --- NML Found
       ! --------------------------------

       ! --- Read the NML from the Internal File
       read(Internal_File,nml=Material)       
    End if
       





    ! --- Copy the date
    ! ......

    
    ! --- Testing
    Write(*,*) " Read Namelist : ", trim(adjustl(NML))
    Write(*,*) "    Value  : ", Value
    Write(*,*) "    Delta  : ", Delta
    Write(*,*) "    String : ", String
    Write(*,*)
 

  contains


    Function at_the_end_of_nml() Result(res)
      Implicit None
      Logical :: Res


      ! --- Default is not found
      Res = .false.

      ! ------------------------
      ! --- The NML end-field
      ! ------------------------
      !
     
      
      ! --- Is the NML end field found?
      
      ! --- Left adjust the line
      line = adjustl(line)
      if ( line(1:1) == "/") Res=.true.
      
      
      ! --- Right adjust the line
      line = adjustr(line)
      if ( line(len(line):len(line)) == "/") Res = .true.
      
    End Function at_the_end_of_nml



  End Subroutine Read_Unknown_Named_NML


  ! =================================================================0


  Subroutine Read_Unknown_Named_NML_2(fid,NML)
    Implicit None
    Integer,           Intent(In) :: fid
    Character(len=*),  Intent(In) :: NML
    ! --- Local
    Integer            :: fidtmp=80
    Integer            :: iostat
    Character(len=250) :: line
    Character(len=3000):: Internal_File
    Logical            :: in_nml

    Integer    :: Delta
    Real       :: Value
    Character(len=20) :: String
    
    namelist /Material/ Value, Delta, String

    
    ! --- Rewind the file
    rewind(fid)


    open(unit=fidtmp, file="Kast.in")
    

    !--- Read the file line by line and process it
    iostat = 0
    in_nml = .false.
    line_loop : do while (iostat >= 0 )
       
       ! --- Read a line
       read(fid,"(A)",iostat=iostat) line

       ! --- Left Adjust the line
       line = adjustl(line)


       ! ------------------------
       ! --- The NML start-field
       ! ------------------------

       !Write(*,*) "Comparing : ", line(1:len(trim(adjustl(nml)))+1) , "#&"//trim(adjustl(nml))  

       ! --- Are we in the NML?
       if ( line(1:len(trim(adjustl(nml)))+1) == "&"//trim(adjustl(nml)) ) then
          ! --- The start of the NML
          in_nml = .true.
          ! ---The first line of the Internal File
          Write(fidtmp,*)  "&Material " // line(len(trim(adjustl(nml)))+2:len(line))
          ! --- Are we at the end of the NML?
          if (at_the_end_of_nml()) exit

       elseif (in_nml) then           
          ! --- If we are in the actual namelist
          !     generate the rest of the Internal File.....
          Write(fidtmp,*) line
       
          ! --- Are we at the end of the NML?
          if (at_the_end_of_nml()) exit
       Endif

    enddo line_loop


    ! --- After reading part of or the whole file
    if (iostat<0) then
       ! --------------------------------
       ! --- NML Not Found
       ! --------------------------------
       Write(*,*) " ERROR : NML not found!"
       stop
    else
       ! --------------------------------
       ! --- NML Found
       ! --------------------------------

       ! --- Read the NML from the Internal File
       !read(Internal_File,nml=Material)
       rewind(fidtmp)
       read(fidtmp,nml=Material)
    End if
       


    Close(fidtmp)


    ! --- Copy the date
    ! ......

    
    ! --- Testing    
    Write(*,*) " Read Namelist : ", trim(adjustl(NML))
    Write(*,*) "    Value  : ", Value
    Write(*,*) "    Delta  : ", Delta
    Write(*,*) "    String : ", String
    Write(*,*)
 

  contains


    Function at_the_end_of_nml() Result(res)
      Implicit None
      Logical :: Res


      ! --- Default is not found
      Res = .false.

      ! ------------------------
      ! --- The NML end-field
      ! ------------------------
      !
     
      
      ! --- Is the NML end field found?
      
      ! --- Left adjust the line
      line = adjustl(line)
      if ( line(1:1) == "/") Res=.true.
      
      
      ! --- Right adjust the line
      line = adjustr(line)
      if ( line(len(line):len(line)) == "/") Res = .true.
      
    End Function at_the_end_of_nml



  End Subroutine Read_Unknown_Named_NML_2

  

  Subroutine TryIt(fid)
    Implicit None
    Integer :: fid

    Character(len=25) :: Tag, File=""
 
    namelist /Media/ Tag, File
 

   rewind( fid )
    
    Tag = " ....."
    
    read(fid, nml=Media)
    Write(*,*) " From TryIt :", Tag
    
  End Subroutine TryIt





End Module Read_Unknown_NML_Module







Program Read_Unknown_NML
  Use Read_Unknown_NML_Module
  Implicit None
  Character(len=10), dimension(2) :: MediaVec

  Integer :: fid=79, i, iostat
  !Real, allocatable, dimension(:)    :: Vector
  Real    :: Vector(2)
  character(len=10) :: str(2)
  character(len=50) :: Tag, File


  namelist /Test/        MediaVec
  namelist /VectorFelt/  Vector
  namelist /StringVector/ str
  namelist /Media/ Tag, File

  ! Reading the NameList of known name
  open(unit=fid,file="input.in")
  read(fid,nml=Test)
  Write(*,*) " MediaVec = ", MediaVec
  Write(*,*) 

  ! --- Reading the NameList of UNKNOWN name
  do i=1,size(MediaVec,1)
     call Read_Unknown_Named_NML_2(fid,MediaVec(i))  
  enddo


!  do i=1,10
     !allocate( Vector(i) )
     read(fid, nml=VectorFelt)!, iostat=iostat)
     if (iostat<0) Write(*,*) "Feil oppstod ", i
     Write(*,*) " Vector = ", vector!(1:i)
     !deallocate(vector)
!  enddo

  
     read(fid, nml=StringVector)
     Write(*,*) str(1), str(2)


     read(fid, nml=Media)
     Write(*,*) Tag, File


     call Tryit( FID )


     Tag = "'Testing'"
     File = "'Yes, Testing'"
     Write(10,nml=Media)

!     Tag  = "NOT_SET"
!     File = "NOT_SET"
!     Read(10,nml=Media)
!     Write(*,*) Tag, File


End Program Read_Unknown_NML
