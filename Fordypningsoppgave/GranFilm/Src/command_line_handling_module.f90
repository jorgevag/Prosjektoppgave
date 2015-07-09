! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     Reads and process the command line options and 
!     input.
!     
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!----------------------------------!
Module Command_Line_Handling_Module
!----------------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : Param, Software_Information


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Read_Command_Line


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private 




Contains



  
  !-----------------------------!
  Subroutine Read_Command_Line()
  !-----------------------------!
    ! 
    ! This routine reads the command line to get name of 
    ! the parameter file (input file) and the output filename.
    !
    ! Ingve Simonsen, Sep. 2008
    !
    Use Supported_Commandline_Option_Module, only : GF_CommandLine_Option_T, Set_Granfilm_Commandline_Options 
    Use Software_Info_Mod,                   only : Software_Info_Set!, Software_Info_String 
    Use F2003_Wrapper_Mod,                   only : Command_Argument_Count, Get_Command_Argument
    Use Error_Module,                        only : Error_Warning
    Implicit None
    ! Local
    Type(GF_CommandLine_Option_T)     :: GF_CommandLine_Option_Table
    Character(len=*),   parameter     :: routine = "Read_Command_Line"
    Integer                           :: Narg, Narg_Needed, i 
    Character(len=1)                  :: str
    Character(len=200), dimension(3)  :: string
    Character(len=100), allocatable   :: Prog_Options(:)
    logical                           :: file_exist

    
    ! Get number of command line arguments
    Narg = Command_Argument_Count()   !iargc() 

    ! Loop over the command line arguments.....
    allocate( Prog_Options( 0:Narg ) )   
    do i=0,Narg
       call Get_Command_Argument( Number=i, Value=Prog_Options(i) )
    enddo

    ! Set the program name 
    call Software_Info_Set( Software_Information, Prog_Options(0) )

    ! --- Set the supported command line options
    call Set_Granfilm_Commandline_Options( GF_CommandLine_Option_Table )

    ! --- Needed command line arguments
    call Get_Needed_CommandLine_Options(Narg_Needed, Prog_Options, GF_CommandLine_Option_Table ) 


    ! --- Too few options....?
    !     Four arguments is the minimum 
    !     Five is the maximum    
    if ( ( Narg /= Narg_Needed) .or. (Narg==0) ) then
       call Error_Warning (routine, "Inconsistent or unsupported command line options")
       call Usage()   
    endif


    ! Get the parameter and output filename....
    ! This is NOT robust, and should be changed ....
    do i=1,Narg 

       str = adjustl(Prog_Options(i))

       ! --- loop over the options and not their arguments 
       if (str=="-") then
          
          select case( trim(adjustl(Prog_Options(i))) )
             
          case("-p")
             ! --- Parameter file option
             !if (i==Narg) call Usage() 
             Param%InOut%Input_File_Name = Trim(adjustl(Prog_Options(i+1)))

          case("-o")
             ! --- Output file option
             !if (i==Narg) call Usage() 
             Param%InOut%Output_File_Name = Trim(adjustl(Prog_Options(i+1)))
             
          case("-help")
             ! --- experimental data to be fitted
             call Usage()
             
          case("-f")
             ! --- experimental data to be fitted
             !if (i==Narg) call Usage()
             Param%InOut%do_curve_fitting = .true.
             Param%InOut%Experimental_File_Name = Trim(adjustl(Prog_Options(i+1)))
             
          case("-V")
             ! --- Version option
             call Version()
             
          case("-v")
             ! --- verbose option
             Param%InOut%Verbose   =   .true.
             
             
          case("-P")
             ! --- verbose option
             Param%InOut%Print_Parameters  =   .true.
             
             
          case("-gui_mode")
             ! --- Gui option (to be used with the gui)
             Param%InOut%Gui_Mode=.true.


          case("-py")
             ! --- python interface
             Param%InOut%python_mode=.true.
             
             
          case default
             Write(*,*) 
             Write(*,*) " Unknown option : ", trim(adjustl(Prog_Options(i)))
             Write(*,*) 
             call Usage()
          end select
          
       endif
       
    enddo
    
    ! Deallocate 
    if (allocated(Prog_Options)) deallocate(Prog_Options)
    
   contains
     

    !------------------------!
    Subroutine Version()
    !------------------------!
      !Prints the version of the software
      Implicit None
      Integer :: i
      character(len=2) :: str = " "
      ! --- Usage
      Write(*,*) str,trim(Adjustl(Software_Information%Name)), "  Ver: ", trim(Adjustl(Software_Information%Version)), &
           "   (HG-tag: ", trim(Adjustl(Software_Information%Revision)), ")"
      Write(*,*) str,trim(Adjustl(Software_Information%Copyright))
      !write(*,*) str,trim(Adjustl(Software_Information%Author(1)))," ",trim(Adjustl(Software_Information%Email(1))), &
      !     " and ", trim(Adjustl(Software_Information%Author(2)))," ",trim(Adjustl(Software_Information%Email(2)))
      Write(*,*) str,"Compiled on ", trim(Adjustl(Software_Information%Compile_Date)), " at ", &
           trim(Adjustl(Software_Information%Compile_Time))
     !
      call exit(0)  ! stop ""
    End Subroutine Version
    !------------------------!



    !------------------------!
    Subroutine Usage()
    !------------------------!
      ! This routine is not very general.
      ! It should probably be updated in the future to loop over supported options
      Implicit None
      Integer :: i
      character(len=200):: options

      options = "[options] -p <parameter_file> -o <output_file>" 

      ! --- Usage
      Write(*,*) 
      Write(*,*) " USAGE : ", trim(Adjustl(Software_Information%Name)),  " " , trim(adjustl(options))
      Write(*,*)
      Write(*,*) " Options"
      call  Print_CommandLine_Options()
      Write(*,*)
      call exit(0)  ! stop ""

    End Subroutine Usage
    !------------------------!


    !----------------------------------------!
    Subroutine Print_CommandLine_Options()
    !----------------------------------------!
      implicit None
      ! --- Local
      integer :: i

      do i=1,size(GF_CommandLine_Option_Table%option,1)
         Write(*,'(4x,a10,3x,a80)') adjustl( GF_CommandLine_Option_Table%option(i) ),  &
              adjustl( GF_CommandLine_Option_Table%text(i) )
      enddo

    End Subroutine Print_CommandLine_Options
    !----------------------------------------!


  End Subroutine Read_Command_Line
  !------------------------------!




  !-------------------------------------------------------------------------------------------------!
  Subroutine Get_Needed_CommandLine_Options( Narg_Needed, Prog_Options, GF_CommandLine_Option_Table ) 
  !-------------------------------------------------------------------------------------------------!
    !
    ! PURPOSE
    !    Returns the number of arguments needed given the command line options....
    !
    ! AUTHOUR
    !    Ingve Simonsen, Sandnes, 23 Dec. 2013
    !
    Use Supported_Commandline_Option_Module, only : GF_CommandLine_Option_T, Set_Granfilm_Commandline_Options 
    implicit none
    integer, intent(out) :: Narg_Needed
    character(len=100), dimension(:) :: Prog_Options
    type(GF_CommandLine_Option_T)    :: GF_CommandLine_Option_Table
    ! --- Local
    integer :: i, j, N
    logical :: found

    ! --- Initialize
    Narg_Needed = 0
    N = size(GF_CommandLine_Option_Table%option,1)

    ! --- loop
    do i=1,size(Prog_Options,1)
       
       do j=1,N
          found = ( trim(Adjustl(Prog_Options(i))) ==  trim(adjustl( GF_CommandLine_Option_Table%option(j) )) )
          if ( found ) then
             Narg_Needed = Narg_Needed + 1 + GF_CommandLine_Option_Table%arguments(j)
          end if

       enddo
    
    enddo

  End Subroutine Get_Needed_CommandLine_Options
  !-------------------------------------------------------------------------------------------------!

End Module Command_Line_Handling_Module
!--------------------------------------!
