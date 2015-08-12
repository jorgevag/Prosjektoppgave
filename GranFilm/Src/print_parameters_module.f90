! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     Provides a routine for printing parameter settings 
!     active in the code at the time of routine call.
!    
! 
! --- AUTHOR : Ingve Simosnen, Paris, May, 2012
!
! ----------------------------------------------------------
!


!------------------------------!
Module Print_Parameters_Module
!------------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp, Param 


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Print_Parameters


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  ! --- Parameters  
  Integer,          Parameter :: Field_Size         = 10
  Integer,          Parameter :: Decimals           = 4
  Integer,          Parameter :: Line_Limit         = 250
  Character(len=1), Parameter :: String_Delimiter   = "'"
  Character(len=1), Parameter :: String_Comma       = ","
  Character(len=1), Parameter :: Blank              = " "
  Character(len=1), Parameter :: Empty              = ""
  Character(len=1), Parameter :: Namelist_Delimiter = "/"
  Character(len=1), Parameter :: Namelist_Starter   = "&"
  Character(len=6), Parameter :: True               = ".true."
  Character(len=7), Parameter :: False              = ".false."


  ! --- Format statment
  Character(len=15), parameter :: STD_FMT  = '("  ",a,"  ",a)'
  Character(len=10), parameter :: F10_4    = '(F10.4)'
  Character(len=10), parameter :: I10      = '(I10)'

  !-------!
Contains
  !-------!




  !-------------------------------------------------!
  Subroutine Print_Parameters( output_filename )
  !-------------------------------------------------!
    !
    ! --- This routine prints the parameters of the simulations as set
    !     at the time the routine is called. If the optional argument
    !     output_filename is given, the result will be printed to this
    !     file. Otherwise, results are printed to screen.
    !
    !
    !     Ingve Simonsen, Paris, May 2012
    !
    !     Modified :
    !
    !
    !
    Use SFL_Logical_Units,    only : SFL_Get_Free_Unit
    Use Error_Module,         only : Error_Warning
    Implicit None
    character(len=*), Intent(In), optional :: output_filename
    ! --- Local 
    Character(len=*), parameter :: routine = "Print_Parameters"
    !
    Integer :: ounit, i, previous
    Character(len=250)               :: string
    ! --- String constants
    ! --- Format statments
    Character(len=1),   parameter    :: f6_4 = "(a,f6.4)" 
    Logical     :: not_done



    if (present(output_filename)) then
       ! --- Print to file
       call SFL_Get_Free_Unit( ounit )
       open(unit=ounit,file=trim(adjustl( output_filename )),status='unknown') 
    else
       ! --- Print to SCREEN!
       !     By default unit=6 is the SCREEN in Fortran
       ounit = 6 
    endif



    ! --- Warn that the routine is not complete...
    call Error_Warning( routine, "This routine is not completeted yet!" )



    Write(ounit,*)
    Write(ounit,*)
    ! --- Global
    ! ----------------------------
    !
    Write(ounit,*) "&Global"
    ! ---
        Write(ounit,fmt=STD_FMT) "  Title                      =  " ,  Stringify_Char( param%Global%Title      )
    Write(ounit,fmt=STD_FMT) "  SOPRA_ROOT                 =  " ,  Stringify_Char( param%Global%SOPRA_ROOT )  
    ! 
    ! ---
    Write(ounit,*) Namelist_Delimiter
    Write(ounit,*)






    ! --- Source
    ! ----------------------------
    !
    Write(ounit,*) "&Source"
    ! ---
    !
    Write(ounit,fmt=STD_FMT) "  Theta0                     =  ",  Stringify_Real(        param%Source%Theta0 )
    Write(ounit,fmt=STD_FMT) "  Phi0                       =  ",  Stringify_Real(        param%Source%Phi0   )
    Write(ounit,fmt=STD_FMT) "  Polarization               =  ",  Stringify_Char(        param%Source%Polarization )
    Write(ounit,fmt=STD_FMT) "  Energy_Range               =  ",  Stringify_Real_Vector( param%Source%Energy_Range )
    !
    ! ---
    Write(ounit,*) Namelist_Delimiter
    Write(ounit,*)
    !Write(ounit,*)





    ! --- Geomtry
    ! ----------------------------
    !
    Write(ounit,*) "&Geometry"
    ! ---
    !
    Write(ounit,fmt=STD_FMT) "  Radius                     =  ",   Stringify_real_vector( param%Geometry%Radius )
    Write(ounit,fmt=STD_FMT) "  Truncation_Ratio           =  ",   Stringify_real(        param%Geometry%Truncation_Ratio)
    Write(ounit,fmt=STD_FMT) "  Radius_Ratios              =  ",   Stringify_real_vector( param%Geometry%Radius_Ratios, &
                                                                                          Add_Quotation_Marks=.true. )
    Write(ounit,fmt=STD_FMT) "  Media                      =  ",   trim(adjustl(  Get_Media_String()  ))   
    ! ---
    Write(ounit,*) Namelist_Delimiter
    Write(ounit,*)
    !Write(ounit,*)

!!$    ! ESKILs definition
!!$    ! the parameter is set if the -f flag is given param%inout%do_curve_fitting 
!!$    if (param%inout%do_curve_fitting) then
!!$       param%Geometry%Broadening_Perp = 0.1_wp
!!$       param%Geometry%Broadening_Par  = 0.1_wp
!!$    else 
!!$       param%Geometry%Broadening_Perp = 0.0_wp
!!$       param%Geometry%Broadening_Par  = 0.0_wp
!!$    end if





 
    ! --- Intercation
    ! ----------------------------
    !
    Write(ounit,*) "&Intercation"
    ! ...
    Write(ounit,fmt=STD_FMT) "  Arrangement                =  ", Stringify_Char(  param%Interaction%Arrangement      )
    Write(ounit,fmt=STD_FMT) "  Lattice_Type               =  ", Stringify_Char(  param%Interaction%Lattice_Type     )
    Write(ounit,fmt=STD_FMT) "  Lattice_Constant           =  ", Stringify_Real(  param%Interaction%Lattice_Constant )
    Write(ounit,fmt=STD_FMT) "  Island_Island_Interaction  =  ", Stringify_Char(  param%Interaction%Island_Island_Interaction )
    ! ...
    Write(ounit,*) Namelist_Delimiter
    Write(ounit,*)



    ! --- Numerics
    ! ----------------------------
    !
    Write(ounit,*) "&Numerics"
    ! ...
    Write(ounit,fmt=STD_FMT) "  Multipole_Order            =  ", Stringify_Int(   param%Numerics%Multipole_Order    )
    Write(ounit,fmt=STD_FMT) "  Multipole_Position_Ratio   =  ", Stringify_Real(  param%Numerics%Multipole_Position_Ratio  )
    Write(ounit,fmt=STD_FMT) "  No_Energy_Points           =  ", Stringify_Int(   param%Numerics%No_Energy_Points   )
     ! ...
    Write(ounit,*) Namelist_Delimiter
    Write(ounit,*)


!!$
!!$
!!$    !
!!$    ! ----------------------------------------
!!$    ! --- The optional NameLists
!!$    ! ----------------------------------------
!!$    !
!!$
!!$
!!$
!!$    ! --- Curvefitting
!!$    ! ----------------------------
!!$    !
!!$
!!$
!!$    ! --- Potential
!!$    ! ----------------------------
!!$    !
!!$
!!$
!!$
!!$    !
!!$    ! .......................................








    ! --- Media
    ! ----------------------------
    !
    !  --- Finally we loop over all the media given, but print only
    !  --- each of the Namelist once, even if it is given several
    !  --- times.
    !
    Write(ounit,fmt=STD_FMT) "! . . . . . . . . . . . . . . . . . . ."
    Write(ounit,*)
    Do i=1,size(param%Media,1)
   
       ! --- Check if the Namelist has been printed previously
       not_done = .true.
       do previous = 1,i-1
          if ( param%Media(previous)%Tag == param%Media(i)%Tag ) not_done =.false.
       enddo

       If (not_done) then          
          ! --- Media
          ! ----------------------------
          !
          Write(ounit,*) Namelist_Starter // trim(adjustl(param%Media(i)%Tag)) 
          ! ...................................................................
          !
          Write(ounit,fmt=STD_FMT) "  Material                   =  ", &
               Stringify_Char(         param%Media(i)%material    )
          ! --- Epsilon_Scale 
          If ( .not. all( abs(param%Media(i)%Epsilon_Scale-1._wp) < 2*epsilon(1._wp) ) ) then
             Write(ounit,fmt=STD_FMT) "  Epsilon_Scale              =  ", &
                  Stringify_Real_vector(  param%Media(i)%Epsilon_Scale    )
          End If
          ! --- Temparetaure corrections
          If ( param%Media(i)%Do_Temperature_Correction) then
             Write(ounit,fmt=STD_FMT) "  ! . . . . . . "
             Write(ounit,fmt=STD_FMT) "  Do_Temperature_Correction  =  ", True
             ! --- More should go here....
          End If
          ! ---Finite Size corrections
          If ( param%Media(i)%Do_Size_Correction) then
             Write(ounit,fmt=STD_FMT) "  ! . . . . . . "
             Write(ounit,fmt=STD_FMT) "  Do_Size_Correction         =  ", True
             ! --- More should go here....
          End If
          !
          ! ...................................................................
          Write(ounit,*) Namelist_Delimiter
          Write(ounit,*)

       End If

    End Do




  End Subroutine Print_Parameters
  !------------------------------------------------------!


  !
  ! ==============================
  ! === Local Routines
  ! ==============================
  !





  !-----------------------------------------!
  Function Stringify_Int(Heltall) Result(Res)
  !-----------------------------------------!
    Implicit None
    Integer, Intent(In)         :: heltall
    Character(len=Field_Size)   :: Res
    ! --- Local
    Character(len=Field_Size)   :: String
    
    
    Write(String,fmt=I10) Heltall
    Res    =  Adjustl(String)
    
  End Function Stringify_Int
  !-----------------------------------------!



  !-----------------------------------------!
  Function Stringify_Real(Float) Result(Res)
  !-----------------------------------------!
    Implicit None
    Real(wp), Intent(In)         :: Float
    Character(len=Field_Size)    :: Res
    ! --- Local
    Character(len=Field_Size)    :: String
    
    
    Write(String,fmt=F10_4) Float
    Res         =  Adjustl(String)
    
  End Function Stringify_Real
  !-----------------------------------------!



  !------------------------------------------------------------------------!
  Function Stringify_Real_Vector(Floats, Add_Quotation_Marks) Result(Res)
  !------------------------------------------------------------------------!
    Implicit None
    Real(wp), dimension(:), Intent(In)         :: Floats
    Logical,                optional           :: Add_Quotation_Marks
    Character(len=size(Floats,1)*Field_Size+size(Floats,1)+1)    :: Res
    ! --- Local
    Character(len=size(Floats,1)*Field_Size+size(Floats,1)+1)    :: Total_String
    Character(len=Field_Size)                                 :: String
    Integer :: i, N
    
    N = size(Floats,1)

    if (N==1) then
       ! --- One-element vector
       total_string = Stringify_Real( Floats(1) )       

       if (present(Add_Quotation_Marks)) then
          if (Add_Quotation_Marks) then
             total_string  =  String_Delimiter // Trim(Adjustl(total_string)) // String_Delimiter
          end if
       endif
       
       Res = Trim(Adjustl(total_string))

    else
       ! --- Multidimensional vector
       total_string = Empty !String_Delimiter
       do i=1,N-1
          Write(String,fmt=F10_4) Floats(i)
          total_string = trim(adjustl( total_string )) // Blank // Trim(Adjustl( string )) // String_Comma
       enddo
       Write(String,fmt=F10_4) Floats(N)
       total_string = trim(adjustl( total_string )) // Blank // Trim(Adjustl( string )) !// String_Delimiter

       if (present(Add_Quotation_Marks)) then
          if (Add_Quotation_Marks) then
             total_string  =  String_Delimiter // Trim(Adjustl(total_string)) // String_Delimiter
          end if
       endif
       
       Res = Trim(Adjustl(total_string))
       
    endif
    
  End Function Stringify_Real_Vector
  !-------------------------------------------------!



  !-----------------------------------------!
  Function Stringify_Char(String) Result(Res)
  !-----------------------------------------!
    Implicit None
    Character(len=*), Intent(In)                 :: String
    Character(len=len(Trim(Adjustl(String)))+2)  :: Res
    ! --- Local

    Res  =  String_Delimiter // Trim(Adjustl( String )) // String_Delimiter
    
  End Function Stringify_Char
  !-----------------------------------------!




  !-----------------------------------------!
  Function Get_Media_String()   Result(Res)
  !-----------------------------------------!
    Implicit None
    Character(len=Line_Limit)        :: Res
    ! --- Local
    Character(len=Line_Limit)        :: String
    Integer :: i

    string = String_Delimiter // trim(adjustl( param%Media(1)%Tag )) // String_Comma
    do i=2,size(param%Media,1)-1
       string = trim(adjustl( string )) // Blank // trim(adjustl( param%Media(i)%Tag )) // String_Comma
    enddo
    string = trim(adjustl( string )) // Blank // trim(adjustl( param%Media(i)%Tag )) // String_Delimiter

    Res = trim(adjustl( String ))
    
  End Function Get_Media_String
  !-----------------------------------------!








End Module Print_Parameters_Module
!--------------------------------------------------------!






