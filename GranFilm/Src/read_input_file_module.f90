! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     This module contains the main routine for reading the
!     input parameter file used by GranFilm in the 
!     sif format ("Scientific Input Format")
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!                              
! --- Modified :
!
!                  - May 2012      IS
!                  - Jan 2014      IS
!
! ----------------------------------------------------------
!


!----------------------------!
Module Read_Input_File_Module
!----------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp, Param	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Read_Input_Parameter_File


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  


  ! --- Some Private Constants
  Integer, parameter :: STR_TINY  = 15
  Integer, parameter :: STR_SHORT = 100
  Integer, parameter :: STR_LONG  = 300
  !Real(wp), parameter, private :: MOST_POSITIVE = Huge(1._wp)
  !Real(wp), parameter, private :: VERY_POSITIVE = MOST_POSITIVE - MOST_POSITIVE /1000
  !Real(wp), parameter, private :: DEFAULT_FLOAT = MOST_POSITIVE





Contains



  !-------------------------------------!
  Subroutine Read_Input_Parameter_File()
  !-------------------------------------!
    !
    !
    !  --- NOTE : Since we do not reopen the input file between reading 
    !             each namelist it is essential to rewind the file between 
    !             each time it is read.
    !
    Use Shared,                       only : Param
    Use SFL_Logical_Units,            only : SFL_Get_Free_Unit
    Use Error_Module
    Use Tools_Read_Input_File_Module, only : NameList_Exists, Field_in_NameList_Exists

    Implicit None
    ! --- Local
    character(len=*),parameter :: routine="Read_Input_Parameter_File"
    Integer :: ifile, iostat
    Logical :: file_exists

    ! ---  Verbose
    If (Param%InOut%Verbose) then
       Write(*,*) " +++ Entering : ", routine
       Write(*,*) "      ... Reading Input Parameter File : ", trim(adjustl(Param%InOut%Input_File_Name))
    End If


    ! --- Get free unit
    call SFL_Get_Free_Unit( ifile ) 
    ! --- Does the input parameter file exist?
    inquire(file = Trim(Adjustl(param%InOut%Input_File_Name)), exist = file_exists)
    if ( file_exists) then
       ! --- Open the input parameter file
       open(unit=ifile, file=Trim(Adjustl(param%InOut%Input_File_Name)))    
    else
       ! Give an error message
       call Error_Fatal( routine, &
          "Input Parameter File ("//Trim(Adjustl(param%InOut%Input_File_Name))//") does not exist!")       
    End if


    ! -----------------------------------
    ! --- READ Parameters FROM FILE
    ! -----------------------------------
    !
    ! --- The Required NameLists
    ! ----------------------------------------
    !
    call Read_Global_nml( ifile )             ! --- Accepting default values
    
    call Read_Geometry_nml( ifile )           ! --- Accepting default values   
 
    call Read_Source_nml( ifile )             ! --- Accepting default values

    call Read_Interaction_nml( ifile )        ! --- Accepting default values

    call Read_Numerics_nml( ifile )           ! --- Accepting default values

    ! --- This routine uses special read statments
    !call Read_Media_nml_Old( ifile )              ! --- Accepting default values
    call Read_Media_nml( ifile )              ! --- Accepting default values
    !
    ! .......................................


    ! --- The optional NameLists
    ! ----------------------------------------
    !
    call Read_Curvefitting_nml( ifile )       ! --- Has NO default values (so far)

    call Read_Potential_nml( ifile )          ! --- Has NO default values (so far)

    call Read_EELS_nml( ifile )               ! --- Has NO default values (so far)
    !
    ! .......................................
   



    ! --- Closing the input parameter file
    close( unit = ifile)


    ! ---  Verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

                                                                                       
  !------!  
  Contains

  !------!  

    ! ==========================================
    ! === Local Routines below this line....
    ! ==========================================



    !
    ! ----------------------------------
    ! --- Required Namelists 
    ! ----------------------------------
    !



    !----------------------------------!
    Subroutine Read_Global_nml( ifile )
    !----------------------------------!
      ! 
      ! --- Reading the GLOBAL namelist
      !
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      character(len=*), parameter :: NML = "Global"
      character(len=STR_SHORT)    :: Field
      ! --- Namelist variables
      Character(len=200)  :: Title
      Character(len=1000) :: SOPRA_ROOT
      ! --- The NameList statments
      namelist /Global/      Title, SOPRA_ROOT     


      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Global"
    

      ! --- Read the namelist variables 
      ! ---------------------------------------------
      if ( NameList_Exists(ifile, NML) ) then
         rewind(ifile)
         Read(ifile,nml=Global, iostat=iostat)
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist Global !")
      end if

      ! --- Copy the read variables to the derived type
      ! ------------------------------------------------
      !
      Field = "Title"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           param%Global%Title   = Title
      !
      Field = "SOPRA_ROOT"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           param%Global%SOPRA_Root = sopra_root
      
      ! --- What did we read
      !Write(*,*) Param%Global

    End Subroutine Read_Global_nml
    !-----------------------------!


    !
    ! ---
    !


    !------------------------------------!
    Subroutine Read_Geometry_nml( ifile )
    !------------------------------------!  
      ! 
      ! --- Reading the GEOMETRY namelist
      !
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      character(len=*), parameter :: NML = "Geometry"
      character(len=STR_SHORT)    :: Field
      Integer   :: dim 
      ! --- Namelist variables
      !Real(wp)                :: Radius
      Character(len=STR_LONG) :: Radius
      Real(wp)                :: Truncation_Ratio
      Real(wp)                :: Broadening_Par
      Real(wp)                :: Broadening_Perp
      Character(len=STR_LONG) :: Radius_Ratios       ! NOTE: This is a string
      Character(len=STR_LONG) :: Media               ! NOTE: This is a string
      ! --- The NameList statments 
      namelist /Geometry/  Radius, Truncation_Ratio, Radius_Ratios, Media,  &
                           Broadening_Par, Broadening_Perp

      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Geometry"


      ! --- Read the namelist variables 
      ! ------------------------------------------------
      if ( NameList_Exists(ifile, NML) ) then
         rewind(ifile)
         Read(ifile,nml=Geometry, iostat=iostat)
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist Geometry !")
      end if
      ! --- Get dimension and allocate needed storage
      ! ------------------------------------------------
      
      !dim = Get_Vector_Dimension(Radius) + 1
      !if (.not. allocated( Param%Geometry%Radius) ) &
      !     allocate(  Param%Geometry%Radius ( dim ) )
      
      !dim = Get_Vector_Dimension(Radius_Ratios) + 1
      !if (.not. allocated( Param%Geometry%Radius_Ratios ) ) &
      !     allocate(  Param%Geometry%Radius_Ratios ( dim ) )

      !dim = Get_Vector_Dimension(Media) + 1
      !if (.not. allocated( Param%Geometry%Media ) ) &
      !     allocate(  Param%Geometry%Media ( dim ) )

      ! --- Copy the read variables to the derived type
      ! --------------------------------------------------
      Field = "Radius"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) then
         dim = Get_Vector_Dimension(Radius) + 1
         if ( allocated( Param%Geometry%Radius) ) deallocate( Param%Geometry%Radius )
         allocate(  Param%Geometry%Radius ( dim ) )
         Read(Radius, *, iostat=iostat)     Param%Geometry%Radius
         if (iostat<0) call Error_Failure( routine, " Radius@Geometry reading failure ")
      end if
      !
      Field = "Truncation_Ratio"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
         Param%Geometry%Truncation_Ratio     =  Truncation_Ratio
      !
      Field = "Radius_Ratios"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) then
         dim = Get_Vector_Dimension(Radius_Ratios) + 1
         if (allocated( Param%Geometry%Radius_Ratios ) ) deallocate( Param%Geometry%Radius_Ratios )
         allocate(  Param%Geometry%Radius_Ratios ( dim ) )
         Read(Radius_Ratios, *, iostat=iostat)     Param%Geometry%Radius_Ratios
         if (iostat<0) call Error_Failure( routine, " Radius_Ratios@Geometry reading failure ")
      end if
      !
      Field = "Media"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) then
         dim = Get_Vector_Dimension(Media) + 1
         if ( allocated( Param%Geometry%Media ) ) deallocate( Param%Geometry%Media ) 
         allocate(  Param%Geometry%Media ( dim ) ) 
         Read(Media,      *, iostat=iostat)     Param%Geometry%Media     
         if (iostat<0) call Error_Failure( routine, " Media@Geometry reading failure ")
      end if
      !
      Field = "Broadening_Par"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Geometry%Broadening_Par  =  Broadening_Par
      !
      Field = "Broadening_Perp"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Geometry%Broadening_Perp  =  Broadening_Perp 

      ! --- What did we read
      !Write(*,*) Param%Geometry


    End Subroutine Read_Geometry_nml
    !-------------------------------!


    !
    ! ---
    !


    !----------------------------------!
    Subroutine Read_Source_nml( ifile )
    !----------------------------------!
      ! 
      ! --- Reading the SOURCE namelist
      !
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      character(len=*), parameter :: NML = "Source"
      character(len=STR_SHORT)    :: Field
      ! --- Namelist variables
      Real(wp)           :: Theta0, Phi0, Energy_Range(2), Wavelength_Range(2) 
      Character(len=2)   :: Polarization 
      ! --- The NameList statments
      namelist /Source/  Theta0, Phi0, Polarization, Energy_Range  !, Wavelength_Range


      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Source"

      ! --- Read the namelist variables 
      if ( NameList_Exists(ifile, NML) ) then
         rewind(ifile)
         Read( ifile, nml=Source, iostat=iostat )
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist Source !")
      end if

      ! --- Copy the read vaiables to the derived type
      ! -----------------------------------------------------
      Field = "Theta0"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Source%Theta0  = Theta0
      !
      Field = "Phi0"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Source%Phi0  = Phi0
      !
      Field = "Polarization"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Source%Polarization  = Polarization
      !
      Field = "Energy_Range"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Source%Energy_Range     = Energy_Range

      ! --- What did we read
      !Write(*,*) Param%Source

    End Subroutine Read_Source_nml
    !-----------------------------!


    !
    ! ---
    !


    !--------------------------------------!
    Subroutine Read_Interaction_nml( ifile )
    !--------------------------------------!
      ! 
      ! --- Reading the INTERACTION namelist
      !
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      character(len=*), parameter :: NML = "Interaction"
      character(len=STR_SHORT)    :: Field
      ! --- Namelist variables
      Real(wp)                   :: Lattice_Constant
      Character(len=STR_SHORT)   :: Arrangement
      Character(len=STR_SHORT)   :: Lattice_Type
      Real(wp)                   :: Coverage
      Character(len=STR_SHORT)   :: Island_Island_Interaction
      ! --- The NameList statments
      namelist /Interaction/   Lattice_Constant, Arrangement, Lattice_Type, Island_Island_Interaction


      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Interaction"

      ! --- Read the namelist variables 
      ! ------------------------------------------------
      if ( NameList_Exists(ifile, NML) ) then
         rewind(ifile)
         Read( ifile, nml=Interaction, iostat=iostat )
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist Interaction !")
      end if


      ! --- Copy the read vaiables to the derived type
      ! -------------------------------------------------
      Field = "Lattice_Constant"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
          Param%Interaction%Lattice_Constant  =  Lattice_Constant 
      !
      Field = "Arrangement"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
          Param%Interaction%Arrangement  =  Trim(adjustl(Arrangement))
      !
      Field = "Lattice_Type"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Interaction%Lattice_Type  =  Trim(adjustl(Lattice_Type))
      !
      Field = "Island_Island_Interaction"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Interaction%Island_Island_Interaction  =  Trim(adjustl(Island_Island_Interaction))


      ! --- What did we read
      !Write(*,*) Param%Intercation

    End Subroutine Read_Interaction_nml
    !----------------------------------!




    !
    ! ---
    !


    !----------------------------------!
    Subroutine Read_Numerics_nml( ifile )
    !----------------------------------!
      ! 
      ! --- Reading the NUMERICS namelist
      !
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      character(len=*), parameter :: NML = "Numerics"
      character(len=STR_SHORT)    :: Field
      ! --- Namelist variables
      Integer   :: Multipole_Order, No_Energy_Points 
      Real(wp)  :: Multipole_Position_Ratio 
      ! --- The NameList statments
      namelist /Numerics/  Multipole_Order, Multipole_Position_Ratio, No_Energy_Points 


      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Numerics"

      
      ! --- Read the namelist variables 
      ! --------------------------------
      if ( NameList_Exists(ifile, NML) ) then
         rewind(ifile)
         Read( ifile, nml=Numerics, iostat=iostat )
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist Numerics !")
      end if


      ! --- Copy the read vaiables to the derived type
      ! -------------------------------------------------
      Field = "Multipole_Order"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Numerics%Multipole_Order   =  Multipole_Order 
      !
      Field = "Multipole_Position_Ratio"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Numerics%Multipole_Position_Ratio   =  Multipole_Position_Ratio
      !
      Field = "No_Energy_Points"
      if( Field_in_NameList_Exists(ifile, NML, Field) ) &
           Param%Numerics%No_Energy_Points  =  No_Energy_Points 

      ! --- What did we read
      !Write(*,*) Param%Numerics

    End Subroutine Read_Numerics_nml
    !-------------------------------!



    !
    ! ---
    !



    !--------------------------------!
    Subroutine Read_Media_nml( ifile )
    !--------------------------------!
      ! 
      ! --- Reading the Media namelist
      !
      Use Tools_Module, only : tolower
      Use Tools_Read_Input_File_Module, only : Get_StringValue_for_NameList_Field, String_Value_Length
      Use Error_Module,                 only : Error_Warning
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      Integer :: i,dim, iostat
      character(len=STR_SHORT)           :: NML
      character(len=STR_SHORT)           :: Field
      Character(len=String_Value_Length) :: String_Value
      !character(len=STR_SHORT)           :: Epsilon_Scale

      ! --- The NameList statments
      !namelist /Media/  Epsilon_Scale

      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Media"
      
      if (.not. allocated( Param%Geometry%Media ) ) &
           call Error_Fatal( routine, "Internal error; Param%Geometry%Media NOT allocated")

      ! --- Allocate the container....
      dim = size( Param%Geometry%Media, 1 )
      if ( allocated( Param%Media) )  deallocate( Param%Media ) 
      allocate (  Param%Media (dim)  ) 

      ! --- Loop over the media
      do i=1,dim

         ! --- Set the tag
         Param%Media(i)%Tag  = trim(adjustl(Param%Geometry%Media(i)))

         ! ... Set the NameList (unknown at time of coding) 
         NML = trim(adjustl( Param%Media(i)%Tag ))

         !
         ! -------------------------------------------------
         ! ---Read the various Fields.....
         ! --------------------------------------------------
         !


         ! --- Material
         ! ------------------------
         Field = "Material"
         if ( Field_in_NameList_Exists(ifile, NML, Field) ) then
            ! ... Reading data from parameter file
            call Get_StringValue_for_NameList_Field(ifile, String_Value, NML, Field ) 
            read(String_Value,*,iostat=iostat)  Param%Media(i)%Material
            String_Value = "Reading error in namelist " // trim(adjustl(Field)) // "@" // trim(adjustl(NML))
            if (iostat<0) call Error_Failure( routine, String_Value )
         else
            ! ... Default
            Param%Media(i)%Material  = tolower( NML )            
         end if



         ! --- Path
         ! ------------------------
         Field = "Path"
         if ( Field_in_NameList_Exists(ifile, NML, Field) ) then
            ! ... Reading data from parameter file
            call Get_StringValue_for_NameList_Field(ifile, String_Value, NML, Field ) 
            read(String_Value,*,iostat=iostat)  Param%Media(i)%Path
            String_Value = "Reading error in namelist " // trim(adjustl(Field)) // "@" // trim(adjustl(NML))
            if (iostat<0) call Error_Failure( routine, String_Value )
         else
            ! ... Default
            Param%Media(i)%Path   = trim(adjustl(Param%Global%SOPRA_ROOT))
         end if


         ! --- Epsilon_Scale
         ! ------------------------
         Field = "Epsilon_Scale"
         if ( Field_in_NameList_Exists(ifile, NML, Field) ) then
            ! ... Reading data from parameter file
            write(*,*) "Epsilon scale field found"
            call Get_StringValue_for_NameList_Field(ifile, String_Value, NML, Field ) 
            read(String_Value,*,iostat=iostat)  Param%Media(i)%Epsilon_Scale
            String_Value = "Reading error in namelist " // trim(adjustl(Field)) // "@" // trim(adjustl(NML))
            if (iostat<0) call Error_Failure( routine, String_Value )
         else
            ! ... Default
            Param%Media(i)%Epsilon_Scale  = (/ 1._wp, 1._wp /) 
         end if




         ! --- Do_Temperature_Correction
         ! -------------------------------
         Field = "Do_Temperature_Correction"
         if ( Field_in_NameList_Exists(ifile, NML, Field) ) then
            ! ... Reading data from parameter file
            call Get_StringValue_for_NameList_Field(ifile, String_Value, NML, Field ) 
            read(String_Value,*,iostat=iostat)  Param%Media(i)%Do_Temperature_Correction
            String_Value = "Reading error in namelist " // trim(adjustl(Field)) // "@" // trim(adjustl(NML))
            if (iostat<0) call Error_Failure( routine, String_Value )
         else
            ! ... Default
            Param%Media(i)%Do_Temperature_Correction  =  .false.
         end if





         ! --- Do_Size_Correction
         ! -------------------------------
         Field = "Do_Size_Correction"
         if ( Field_in_NameList_Exists(ifile, NML, Field) ) then
            ! ... Reading data from parameter file
            call Get_StringValue_for_NameList_Field(ifile, String_Value, NML, Field ) 
            read(String_Value,*,iostat=iostat)  Param%Media(i)%Do_Size_Correction
            String_Value = "Reading error in namelist " // trim(adjustl(Field)) // "@" // trim(adjustl(NML))
            if (iostat<0) call Error_Failure( routine, String_Value )
         else
            ! ... Default
            Param%Media(i)%Do_Size_Correction  =  .false.
         end if



      


!!$    
!!$
!!$
!!$    ! --- Copy the data
!!$    ! ------------------------------------------------
!!$    !Param%Media(imedia)%Tag        =  Trim(adjustl(Tag))
!!$    Param%Media(imedia)%Material            =  Trim(adjustl(Material))
!!$    Param%Media(imedia)%Path                =  Trim(adjustl(Path))
!!$    Param%Media(imedia)%temp_corr           = temp_corr
!!$    Param%Media(imedia)%finitesize_corr     = finitesize_corr
!!$    Param%Media(imedia)%temperature         = temperature
!!$    Param%Media(imedia)%Ep                  = Ep
!!$    Param%Media(imedia)%d_Ep_dT             = d_Ep_dT
!!$    Param%Media(imedia)%hbar_fermi_velocity = hbar_fermi_velocity 
!!$    Param%Media(imedia)%hbar_inv_tau        = hbar_inv_tau
!!$    Param%Media(imedia)%B_plasmonshift      = B_plasmonshift
!!$
!!$    ! --- 
!!$    Field = "Epsilon_Scale"
!!$    if ( Field_in_NameList_Exists( fid, nml, Field)  )then
!!$       Param%Media(imedia)%Epsilon_Scale =  Epsilon_Scale
!!$    else
!!$       Param%Media(imedia)%Epsilon_Scale = (/ 1._wp,  1._wp /)
!!$    endif
!!$
!!$    !-----TESTING ------------------------------------------
!!$    !Write(*,*)
!!$    !Write(*,*) "           * Processing : ", Trim(adjustl(          Internal_file ))
!!$    !Write(*,*) "           Read : "
!!$    !Write(*,*) "       Tag  : ", Trim(adjustl( Param%Media(imedia)%Tag  ))
!!$    !Write(*,*) "                Material : ", Trim(adjustl( Param%Media(imedia)%Material ))
!!$    !Write(*,*) "       Path     : ", Trim(adjustl( Param%Media(imedia)%Path ))
!!$    !-----TESTING ------------------------------------------
!!$
!!$
!!$
!!$
!!$
!!$            





      enddo

      ! --- What did we read
      !Write(*,*) Param%Media(i)

      

      ! --- WARNING
      call Error_Warning( "Read_Media_nml", "Complete this routine" )
          



    End Subroutine Read_Media_nml
    !----------------------------!









!!$    !--------------------------------!
!!$    Subroutine Read_Media_nml_Working( ifile )
!!$    !--------------------------------!
!!$      ! 
!!$      ! --- Reading the Media namelist
!!$      !
!!$      Use Tools_Module, only : tolower
!!$      Implicit None
!!$      Integer, Intent(in) :: ifile
!!$      ! --- Local
!!$      Integer :: i,dim
!!$      character(len=STR_SHORT) :: NML
!!$      character(len=STR_SHORT) :: Field
!!$      ! ---  Verbose
!!$      If (Param%InOut%Verbose) &
!!$           Write(*,*) "      ... reading namelist Media"
!!$
!!$      if (.not. allocated( Param%Geometry%Media ) ) &
!!$           call Error_Fatal( routine, "Internal error; Param%Geometry%Media NOT allocated")
!!$
!!$      ! --- Allocate the container....
!!$      dim = size( Param%Geometry%Media, 1 )
!!$      if ( allocated( Param%Media) )  deallocate( Param%Media ) 
!!$      allocate (  Param%Media (dim)  ) 
!!$
!!$      ! --- Loop over the media
!!$      do i=1,dim
!!$
!!$         ! ... Set parameters
!!$         NML = trim(adjustl(Param%Geometry%Media(i)))
!!$         if( NameList_Exists(ifile, NML) ) then
!!$            ! --- Read the namelist (and update Param%Media(i) )
!!$            call Read_Unknown_Named_Media_NML_InternalFile( ifile, Trim( adjustl( Param%Geometry%Media(i) )), i )
!!$         else
!!$            ! --- Use the default material given by Param%Geometry%Media
!!$            !     and read from the default dielectric database
!!$            Param%Media(i)%Material       = tolower( NML )
!!$            Param%Media(i)%Path           = trim(adjustl(Param%Global%SOPRA_ROOT))
!!$            Param%Media(i)%Epsilon_Scale  = (/ 1._wp, 1._wp /)  !  Default value      
!!$         endif
!!$
!!$
!!$      enddo
!!$
!!$      ! --- What did we read
!!$      !Write(*,*) Param%Media
!!$      
!!$    End Subroutine Read_Media_nml_Working
!!$    !----------------------------!
!!$

    !
    ! ----------------------------------
    ! --- Optional Namelists 
    ! ----------------------------------
    !


    !----------------------------------!
    Subroutine Read_Potential_nml( ifile )
    !----------------------------------!
      ! 
      ! --- Reading the POTENTIAL namelist
      !
      !     This namelist is OPTIONAL
      !
      Use Tools_Read_Input_File_Module,  only :  NameList_Exists, Field_in_NameList_Exists
      Implicit None
      Integer, Intent(in)     :: ifile
      !  --- Local
      Character(len=*), parameter :: NML = "Potential"
      character(len=STR_SHORT)    :: Field
      Integer                 :: dim
      character(len=1000)     :: Points_File
      Character(len=STR_LONG) :: Energy    ! NOTE: This is a string
      Logical                 :: Potential_Calculation,file_exists
    
      ! --- The NameList statments
      namelist /Potential/  Points_File, Energy


      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Potential"


      ! --- Setting potential defaults
      ! ---------------------------------------------
      !Points_File                 = "none" 
      !Energy                      = "1.0"
      ! IS THIS NEEDED?

      ! --- Shall we calculate Potentials?
      ! -----------------------------------
      Param%Potential%Potential_Calculation =  NameList_Exists(ifile, NML) 
      

      
      ! --- Do / DO NOT read Parameters
      ! ------------------------------------------------------
      if (Param%Potential%Potential_Calculation) then

         !
         ! --- Potential Calculation to BE done
         !


         ! --- Check for REQUIRED fields?
         if ( .not. Field_in_NameList_Exists(ifile, NML, "Points_File") ) &
              call Error_Failure( routine, "Points_File@Potential is not given!")
         if ( .not. Field_in_NameList_Exists(ifile, NML, "Energy") ) &
              call Error_Failure( routine, "Energy@Potential is not given!")
      

         ! --- Read the namelist variables 
         rewind(ifile)
         Read( ifile, nml=Potential, iostat=iostat )
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist Potential !")

         dim = Get_Vector_Dimension(Energy) + 1
         if (allocated(Param%Potential%Energy)) deallocate(Param%Potential%Energy)
         allocate(  Param%Potential%Energy ( dim ) )

         ! --- Does Points-File Exist?
         inquire(file = points_file, exist = file_exists)
         if ( .not. file_exists) then
            Write(*,*)
            Write(*,*) " Points-File Name : ", trim(adjustl(points_file))
            call Error_Failure( routine, " Points_File@Potential does not exist.")
         end if

         ! --- Copy the read variables to the derived type
         Param%Potential%Points_File   =  Points_File   
         Read(Energy, *, iostat=iostat)  Param%Potential%Energy
         if (iostat<0) call Error_Failure( routine, " Energy@Potential reading failure ")


      Else
         
         !
         ! --- Potential Calculation NOT TO BE done
         !
         

      End if

    End Subroutine Read_Potential_nml
    !-------------------------------!



    !
    ! ---
    !



    !----------------------------------!
    Subroutine Read_EELS_nml( ifile )
    !----------------------------------!
      ! 
      ! --- Reading the EELS namelist
      !
      !     This namelist is OPTIONAL
      !
      Use Tools_Read_Input_File_Module,  only :  NameList_Exists, Field_in_NameList_Exists
      Implicit None
      Integer, Intent(in)     :: ifile
      !  --- Local
      character(len=*), parameter :: routine = "Read_EELS_nml"
      character(len=*), parameter :: NML = "EELS"
      character(len=STR_SHORT)    :: Field
      real(wp)                    :: Impact_Energy
      real(wp)                    :: Wavevector_Transfer
    
      ! --- The NameList statments
      namelist /EELS/  Impact_Energy, Wavevector_Transfer


      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist EELS"


      ! --- Shall we calculate the EELS Spectrum?
      ! -----------------------------------------------
      Param%EELS%EELS_Calculation  =  NameList_Exists(ifile, NML) 
      

      
      ! --- Do / DO NOT read Parameters
      ! ------------------------------------------------------
      if (Param%EELS%EELS_Calculation) then

         !
         ! --- EELS Calculation to BE done
         !


         ! --- Read the namelist variables 
         rewind(ifile)
         Read( ifile, nml=EELS, iostat=iostat )
         if (iostat<0) call Error_Failure( routine, "Reading error in namelist EELS!")

         
         ! --- Read the REQUIRED fields
         Field = "Impact_Energy"
         if( Field_in_NameList_Exists(ifile, NML, Field) ) then
            param%EELS%Impact_Energy   =  Impact_Energy
         else
            call Error_Required( routine, NML, Field )
            !call Error_Failure( routine, "Impact_Energy@EELS is not given!" )
         endif
         !
         Field = "Wavevector_Transfer"
         if( Field_in_NameList_Exists(ifile, NML, Field) ) then
            param%EELS%Wavevector_Transfer   =  Wavevector_Transfer
         else
            call Error_Required( routine, NML, Field )
            !call Error_Failure( routine, "Wavevector_Transfer@EELS is not given!" )
         endif



      Else
         
         !
         ! --- EELS Calculation NOT TO BE done
         !
         

      End if



    End Subroutine Read_EELS_nml
    !-------------------------------!


    !
    ! ---
    !


    !--------------------------------------!
    Subroutine Read_Curvefitting_nml( ifile )
    !--------------------------------------!
      ! 
      ! --- Reading the CURVEFITTING namelist
      !
      !     This namelist is OPTIONAL
      !
      
      Implicit None
      Integer, Intent(in) :: ifile
      ! --- Local
      character(len=*), parameter :: NML = "Curvefitting"
      character(len=STR_SHORT)    :: Field

      Real(wp)                   :: sigma
      Real(wp), dimension(3)     :: Lower_Constraint, Upper_Constraint
      logical                    :: freeze_broadening
      ! --- The NameList statments
      namelist /Curvefitting/   Lower_Constraint, Upper_Constraint, &
                                      sigma, freeze_broadening

      ! ---  Verbose
      If (Param%InOut%Verbose) &
           Write(*,*) "      ... reading namelist Curvefitting"


      ! --- Setting potential defaults
      ! ---------------------------------------------
      Lower_Constraint            = (/0.0_wp, 0.0_wp, 0.0_wp/)
      Upper_Constraint            = (/huge(1.0_wp), 1.0_wp, huge(1.0_wp)/)
      sigma                       = 0.005_wp
      freeze_broadening           = .false.

      ! --- Read the namelist variables 
      ! ------------------------------------------------
      rewind(ifile)
      Read( ifile, nml=Curvefitting, iostat=iostat )
      if (iostat<0 .and. Param%InOut%Do_Curve_Fitting) then
        write(*,*) "Using all defaults for namelist Curvefitting!"
      else if (iostat>0) then
        call Error_Failure( routine, "Reading error in namelist Curvefitting !")
      end if

      ! --- Copy the read vaiables to the derived type
      ! -------------------------------------------------
      Param%Curvefitting%Lower_Constraint           =  Lower_Constraint
      Param%Curvefitting%Upper_Constraint           =  Upper_Constraint
      Param%Curvefitting%Sigma                      =  Sigma
      Param%Curvefitting%freeze_broadening          =  freeze_broadening

      ! --- What did we read
      !Write(*,*) Param%Numerics

    End Subroutine Read_Curvefitting_nml
    !----------------------------------!





    !
    ! === Support Routines ===
    !





    !-------------------------------------------------------------------------!
    Subroutine Error_Required( routine, NML, Field )
    !-------------------------------------------------------------------------!
      !
      ! PURPOSE
      !    Give standardized error when reqiered field is not found
      !
      ! AUTHOR
      !    Ingve Simonsen, Jan-2014, Paris.
      !
      implicit none
      character(len=*), intent(in) :: routine
      character(len=*), intent(in) :: NML
      character(len=*), intent(in) :: Field
      
      call Error_Failure( trim(adjustl(routine)), trim(adjustl(Field)) //"@" // trim(adjustl(NML)) // &
            " is required, but not given!" )
 
    end Subroutine Error_Required
    !-------------------------------------------------------------------------!


  End Subroutine Read_Input_Parameter_File
  !---------------------------------------!















!!$
!!$
!!$
!!$
!!$  !----------------------------------------------------!
!!$  Subroutine Read_Island_Dielectric_Corrections( ifile )
!!$  !----------------------------------------------------!
!!$    ! This routine should read the appropriate namelists needed
!!$    ! to correct the island dielectric functions....
!!$    ! 
!!$    ! Ingve Simonsen, Irvine, Aug. 2009
!!$    ! 
!!$    Implicit None
!!$    Integer, Intent(in) :: ifile
!!$    ! --- Local
!!$    Integer :: i
!!$
!!$    ! --- Namelist variables
!!$    !integer            :: Multipole_Order, Multipole_Position, No_Energy_Points 
!!$    !Character(len=20)  :: Island_Island_Interaction
!!$    
!!$    ! --- The NameList statments
!!$    !namelist /Numerics/    Multipole_Order, Multipole_Position, No_Energy_Points, Island_Island_Interaction
!!$    
!!$
!!$    ! Try to read the Island_Corrections fields (if needed)
!!$    do i=1, size(param%Materials%Island_Corrections,1)
!!$
!!$       select case( Trim(Adjustl(param%Materials%Island_Corrections(i))) )
!!$          
!!$       case( 'Finite_Size_Correction' )
!!$          call Read_Finite_Size_Correction_nml( ifile )
!!$
!!$          !case( 'Temperature_Correction' )
!!$          !call Read_Temperature_Coorection_nml( ifile )
!!$          !case( 'Surface_Correction' )
!!$          !call Read_Surface_Coorection_nml( ifile )
!!$
!!$       !case (NOT_SET)
!!$       !   ! ---- Noting (more) defined, so do NOTHING....
!!$         
!!$       case Default
!!$          ! --- Error : Option not supported....        
!!$          Write(*,*) " ERROR : Option NOT supported for Island_Corrections.... Exiting!"
!!$          Stop
!!$       End select
!!$       
!!$    End Do
!!$
!!$
!!$
!!$  contains
!!$
!!$  
!!$
!!$    !
!!$    ! --- 
!!$    !
!!$
!!$    Subroutine Read_Finite_Size_Correction_nml( ifile )
!!$      ! 
!!$      ! --- Reading the FINITE_SIZE_CORRECTION namelist
!!$      !
!!$      Implicit None
!!$      Integer, Intent(in) :: ifile
!!$      ! --- Local
!!$
!!$      ! --- Namelist variables
!!$      Character(len=50)  :: Material 
!!$      Real(wp)           :: Plasma_Frequency         ! Plasma frequency times hbar (eV)
!!$      Real(wp)           :: Fermi_Velocity           ! Fermi velocity times hbar (eV nm)
!!$      Real(wp)           :: Inverse_Relaxation_Time  ! Invers relaxation time times hbar (eV) 
!!$      Real(wp)           :: Amplitude_Parameter      ! Amplitude (dimensionless)
!!$
!!$      Write(*,*)
!!$      Write(*,*) " WARNING : the Finite_Size_Correction is still not properly implmented....."
!!$      Write(*,*)
!!$
!!$      ! --- The NameList statments
!!$      namelist /Finite_Size_Correction/   Material, Plasma_Frequency, &
!!$           Fermi_Velocity, Inverse_Relaxation_Time, Amplitude_Parameter 
!!$      
!!$      ! --- Set Initial Values        
!!$      Material                  =  NOT_SET
!!$      Plasma_Frequency          =  DEFAULT_FLOAT
!!$      Fermi_Velocity            =  DEFAULT_FLOAT
!!$      Inverse_Relaxation_Time   =  DEFAULT_FLOAT
!!$      Amplitude_Parameter       =  DEFAULT_FLOAT
!!$      
!!$
!!$      ! --- Read the namelist variables 
!!$      Read( ifile, nml=Finite_Size_Correction )
!!$
!!$      
!!$      ! --- Some Consistancy checking....
!!$      if ( trim(Adjustl(param%Materials%Island))/= Trim(Adjustl(Material)) ) then
!!$         Write(*,*)
!!$         Write(*,*) " ERROR : Island materials given in MATERIALS and FINITE_SIZE_CORRECTION are not consitant!"
!!$         stop ""
!!$      End if
!!$
!!$
!!$      ! --- Default data base....
!!$      Select Case ( Trim(Adjustl(Material)) )
!!$      case( 'ag','AG','Ag', 'aG' )
!!$         ! --- Set defaults for Ag
!!$         call Set_Default_Value( Plasma_Frequency,         9.170086_wp  )
!!$         call Set_Default_Value( Fermi_Velocity,           0.9149227_wp )
!!$         call Set_Default_Value( Inverse_Relaxation_Time,  0.01803_wp   )
!!$         call Set_Default_Value( Amplitude_Parameter,      0.8_wp       )
!!$         
!!$      case( 'au','AU','Au', 'aU' )
!!$         ! --- Seting defaults for Au
!!$         Write(*,*)  " Set Au default values if not set....."
!!$         stop " No Defsault Values for Au yet!"
!!$         
!!$      Case Default
!!$         ! --- No defaults so thay have to be provided via the input file....
!!$         call Provide_Input_Value( Plasma_Frequency,         "Plasma_Frequency"    )
!!$         call Provide_Input_Value( Fermi_Velocity,           "Fermi_Velocity"      )
!!$         call Provide_Input_Value( Inverse_Relaxation_Time,  "Inverse_Relaxation"  )
!!$         call Provide_Input_Value( Amplitude_Parameter,      "Amplitude_Parameter" )
!!$           
!!$      End Select
!!$
!!$      ! --- Copy the read or default vaiables to the derived type
!!$      param%Materials%Corrections%Finite_Size%Do_Correction    =  .true.
!!$      !
!!$      param%Materials%Corrections%Finite_Size%Material                =    Trim(Adjustl(Material))
!!$      param%Materials%Corrections%Finite_Size%Plasma_Frequency        =    Plasma_Frequency
!!$      param%Materials%Corrections%Finite_Size%Fermi_Velocity          =    Fermi_Velocity
!!$      param%Materials%Corrections%Finite_Size%Inverse_Relaxation_Time =    Inverse_Relaxation_Time  
!!$      param%Materials%Corrections%Finite_Size%Amplitude_Parameter     =    Amplitude_Parameter
!!$
!!$    End Subroutine Read_Finite_Size_Correction_nml
!!$
!!$
!!$    !
!!$    ! ------
!!$    !
!!$
!!$  End Subroutine Read_Island_Dielectric_Corrections
!!$  !------------------------------------------------!
!!$
!!$
!!$
!!$  
!!$  
!!$  Subroutine Set_Default_Value( Variable, Value )
!!$    !
!!$    ! --- Set the value of the valiable to Value
!!$    !
!!$    Implicit None
!!$    Real(wp), intent(InOut) :: Variable
!!$    Real(wp), intent(In)    :: Value
!!$
!!$    if (Variable > VERY_POSITIVE)  Variable = Value 
!!$    
!!$  End Subroutine Set_Default_Value
!!$  !------------------------------!
!!$  
!!$
!!$  !
!!$  ! -----
!!$  !
!!$  
!!$  
!!$  !--------------------------------------------------!
!!$  Subroutine Provide_Input_Value( Variable, String  )
!!$  !--------------------------------------------------!
!!$    !
!!$    ! --- Inform the user that Variable needs to be set...
!!$    !
!!$    Implicit None
!!$    Real(wp), intent(In)         :: Variable
!!$    Character(len=*), Intent(In) :: String
!!$    
!!$    if (Variable > VERY_POSITIVE) then
!!$       Write(*,*)
!!$       Write(*,*) " ERROR : Variable "//Trim(Adjustl(String)) // " is required!"
!!$       stop ""
!!$    endif
!!$
!!$  end Subroutine Provide_Input_Value
!!$  !---------------------------------!
!!$     
!!$
!!$
!!$End Module Get_Input_Parameters_Mod
!!$!----------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$
!!$  !================================================================================
!!$
!!$    ! ---
!!$    !
!!$
!!$    !---------------------------------!
!!$    Subroutine Set_Physical_Constants()
!!$    !---------------------------------!
!!$      !
!!$      ! Defines some physical conststants.
!!$      ! NOTE : The units.
!!$      !
!!$      Implicit None
!!$      param%PhysConst%epsilon_vacuum   = 1._wp                       ! Dielectric functin of vacuum             
!!$      param%PhysConst%Planck_over_2_pi = real(6.582122D-16,wp)       ! Plancks constant     [eV s]
!!$      param%PhysConst%Speed_of_Light   = real(2.99792458D17,wp)      ! Speed of light       [nm / s]
!!$      param%PhysConst%Mass_Electron    = &
!!$           real(0.511D6,wp) /(param%PhysConst%Speed_of_Light**2)     ! Mass of the electron [eV.nm-2.s2] 
!!$    End subroutine Set_Physical_Constants
!!$    !------------------------------------!     
!!$
!!$
!!$  End Subroutine Get_Input_Parameters
!!$  !----------------------------!
!!$
!!$
!!$









  !
  !
  ! ------------------------------------------------
  ! --- Some Auxiliary local Routines/Functions
  ! ------------------------------------------------
  !
  !


  !--------------------------------------------!
  Function Get_Vector_Dimension(Str) Result(Res)
  !--------------------------------------------!
    Implicit None
    Character(len=*), Intent(In) :: Str
    Integer                      :: Res
    ! --- Local
    Integer :: i
    
    res = 0
    do i=1,len(Str)
       if (Str(i:i)==",") res=res+1
    enddo
    
  End Function Get_Vector_Dimension
  !--------------------------------!


  !
  ! ---
  !

!!$
!!$  !----------------------------------------------------------------------!
!!$  Subroutine Read_Unknown_Named_Media_NML_InternalFile(fid, NML, imedia )
!!$  !----------------------------------------------------------------------!
!!$    !
!!$    ! --- This routine reads a MEDIA nml of KNOWN structure
!!$    !     but UNKNOWN name NML
!!$    !
!!$    !     Ingve Simonsen, Paris, Jul. 2010
!!$    !
!!$    !     Modified :
!!$    !                      IS   -  May, 2012
!!$    !
!!$    Use Error_Module
!!$    Use Tools_Read_Input_File_Module, only : NameList_Exists, Field_in_NameList_Exists, Get_StringValue_for_NameList_Field
!!$    Implicit None
!!$    Integer,           Intent(In)  :: fid
!!$    Character(len=*),  Intent(In)  :: NML
!!$    Integer,           Intent(In)  :: imedia
!!$    !Type(Media_Type),  Intent(InOut) :: Param_Media
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Read_Unknown_Named_Media_NML"
!!$    Integer            :: iostat
!!$    Character(len=250) :: line
!!$    Character(len=3000):: Internal_File
!!$    Logical            :: in_nml
!!$    Character(len=*), parameter :: PATH_NOT_SET="PATH_NOT_SET"
!!$    ! --- NML fields
!!$    !Character(len=STR_SHORT) :: Tag
!!$    Character(len=STR_SHORT) :: Material 
!!$    Character(len=STR_LONG)  :: Path
!!$    Real(wp), dimension(2)   :: Epsilon_Scale
!!$    ! For corrections (init to huge() as default value to detect a no-read):
!!$    logical                     :: temp_corr = .false.
!!$    logical                     :: finitesize_corr = .false.
!!$    real(wp), parameter         :: NOTREAD = huge(1.0_wp)
!!$
!!$    real(wp)                    :: temperature            ! K
!!$    real(wp)                    :: Ep                     ! eV
!!$    real(wp)                    :: d_Ep_dT                ! eV/K
!!$    real(wp)                    :: hbar_fermi_velocity    ! eV nm 
!!$    real(wp)                    :: hbar_inv_tau           ! eV
!!$    real(wp)                    :: B_plasmonshift         ! eV^2 nm 
!!$    !
!!$    Character(len=STR_LONG)     :: Field
!!$    Character(len=300) :: Provided_String
!!$    !
!!$    namelist /Media/ Material, Path, Epsilon_Scale, temperature, Ep, d_Ep_dT, &
!!$                        hbar_fermi_velocity, hbar_inv_tau, &
!!$                        temp_corr, finitesize_corr, B_plasmonshift
!!$
!!$
!!$    ! --- Defaults
!!$    Path                 = PATH_NOT_SET
!!$    temp_corr            = .false.
!!$    finitesize_corr      = .false.
!!$    temperature          = NOTREAD
!!$    Ep                   = NOTREAD
!!$    d_Ep_dT              = NOTREAD
!!$    hbar_fermi_velocity  = NOTREAD
!!$    hbar_inv_tau         = NOTREAD
!!$    B_plasmonshift       = 0.0_wp
!!$    
!!$
!!$    ! --- Some error checking
!!$    if (imedia>size( Param%Media,1) ) call Error_Failure(routine, "Internal error; imedia> size(Param%Media)")
!!$    
!!$    
!!$    ! --- Rewind the file
!!$    rewind(fid)
!!$    
!!$
!!$    !--- Read the file line by line and process it
!!$    iostat = 0
!!$    in_nml = .false.
!!$
!!$    
!!$    line_loop : do while (iostat == 0 )
!!$
!!$ 
!!$       ! --- Read a line
!!$       read(fid,"(A)",iostat=iostat) line
!!$       if (iostat/=0)  &
!!$            Call Error_Failure( routine, "NameList "//Trim(Adjustl(NML))//" not found (or reading failure)!" )
!!$
!!$       ! --- Left Adjust the line
!!$       line = adjustl(line)
!!$
!!$
!!$       ! ------------------------
!!$       ! --- The NML start-field
!!$       ! ------------------------
!!$
!!$       !Write(*,*) "Comparing : ", line(1:len(trim(adjustl(nml)))+1) , "#&"//trim(adjustl(nml))  
!!$
!!$       ! --- Are we in the NML?
!!$       !if ( line(1:len(trim(adjustl(nml)))+1) == "&"//trim(adjustl(nml)) ) then
!!$       if ( trim(adjustl(line)) == "&"//trim(adjustl(nml)) ) then
!!$          ! --- The start of the NML
!!$          in_nml = .true.
!!$          ! ---The first line of the Internal File
!!$          Internal_File =  "&Media "
!!$          ! --- Process the rest of the line
!!$          Internal_File =  trim(Adjustl(Internal_File)) // " " &
!!$               // Trim(Adjustl(line( len(trim(adjustl(nml)))+2:) )) 
!!$          ! --- Are we at the end of the NML?
!!$          if (at_the_end_of_nml()) exit
!!$
!!$       elseif (in_nml) then           
!!$          ! --- if we are in the actual namelist
!!$          !     generate the rest of the Internal File.....
!!$          Internal_file =  trim(adjustl(Internal_File)) // " " // Trim(Adjustl(line)) 
!!$             
!!$          ! --- Are we at the end of the NML?
!!$          if (at_the_end_of_nml()) exit
!!$       Endif
!!$
!!$    enddo line_loop
!!$
!!$
!!$
!!$   
!!$    ! --------------------------------
!!$    ! --- NML Found
!!$    ! --------------------------------
!!$
!!$
!!$    ! --- Read the NML from the Internal File
!!$    read(Internal_File,nml=Media,iostat=iostat) 
!!$    if ( iostat /= 0) &
!!$         call Error_Failure ( routine , "Reading failure for namelist "//Trim(Adjustl(NML)) )
!!$ 
!!$    ! --- If Path Not set
!!$    If ( Trim(Adjustl(Path))==Trim(Adjustl(PATH_NOT_SET)) ) then
!!$       Path = Param%Global%SOPRA_ROOT
!!$    endif
!!$      
!!$
!!$    ! --- Add a path to the filename if needed
!!$    !call Add_PATH( )
!!$
!!$
!!$    ! --- Check for inconsistent correction values
!!$    if (finitesize_corr .or. temp_corr) then 
!!$      if (Ep==NOTREAD) then
!!$        call Error_Failure(routine,"Corrections need Ep!")
!!$      end if
!!$      if (hbar_inv_tau==NOTREAD) then
!!$        call Error_Failure(routine,"Corrections need hbar_inv_tau!")
!!$      end if
!!$    end if
!!$
!!$    if (temp_corr) then
!!$      if (temperature==NOTREAD) then 
!!$        call Error_Failure(routine,"Temp. corrections need temperature!")
!!$      end if
!!$      if (d_Ep_dT==NOTREAD) then
!!$        call Error_Failure(routine,"Temp. corrections need d_Ep_dT!")
!!$      end if
!!$    end if
!!$
!!$    if (finitesize_corr) then
!!$      if (hbar_fermi_velocity==NOTREAD) then
!!$        call Error_Failure(routine,"Size corrections need hbar_fermi_velocity!")
!!$      end if
!!$    end if
!!$    
!!$
!!$
!!$    ! --- Copy the data
!!$    ! ------------------------------------------------
!!$    !Param%Media(imedia)%Tag        =  Trim(adjustl(Tag))
!!$    Param%Media(imedia)%Material            =  Trim(adjustl(Material))
!!$    Param%Media(imedia)%Path                =  Trim(adjustl(Path))
!!$    Param%Media(imedia)%temp_corr           = temp_corr
!!$    Param%Media(imedia)%finitesize_corr     = finitesize_corr
!!$    Param%Media(imedia)%temperature         = temperature
!!$    Param%Media(imedia)%Ep                  = Ep
!!$    Param%Media(imedia)%d_Ep_dT             = d_Ep_dT
!!$    Param%Media(imedia)%hbar_fermi_velocity = hbar_fermi_velocity 
!!$    Param%Media(imedia)%hbar_inv_tau        = hbar_inv_tau
!!$    Param%Media(imedia)%B_plasmonshift      = B_plasmonshift
!!$
!!$    ! --- 
!!$    Field = "Epsilon_Scale"
!!$    if ( Field_in_NameList_Exists( fid, nml, Field)  )then
!!$       Param%Media(imedia)%Epsilon_Scale =  Epsilon_Scale
!!$    else
!!$       Param%Media(imedia)%Epsilon_Scale = (/ 1._wp,  1._wp /)
!!$    endif
!!$
!!$    !-----TESTING ------------------------------------------
!!$    !Write(*,*)
!!$    !Write(*,*) "           * Processing : ", Trim(adjustl(          Internal_file ))
!!$    !Write(*,*) "           Read : "
!!$    !Write(*,*) "       Tag  : ", Trim(adjustl( Param%Media(imedia)%Tag  ))
!!$    !Write(*,*) "                Material : ", Trim(adjustl( Param%Media(imedia)%Material ))
!!$    !Write(*,*) "       Path     : ", Trim(adjustl( Param%Media(imedia)%Path ))
!!$    !-----TESTING ------------------------------------------
!!$
!!$
!!$
!!$  contains
!!$
!!$    !--------------------------------------!
!!$    Function at_the_end_of_nml() Result(res)
!!$    !--------------------------------------!
!!$      Implicit None
!!$      Logical :: Res
!!$
!!$
!!$      ! --- Default is not found
!!$      Res = .false.
!!$
!!$      ! ------------------------
!!$      ! --- The NML end-field
!!$      ! ------------------------
!!$      !
!!$     
!!$      
!!$      ! --- Is the NML end field found?
!!$      
!!$      ! --- Left adjust the line
!!$      line = adjustl(line)
!!$      if ( line(1:1) == "/") Res=.true.
!!$      
!!$      
!!$      ! --- Right adjust the line
!!$      line = adjustr(line)
!!$      if ( line(len(line):len(line)) == "/") Res = .true.
!!$      
!!$    End Function at_the_end_of_nml
!!$    !-----------------------------!




!!$
!!$    !--------------------!
!!$    Subroutine Add_PATH()
!!$    !--------------------!  
!!$      !
!!$      ! --- Add the SOPRA_PATH to the file name if it does not exist
!!$      !
!!$      Implicit None
!!$      if ( (index(File,"/")==0) ) then !.and. (index(File,"\")==0) ) then
!!$         File = Trim(Adjustl(Param%Global%SOPRA_ROOT))//Trim(Adjustl(File))
!!$      endif
!!$      
!!$    End Subroutine Add_PATH
!!$    !----------------------!


!!$
!!$  End Subroutine Read_Unknown_Named_Media_NML_InternalFile
!!$  !-------------------------------------------------------!
!!$



!!$
!!$  ! --------------------------------------------------!
!!$  Function Does_NameList_Exist(fid, NML)   Result(Res) 
!!$  ! --------------------------------------------------!
!!$    ! 
!!$    ! --- This function checks if a NameList exists 
!!$    !     in an already opened (parameter) file of 
!!$    !     fileID 
!!$    !
!!$    !     Ingve Simonsen, Paris, Apr 2012
!!$    !
!!$    Implicit None
!!$    Integer,          Intent(In) :: fid
!!$    Character(len=*), Intent(In) :: NML   ! = NameList
!!$    Logical                      :: Res
!!$    ! --- Local
!!$    Integer :: iostat
!!$    Character(len=250) :: line
!!$
!!$    ! --- Default Value
!!$    Res = .false.
!!$
!!$    ! --- Rewind the file
!!$    rewind(fid)
!!$    
!!$    !--- Read the file line by line and process it
!!$    iostat = 0
!!$    line_loop : do while (iostat == 0 )
!!$
!!$       ! --- Read a line
!!$       read(fid,"(A)",iostat=iostat) line
!!$
!!$       ! --- Process line......
!!$       if ( trim(adjustl(line)) == "&"//trim(adjustl(NML)) ) then
!!$          Res = .true.
!!$          exit
!!$       End if
!!$
!!$    enddo line_loop
!!$    
!!$  End Function Does_NameList_Exist
!!$  ! --------------------------------------------------!
!!$
!!$
!!$  ! ------------------------------------------------------------------!
!!$  Function Does_Field_in_NameList_Exist(fid, NML, Field)   Result(Res) 
!!$  ! ------------------------------------------------------------------!
!!$    ! 
!!$    ! --- This function checks if a field (Field) exists in 
!!$    !     a NameList (NML) in an already opened (parameter) file (fid)
!!$    !
!!$    !     Ingve Simonsen, Paris, Apr 2012
!!$    !
!!$    Implicit None
!!$    Integer,          Intent(In) :: fid
!!$    Character(len=*), Intent(In) :: NML   ! = NameList
!!$    Character(len=*), Intent(In) :: Field ! = Field
!!$    Logical                      :: Res
!!$    ! --- Local
!!$    Integer :: iostat, Field_Length
!!$    Character(len=250) :: line
!!$    Logical            :: Found_NML
!!$
!!$
!!$    ! --- Initialize
!!$    Field_Length = len( Trim(adjustl(Field)) )
!!$
!!$    ! --- Default Value
!!$    Res = .false.
!!$
!!$    ! --- Rewind the file
!!$    rewind(fid)
!!$
!!$ 
!!$    ! --- Find the namelist (NML)
!!$    ! ----------------------------------
!!$    iostat = 0
!!$    ! ... Read the file line by line and process it
!!$    do while (iostat == 0 )
!!$
!!$       ! --- Read a line
!!$       read(fid,"(A)",iostat=iostat) line
!!$
!!$       ! --- Process line......
!!$       if ( trim(adjustl(line)) == "&"//trim(adjustl(NML)) ) then
!!$          Found_NML = .true.
!!$          exit
!!$       End if
!!$
!!$    enddo
!!$
!!$
!!$    ! --- Find the field (Field)
!!$    ! ---------------------------------
!!$    If (Found_NML) then
!!$ 
!!$      ! ... Read the file line by line and process it
!!$       do while (iostat == 0 )
!!$       
!!$          ! --- Read a line
!!$          read(fid,"(A)",iostat=iostat) line
!!$                  
!!$          ! --- Process line......
!!$          ! -----------------------------------
!!$          ! ... Exit if End of NameList is found
!!$          If ( trim(adjustl(line)) == "/" ) then
!!$             exit
!!$          End if
!!$          ! ... Is the field found?
!!$          line = trim(adjustl(line))   ! stripping off trailing blanks
!!$          if ( line(1:Field_Length) == trim(adjustl(Field)) ) then
!!$             Res=.true.
!!$             exit
!!$          End if
!!$
!!$       enddo
!!$      
!!$    End If
!!$
!!$    
!!$  End Function Does_Field_in_NameList_Exist
!!$  ! --------------------------------------------------!
!!$


!!$
!!$
!!$  !--------------------------------------------------------!
!!$  Subroutine Read_Unknown_Named_Media_NML(fid, NML, imedia)
!!$  !--------------------------------------------------------!  
!!$    Use Error_Module
!!$    Use SFL_Logical_Units,   only : SFL_Get_Free_Unit
!!$    Implicit None
!!$    Integer,           Intent(In) :: fid
!!$    Character(len=*),  Intent(In) :: NML
!!$    Integer,           Intent(In) :: imedia
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Read_Unknown_Named_Media_NML"
!!$    Integer            :: iostat, fid_scratch
!!$    Character(len=150) :: line
!!$    !Character(len=3000):: Internal_File
!!$    Logical            :: in_nml
!!$
!!$    ! --- NML fields
!!$    Character(len=STR_SHORT) :: Tag
!!$    Character(len=STR_LONG)  :: File
!!$    
!!$    namelist /Media/ Tag, File
!!$
!!$    Write(*,*) " Processing Media : ", trim(adjustl(NML))
!!$
!!$
!!$    ! --- 
!!$    Tag  = "NOT_SET"
!!$    FILE = "NOT_SET"
!!$
!!$    ! --- Rewind the file
!!$    rewind(fid)
!!$
!!$    ! -- Open the scratch file....
!!$    call  SFL_Get_Free_Unit(fid_scratch)
!!$    open(unit=fid_scratch, file=Trim(Adjustl(Param%InOut%Input_File_Name))//"_Scratch")
!!$    
!!$
!!$    !--- Read the file line by line and process it
!!$    iostat = 0
!!$    in_nml = .false.
!!$    line_loop : do while (iostat == 0 )
!!$
!!$       ! --- Read a line 
!!$       read(fid,"(A)",iostat=iostat) line
!!$       if (iostat/=0)  &
!!$            Call Error_Failure( routine, "NameList "//Trim(Adjustl(NML))//" not found (or reading failure)!" )
!!$       !!!if (iostat<0) Call Error_Failure( routine, "Reading failure on file " // Trim(Adjustl(Param%InOut%Input_File_Name)) )
!!$
!!$       ! ------------- TESTING --------------
!!$       !Write(*,*) " line :", Trim(line) 
!!$
!!$
!!$       ! --- Left Adjust the line
!!$       line = adjustl(line)
!!$
!!$
!!$
!!$       ! ------------------------
!!$       ! --- The NML start-field
!!$       ! ------------------------
!!$
!!$       ! --- Are we in the NML?
!!$       if ( line(1:len(trim(adjustl(nml)))+1) == "&"//trim(adjustl(nml)) ) then
!!$          ! --- The start of the NML
!!$          in_nml = .true.
!!$          ! ---The first line of the Internal File
!!$          Write(fid_scratch,*)  "&Media " // Trim(line(len(trim(adjustl(nml)))+2:len(line)))
!!$          ! --- Are we at the end of the NML?
!!$          if (at_the_end_of_nml()) exit
!!$
!!$       elseif (in_nml) then           
!!$          ! --- If we are in the actual namelist
!!$          !     generate the rest of the Internal File.....
!!$          Write(fid_scratch,*) Trim(line)
!!$       
!!$          ! --- Are we at the end of the NML?
!!$          if (at_the_end_of_nml()) then
!!$             ! --- Close the file
!!$             Close(fid_scratch)
!!$             exit
!!$          endif
!!$
!!$       Endif
!!$
!!$    enddo line_loop
!!$
!!$
!!$    
!!$    ! --------------------------------
!!$    ! --- NML Found
!!$    ! --------------------------------
!!$    
!!$    ! --- Read the NML from the scratch file
!!$    !rewind(fid_scratch)
!!$    ! --- DO WE REALLY NEED TO REOPEN THE FILE??????
!!$    Open(fid_scratch,file=Trim(Adjustl(Param%InOut%Input_File_Name))//"_Scratch", status="old")
!!$    read(fid_scratch, nml=Media, iostat=iostat)
!!$    if ( iostat<0) &
!!$         call Error_Failure ( routine , "Reading failure for namelist "//Trim(Adjustl(NML)) )
!!$    
!!$
!!$
!!$    !-----TESTING -------------
!!$    Write(*,*) "FOR NML : ", Trim(Adjustl(NML)) 
!!$    Write(*,*) " Tag    : ", Trim(adjustl(Tag))
!!$    Write(*,*) " File   : ", Trim(adjustl(File))
!!$    Write(*,*)
!!$    !-----TESTING -------------
!!$
!!$
!!$
!!$    ! -- Close (and delete) the scratch file
!!$    Close(fid_scratch)
!!$
!!$
!!$    ! --- Add a path to the filename if needed
!!$    !call Add_PATH( )
!!$
!!$
!!$    ! --- Copy the date
!!$    ! --------------------------------
!!$    Param%Media(imedia)%Tag    =  Trim(adjustl(Tag))
!!$    Param%Media(imedia)%File   =  Trim(adjustl(File))
!!$    !Param%Media(imedia)%Tag   =  " .. "
!!$    !Param%Media(imedia)%File   = ".."
!!$   
!!$
!!$  contains
!!$
!!$
!!$    !---------------------------------------!
!!$    Function at_the_end_of_nml() Result(res)
!!$    !---------------------------------------!  
!!$      Implicit None
!!$      Logical :: Res
!!$      
!!$
!!$      ! --- Default is not found
!!$      Res = .false.
!!$      
!!$      ! --- Is the NML end field found?
!!$      
!!$      ! --- Left adjust the line
!!$      line = adjustl(line)
!!$      if ( line(1:1) == "/") Res=.true.
!!$      
!!$      
!!$      ! --- Right adjust the line
!!$      line = adjustr(line)
!!$      if ( line(len(line):len(line)) == "/") Res = .true.
!!$      
!!$    End Function at_the_end_of_nml
!!$    !-----------------------------!
!!$
!!$
!!$    !--------------------!
!!$    Subroutine Add_PATH()
!!$    !--------------------!  
!!$      !
!!$      ! --- Add the SOPRA_PATH to the file name if it does not exist
!!$      !
!!$      Implicit None
!!$      if ( (index(File,"/")==0) ) then !.and. (index(File,"\")==0) ) then
!!$         File = Trim(Adjustl(Param%Global%SOPRA_ROOT))//Trim(Adjustl(File))
!!$      endif
!!$      
!!$    End Subroutine Add_PATH
!!$    !----------------------!
!!$
!!$
!!$
!!$  End Subroutine Read_Unknown_Named_Media_NML
!!$  !-----------------------------------------!
!!$  








End Module Read_Input_File_Module
!--------------------------------!
