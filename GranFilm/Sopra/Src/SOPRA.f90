
Program Dielectric_Test
  !
  ! Tool to report the dielectric functions of the SOPRA data base....
  !
  !  Ingve Simonsen, Paris Oct. 2007.
  !
  Use SOPRA_Dielectric_Function_Module
  Use SFL_Precision, only : wp=>sp
  Use SFL_Get_Options
  Use SFL_Error_Handling, only : SFL_Error_Init,  SFL_Error_Type
  Implicit None
  Real(wp)              :: Wavelength
  complex(wp)           :: eps
  Character(len=100)    :: Material
  Character(len=2000)   :: Path = "../../DataBase/", Tmp_Path
  Type( SFL_Error_Type) :: error


  ! Init the error 
  call SFL_Error_Init(error)

  ! ---------------------------------
  ! --- Set the default SOPRA PATH
  ! ---------------------------------
  call getenv("SOPRA_ROOT", Path)
  !call getenv("SOPRA_ROOT", Tmp_Path)
  !if (len( trim(adjustl(Tmp_Path)) ) /=0 ) then
  !   Path = trim(adjustl(Tmp_Path)) // "/"
  !endif
  !Write(*,*) Trim(adjustl(Path))

  ! ---------------------------------
  ! --- Get command line options
  ! ---------------------------------
  call AddOption("w",   "The wavelngth (microns)",           TakeValues=.true.)
  call AddOption("m",   "The material",                      TakeValues=.true.)
  call AddOption("db",  "The SOPRA database root-path (Default: SOPRA_ROOT env. var.)",      TakeValues=.true.)

  ! --- Get the input from the command line....
  call ParseOption("[option]")

  ! --- Start New simulation
  call GetOption(wavelength,          "w")
  call GetOption(Material,            "m")
  if (PresentOption("db"))   &
       call GetOption(Path,            "db")


  ! --- Get the dielectric function
  ! -------------------------------------
  call SOPRA_dielectric_function(wavelength,eps,Material,Path)
  

  Write(*,*) 
  Write(*,*) "  Wavelength (microns)    Espilon(omega)    "
  Write(*,*) "-------------------------------------------------"
  Write(*,*) "   ", wavelength, "    ",eps
  Write(*,*) 
  
End Program Dielectric_Test
