Module SOPRA_Dielectric_Function_Module


  ! --- Module global use statments....
  Use Shared,   only : wp


  !  Create generic name 
  interface SOPRA_dielectric_function
     module procedure &
          SOPRA_dielectric_function_vector,     &
          SOPRA_dielectric_function_scalar
  end interface


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: SOPRA_dielectric_function

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  
  ! --- Some Global parameters
  Character(len=*), parameter  :: routine = "SOPRA_dielectric_function" 
  Character(len=*), parameter  :: Default_SOPRA_ext  = ".nk"


Contains



  !------------------------------------------------------------------------------!
  Subroutine SOPRA_dielectric_function_vector(arg,epsilon,material,path,arg_unit)
  !------------------------------------------------------------------------------!
    !
    ! ---  The main routne returns the SOPRA dielctric function for 
    !      the given material and wavelength/energy in MICRONS/eV
    !      Default is argument unit is microns
    !
    !      I. Simonsen, Paris, 2007.
    !      
    !
    Use  SFL_Util,           only : SFL_Assert  
    !Use  SFL_Error_Handling, only : SFL_Error_Fatal
    Use  Error_Module, only : Error_Fatal
    Implicit None
    Real(wp),         intent(in)          :: arg(:)
    Complex(wp),      intent(out)         :: epsilon(:)
    Character(len=*), intent(in)          :: material
    Character(len=*), intent(in)          :: path
    Character(len=*), intent(in),optional :: arg_unit
    ! Local
    Character(len=50)   :: local_arg_unit 

    ! Check the dimmensions
    call SFL_Assert(size(arg,1)==size(epsilon,1),  &
         Trim(Adjustl(routine))// ": arg and epsilon do not have the same dimensions!")

    ! Copy the optional argument if present
    if (present(arg_unit)) then
       local_arg_unit = arg_unit
    else
       local_arg_unit = "microns"   ! Default....
    end if

    select case( trim(adjustl(local_arg_unit)) )
       ! Microns
    case("microns")
       call SOPRA_dielectric_function_wavelength__(arg,epsilon,material,path)
    case("eV")
       call SOPRA_dielectric_function_energy__(arg,epsilon,material,path)
    case default
       !call SFL_Error_Fatal(routine, "Value of UNIT not supported!   Allowed values are: 'microns' or 'eV'")
       call Error_Fatal(routine, "Value of UNIT not supported!   Allowed values are: 'microns' or 'eV'")
    End select

  End Subroutine SOPRA_dielectric_function_vector
  !------------------------------------------------------------------------------!

  !
  ! -----
  !

  !------------------------------------------------------------------------------!
  Subroutine SOPRA_dielectric_function_scalar(arg,epsilon,material,path,arg_unit)
  !------------------------------------------------------------------------------!
    !
    ! ---  Wrapper routine ....
    !
    !Use  SFL_Util,           only : SFL_Assert  
    !Use  SFL_Error_Handling, only : SFL_Error_Fatal
    Implicit None
    Real(wp),         intent(in)          :: arg
    Complex(wp),      intent(out)         :: epsilon
    Character(len=*), intent(in)          :: material
    Character(len=*), intent(in)          :: path
    Character(len=*), intent(in),optional :: arg_unit

    ! Local
    Character(len=200)  :: local_path  
    Real(wp)            :: arg_vec(1) 
    Complex(wp)         :: epsilon_vec(1) 

    ! Copy the data......
    arg_vec(1)     = arg

    ! Copy the optional argument if present
    if (present(arg_unit)) then
       call SOPRA_dielectric_function_vector(arg_vec,epsilon_vec,material,path,arg_unit)
    else
       call SOPRA_dielectric_function_vector(arg_vec,epsilon_vec,material,path)
    end if
    
    ! Before returning....
    epsilon = epsilon_vec(1)
    
  End Subroutine SOPRA_dielectric_function_scalar
  !------------------------------------------------------------------------------!

  !
  ! -----
  !


  !------------------------------------------------------------------------------!
  Subroutine SOPRA_dielectric_function_wavelength__(wavelength,epsilon,material,path)
  !------------------------------------------------------------------------------!
    !
    ! ---  This routne returns the SOPRA dielctric function for 
    !      the given material and wavelength in MICRONS
    !
    !      I. Simonsen, Paris, 2007.
    !      
    !
    Use SFL_Interpolation,  only : SFL_AkimaSpline
    !Use SFL_Error_Handling, only : SFL_Error_Failure,SFL_Error_Fatal
    Use Error_Module, only : Error_Failure,Error_Fatal
    Implicit None
    Real(wp),        intent(in)  :: wavelength(:)
    Complex(wp),     intent(out) :: epsilon(:)
    Character(len=*),intent(in)  :: material  
    Character(len=*),intent(in)  :: path
    ! Local
    Character(len=200)          :: filename
    Logical			:: exi
    Integer			:: lines,start,i, ierr
    Real(wp),    Allocatable	:: x(:)
    Complex(wp), Allocatable    :: y(:)
    Real(wp)                    :: tmp(2),x1,x2,dx
    Integer			:: unit
    Complex(wp)                 :: slope
    Complex(wp),parameter	:: imu=(0._wp,1._wp)

    ! Opens the relevant data file for the given material
    ! Notice that the data-files contains the values for the 
    ! index of refracction, n,k extacted from the data base of
    ! SOPRA.
    ! Therefore "epsilon = epsilon**2" at the bottom of this routine    
    !

    filename = Trim(Adjustl(path))//Trim(Adjustl(material))//Default_SOPRA_ext
    Inquire(file = filename, exist = exi)
    If(.not. exi) & !call SFL_Error_Fatal(routine, "File "//trim(adjustl(filename))//" does not exist!")
         call Error_Fatal(routine, "File "//trim(adjustl(filename))//" does not exist!")

    ! --- Read the data.....
    Open(unit=10,file=filename,status="old")
    Read(10,*) unit,x1,x2,lines
    dx = (x2 - x1)/(lines-1)

    Allocate( x(lines), y(lines) )
    Select Case (unit)
    Case(1)
       ! Unit = Electron Volt (eV)
       Do i=1,lines 
          Read(10,*) tmp(1), tmp(2)
          x(i) = x1 + (i-1)*dx
          y(i) = tmp(1)+imu*tmp(2)
       Enddo
       ! Convert to microns...... 
       x = eV2micron__(x)   !  CHECK THIS......
      Case(2)
       ! Unit = WaveLenght (um)
       Do i=lines,1,-1 
          Read(10,*) tmp(1), tmp(2)
          x(i) = x1 + (lines-i)*dx
          y(i) = tmp(1)+imu*tmp(2)
       Enddo
       !x(:) = 1.243_wp/x(:) ! Conversion  um-->eV    CHECK THIS......
    End Select

    Close(10)

    ! ---- Do the interpolation
    call SFL_AkimaSpline(X,Y,ierr,wavelength,epsilon)
    if (ierr<0) then
       !call SFL_Error_Failure(routine, "SFL_AkimaSpline reported an error") 
       call Error_Failure(routine, "SFL_AkimaSpline reported an error") 
    endif
    !    Do i=1,Size(wavelength,1)
    !       start=locate(Real(x(:),wp),Real(wavelength(i),wp))
    !       If ((start==0).Or.(start==lines)) then
    !          Write(*,*) 'ERROR (get_epsilon) : Wavelength not in range : ',wavelength(i)
    !          stop
    !       End If
    !       ! Linear interpolation
    !       slope = (y(start+1)-y(start))/(x(start+1)-x(start))
    !       Epsilon(i) = y(start) + slope*(wavelength(i)-x(start))
    !    Enddo

    

    !do i=1,size(x)
    !   write(*,*) x(i), y(i)
    !enddo

    !do i=1,size(epsilon)
    !   write(*,*) wavelength(i), epsilon(i)
    !enddo


    ! --- Calculates the dielectric constant (from the refraction index)
    epsilon = epsilon**2
    Deallocate(x,y)

    Return


  contains
    
    FUNCTION locate(xx,x)
      !USE nrtype
      IMPLICIT NONE
      REAL(wp), DIMENSION(:), INTENT(IN) :: xx
      REAL(wp), INTENT(IN) :: x
      INTEGER :: locate
      INTEGER :: n,jl,jm,ju
      LOGICAL :: ascnd
      n=size(xx)
      ascnd = (xx(n) >= xx(1))
      jl=0
      ju=n+1
      do
         if (ju-jl <= 1) exit
         jm=(ju+jl)/2
         if (ascnd .eqv. (x >= xx(jm))) then
            jl=jm
         else
            ju=jm
         end if
      end do
      if (x == xx(1)) then
         locate=1
      else if (x == xx(n)) then
         locate=n-1
      else
         locate=jl
      end if
    END FUNCTION locate
    
  End Subroutine SOPRA_dielectric_function_wavelength__
  !------------------------------------------------------------------------------!


  !
  ! -----
  ! 

  !------------------------------------------------------------------------------!
  Subroutine SOPRA_dielectric_function_energy__(energy,epsilon,material,path)
  !------------------------------------------------------------------------------!
    !
    ! ---  This routne returns the SOPRA dielctric function for 
    !      the given material and energy in eV
    !
    !      I. Simonsen, Paris, 2007.
    !      
    !
    Implicit None
    Real(wp),        intent(in)  :: energy(:)
    Complex(wp),     intent(out) :: epsilon(:)
    Character(len=*),intent(in)  :: material 
    Character(len=*),intent(in)  :: path
    
    call SOPRA_dielectric_function_wavelength__(eV2micron__(energy),epsilon,material,path)
    
  End Subroutine SOPRA_dielectric_function_energy__
  !------------------------------------------------------------------------------!


  !
  ! ----- 
  !


  !------------------------------------------------------------------------------!
  Elemental Function eV2micron__(indata) result(res)
  !------------------------------------------------------------------------------!
    !
    ! --- This routine converts from eV to microns
    ! 
    !     I. Simonsen, Paris, 2007.
    !
    Implicit none
    real(wp), intent(in) :: indata
    real(wp)             :: res
    ! Local
    real(wp), parameter :: convertion = 1.243_wp

    res = convertion / indata
    
  End Function eV2micron__
  !------------------------------------------------------------------------------!



End Module SOPRA_Dielectric_Function_Module
!------------------------------------------------------------------------------!


! =========================================================
! =========================================================
! =========================================================
!!$
!!$
!!$Program Dielectric_Test
!!$  Use SOPRA_Dielectric_Function_Module
!!$  Implicit None
!!$  Integer, parameter :: N=3
!!$  Integer     :: i
!!$  real(wp)    :: wavelength(N) = [0.4, 0.6328, 0.6127]
!!$  real(wp)    :: energy(N)     = [0.6, 0.8, 6.6]
!!$  complex(wp) :: eps(N)
!!$
!!$
!!$
!!$  ! vs. wavelength
!!$  !call SOPRA_dielectric_constant_wavelength(wavelength,eps,"ag",".")
!!$  call SOPRA_dielectric_function(wavelength,eps,"ag")
!!$  Write(*,*) " Dielectric function of Ag vs. wavelength (um)"
!!$  do i=1,N
!!$     write(*,*) "  ", wavelength(i), eps(i)
!!$  enddo
!!$
!!$
!!$
!!$  ! vs. energy
!!$  !call SOPRA_dielectric_function_energy(energy,eps,"ag",".")
!!$  call SOPRA_dielectric_function(energy,eps,"ag","eVs")
!!$  Write(*,*) " Dielectric function of Ag vs. energy (eV)"
!!$  do i=1,N
!!$     write(*,*) "  ", energy(i), eps(i)
!!$  enddo
!!$  
!!$End Program Dielectric_Test
