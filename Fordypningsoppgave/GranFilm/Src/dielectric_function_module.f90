!-------------------------------!
Module Dielectric_Function_Module
!-------------------------------!

  ! 
  ! This module takes care of setting up the dielectric functions 
  !
  ! History :
  !
  ! Ingve Simonsen, Irvine Aug. 2009
  !




  
  ! ---------------------------
  ! --- The use statement
  ! ---------------------------
  Use Shared,    only : wp, imu, pi, param


  ! --------------------------------------
  ! --- The Publicly avaiable quantities
  ! --------------------------------------
  Public  :: Get_Dielectric_Function

  
  !Public :: Initialize_dielectric_constants,            &
  !          Initialize_dielectric_constants_island 
  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private 





  ! Some private variables.....
  !!!Real(wp), Save      :: omega_p,inv_relaxation_time,fermi_velocity,surf_eff,disp_T





Contains



!!$
!!$  !------------------------------------------!
!!$  Subroutine Initialize_dielectric_constants()
!!$  !------------------------------------------!
!!$    Use Special_Functions_mod,   only : arth, locate
!!$    Implicit None
!!$    ! --- Local
!!$    integer        :: di, ienergy
!!$    Real(wp)       :: denergy,de
!!$    Real(wp)       :: dwavelength 
!!$    Real(wp)       :: eps
!!$
!!$
!!$    ! ------------------------------------------
!!$    ! --- Fill the energy/wavelength vectors
!!$    ! ------------------------------------------
!!$    eps      = 1000* epsilon(0._wp)
!!$
!!$    ! Define the energy and wavelength steps
!!$    denergy       =  (param%Source%Energy_Range(2)-param%Source%Energy_Range(1))         
!!$    dwavelength   =  (param%Source%Wavelength_Range(2)-param%Source%Wavelength_Range(1)) 
!!$
!!$    ! ... Identify the energy range 
!!$    if  (  (abs(denergy)<eps) .and. ( abs(dwavelength)<eps)  ) then 
!!$       ! An error occured
!!$       Write(*,*) " ERROR : either energy_range or wavelength_range must be given!"
!!$       stop ""
!!$    End if
!!$    if  (  (abs(denergy)>eps) .and. ( abs(dwavelength)>eps)  ) then 
!!$       ! An error occured
!!$       Write(*,*) " ERROR : both energy_range and wavelength_range cannot be given!"
!!$       stop ""
!!$    End if
!!$
!!$    ! --- At this point either the energy limits or wavelength limtis
!!$    !     are given, but not both.  
!!$    !     We sample equivadistantly for the variable where limits are given
!!$    !
!!$    ! --- Fill Energy vector (with uniform sampling)
!!$    if ( abs(dwavelength)<eps ) then
!!$       ! Energy
!!$       denergy       =  (param%Source%Energy_Range(2)-param%Source%Energy_Range(1))         &
!!$                         /  (param%Numerics%NEnergy-1)
!!$       param%Numerics%Energy   =  arth(param%Source%Energy_Range(1),denergy,param%Numerics%NEnergy)
!!$       ! Wavelength
!!$       param%Numerics%Wavelength = Energy_to_Wavelength( param%Numerics%Energy )
!!$       param%Source%Wavelength_Range = (/ param%Numerics%Wavelength(1),  &
!!$                      param%Numerics%Wavelength( param%Numerics%NEnergy ) /)   
!!$    End if
!!$    !
!!$    ! --- Fill the Wavelength vector (with uniform sampling)
!!$    if ( abs(denergy)<eps ) then
!!$       dwavelength       =  (param%Source%Wavelength_Range(2)-param%Source%Wavelength_Range(1))         &
!!$                         /  (param%Numerics%NEnergy-1)
!!$       param%Numerics%Wavelength   =  arth(param%Source%Wavelength_Range(1),dwavelength,param%Numerics%NEnergy)
!!$       ! Energy
!!$       param%Numerics%Energy = wavelength_to_energy( param%Numerics%Wavelength )
!!$       param%Source%Energy_Range = (/ param%Numerics%Energy(1),  &
!!$                     param%Numerics%Energy(  param%Numerics%NEnergy ) /)  
!!$    End if
!!$
!!$    
!!$    ! ---------------------------------------------------
!!$    ! ---  Get the needed Dielectric functions for the 
!!$    ! ---  media involved....
!!$    ! ---------------------------------------------------
!!$
!!$    ! ... Fills the dielectric constant of the ambient
!!$    if (allocated(param%Materials%Epsilon%Ambient))                       &
!!$         Call dielectric_constants(param%Numerics%Energy,param%Materials%Epsilon%Ambient,       &
!!$                                   param%Materials%Ambient,param%Global%sopra_root)
!!$
!!$    ! ... Fills the dielectric constant of the island or main film
!!$    if (allocated(param%Materials%Epsilon%Island_file))                   &
!!$         Call dielectric_constants(param%Numerics%Energy,param%Materials%Epsilon%Island_file,   &
!!$                                    param%Materials%Island,param%Global%sopra_root)
!!$
!!$    ! ... Fills the dielectric constant of the substrate
!!$    if (allocated(param%Materials%Epsilon%Substrate_file))                &
!!$         Call dielectric_constants(param%Numerics%Energy,param%Materials%Epsilon%Substrate_file,&
!!$                                   param%Materials%Substrate,param%Global%sopra_root)
!!$
!!$    ! ... Get the dielectric constant of the coating  
!!$    if (allocated(param%Materials%Epsilon%Film))                          &
!!$         Call dielectric_constants(param%Numerics%Energy,param%Materials%Epsilon%Film,          &
!!$                                   param%Materials%Film,param%Global%sopra_root)
!!$    ! ... Get the dielectric constant of the coating  
!!$    if (allocated(param%Materials%Epsilon%Coating))                       &
!!$         Call dielectric_constants(param%Numerics%Energy,param%Materials%Epsilon%Coating,       &
!!$                                   param%Materials%Coating,param%Global%sopra_root)
!!$
!!$
!!$
!!$    ! ----------------------------------------------------
!!$    ! --------  THIS WAS NEEDED IN THE OLD VERSION -------
!!$    ! ----------------------------------------------------
!!$    ! 
!!$    ! It should be removed in newer versions....
!!$    !
!!$
!!$    ! ... Get the parameters for the finite size corrections
!!$    If(param%Misc%Mean_Free_Path/='none') Call read_func_corrections(param%Global%Sopra_root,param%Materials%Island)
!!$
!!$
!!$    ! ... To take into account the thermoreflectance
!!$    If(param%Misc%Thermoreflectance) Then
!!$       Write(unit=6,fmt=*)
!!$       Write(unit=6,fmt=*) ' Enter the energy shift(eV) for the substrate : ' !'(a\)'
!!$       Read(unit=5,fmt=*) de
!!$       di = Int(de/denergy)
!!$       Do ienergy=param%Numerics%NEnergy,di+1,-1
!!$          param%Materials%Epsilon%Substrate_file(ienergy) = param%Materials%Epsilon%Substrate_file(ienergy-di)
!!$       Enddo
!!$    Endif
!!$
!!$    !-----------------------------------------------------------------
!!$
!!$
!!$
!!$    ! Finite Size scaling of the dielectric constant of the island 
!!$    !      If(param%Misc%Mean_Free_Path/='none')  Then
!!$    !       Call dielectric_func_corrections(param%Misc%Eps_Coating,param%Island%Coating_Thickness)
!!$    !      Endif
!!$
!!$
!!$
!!$  Contains
!!$
!!$
!!$    Function Energy_to_Wavelength( Energy )        Result(Wavelength)
!!$      !
!!$      ! Converts from photon Energy (eV) to Wavelength (nm) 
!!$      !
!!$      !      Use Share_Parameters_Mod, only : HBAR, Speed_of_Light
!!$      Implicit None
!!$      Real(wp), dimension(:)              :: Energy
!!$      Real(wp), dimension(size(Energy,1)) :: Wavelength
!!$
!!$      !Wavelength = 2*pi*HBAR*Speed_of_Light / Energy
!!$      Wavelength = 2*pi* param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light / Energy
!!$      
!!$    End Function Energy_to_Wavelength
!!$
!!$    !
!!$    ! --- 
!!$    !
!!$
!!$    Function Wavelength_to_Energy( Wavelength )    Result(Energy)
!!$      !
!!$      ! Converts from Wavelength (nm) to photon Energy (eV)
!!$      !
!!$      !      Use Share_Parameters_Mod, only : HBAR, Speed_of_Light
!!$      Implicit None
!!$      Real(wp), dimension(:)                  :: Wavelength
!!$      Real(wp), dimension(size(Wavelength,1)) :: Energy
!!$
!!$      !Energy = 2*pi*HBAR*Speed_of_Light / Wavelength
!!$      Energy = 2*pi* param%PhysConst%Planck_over_2_pi * param%PhysConst%Speed_of_Light / Wavelength
!!$
!!$    End Function Wavelength_to_Energy
!!$    
!!$
!!$  End Subroutine Initialize_dielectric_constants
!!$  !---------------------------------------------!
!!$
!!$
!!$  !
!!$  !----------------------------------------------------------------------------------------------------
!!$  !
!!$  
!!$
!!$  !-------------------------------------------------!
!!$  Subroutine Initialize_dielectric_constants_island()
!!$  !-------------------------------------------------!
!!$    !Use Interaction_mod, Only : Get_Dielectric_Thin_Layer
!!$    Implicit None
!!$
!!$    ! ---- Finite Size scaling of the dielectric constant of the island 
!!$    param%Materials%Epsilon%Island(:) = param%Materials%Epsilon%Island_file(:)
!!$    If(param%Misc%Mean_Free_Path/='none')  &
!!$       Call dielectric_func_corrections(param%Materials%Epsilon%Island)
!!$
!!$    ! Effective correction of the dielectric constant of the substrate if there is a coating
!!$    If(param%Island%Coating_Thickness/=0._wp .and. param%Island%Derived%Geometry/='rough'  &
!!$         .and. param%Island%Derived%Geometry/='film') Then
!!$       ! Get the equivalent dielectric constant of the substrate+coating
!!$       !Call Get_Dielectric_Thin_Layer()
!!$       ! TO REMOVE
!!$       !       param%Misc%Eps_Substrate(:) = param%Misc%Eps_Coating(:)        
!!$    Else
!!$       param%Materials%Epsilon%Substrate(:) = param%Materials%Epsilon%Substrate_file(:)
!!$    Endif
!!$
!!$    ! Check for compatibility
!!$    If(param%Island%Coating_Thickness/=0._wp.and.param%Island%Derived%Geometry/='rough'.and.param%Island%Derived%Geometry/='film') Then
!!$       Select Case(param%Misc%Output_Type)
!!$       Case('R','R2')
!!$        param%Misc%Output_Type='R2'
!!$       Case('DR','DR2')
!!$        param%Misc%Output_Type='DR2'
!!$       Case Default
!!$        Write(unit=6,fmt=*) 'ERROR form Initialize_dielectric_constants_island : output not compatible with geomtry !'
!!$        Pause
!!$        Stop
!!$       End Select
!!$    Endif
!!$
!!$
!!$  End Subroutine Initialize_dielectric_constants_island
!!$  !----------------------------------------------------!
!!$
!!$
!!$
!!$
!!$
!!$  !
!!$  !
!!$  ! **************************************************
!!$  !
!!$  ! *****  SUPPORT ROUTINES **************************
!!$  !
!!$  ! **************************************************
!!$  !
!!$  !



  !------------------------------------------------------------!
  Subroutine Get_Dielectric_Function(energy,epsilon,material,path)
  !Subroutine dielectric_constants(energy,epsilon,material,path)
  !------------------------------------------------------------!
    Use SFL_Logical_Units,       only : SFL_Get_Free_Unit
    Use Error_Module,            only : Error_Failure
    Implicit None
    Real(wp)                          :: energy(:)
    complex(wp )                      :: Epsilon(:)
    Character(len=*)                  :: material
    Character(len=*)                  :: path
    ! --- Local
    Character(len=*), parameter       :: routine = "Get_Dielectric_Function"
    !complex(wp ),Parameter            :: im=(0._wp,1._wp)
    Character(len=250)                :: filename, str
    Logical                           :: exi
    integer                           :: lines,start,i, ifile
    Real(wp),     Allocatable         :: x(:)
    complex(wp ), Allocatable         :: y(:)
    Real(wp)                          :: tmp(2),x1,x2,dx
    integer                           :: unit, istat
    complex(wp )                      :: slope
    

    ! Opens the relevant data file for the given material
    ! Notice that the data-files contains the values for the 
    ! index of refraction, n,k extacted from the data base of
    ! SOPRA.
    ! Therefore "epsilon = epsilon**2" at the bottom of this routine    
    filename = Trim(Adjustl(path))//'/'//Trim(Adjustl(material))//'.nk'
    Inquire(file=Trim(Adjustl(filename)),exist = exi)
    If( .NOT. exi ) Then
       write(str,*) trim(adjustl(filename))
       str = "File = " // trim(adjustl(str)) // " non existing" 
       Call Error_Failure(routine, trim(adjustl(str)) )
    Endif
    call SFL_Get_Free_Unit( ifile )
    Open(unit=ifile,file=filename,status='old')
    Read(unit=ifile,fmt=*) unit,x1,x2,lines
    dx = (x2 - x1)/(lines-1)

    Allocate( x(lines), y(lines) )
    Select Case (unit)
    Case(1)
       ! Unit = Electron Volt (eV)
       Do i=1,lines 
          Read(unit=ifile,fmt=*) tmp(1), tmp(2)
          x(i) = x1 + (i-1)*dx
          y(i) = tmp(1)+imu*tmp(2)
       Enddo
    Case(2)
       ! Unit = WaveLength (µm)
       Do i=lines,1,-1 
          Read(unit=ifile,fmt=*) tmp(1), tmp(2)
          x(i) = x1 + (lines-i)*dx
          y(i) = tmp(1)+imu*tmp(2)
       Enddo
       x(:) = 1.243_wp/x(:) ! Conversion µm-->eV
    End Select

    Close(unit=ifile)

    ! --- Do the interpolation
    Do i=1,Size(energy,1)
       start=locate(x(:),energy(i))
       If((start==0).Or.(start==lines)) Then
          write(str,"('Energy not in range : ',f5.2,' for i=',i3)") energy(i),i
          call Error_Failure( routine, trim(adjustl(str)))
       Endif
       ! Linear interpolation
       slope = (y(start+1)-y(start))/(x(start+1)-x(start))
       Epsilon(i) = y(start) + slope*(energy(i)-x(start))
       !            Write(unit=67,*) energy(i),Real(epsilon(i)),Aimag(epsilon(i))
    Enddo
    
    ! --- Calculates the dielectric constant (from the refraction index)
    epsilon = epsilon**2
    Deallocate(x,y,stat=istat)

  contains
    
    Function Locate(xx,x)
      Implicit None
      Real(wp), Dimension(:), Intent(In)  :: xx
      Real(wp), Intent(In)                :: x
      Integer                             :: locate
      Integer                             :: n,jl,jm,ju
      Logical                             :: ascnd
      
      n=Size(xx)
      ascnd = (xx(n) >= xx(1))
      jl=0
      ju=n+1
      Do
         If(ju-jl <= 1) exit
         jm=(ju+jl)/2
         If(ascnd .eqv. (x >= xx(jm))) Then
            jl=jm
         Else
            ju=jm
         Endif
      Enddo
      If(x == xx(1)) Then
         locate=1
      Else If(x == xx(n)) Then
         locate=n-1
      Else
         locate=jl
      Endif
      
    End Function Locate
    

  End Subroutine Get_Dielectric_Function
  !-------------------------------------!


  !
  !----------------------------------------------------------------------------------------------------
  !

!!$
!!$  !-----------------------------------------------!
!!$  Subroutine read_func_corrections( path, material)
!!$  !-----------------------------------------------!
!!$    Use SFL_Logical_Units,   only : SFL_Get_Free_Unit
!!$    Implicit None
!!$    Character(len=*)           :: path
!!$    Character(len=*)           :: material
!!$    Character(len=200)         :: filename
!!$    Integer                    :: ifile
!!$    Logical                    :: exi
!!$
!!$    ! Opens the relevant data file for the given material
!!$    filename = TRIM(path)//'/Finite_Size/'//TRIM(material)//'.dat'
!!$    Inquire(file = filename, exist = exi)
!!$    If( .NOT. exi ) Then
!!$       Write(unit=6,fmt=*) 'ERROR_03 (read_func_corrections) : File = ', &
!!$            &       TRIM(filename),' non existing'
!!$       Pause
!!$       Return
!!$    Endif
!!$
!!$    call SFL_Get_Free_Unit( ifile )
!!$    Open(unit=10,file=filename,status='old')
!!$    ! Reads the data (all quantities are multiplied by hbar)
!!$    Read(unit=ifile,fmt=*) omega_p             ! Plasma frequency  (eV)
!!$    Read(unit=ifile,fmt=*) inv_relaxation_time ! Inverse bulk relaxation time (eV) 
!!$    Read(unit=ifile,fmt=*) fermi_velocity      ! Fermi velicity (eV nm)
!!$    Read(unit=ifile,fmt=*) surf_eff            ! Surface effects (eV^2) 
!!$    Read(unit=ifile,fmt=*) disp_T              ! Dispersion of plasma frequency with T
!!$    Close(unit=ifile)
!!$
!!$  End Subroutine read_func_corrections
!!$  !-----------------------------------!
!!$
!!$
!!$
!!$
!!$  !
!!$  !----------------------------------------------------------------------------------------------------
!!$  !
!!$
!!$
!!$
!!$  !-----------------------------------------------!
!!$  Subroutine dielectric_func_corrections(epsilon,d)
!!$  !-----------------------------------------------!
!!$    !
!!$    ! Calculates the correction to the dielectric function due to the 
!!$    ! finit Size of the islands
!!$    ! See Physica A143, 164 (1987), for details 
!!$    !
!!$    ! See Also :  PhysRevB 48, 18178 (1993) 
!!$    !
!!$    Implicit None
!!$    Complex(wp )               :: Epsilon(:)
!!$    Real(wp),Optional          :: d
!!$    ! --- Local
!!$    integer                    :: ienergy 
!!$    Real(wp)                   :: R_eff,inv_tau_bulk,inv_tau,E
!!$    Real(wp)                   :: percent
!!$    Real(wp)                   :: A, VoS
!!$    complex(wp )               :: tmp_surf_eff
!!$
!!$    inv_tau_bulk = inv_relaxation_time
!!$    A            = 0.8_wp
!!$    Select Case ( Trim(Adjustl(param%Island%Derived%Geometry)) )
!!$    Case('film','rough')
!!$       R_eff     = param%Film%Thickness
!!$       VoS       = R_eff
!!$    Case('island','resonances')
!!$       R_eff     = (1+param%Island%Truncation_Ratio)*param%Island%Radius         ! Physica A, Eq. (3.7)  
!!$       VoS       = param%Interaction%Derived%Cluster_Volume/param%Interaction%Derived%Cluster_Surface       ! volume over surface
!!$       ! To remove
!!$       R_eff     =      3._wp*VoS
!!$    End Select
!!$
!!$    ! Value of effective radius
!!$    If(Present(d)) R_eff = d
!!$
!!$    ! Temperature effects on the dielectric constant
!!$    ! See Rocca Moresco Valbusa PRB 45,3 1399 (1992)
!!$    omega_p = omega_p + disp_T * (param%Misc%Temperature-300)
!!$
!!$    ! Nature of mean free path limitation
!!$    Select Case(param%Misc%Mean_Free_Path)
!!$    Case('finite_size')
!!$       inv_tau      = inv_tau_bulk + A*fermi_velocity/R_eff  ! Physica A, Eq. (3.6)
!!$    Case('s-only','tau')
!!$       !Write(unit=6,fmt='(a,f6.2,a\)') ' Enter the inverse of relaxation time (eV-1)' &
!!$       !     &  // '(bulk= ",inv_tau_bulk,")  : '
!!$       Print*, "commented by LAL des 2008 in order to compile using gfortran. Found in subroutine dielectric_func_corrections"
!!$       Read(unit=5,fmt=*) inv_tau
!!$    Case('manual')
!!$       !Write(unit=6,fmt='(a\)') ' Enter the percentage of bulk Absorption : '
!!$       Print*, 'commented by LAL des 2008 in order to compile using gfortran. Found in subroutine dielectric_func_corrections' 
!!$       Read(unit=5,fmt=*) percent
!!$       Write(unit=6,fmt=*)
!!$       inv_tau      = inv_tau_bulk + A*fermi_velocity/R_eff
!!$    Case('A-parameter')
!!$       !Write(unit=6,fmt='(a\)') ' Enter the A-Parameter : '
!!$       Print*, "commented by LAL des 2008 in order to compile using gfortran. Found in subroutine dielectric_func_corrections"      
!!$       Read(unit=5,fmt=*) A
!!$       inv_tau      =   inv_tau_bulk + A*fermi_velocity/R_eff
!!$    Case default
!!$       Write(unit=6,fmt=*) 'Error in dielectric_func_corrections for param%Misc%Mean_Free_Path'
!!$    End Select
!!$
!!$
!!$    ! Update the dielectric constant of the island-material 
!!$    ! See Physica A143, 164 (1987)
!!$    !
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       E               =    param%Numerics%Energy(ienergy) 
!!$       ! Surface effects 
!!$       ! See Liebsch, PRB B48, 11 317 (1993).
!!$       ! Eq. (5.1) and (5.2) 
!!$       If(param%Misc%Surface_Effects) Then
!!$          ! To remove
!!$          !         tmp_surf_eff = omega_p**2/(E**2 + surf_eff/VoS + imu*E*inv_tau)
!!$          tmp_surf_eff = omega_p**2/(E**2 + surf_eff/(3*VoS) + imu*E*inv_tau)
!!$       Else
!!$          tmp_surf_eff = omega_p**2/(E**2 + imu*E*inv_tau)
!!$       Endif
!!$       ! Dielectric constant of d-electrons for silver
!!$       Epsilon(ienergy)  =  Epsilon(ienergy)                         &
!!$            + omega_p**2/(E**2 + imu*E*inv_tau_bulk)
!!$       ! Type of mean free path 
!!$       Select Case(param%Misc%Mean_Free_Path)
!!$       Case('s-only')
!!$          Epsilon(ienergy)  =  Real(Epsilon(ienergy)) - tmp_surf_eff
!!$       Case('tau')
!!$          Epsilon(ienergy)  =  Epsilon(ienergy) - tmp_surf_eff
!!$       Case('manual')
!!$          Epsilon(ienergy)  =  Real(Epsilon(ienergy)-tmp_surf_eff) + &
!!$               imu*percent*Aimag(Epsilon(ienergy)-tmp_surf_eff)
!!$       Case('finite_size','A-parameter')
!!$          Epsilon(ienergy)  =  Epsilon(ienergy) - tmp_surf_eff
!!$       Case default
!!$          Write(unit=6,fmt=*) 'ERROR (dielectric_func_corrections) : param%Misc%Mean_Free_Path = ', &
!!$               ' non existing'
!!$       End Select
!!$    Enddo
!!$
!!$  End Subroutine dielectric_func_corrections
!!$  !-----------------------------------------!
!!$




End Module Dielectric_Function_Module
!-------------------------------------!
