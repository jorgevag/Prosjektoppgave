! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!
! 
! --- AUTHOR : Ingve Simonsen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!

!---------------------!
Module Finalize_Module
!---------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp, param	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Finalize_GranFilm

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  


!-------!
Contains
!-------!



  !-----------------------------!
  Subroutine Finalize_GranFilm()
  !-----------------------------!
    Use shared,   only : Param
    Implicit None
    Character(len=*), parameter :: routine = "Finalize_GranFilm"
    Character(len=*), parameter :: reftrans_extension ="_RefTrans"
    Character(len=200)   :: saved_out_filename 


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Write results to ascii file
    if ( Param%InOut%GUI_Mode ) then
       ! --- In GUI Mode
       call Write_Result_File_ASCII_GUI_Mode()
       Write(*,*)"-------------------------------------------------"
       Write(*,*) "GRANFILM (in gui_mode) completed successfully!"
    elseif ( Param%InOut%python_mode ) then
       ! --- Python interface 
       call Write_Result_File_ASCII()
       ! dirty trick with the filename.....
       saved_out_filename = Trim(Adjustl(Param%InOut%Output_File_Name))
       Param%InOut%Output_File_Name = Trim(Adjustl(Param%InOut%Output_File_Name)) // reftrans_extension 
       call Write_Result_File_ASCII_GUI_Mode()
       Param%InOut%Output_File_Name = saved_out_filename
       !
       call Write_Epsilon_File()
    else
       ! --- Otherwize
       call Write_Result_File_ASCII()
    endif


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine



  End Subroutine Finalize_GranFilm
  !--------------------------------!




  !
  ! ---------------------------------
  ! --- Local Routines
  ! ---------------------------------
  !





  !-----------------------------------!
  Subroutine Write_Result_File_ASCII()
  !-----------------------------------!
    Use shared,               only : Param, Results, pi, imu, Physical_Constants
    Use SFL_Precision,        only : sp
    Use SFL_Logical_Units,    only : SFL_Get_Free_Unit
    Use  SFL_Error_Handling,  only : SFL_Error_Fatal
    Use Shared,               only : Software_Information   
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Write_Result_File_ASCII"
    Integer    :: ofile
    Integer    :: ienergy,i
    real(wp)   :: h_times_c 
    
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine



    ! --- Components allocated
    if (.not.allocated(Results % Polarizabilities))   call SFL_Error_Fatal( routine, "Polarizabilities not allocated" )
    if (.not.allocated(Results % Susceptibilities))   call SFL_Error_Fatal( routine, "Susceptibilities not allocated" )


    ! --- Some constatants
    h_times_c =  2*pi*Physical_Constants%Planck_Const_over_2_pi * Physical_Constants%Speed_of_Light_in_vacuum


    ! Write out the results
    !Write(unit=6,fmt=*)  
    !Write(unit=6,fmt=*)  '--------------------------------------------------'
    !Write(unit=6,fmt=*)  '----------- Calculated Parameters ----------------' 
    !Write(unit=6,fmt=*)  '--------------------------------------------------'
    !Write(unit=6,fmt=*)
    !If(param%Misc%Output_Type=='DR'.or.param%Misc%Output_Type=='DR2'.or.param%Misc%Output_Type=='DT') Then
    !   Write(unit=6,fmt='(a,f8.4,a,f10.4)') ' Max  at :   E = ', param%Numerics%Energy(Iextremal(1)), '   value = ', extremal(1)
    !   Write(unit=6,fmt='(a,f8.4,a,f10.4)') ' Min  at :   E = ', param%Numerics%Energy(Iextremal(2)), '   value = ', extremal(2)
    !Endif
    !Write(unit=6,fmt=*)  
    !If(.not.(TRIM(param%Island%Derived%Geometry)=='film')) Then
    !   ! Mean thickness/shape ratio    
    !   mean_thickness =  param%Interaction%Derived%Density * param%Interaction%Derived%Cluster_Volume
    !   Write(unit=6,fmt='(a,f8.4)')  ' Shape ratio               : ',  param%Interaction%Derived%Cluster_Shape_Ratio 
    !   Write(unit=6,fmt='(a,f8.4)')  ' Contact angle (deg)       : ',  param%Island%Derived%Contact_Angle 
    !   Write(unit=6,fmt='(a,f8.4)')  ' Mean film thickness (nm)  : ',  mean_thickness 
    !   Write(unit=6,fmt=*)  
    !Endif

    !**************************************
    !*** Write Parameter-file  as well 
    !**************************************     

    ! --- Open the output file
    call SFL_Get_Free_Unit( ofile ) 
    Open(unit=ofile, file=Trim(Adjustl(Param%InOut%Output_File_Name)))


    ! ---------------------------------------------------------
    ! --- HEADER of the output file
    ! ---------------------------------------------------------
    Write(unit=ofile,fmt=*) ' # '
    Write(unit=ofile,fmt=*) ' # ',   Trim(adjustl(Software_Information%Name)), " ", trim(adjustl(Software_Information%Version))
    Write(unit=ofile,fmt=*) ' # ---------------------------------------------------------------------------- '
    !Write(unit=ofile,fmt=*) ' # Geometry             : ',param%Island%Derived%Geometry
    !Write(unit=ofile,fmt=*) ' # Calculation of       : ',param%Misc%Output_Type
   ! Write(unit=ofile,fmt="(a27,f15.8,l3,i5,'  ',f15.8,l3,f15.8,l3)")               &
   !                      ' # DE parallel (eV)     : ',param%Misc%sigEpar
   ! Write(unit=ofile,fmt="(a27,f15.8,l3,i5,'  ',f15.8,l3,f15.8,l3)")               &
   !                      ' # DE perpendicular (eV): ',param%Misc%sigEperp
   ! Write(unit=ofile,fmt=*) ' # DE parallel (eV)     : ',param%Misc%sigEpar
   ! Write(unit=ofile,fmt=*) ' # DE perpendicular (eV): ',param%Misc%sigEperp
    Write(unit=ofile,fmt=*) ' # '
   ! Write(unit=ofile,fmt=*) ' # SOURCE'
    Write(unit=ofile,fmt=*) ' # Angles of incidence   : ', real(param%Source%Theta0,sp), real(param%Source%Phi0,sp)  !, & "  theta0, phi0 (rad)"
    Write(unit=ofile,fmt=*) ' # Polarisation          :     ',param%Source%Polarization
    Write(unit=ofile,fmt=*) ' # Energy min, max  (eV) : ', real(param%Source%Energy_Range,sp)
    Write(unit=ofile,fmt=*) ' # No of Energy Points   : ', param%Numerics%No_Energy_Points
    !
    do i=1,size(Param%Media,1)
    Write(unit=ofile,fmt=*) ' # Materials             : ', " No.",i,  ":  ", trim(adjustl(param%Media(i)%Material)) 
    enddo
   ! Write(unit=ofile,fmt=*) ' # Fresnel formulae     : ',param%Misc%Fresnel_DR_Formulae
   ! Write(unit=ofile,fmt=*) ' # Finite Size effects  : ',param%Misc%Mean_Free_Path
   ! Write(unit=ofile,fmt=*) ' # Surface effects      : ',param%Misc%Surface_Effects
   ! Write(unit=ofile,fmt=*) ' # Temperature (K)      : ',param%Misc%Temperature
   ! If(param%Misc%Output_Type=='DR2'.or.param%Misc%Output_Type=='R2') Then 
   !    Write(unit=ofile,fmt=*) ' # Coating material       : ',param%Materials%Coating
   !    Write(unit=ofile,fmt="(a27,f15.8,l3,i5,'  ',f15.8,l3,f15.8,l3)")             &
   !                         ' # Coating thichness (nm) : ',param%Island%Coating_Thickness
   !    !Write(unit=ofile,fmt=*) ' # Coating thichness (nm) : ',param%Island%Coating_Thickness
   ! Endif
   ! If(param%Island%Derived%Geometry/='film'.or.param%Island%Derived%Geometry/='rough') Then 
   !    Write(unit=ofile,fmt=*) ' # Interactions         : ',param%Numerics%Island_Island_Interaction
   !    If(param%Numerics%Island_Island_Interaction=='Size') Then
   !       Write(unit=ofile,fmt=*) ' # Size distribution    : ',param%Island%Size_Distribution
   !       Write(unit=ofile,fmt=*) ' # Mean radius (nm)     : ',param%Island%Derived%Radius_Mean
   !       Write(unit=ofile,fmt=*) ' # Centroid (nm)        : ',param%Island%Radius
   !       Write(unit=ofile,fmt=*) ' # Sigma (nm)           : ',param%Misc%Distribution_Sigma*param%Island%Radius
   !       Write(unit=ofile,fmt=*) ' # Minimal radius (nm)  : ',param%Misc%Distribution_xRmin*param%Island%Radius
   !       Write(unit=ofile,fmt=*) ' # Maximal radius (nm)  : ',param%Misc%Distribution_xRmax*param%Island%Radius
   !       Write(unit=ofile,fmt=*) ' # # of Sizes classes   : ',param%Misc%Distribution_Classes
   !    Endif
    Write(unit=ofile,fmt=*) ' # Island radius (nm)        : ',param%Geometry%Radius
    Write(unit=ofile,fmt=*) ' # Radius ratios             : ',param%Geometry%Radius_Ratios
    Write(unit=ofile,fmt=*) ' # Truncation Parameter      : ',param%Geometry%Truncation_Ratio
    Write(unit=ofile,fmt=*) ' # Coverage                  : ',param%Interaction%Coverage
   !    Write(unit=ofile,fmt=*) ' # Geometry             : ',param%Island%Derived%Geometry
    Write(unit=ofile,fmt=*) ' # Network                   : ',trim(adjustl(param%Interaction%Lattice_Type)) 
    Write(unit=ofile,fmt=*) ' # Lattice constant (nm)     : ',param%Interaction%Lattice_Constant
    Write(unit=ofile,fmt=*) ' # Multipole position ratio  : ',param%Numerics%Multipole_Position_Ratio
    Write(unit=ofile,fmt=*) ' # # multipoles         : ',param%Numerics%Multipole_Order
   !    Write(unit=ofile,fmt=*) ' # Surface/Volume (nm-1): ',param%Interaction%Derived%Cluster_Surface/param%Interaction%Derived%Cluster_Volume
   !    Write(unit=ofile,fmt=*) ' # Shape ratio          : ',param%Interaction%Derived%Cluster_Shape_Ratio
    Write(unit=ofile,fmt=*) ' # Density                   : ',param%Interaction%Density
   !    Write(unit=ofile,fmt=*) ' # Mean thichkness (nm) : ',mean_thickness
   ! Else
   !    Write(unit=ofile,fmt=*) ' # Film thichkness (nm) : ',param%Film%Thickness
   ! Endif
    !Write(unit=ofile,fmt=*) ' # Max  at :   E = ', param%Numerics%Energy(Iextremal(1)), '   value = ', extremal(1)
    !Write(unit=ofile,fmt=*) ' # Min  at :   E = ', param%Numerics%Energy(Iextremal(2)), '   value = ', extremal(2)
    

    ! ---------------------------------------------------------
    ! --- DATA of the output file
    ! ---------------------------------------------------------

    Write(unit=ofile,fmt=*) ' # '
    Write(unit=ofile,fmt=*) ' # -------------------------------------------------------------'
    Write(unit=ofile,fmt=*) ' # FORMAT :    column 01      -  energy [eV]' 
    Write(unit=ofile,fmt=*) ' #                "   02:03   -  dR/R (amplitude, phase(deg))' 
    Write(unit=ofile,fmt=*) ' #                "   04:05   -  alpha_par (real, imag)'
    Write(unit=ofile,fmt=*) ' #                "   06:07   -  alpha_perp '
    Write(unit=ofile,fmt=*) ' #                "   08:09   -  alpha^{(10)}_par '
    Write(unit=ofile,fmt=*) ' #                "   10:11   -  alpha^{(10)}_perp '
    Write(unit=ofile,fmt=*) ' #                "   12:13   -  gamma '
    Write(unit=ofile,fmt=*) ' #                "   14:15   -  beta '
    Write(unit=ofile,fmt=*) ' #                "   16:17   -  delta '
    Write(unit=ofile,fmt=*) ' #                "   18:19   -  tau '
    Write(unit=ofile,fmt=*) ' #                "   20      -  wavelength [nm]'
    Write(unit=ofile,fmt=*) ' # -------------------------------------------------------------'


    Do ienergy=1,size(param%Numerics%Energy,1)
       !
       ! --- energy
       ! ........................................................................................
       Write(unit=ofile,fmt="(E13.5)",advance='no') real(param%Numerics%Energy(ienergy),wp)
       !
       !
       ! --- Delta R over R
       ! ........................................................................................
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(Results%Observables%Delta_R_over_R(ienergy,1),wp), &     ! Amplitude
            real(Results%Observables%Delta_R_over_R(ienergy,2),wp)        ! Phase
       !
       ! --- Polarizabilities
       ! ........................................................................................
       !
       !--- alpha_par (dipole) : We here assume that : no_siland_type == 1
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Polarizabilities(ienergy,1)%Dipole(Results%par),wp) , &
            real(-imu* Results%Polarizabilities(ienergy,1)%Dipole(Results%par),wp) 
       !--- alpha_per (dipole)
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Polarizabilities(ienergy,1)%Dipole(Results%perp),wp) , &
            real(-imu* Results%Polarizabilities(ienergy,1)%Dipole(Results%perp),wp)  
       !
       !--- alpha^{(10)}_par (quadrupole) : We here assume that : no_siland_type == 1
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Polarizabilities(ienergy,1)%Quadrupole(Results%par),wp) , &
            real(-imu* Results%Polarizabilities(ienergy,1)%Quadrupole(Results%par),wp) 
       !--- alpha^{(10)}_per (Quadrupole)
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Polarizabilities(ienergy,1)%Quadrupole(Results%perp),wp) , &
            real(-imu* Results%Polarizabilities(ienergy,1)%Quadrupole(Results%perp),wp)  
       !
       !
       ! --- Susceptibilities
       ! ........................................................................................
       !
       ! --- gamma
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Susceptibilities(ienergy)%gamma,wp) , &
            real(-imu* Results%Susceptibilities(ienergy)%gamma,wp)  
       !
       ! --- beta
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Susceptibilities(ienergy)%beta,wp) , &
            real(-imu* Results%Susceptibilities(ienergy)%beta,wp)  
       !
       ! --- delta
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Susceptibilities(ienergy)%delta,wp) , &
            real(-imu* Results%Susceptibilities(ienergy)%delta,wp)  
       !
       ! --- tau
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            real(      Results%Susceptibilities(ienergy)%tau,wp) , &
            real(-imu* Results%Susceptibilities(ienergy)%tau,wp)  
       !
       !
       ! --- Wavelength [nm]
       Write(unit=ofile,fmt="('  ',2E13.5)",advance='no') &
            h_times_c / real(param%Numerics%Energy(ienergy),wp)
       !    wavelength = 2*pi*HBAR*Speed_of_Light/param%Misc%Energy(ienergy)
       !
       !
       !
       ! --- Ending line.....
       Write(unit=ofile,fmt=*) 
       
    Enddo

       
    ! --- Close the output file.....
    Close(unit=ofile, status='keep')


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    
  End Subroutine Write_Result_File_ASCII
  !-------------------------------------!







  !--------------------------------------------!
  Subroutine Write_Result_File_ASCII_GUI_Mode()
  !--------------------------------------------!
    !
    ! ----------------------------------------------------------
    ! 
    ! --- PURPOSE
    !    
    !     Write out the results of the simulations in a format
    !     that can be used by the GUI/Python Interface to GranFilm
    !
    ! 
    ! --- AUTHOR : Ingve Simonsen, Trondheim, Apr 2011.
    !
    ! ----------------------------------------------------------
    !
    Use shared,               only : Param, Results
    Use SFL_Logical_Units,    only : SFL_Get_Free_Unit
    !Use SFL_Precision,        only : sp
    !Use Shared,               only : Software_Information   
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Write_Result_File_ASCII_GUI_Mode"
    Integer    :: ofile
    Integer    :: ienergy,i
    Complex(wp):: r_p,r_p_fresnel, r_s,r_s_fresnel, t_p,t_p_fresnel, t_s,t_s_fresnel    
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    
    
    ! Open the output-file 
    call SFL_Get_Free_Unit( ofile ) 
    Open(unit=ofile, file=Trim(Adjustl(Param%InOut%Output_File_Name)))

    Write(ofile, *) " # Results from GranFilm in GUI Mode "
    Write(ofile, *) " #"
    Write(ofile, *) " # FORMAT:  Energy(eV), r_p, r_p_fresnel, r_s, r_s_fresnel,   t_p, t_p_fresnel, t_s, t_s_fresnel"
    Write(ofile,*)  " # ---------------------------------------------------------------------------------------------"
    
    
    ! --- Loop over energy
    Do ienergy=1,size(param%Numerics%Energy,1)

       ! ---Copy the data
       call Copy_RefTrans_Amplitudes(ienergy )  ! r_p,r_p_fresnel, r_s,r_s_fresnel,      &
                                                ! t_p,t_p_fresnel, t_s,t_s_fresnel    

       ! --- Write data
       Write(unit=ofile,fmt='(f10.5,"  ",8(f9.5,f9.5,"     "))')               &
            real(param%Numerics%Energy(ienergy),wp),   &
            r_p,r_p_fresnel, r_s,r_s_fresnel,      &
            t_p,t_p_fresnel, t_s,t_s_fresnel           
    Enddo

    ! --- Close the output file
    Close(unit=ofile, status='keep')


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine


  !---------!
  contains
  !---------!
  
    Subroutine Copy_RefTrans_Amplitudes( ienergy ) !, r_p,r_p_substrate, r_s,r_s_substrate  &
      !t_p,t_p_substrate, t_s,t_s_substrate    )
         ! --- Copy the Ref/Trans data
         !     If not calculated 0 is returned for the corresponding amplitude
         Implicit None
         Integer :: ienergy


         ! --- Reflection
         ! ------------------
         if ( allocated(Results%Reflection_Amplitudes) ) then
            r_p           =  Results%Reflection_Amplitudes(ienergy)%p
            r_p_fresnel   =  Results%Reflection_Amplitudes(ienergy)%fresnel_p
            r_s           =  Results%Reflection_Amplitudes(ienergy)%s
            r_s_fresnel   =  Results%Reflection_Amplitudes(ienergy)%fresnel_s
         else
            r_p           =  0._wp 
            r_p_fresnel   =  0._wp 
            r_s           =  0._wp 
            r_s_fresnel   =  0._wp 
         endif

         ! --- Transmission
         ! -------------------
         if ( allocated(Results%Transmission_Amplitudes) ) then
            t_p           =  Results%Transmission_Amplitudes(ienergy)%p
            t_p_fresnel   =  Results%Transmission_Amplitudes(ienergy)%fresnel_p
            t_s           =  Results%Transmission_Amplitudes(ienergy)%s
            t_s_fresnel   =  Results%Transmission_Amplitudes(ienergy)%fresnel_s
         else
            t_p           =  0._wp 
            t_p_fresnel   =  0._wp 
            t_s           =  0._wp 
            t_s_fresnel   =  0._wp 
         endif

       End Subroutine Copy_RefTrans_Amplitudes


  End Subroutine Write_Result_File_ASCII_GUI_Mode
  !----------------------------------------------!





  !--------------------------------------------!
  Subroutine Write_Epsilon_File()
  !--------------------------------------------!
    !
    ! ----------------------------------------------------------
    ! 
    ! --- PURPOSE
    !    
    !     Write out the epsilon values for all media invilved
    !
    ! 
    ! --- AUTHOR : Ingve Simonsen, Trondheim, Apr 2014.
    !
    ! ----------------------------------------------------------
    !
    Use shared,               only : Param
    Use SFL_Logical_Units,    only : SFL_Get_Free_Unit
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Write_Epsilon_File"
    Character(len=*), parameter :: file_extension = "_Epsilon"
    Integer    :: ofile
    Integer    :: ienergy, imedia, Nmedia
    complex(wp), dimension(:), allocatable :: eps


    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! -- initial work
    Nmedia = size( Param%Media, 1 )
    allocate( eps(Nmedia) )

    ! Open the output-file 
    call SFL_Get_Free_Unit( ofile ) 
    Open(unit=ofile, file=Trim(Adjustl(Param%InOut%Output_File_Name)) // file_extension )

    Write(ofile, *) " # Results from python mode "
    Write(ofile, *) " #"
    Write(ofile, *) " # FORMAT:  Energy(eV), epsilon(1:N) [real and imaginary parts as separate columns] "
    Write(ofile,*)  " # ---------------------------------------------------------------------------------------------"
    
    
    ! --- Loop over energy
    Do ienergy=1,size(param%Numerics%Energy,1)

       ! --- Copy the dielectric function
       do imedia=1,Nmedia
          eps(imedia) = Param%Media(imedia)%epsilon(ienergy)
       enddo

       ! --- Write data
       Write(unit=ofile,fmt='(f10.5,"  ",15(f9.5,f9.5,"     "))') &
            real(param%Numerics%Energy(ienergy),wp), eps
    Enddo

    ! --- Close the output file
    Close(unit=ofile, status='keep')

    if (allocated(eps)) deallocate(eps)
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  End Subroutine Write_Epsilon_File
  !----------------------------------------------!


End Module Finalize_Module
!-------------------------!

