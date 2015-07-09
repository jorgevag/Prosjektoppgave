! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!         This module contains routine sfor calculating the
!         polarizabilities of a single island.
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!------------------------------!
Module Polarizabilities_Module
!------------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp, pi	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Polarizabilities


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!




  !---------------------------------!
  Subroutine Get_Polarizabilities()
  !---------------------------------!
    !
    !  PURPOSE
    !
    !     This routine implements the calculation of the polarizabilities 
    !     of a particle. The polarizabilities are in DIMMENSIONAL form
    !     and the used MP coefficients are nomalized according to the 
    !     convention
    !
    !         \hat{A}_{\ell m}  = A_{\ell m} /  ( R^{\ell+2} * E_0 )
    !
    !     where E_0 is the amplitude of the incident electric field.
    !
    !     When a coated particle is used, it is the radius of the outer particle 
    !     that is used for defining the dimensionless MP coefficients.
    !
    !     Note : Depending on how the matrix system used to determine 
    !     the MP coefiecients was scaled there might also additional 
    !     constants going into defing the renomalized MP coefficients.
    !     
    !      ----
    !
    !     The equations that are implemented are:
    !     (referring to the book of Bedeaux and Vlieger, 1st ed.) 
    !      
    !      MP above substrate :  Eq. (8.31)
    !      MP below substrate :  Eq. (8.51)
    !
    !
    !  AUTHOR
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    !
    Use Shared,             only : pi, imu, param, Results
    Use Tools_Module,       only : deg2rad
    Use SFL_Logical_Units,  only : SFL_Get_Free_Unit
    Use Broadening_Module,  only : Broaden_Alpha
    Implicit None

    ! --- Local
    Character(len=*), parameter :: routine = "Get_Polarizabilities"
    Integer      :: ienergy, par, perp, file_id
    Real(wp)     :: c0,s0
    Complex(wp)  :: e1, e2
    Complex(wp)  :: exp0, prefactor_par, prefactor_perp
    Complex(wp)  :: A10, A11, A20, A21
    Complex(wp)  :: prefactor10, prefactor11, prefactor20, prefactor21 


    ! --- TESTING
    complex(wp) :: tmp
    ! --- TESTING


    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some energy independent abbreviations
    c0    =  Cos(        param%Source%ThetaE   * deg2rad() )
    s0    =  Sin(        param%Source%ThetaE   * deg2rad() ) 
    exp0  =  Exp( -imu * param%Source%PhiE     * deg2rad() )
    ! 
    par   =  Results%par
    perp  =  Results%perp
    

    ! --- Prefactors   
    !     NOTE: these prefactors depend on how the normalization was used 
    !           when defining the matrix system
    prefactor10    =     2*pi / (sqrt(pi/3)*c0)
    prefactor11    =   - 4*pi / (sqrt(2*pi/3)*s0*exp0) 
    prefactor20    =       pi / (sqrt(pi/5)*c0)
    prefactor21    =   - 4*pi / (sqrt(6*pi/5)*s0*exp0)  


    If(param%Numerics%Multipole_Above_Substrate) Then
       !
       ! ----------------------------------------------------
       ! --- Multipole ABOVE subsrate; Eq. (8.31)
       ! ----------------------------------------------------
       !
       Do ienergy = 1, size( Param%Numerics%Energy,1)
          
          ! ... Dielectric functions
          e1  =  Param%Media( Param%Source%iambient )%Epsilon(ienergy)
          
          ! ... MultiPole coefficients....
          A10 = Results%MultiPoles%Coefficients( ienergy, 1, 0 )
          A11 = Results%MultiPoles%Coefficients( ienergy, 1, 1 )
          A20 = Results%MultiPoles%Coefficients( ienergy, 2, 0 )
          A21 = Results%MultiPoles%Coefficients( ienergy, 2, 1 )
          

          ! --- Polarizabilities (for single particle )
          !
          ! .... Dipole
          Results%Polarizabilities(ienergy,1)%Dipole( perp )      =  A10 * e1 * prefactor10 
          Results%Polarizabilities(ienergy,1)%Dipole( par  )      =  A11 * e1 * prefactor11 
          ! ... Quadrupole
          Results%Polarizabilities(ienergy,1)%Quadrupole( perp )  =  A20 * e1 * prefactor20 
          Results%Polarizabilities(ienergy,1)%Quadrupole( par  )  =  A21 * e1 * prefactor21 



       Enddo


    Else
       !
       ! ------------------------------------------------------
       ! --- MultiPole BELOW subsrate; Eq. (8.51)
       ! ------------------------------------------------------
       !
       Do ienergy = 1, size( Param%Numerics%Energy,1)
          
          ! ... Dielectric functions
          e1  =  Param%Media( Param%Source%iambient   )%Epsilon(ienergy)
          e2  =  Param%Media( Param%Source%isubstrate )%Epsilon(ienergy)

          
          ! ... MultiPole coefficients....
          A10 = Results%MultiPoles%Coefficients( ienergy, 1, 0 )
          A11 = Results%MultiPoles%Coefficients( ienergy, 1, 1 )
          A20 = Results%MultiPoles%Coefficients( ienergy, 2, 0 )
          A21 = Results%MultiPoles%Coefficients( ienergy, 2, 1 )
          

          ! --- Polarizabilities (for single particle )
          !
          ! .... Dipole
          Results%Polarizabilities(ienergy,1)%Dipole( perp )      =  A10 * (e2**2/e1) * prefactor10 
          Results%Polarizabilities(ienergy,1)%Dipole( par  )      =  A11 * e2         * prefactor11 
          ! ... Quadrupole
          Results%Polarizabilities(ienergy,1)%Quadrupole( perp )  =  A20 * (e2**2/e1) * prefactor20 
          Results%Polarizabilities(ienergy,1)%Quadrupole( par  )  =  A21 * e2         * prefactor21 

            
       Enddo
       
    Endif





    ! -------------------------------------------
    ! --- Writing the Polarizabilities to file
    ! -------------------------------------------
    if (Param%InOut%Debug) then
       
    !-- Open Files
       call SFL_Get_Free_Unit( file_id )
       Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_Polarizabilities.dat")
       Write(file_id,"(a)") &
            "# Format: energy, real(alpha_par), imag(alpha_par), real(alpha_perp), imag(alpha_perp), etc."       

       ! --- Energy loop
       do ienergy=1,size(Results%Polarizabilities,1)

          ! --- Write the result to file....
          Write(file_id,'(f10.5,5x,4(2f13.5,8x))')                                    &
               param%Numerics%Energy(ienergy),                                        &
               ! --- Dipole terms
               Results % Polarizabilities(ienergy,1) % Dipole     ( Results%par  ),   &
               Results % Polarizabilities(ienergy,1) % Dipole     ( Results%perp ),   &
               ! --- Quadrupole terms
               Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%par  ),   & 
               Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%perp )  
       enddo

       ! --- Close the file
       Close( file_id )

    endif





    ! --- Do ad-hoc broadening of polarizabilities according to broadening 
    !     settings in input file.
    !     
    !    THIS ROUTINE SHOULD BE CHECKED AND REWRITTEN 
    !
    call Broaden_Alpha()




    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine



  End Subroutine Get_Polarizabilities
  !----------------------------------!









!!$
!!$
!!$
!!$  !--------------------------------!
!!$  Subroutine Get_Polarizabilities()
!!$  !--------------------------------!
!!$    !
!!$    ! --- This routine calculates the DIMENSIONLESS
!!$    !      dipole and quadrupolar polarizabilities
!!$    !     
!!$    !     The renormalization is done so that
!!$    !        
!!$    !         \hat{\alpha}         =    \alpha      / V
!!$    !         \hat{\alpha}^{10}    =    \alpha^{10} / (VR)
!!$    !
!!$    !     for bot parallel and perpendicular pol. 
!!$    !     V denotes the  volume of the particle.
!!$    !     
!!$    !     For the MP expansion point above the substrate the equation
!!$    !     that are implmented is Eq. (8.110), and when it is below
!!$    !     Eq. (8.116). There equations refer to Bedeaux/Vlieger 1st ed.
!!$    !     (BV_1)
!!$    !
!!$    !     Ingve simonsen, Paris, Jul 2010
!!$    !
!!$    Use Shared,        only : Param, Results
!!$    Use Error_Module,  only : Error_Fatal, Error_Warning
!!$    Implicit None
!!$    ! --- Local
!!$    Character(len=*), parameter :: routine = "Get_Polarizabilities()"
!!$    Integer      :: ienergy, m
!!$    Complex(wp)  :: e1, e2 
!!$    
!!$
!!$    ! --- If verbose
!!$    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
!!$
!!$
!!$    Write(*,*) " !!!!!Docoment this routine.....!!!!"
!!$
!!$
!!$    ! --- Some Error checking
!!$    if (.not.allocated(Results%MultiPoles%Coefficients))  call Error_Fatal( routine, "Internal failure...!" )
!!$    if (.not.allocated(Results % Polarizabilities))       call Error_Fatal( routine, "Internal failure...!" )
!!$    if (size(Results%MultiPoles%Coefficients,2)<2)        call Error_Fatal( routine, "Internal failure...!" )
!!$
!!$
!!$    ! ==============================================
!!$    ! === MP ABOVE Substrate
!!$    ! ==============================================
!!$    !
!!$    ! ---- Bedeaux/Vlieger 1st ed. : Eq. (8.110) 
!!$    ! ----------------------------------------------
!!$    !
!!$    ! --- The energy loop
!!$    energy_loop : do ienergy=1, size(Results%MultiPoles%Coefficients,1)
!!$          
!!$       m_loop : do m=0,1
!!$          
!!$          ! --- Define the RENORMALIZED dipole and quadropole polarizabilities for a SINGLE island
!!$
!!$          ! --- m=0
!!$          ! ---------
!!$          if (m==0) then
!!$             ! ... Dipole
!!$             ! ................
!!$             Results % Polarizabilities(ienergy,1) % Dipole     ( Results%perp )   &
!!$                  =  4*pi * Results%MultiPoles%Coefficients(ienergy,1,m)
!!$             ! ... Quadrupole
!!$             !.................
!!$             Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%perp )   &
!!$                  =  2*pi * Results%MultiPoles%Coefficients(ienergy,2,m) * sqrt(5._wp/3._wp)
!!$          endif
!!$
!!$
!!$          ! --- m=1
!!$          ! ---------
!!$          if (m==1) then
!!$             ! ... Dipole
!!$             ! ................
!!$             Results % Polarizabilities(ienergy,1) % Dipole     ( Results%par  )   &
!!$                  =  4*pi * Results%MultiPoles%Coefficients(ienergy,1,m)
!!$             ! ... Quadrupole
!!$             ! ................
!!$             Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%par  )   &
!!$                  =  4*pi * Results%MultiPoles%Coefficients(ienergy,2,m) * sqrt(5._wp) / 3._wp
!!$          endif
!!$
!!$       enddo m_loop
!!$
!!$    enddo energy_loop
!!$
!!$
!!$
!!$
!!$
!!$    ! ================================================
!!$    ! === MP BELOW Substrate
!!$    ! ================================================
!!$    !    
!!$    ! ---- Bedeaux/Vlieger 1st ed. : Eq. (8.116) 
!!$    ! -------------------------------------------------
!!$    !
!!$    If ( .not. Param%Numerics%Multipole_Above_Substrate ) then
!!$       !
!!$       !  Calculation as above, but with some different factors
!!$
!!$       do ienergy=1,size(Results%MultiPoles%Coefficients,1)
!!$
!!$          ! --- The dielectric functions
!!$          e1  =   Param % Media(1) % Epsilon(ienergy) 
!!$          e2  =   Param % Media(2) % Epsilon(ienergy) 
!!$
!!$          ! --- Rescaling
!!$          !
!!$          ! ... Dipole
!!$          ! ..................
!!$          Results % Polarizabilities(ienergy,1) % Dipole     ( Results%perp )   &
!!$               = e2**2 * Results % Polarizabilities(ienergy,1) % Dipole     ( Results%perp )   
!!$
!!$          Results % Polarizabilities(ienergy,1) % Dipole     ( Results%par  )   &
!!$               =  e2    * Results % Polarizabilities(ienergy,1) % Dipole     ( Results%par  )   
!!$
!!$          ! ... Quadrupole
!!$          ! ..................
!!$          Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%perp )   &
!!$               = e2**2 * Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%perp )   
!!$
!!$          Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%par  )   &
!!$               = e2      * Results % Polarizabilities(ienergy,1) % Quadrupole ( Results%par  )  
!!$
!!$       enddo
!!$
!!$    end If
!!$
!!$    
!!$ 
!!$
!!$    ! --- If verbose
!!$    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine
!!$
!!$  End Subroutine Get_Polarizabilities
!!$  !----------------------------------!
!!$


End Module Polarizabilities_Module
!---------------------------------!













! =============================================
! ======== OLD CODE
! =============================================




!!$
!!$
!!$  Subroutine polarizabilities(alpha,A)
!!$    !
!!$    !-----------------------------------------------------------
!!$    !--- The polarizabilites (alpha) are calculated from 
!!$    !--- (Bedeaux's book) Eq. (5.6.43) (cf. Eq. (8.2.29))
!!$    !--- when the MP are above the subsrate and from
!!$    !--- Eq. (8.3.7) when thay are below 
!!$    !--- Notice that dimensionless quanities are used.
!!$    !--- Here A is the (dimensionless) multipole coefficients
!!$    !------------------------------------------------------------
!!$    !
!!$    Implicit None    
!!$    complex(wp ), intent(out)           :: alpha(2,2,param%Numerics%NEnergy)
!!$    complex(wp ), intent(in)           :: A(2,0:1,param%Numerics%NEnergy)
!!$    ! -- Local 
!!$    integer                :: ienergy
!!$    Real(wp)               :: c0,s0,vol
!!$    complex(wp )           :: e1,e2
!!$    complex(wp )           :: exp0,A10,A11,A20,A21
!!$
!!$    ! Some energy independent abbreviations
!!$    e1    =  eps_vacuum
!!$    c0    =  Cos(param%Source%Derived%ThetaE_calc)
!!$    s0    =  Sin(param%Source%Derived%ThetaE_calc) 
!!$    exp0  =  Exp(-imu*param%Source%Derived%PhiE_calc)
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       !----------------------------------------
!!$       !---  MP above the subsrate; Eq. (8.2.29)      BV 1st ed : (8.31)
!!$       !----------------------------------------
!!$       Do ienergy = 1, param%Numerics%NEnergy
!!$          ! Some energy dependent abbreviations
!!$          A10 = A(1,0,ienergy)
!!$          A11 = A(1,1,ienergy)
!!$          A20 = A(2,0,ienergy)
!!$          A21 = A(2,1,ienergy)
!!$          ! Calculates the (dimensionless) polarizabilities 
!!$          ! 1st index : 1 : dipole order
!!$          !             2 : quadropole order
!!$          ! 2nd index : 1 : parallel
!!$          !             2 : perpendicular
!!$          ! Dipole order 
!!$          alpha(1,1,ienergy) = -4*pi*e1*A11/(Sqrt(2*pi/3)*s0*exp0)
!!$          alpha(1,2,ienergy) =  2*pi*e1*A10/(Sqrt(pi/3)*c0)
!!$          ! Quadropole order
!!$          alpha(2,1,ienergy) = -4*pi*e1*A21/(Sqrt(6*pi/5)*s0*exp0)
!!$          alpha(2,2,ienergy) =    pi*e1*A20/(Sqrt(pi/5)*c0)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             Open(unit=31,file='Polarizability.dat',status='unknown')
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),alpha(1,1,ienergy)/vol,alpha(1,2,ienergy)/vol
!!$          Endif
!!$       Enddo
!!$    Else
!!$       !----------------------------------------
!!$       !---  MP below the subsrate; Eq. (8.3.7)       BV 1st ed : (8.51)
!!$       !----------------------------------------
!!$       Do ienergy = 1, param%Numerics%NEnergy
!!$          ! Some energy dependent abbreviations
!!$          e2 = param%Materials%Epsilon%Substrate(ienergy)   ! Misc%Eps_Substrate(ienergy)
!!$          A10 = A(1,0,ienergy)
!!$          A11 = A(1,1,ienergy)
!!$          A20 = A(2,0,ienergy)
!!$          A21 = A(2,1,ienergy)
!!$          ! Calculates the (dimensionless) polarizabilities 
!!$          ! 1st index : 1 : dipole order
!!$          !             2 : quadropole order
!!$          ! 2nd index : 1 : parallel
!!$          !             2 : perpendicular
!!$          ! Dipole order 
!!$          alpha(1,1,ienergy) = -4*pi*e2*A11/(Sqrt(2*pi/3)*s0*exp0)
!!$          alpha(1,2,ienergy) =  2*pi*e2*A10/((e1/e2)*Sqrt(pi/3)*c0)
!!$          ! Quadropole order
!!$          alpha(2,1,ienergy) = -4*pi*e2*A21/(Sqrt(6*pi/5)*s0*exp0)
!!$          alpha(2,2,ienergy) =    pi*e2*A20/((e1/e2)*Sqrt(pi/5)*c0)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             Open(unit=31,file='Polarizability.dat',status='unknown')
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),alpha(1,1,ienergy)/vol,(e1/e2)**2*alpha(1,2,ienergy)/vol
!!$          Endif
!!$       Enddo
!!$    Endif
!!$    Close(unit=31)
!!$
!!$    ! Convolution of the polarizability with a gaussian
!!$    Call Gaussian_Convolution(alpha)
!!$
!!$  End Subroutine polarizabilities
