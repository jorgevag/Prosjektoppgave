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
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!


!-------------------------------------!
Module Susceptibilities_Module
!-------------------------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Surface_Susceptabilities


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!




  !------------------------------------------!
  Subroutine Get_Surface_Susceptabilities(  ) 
  !------------------------------------------!
    !
    !
    !  --- This routine calculates the surface susceptibilities for a
    !      series of interaction types. 
    !      See individual routines for details......
    !
    ! ---  Ingve Simonsen, Paris, Jun 2010.
    !
    Use Shared,                   only : Results, Param	   
    Use Error_Module,             only : Error_Failure, Error_Warning
    Use SFL_Logical_Units,        only : SFL_Get_Free_Unit
    Use Supported_Options_Module, only : ISLAND_ISLAND_INTERACTION_OPTION_TABEL 
    
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Get_Surface_Suceptabilities"
    Character(len=100)          :: str
    Integer    :: ienergy, file_id

    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine


    ! --- Some error checking
    if ( .not.allocated( Results%Susceptibilities ) .or.  &
         .not.allocated( Results%Polarizabilities )      )   then
       call Error_Failure( routine, "Internal allocation error!" )
    end if
    !
    if (size(Results%Susceptibilities,1) /= size(Results%Polarizabilities,1) ) then
       call Error_Failure( routine, "Inconsistent dimensions") 
    endif


    ! --- Island shape averages
    !     If we have more than ONE island-type (size(Results%Polarizabilities,1) > 1)
    !     than we have to make an average
    if (size(Results%Polarizabilities,2) > 1 ) then
       call Error_Failure( routine, "Island shape avereges not yet implmented!" )
    end if



    ! --- The Main work....
    ! ----------------------------
    Select Case( Trim(Adjustl( Param%Interaction%Island_Island_Interaction )) )
     
    Case( Trim(adjustl(ISLAND_ISLAND_INTERACTION_OPTION_TABEL(1)))  )
       ! -------------------------------
       ! --- None
       ! -------------------------------
       Call Surface_Susceptabilities_None_Interacting()
       

    Case( Trim(adjustl(ISLAND_ISLAND_INTERACTION_OPTION_TABEL(2)))  )
       ! -------------------------------
       ! --- Dipole
       ! -------------------------------
       Call Surface_Susceptabilities_Dipole_Interacton()


    Case( Trim(adjustl(ISLAND_ISLAND_INTERACTION_OPTION_TABEL(3)))  )
       ! -------------------------------
       ! --- Quadrupole 
       ! -------------------------------
       Call Surface_Susceptabilities_Quadrupole_Interacton()


    !Case( Trim(adjustl(ISLAND_ISLAND_INTERACTION_OPTION_TABE(4)))  )
       ! -------------------------------
       ! --- Mean Field Theory 
       ! -------------------------------

    Case Default
       ! -- Should never happen, but .....
       str = "Option Island_Island_Interaction="// trim(adjustl(Param%Interaction%Lattice_Type))// "not supported!"
       Call Error_Failure( routine, str )

    End Select







    ! -------------------------------------------
    ! --- Writing the Susceptibilities to file
    ! -------------------------------------------
    if (Param%InOut%Debug) then
       
    !-- Open Files
       call SFL_Get_Free_Unit( file_id )
       Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))//"_Susceptibilities.dat")
       Write(file_id,"(a)") &
            "# Format: energy, real(beta), imag(beta), and same for; gamma, delta and tau "

       ! --- Energy loop
       do ienergy=1,size(Results%Polarizabilities,1)

          ! --- Write the result to file....
          Write(file_id,'(f10.5,5x,4(2f13.5,8x))')             &
               param%Numerics%Energy(ienergy),                 &
               ! --- Dipole terms
               Results%Susceptibilities(ienergy)%beta,         &
               Results%Susceptibilities(ienergy)%gamma,        &   
               ! --- Quadrupole terms
               Results%Susceptibilities(ienergy)%delta,        &
               Results%Susceptibilities(ienergy)%tau 

       enddo

       ! --- Close the file
       Close( file_id )
       
    end if  
    ! --- End writeout
                    
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine





  !-------!  
  Contains
  !-------!





    !======================
    !=== Local Routines
    !====================

    !-----------------------------------------------!
    Subroutine Get_Alpha( ienergy, alpha, alpha_10 )
    !-----------------------------------------------!
      !
      ! --- Auxilliary routine for getting the polarizabilities 
      !     for a given energy. This is done to get a more compact
      !     notation and readable code. 
      !
      ! --- Ingve Simonsen, Paris, Jul 2010.
      !
      Implicit None
      Integer,                   Intent(In)  :: ienergy
      Complex(wp), dimension(2), Intent(Out) :: alpha
      Complex(wp), dimension(2), Intent(Out) :: alpha_10
      
      ! ... Dipole
      alpha( Results%par  )     =   Results%Polarizabilities(ienergy,1)%Dipole( Results%par  )
      alpha( Results%perp )     =   Results%Polarizabilities(ienergy,1)%Dipole( Results%perp )
      ! ... Quadrupole
      alpha_10( Results%par  )  =   Results%Polarizabilities(ienergy,1)%Quadrupole( Results%par  )  
      alpha_10( Results%perp )  =   Results%Polarizabilities(ienergy,1)%Quadrupole( Results%perp )  

    End Subroutine Get_Alpha
    !-----------------------!





    !---------------------------------------------------------------------------------!
    Subroutine Surface_Susceptabilities_Common( density, distance, Lattice_Constant )
    !---------------------------------------------------------------------------------!
      !
      ! --- Set some common parameters used for the definition of the 
      !     surface suseptibilities
      !
      !     Ingve Simonsen, Paris, Jul 2010
      !
      Use Error_Module,             only : Error_Warning
      Real(wp),  Intent(out) ::   density
      Real(wp),  Intent(out) ::   distance
      Real(wp),  Intent(out) ::   Lattice_Constant
      ! --- Local
      Character(len=*), parameter :: routine="Surface_Susceptabilities_Common"

      ! --- Dimensionless density, distance and Lattice_Constant
      ! ..............................................................................
      !     distance = Absolute distance between surface and the location of multipoles
      density            = Param%Interaction%Density * param%Geometry%Radius(1)**2
      distance           = Abs( Param%Geometry%Truncation_Ratio - param%Numerics%Multipole_Position_Ratio )
      Lattice_Constant   = Param%Interaction%Lattice_Constant / Param%Geometry%Radius(1)

      !call Error_Warning( routine, " What if Lattice_Type is MFT or RPT")

      !If(param%Interaction%Lattice_Type=='MFT'.or.param%Interaction%Lattice_Type=='RPT') &
      !   Lattice_Constant       =       1._wp/Sqrt(density)  

      ! --- Renormalization of the polarisabilities in the Barrera model
      !     Phys. Rev. B 43 (17) P13819 (1991)
      !If(param%Interaction%Lattice_Type=='RPT') Then
      !   Call Renorm_Polarizability(d,alpha(1,:,:))       
      !Endif

    End Subroutine Surface_Susceptabilities_Common
    !----------------------------------------------!






    !------------------------------------------------------!
    Subroutine  Surface_Susceptabilities_None_Interacting
    !------------------------------------------------------!
      !
      ! --- This routine implments the surface susceptibilities for
      ! --- none interacting islands. This is only accurate in the low
      ! --- coverage limit. 
      !
      ! --- The equations that are implemented are:
      !     (referring to the book of Bedeaux and Vlieger, 1st ed.) 
      !      
      !      MP above substrate :  Eq. (8.32)
      !      MP below substrate :  Eq. (8.56)
      !
      !
      ! --- Ingve Simonsen, Paris, Jul 2010
      !
      Implicit None
      Integer     ::  par, perp, ienergy
      Real(wp)    :: d, density, L
      Complex(wp) :: e1,e2
      Complex(wp), dimension(2) :: alpha, alpha_10


      ! --- Some shorthands
      par  = Results%par
      perp = Results%perp


      ! --- Set some common constants....
      call Surface_Susceptabilities_Common( density, d, L )

      ! --- Dimensionless d and density
      !     d = Absolute distance between surface and the location of the multipoles
      ! ---------------------------------------------------------------------------
      !density  = param%Interaction%Density * param%Geometry%Radius**2
      !d        = Abs( Param%Geometry%Truncation_Ratio - param%Numerics%Multipole_Position_Ratio )

    
      If(param%Numerics%Multipole_Above_Substrate) Then

         ! ----------------------------------------------------
         ! --- Multipole ABOVE subsrate; Eq. (8.32)
         ! ----------------------------------------------------
         Do ienergy = 1, size( Param%Numerics%Energy,1)
            
            ! ... Dielectric function (for the ambient)
            e1  =   Param%Media( Param%Source%iambient )%Epsilon(ienergy)
            
            ! ... Polarizabilities
            call Get_Alpha(ienergy, alpha, alpha_10)
            
            ! --- Surface Susceptibilities
            ! .... Dipole
            Results%Susceptibilities(ienergy)%gamma  =   density * alpha(par )
            Results%Susceptibilities(ienergy)%beta   =   density * alpha(perp) / e1**2
            ! ... Quadrupole
            Results%Susceptibilities(ienergy)%delta  = - density * ( sum(alpha_10) - d * sum(alpha) ) / e1
            Results%Susceptibilities(ienergy)%tau    = - density * ( alpha_10(par) - d * alpha(par) ) 
            
         Enddo

      Else

         ! ------------------------------------------------------
         ! --- MultiPole BELOW subsrate; Eq. (8.56)
         ! ------------------------------------------------------
         Do ienergy = 1, size( Param%Numerics%Energy,1)
            
            ! ... Dielectric functions
            e1  =  Param%Media( Param%Source%iambient   )%Epsilon(ienergy)
            e2  =  Param%Media( Param%Source%isubstrate )%Epsilon(ienergy)
          
            ! ... Polarizabilities
            call Get_Alpha(ienergy, alpha, alpha_10)

            ! --- Surface Susceptibilities
            ! .... Dipole
            Results%Susceptibilities(ienergy)%gamma  =   density * alpha(par)
            Results%Susceptibilities(ienergy)%beta   =   density * alpha(perp) / e2**2
            ! ... Quadrupole
            Results%Susceptibilities(ienergy)%delta  = - density * ( sum(alpha_10) + d * sum(alpha) ) / e2
            Results%Susceptibilities(ienergy)%tau    = - density * ( alpha_10(par) + d * alpha(par) ) * (e1/e2)
            
         Enddo
         
      Endif

      
    End Subroutine Surface_Susceptabilities_None_Interacting
    !------------------------------------------------------!








    !------------------------------------------------------!
    Subroutine  Surface_Susceptabilities_Dipole_Interacton()
    !------------------------------------------------------!
      Use Shared,               only : pi  
      Use Error_Module,         only : Error_Failure
      Use Interaction_Module,   only : Lattice_Sum
      Use Error_Module,         only : Error_Failure, Error_Warning
      Implicit None
      integer                   :: ienergy, par, perp
      Real(wp)                  :: S_mp, S_imp
      Real(wp)                  :: density, distance, Lattice_Constant
      Real(wp)                  :: factor, sqr
      Complex(wp)               :: e1, e2, eps
      Complex(wp)               :: parallel, perpendicular
      Complex(wp), dimension(2) :: alpha, alpha_10
      character(len=*), parameter :: routine="Surface_Susceptabilities_Dipole_Interacton"


      ! --- Some shorthands
      par  = Results%par
      perp = Results%perp
      sqr  = Sqrt(4._wp*pi/5._wp)


      ! --- Some renomalized parameters 
      call Surface_Susceptabilities_Common( density, distance, Lattice_Constant )


      ! --- Dimensionless density,d,L
      !     d= Absolute distance between surface and the location of multipoles
      !d            = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position_ratio )
      !density      = param%Interaction%Derived%Density*param%Island%Radius**2
      !L            = param%Interaction%Lattice_Constant / param%Island%Radius
      
      
      !      (P269 Bedeaux's book) for a random network
      !If(param%Interaction%Lattice_Type=='MFT'.or.param%Interaction%Lattice_Type=='RPT') &
      !   L       =       1._wp/Sqrt(density)  
    


      ! --- Lattice Sums for the multipoles (MP) and image multipoles (IMP)
      S_mp    = Lattice_Sum(0._wp,        2)
      S_imp   = Lattice_Sum(distance,     2)
      !call Error_Warning( routine, "Should the argument to the Lattice sum be zero?")


      If (Param%Numerics%Multipole_Above_Substrate) Then
         !
         !------------------------------------------------
         !--- MP above the substrate Eqs. 10.29 - 10.28
         !------------------------------------------------
         !
         Do ienergy = 1,size( param%Numerics%Energy,1)
            
            ! ... Dielectric functions
            e1  =  Param%Media( Param%Source%iambient   )%Epsilon(ienergy)
            e2  =  Param%Media( Param%Source%isubstrate )%Epsilon(ienergy)
            eps =  (e1-e2)/(e2+e1)


            ! ... Polarizabilities
            call Get_Alpha(ienergy, alpha, alpha_10)
            
            ! --- Renormalization of the polarisabilities in the Barrera model
            !     Phys. Rev. B 43 (17) P13819 (1991)
            !If(param%Interaction%Lattice_Type=='RPT') Then
            !   Call Renorm_Polarizability(d,alpha(1,:,:))       
            !Endif

            ! --------------------------------------------
            ! --- Calculate separately the 
            !     parallel and perpendicular components
            ! --------------------------------------------
            !
            ! Uses the formulas (5-67) for conversion between various polarizbilities
            !
            ! --- Parallel component (10-29)
            factor    = S_mp + eps * S_imp 
            parallel  =  alpha(par) / (4*pi*e1)
            parallel  =  4*pi*e1 * parallel / ( 1._wp+parallel*sqr/(Lattice_Constant**3)*factor )
            ! --- Perpendicular component (10-28)
            factor          = S_mp - eps * S_imp
            perpendicular   = alpha(perp) / (4*pi*e1)
            perpendicular   = 4*pi*e1*perpendicular / ( 1._wp-2*perpendicular*sqr/(Lattice_Constant**3)*factor )


            ! --- Surface Susceptibilities   (10-30)
            ! .... Dipole
            Results%Susceptibilities(ienergy)%gamma  =  density * parallel
            Results%Susceptibilities(ienergy)%beta   =  density * perpendicular / (e1**2)
            ! ... Quadrupole
            Results%Susceptibilities(ienergy)%delta  =  density * distance * (parallel + perpendicular) / e1
            Results%Susceptibilities(ienergy)%tau    =  density * distance * parallel
 

        Enddo


    Else
       !
       !------------------------------------------------
       !---  MP below the substrate Eqs. 10-33 10-34
       !------------------------------------------------
       !
       !call Error_Warning( routine, "Doublecheck the implmentation of the surf. susceptibilities!")
       Do ienergy = 1,size( param%Numerics%Energy,1)

            ! ... Dielectric functions
            e1  =  Param%Media( Param%Source%iambient   )%Epsilon(ienergy)
            e2  =  Param%Media( Param%Source%isubstrate )%Epsilon(ienergy)
            eps = (e2-e1)/(e2+e1)

            ! ... Polarizabilities
            call Get_Alpha(ienergy, alpha, alpha_10)
            
            ! --- Renormalization of the polarisabilities in the Barrera model
            !     Phys. Rev. B 43 (17) P13819 (1991)
            !If(param%Interaction%Lattice_Type=='RPT') Then
            !   Call Renorm_Polarizability(d,alpha(1,:,:))       
            !Endif

            ! --------------------------------------------
            ! --- Calculate separately the 
            !     parallel and perpendicular components
            ! --------------------------------------------
            !
            ! Uses the formulas (5-67) for conversion between various polarizbilities
            !
            ! --- Parallel component (10-34)
            factor     =  S_mp + eps * S_imp
            parallel   =  alpha(par) / (4*pi*e2)
            parallel   =  4 * pi * e2 * parallel / ( 1._wp + parallel*sqr/(Lattice_Constant**3)*factor )
            ! Perpendicular component (10-33)
            factor          = S_mp - eps*S_imp
            perpendicular   = alpha(perp) / (4*pi*e2)
            perpendicular   = 4*pi*e2*perpendicular / (1._wp - 2*perpendicular*sqr/(Lattice_Constant**3)*factor)


            ! --- Surface Susceptibilities   (10-30)
            ! .... Dipole
            Results%Susceptibilities(ienergy)%gamma  =    density * parallel
            Results%Susceptibilities(ienergy)%beta   =    density * perpendicular / (e2**2)
           ! ... Quadrupole
            Results%Susceptibilities(ienergy)%delta  =  - density * distance * (parallel + perpendicular) / e2
            Results%Susceptibilities(ienergy)%tau    =  - density * distance * parallel * e1 / e2

            ! Fills the surface suceptebilities (10-30)
            !surf_const_coef(ienergy)%gamma  =  density*paral
            !surf_const_coef(ienergy)%beta   =  density*perp/(e2**2)
            !surf_const_coef(ienergy)%delta  =  -density*d*(paral + perp)/e2
            !surf_const_coef(ienergy)%tau    =  -density*d*paral*e1/e2
            
       Enddo

    Endif


  End Subroutine Surface_Susceptabilities_Dipole_Interacton
    !------------------------------------------------------!







    !---------------------------------------------------------!
    Subroutine  Surface_Susceptabilities_Quadrupole_Interacton
    !---------------------------------------------------------!
      Use Error_Module,             only : Error_Failure
      Implicit None
            
      call Error_Failure( routine, "Quadrupole island-island interaction not yet implented!" )

    End Subroutine Surface_Susceptabilities_Quadrupole_Interacton
    !---------------------------------------------------------!







  End Subroutine Get_Surface_Susceptabilities
  !-----------------------------------------!





End Module Susceptibilities_Module
!-----------------------------------------!





! ========================================================
! === THE OLD CODE 
! ========================================================

!!$
!!$
!!$  Subroutine surf_const_coef_nointeract(surf_const_coef,alpha)
!!$    ! Calculate the surface costitutive coefficient from Eq. (8,2.30),
!!$    ! when the MP are located above the subsrate and by Eq. (8.3.12)
!!$    ! when they are located below.
!!$    ! NOTE : All these formulae asSumes LOW COVERAGE
!!$    !        No inter-island interaction is taken into account
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                     :: alpha(2,2,param%Numerics%NEnergy)
!!$    integer                          :: ienergy
!!$    Real(wp)                         :: density,d
!!$    complex(wp )                     :: e1,e2
!!$
!!$    ! Some energy independent abbreviations
!!$    e1       = eps_vacuum      
!!$    ! Dimensionless d and density
!!$    ! d= Absolute distance between surface and the location of multipoles
!!$    d        = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position )
!!$    density  = param%Interaction%Derived%Density * param%Island%Radius**2
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       ! MP above the subsrate; Eq. (8.2.30)
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          surf_const_coef(ienergy)%gamma  =   density*alpha(1,1,ienergy) 
!!$          surf_const_coef(ienergy)%beta   =   density*alpha(1,2,ienergy)/(e1**2)   
!!$          surf_const_coef(ienergy)%delta  = - density/e1 *                          &
!!$               Sum(alpha(2,:,ienergy) - d*alpha(1,:,ienergy))
!!$          surf_const_coef(ienergy)%tau    = - density *                             & 
!!$               ( alpha(2,1,ienergy) - d*alpha(1,1,ienergy) )   
!!$       Enddo
!!$    Else
!!$       ! MP below the subsrate; Eq. (8.3.12)
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          e2 = param%Materials%Epsilon%Substrate(ienergy)   !     Misc%Eps_Substrate(ienergy)
!!$          surf_const_coef(ienergy)%gamma  =   density*alpha(1,1,ienergy) 
!!$          surf_const_coef(ienergy)%beta   =   density*alpha(1,2,ienergy)/(e2**2)   
!!$          surf_const_coef(ienergy)%delta  = - density/e2 *                          &
!!$               Sum(alpha(2,:,ienergy) + d*alpha(1,:,ienergy))
!!$          surf_const_coef(ienergy)%tau    = - density * (e1/e2) *                   & 
!!$               ( alpha(2,1,ienergy) + d*alpha(1,1,ienergy) )
!!$       Enddo
!!$    Endif
!!$  End Subroutine surf_const_coef_nointeract
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_dipole(surf_const_coef,alpha)
!!$    ! Calculate the surface costitutive coefficient
!!$    ! NOTE : All these formulae includes dipole interatction between the 
!!$    !        various islands
!!$    Implicit None    
!!$    Type(surface_constitutive_type)  :: surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                     :: alpha(2,2,param%Numerics%NEnergy)
!!$    integer                          :: ienergy
!!$    Real(wp)                         :: S_mp,S_imp,L,factor,density,d,vol
!!$    complex(wp )                     :: e1,e2,eps
!!$    Complex(wp)                      :: paral,perp
!!$    Real(wp)                         :: sqr
!!$
!!$    ! --- Some energy independent abbreviations
!!$    e1           = eps_vacuum
!!$    sqr          = Sqrt(4._wp*pi/5._wp)
!!$    ! --- Dimensionless density,d,L
!!$    !     d= Absolute distance between surface and the location of multipoles
!!$    d            = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position )
!!$    density      = param%Interaction%Derived%Density*param%Island%Radius**2
!!$    L            = param%Interaction%Lattice_Constant / param%Island%Radius
!!$    ! --- Special value for the dimensionlesslattice constant
!!$    !      (P269 Bedeaux's book) for a random network
!!$    If(param%Interaction%Lattice_Type=='MFT'.or.param%Interaction%Lattice_Type=='RPT') &
!!$         L       =       1._wp/Sqrt(density)  
!!$    ! --- Lattice Sums for the multipoles (MP) and image multipoles (IMP)
!!$    S_mp         = lattice_Sum(0._wp,2)
!!$    S_imp        = lattice_Sum(d,2)
!!$
!!$    ! --- Renormalization of the polarisabilities in the Barrera model
!!$    !     Phys. Rev. B 43 (17) P13819 (1991)
!!$    If(param%Interaction%Lattice_Type=='RPT') Then
!!$       Call Renorm_Polarizability(d,alpha(1,:,:))       
!!$    Endif
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$
!!$       !------------------------------------------------
!!$       !--- MP above the substrate Eqs. 10.29 - 10.28
!!$       !------------------------------------------------
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          e2  =  param%Materials%Epsilon%Substrate(ienergy)    !       Misc%Eps_Substrate(ienergy)    
!!$          eps =  (e1-e2)/(e2+e1)
!!$          ! Uses the formulas 5-67 for conversion betwenn various polarizbilities
!!$          ! Parallel component (10-29)
!!$          factor = S_mp + eps*S_imp
!!$          paral  = alpha(1,1,ienergy)/(4*pi*e1)
!!$          paral  =  4*pi*e1*paral/(1._wp + paral*sqr/(L**3)*factor)
!!$          ! Perpendicular component (10-28)
!!$          factor = S_mp - eps*S_imp
!!$          perp   = alpha(1,2,ienergy)/(4*pi*e1)
!!$          perp   = 4*pi*e1*perp / (1._wp - 2._wp*perp*sqr/(L**3)*factor)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Open(unit=31,file='Polarizability_Int.dat',status='unknown')
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),paral/vol,perp/vol
!!$          Endif
!!$          ! Fills the surface suceptibilities (10-30)
!!$          surf_const_coef(ienergy)%beta   =  density*perp/(e1**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*paral
!!$          surf_const_coef(ienergy)%delta  =  density*d*(paral + perp)/e1
!!$          surf_const_coef(ienergy)%tau    =  density*d*paral
!!$       Enddo
!!$
!!$    Else
!!$
!!$       !------------------------------------------------
!!$       !---  MP below the substrate Eqs. 10-33 10-34
!!$       !------------------------------------------------
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          e2      =  param%Materials%Epsilon%Substrate(ienergy)     !      Misc%Eps_Substrate(ienergy)    
!!$          eps     = (e2-e1)/(e2+e1)
!!$          ! Uses the formulas (5-67) for conversion betwenn various polarizbilities
!!$          ! Parallel component (10-34)
!!$          factor  =  S_mp + eps*S_imp
!!$          paral   =  alpha(1,1,ienergy)/(4*pi*e2)
!!$          paral   =  4*pi*e2*paral/(1._wp + paral*sqr/(L**3)*factor)
!!$          ! Perpendicular component (10-33)
!!$          factor  = S_mp - eps*S_imp
!!$          perp    = alpha(1,2,ienergy)/(4*pi*e2)
!!$          perp    = 4*pi*e2*perp / (1._wp - 2._wp*perp*sqr/(L**3)*factor)
!!$          ! Output for the polarizability
!!$          If(param%Misc%Output_Polarizability) Then
!!$             If(param%Misc%Normalization) Then
!!$                vol = param%Interaction%Derived%Cluster_Volume/(param%Island%Radius**3)
!!$             Else
!!$                vol = 1._wp
!!$             Endif
!!$             Open(unit=31,file='Polarizability_Int.dat',status='unknown')
!!$             Write(unit=31,fmt='(f10.5,5x,2(2f13.5,8x))') &
!!$                  param%Numerics%Energy(ienergy),paral/vol,perp/vol
!!$          Endif
!!$          ! Fills the surface suceptebilities (10-30)
!!$          surf_const_coef(ienergy)%beta   =  density*perp/(e2**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*paral
!!$          surf_const_coef(ienergy)%delta  =  -density*d*(paral + perp)/e2
!!$          surf_const_coef(ienergy)%tau    =  -density*d*paral*e1/e2
!!$       Enddo
!!$
!!$    Endif
!!$
!!$  End Subroutine surf_const_coef_dipole
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_quadrupole(surf_const_coef,alphad,alphaq)
!!$    ! Calculate the surface costitutive coefficients 
!!$    ! NOTE : All these formulae include quadrupole interactions between the 
!!$    !        various islands
!!$    ! Formulas 10-36 --> 10.55
!!$    Implicit None    
!!$    Type(surface_constitutive_type)       ::      surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                          ::      alphad(2,2,param%Numerics%NEnergy)
!!$    complex(wp )                          ::      alphaq(3,param%Numerics%NEnergy) 
!!$    complex(wp )                          ::      A1010,A1111,A2111,A2010,A2121,A2020,A2222,A1020,A1121
!!$    complex(wp )                          ::      Az,Az10,Ap,Ap10 
!!$    complex(wp )                          ::      e1,e2,eps,Dz,Dp
!!$    integer                               ::      ienergy
!!$    Real(wp)                              ::      S2_mp,S2_imp,S3_mp,S3_imp,S4_mp,S4_imp
!!$    complex(wp )                          ::      S2p,S3p,S4p,S2m,S3m,S4m
!!$    Real(wp)                              ::      sq1,sq2,sq3,sq4,sq5,sq6
!!$    Real(wp)                              ::      L,density,d
!!$
!!$    ! Some energy independent abbreviations
!!$    e1       =    eps_vacuum
!!$    sq1      =    Sqrt(15._wp*pi/7._wp)
!!$    sq2      =    Sqrt(pi)
!!$    sq3      =    Sqrt(pi/5._wp)
!!$    sq4      =    Sqrt(3._wp*pi/35._wp)
!!$    sq5      =    Sqrt(pi/35._wp)
!!$    sq6      =    Sqrt(5._wp*pi/7._wp)
!!$
!!$    ! Dimensionless density,d,L
!!$    ! d= Absolute distance between surface and the location of multipoles
!!$    d        = Abs( param%Island%Truncation_Ratio-param%Numerics%Multipole_Position ) 
!!$    density  = param%Interaction%Derived%Density*param%Island%Radius**2
!!$    L        = param%Interaction%Lattice_Constant / param%Island%Radius
!!$    ! Special value for the dimensionlesslattice constant
!!$    ! (P269 Bedeaux's book) for a random network
!!$    If(param%Interaction%Lattice_Type=='MFT'.or.param%Interaction%Lattice_Type=='RPT') &
!!$         L   =  1._wp/Sqrt(density)
!!$
!!$    ! Evaluation of the lattice Sums
!!$    S2_mp    =  lattice_Sum(0._wp,2)
!!$    S2_imp   =  lattice_Sum(d,2)
!!$    S3_mp    =  lattice_Sum(0._wp,3)
!!$    S3_imp   =  lattice_Sum(d,3)
!!$    S4_mp    =  lattice_Sum(0._wp,4)
!!$    S4_imp   =  lattice_Sum(d,4)
!!$
!!$    If(param%Numerics%MP_Above) Then
!!$       !------------------------
!!$       !- MP above the substrate 
!!$       !------------------------        
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          !Some abreviations
!!$          e2    =    param%Materials%Epsilon%Substrate(ienergy)    !    Misc%Eps_Substrate(ienergy)    
!!$          eps   =    (e1-e2)/(e2+e1)
!!$          S2p   =    S2_mp + eps*S2_imp
!!$          S2m   =    S2_mp - eps*S2_imp      
!!$          S3p   =    S3_mp + eps*S3_imp
!!$          S3m   =    S3_mp - eps*S3_imp
!!$          S4p   =    S4_mp + eps*S4_imp
!!$          S4m   =    S4_mp - eps*S4_imp      
!!$
!!$          ! Polarizabilities of clusters interacting with the substrate
!!$          ! Eqs. (5-67) Bedeaux's book
!!$          A1010 =    alphad(1,2,ienergy)/(4*pi*e1)
!!$          A1111 =    alphad(1,1,ienergy)/(4*pi*e1)
!!$          A2111 =    3*alphad(2,1,ienergy)/(4*pi*e1*Sqrt(5._wp))
!!$          A2010 =    alphad(2,2,ienergy)/(2*pi*e1*Sqrt(5._wp/3._wp))
!!$          A2121 =    3*(alphaq(2,ienergy)+2*alphaq(3,ienergy))/(4*pi*e1)
!!$          A2020 =   (2*alphaq(1,ienergy)+3*alphaq(3,ienergy))/(2*pi*e1)
!!$          A2222 =    3*alphaq(3,ienergy)/(2*pi*e1)
!!$          A1020 =    5._wp/3._wp*A2010
!!$          A1121 =    5._wp/3._wp*A2111
!!$
!!$          ! Equations 10-42 10-45
!!$          Dz = (1._wp-4*A1010*S2m/(L**3)*sq3 - 6*A1020*S3m/(L**4)*sq4)* &
!!$               (1._wp+2*A2010*S3p/(L**4)*sq1 + 4*A2020*S4p/(L**5)*sq2)+ &
!!$               (4*A2010*S2m/(L**3)*sq3 + 6*A2020*S3m/(L**4)*sq4)* &
!!$               (2*A1010*S3p/(L**4)*sq1 + 4*A1020*S4p/(L**5)*sq2)
!!$
!!$          Dp = (1._wp+2*A1111*S2p/(L**3)*sq3 + 6*A1121*S3p/(L**4)*sq5)* &
!!$               (1._wp-2*A2111*S3m/(L**4)*sq6 - 8*A2121*S4m/(L**5)*sq2/3)+ &
!!$               (2*A2111*S2p/(L**3)*sq3 + 6*A2121*S3p/(L**4)*sq5)* &
!!$               (2*A1111*S3m/(L**4)*sq6 + 8*A1121*S4m/(L**5)*sq2/3)
!!$
!!$          ! Equations 10-41 10-42
!!$          Az            =       4*pi*e1/(Dz)* &
!!$               ( A1010*(1._wp+2*A2010*S3p/(L**4)*sq1+4*A2020*S4p/(L**5)*sq2) - &
!!$               A2010*(2*A1010*S3p/(L**4)*sq1+4*A1020*S4p/(L**5)*sq2) ) 
!!$
!!$          Az10  =       2*pi*e1*Sqrt(5._wp/3._wp)/(Dz)* &
!!$               ( A2010*(1._wp-4*A1010*S2m/(L**3)*sq3-6*A1020*S3m/(L**4)*sq4) + &
!!$               A1010*(4*A2010*S2m/(L**3)*sq3+6*A2020*S3m/(L**4)*sq4) )
!!$
!!$          Ap            =       4*pi/(Dp)* &
!!$               ( A1111*(1._wp-2*A2111*S3m/(L**4)*sq6-8*A2121*S4m/(L**5)*sq2/3) + &
!!$               A2111*(2*A1111*S3m/(L**4)*sq6+8*A1121*S4m/(L**5)*sq2/3) )
!!$
!!$          Ap10  =       4*pi*e1*Sqrt(5._wp)/3/(Dp)* &
!!$               ( A2111*(1._wp+2*A1111*S2p/(L**3)*sq3+6*A1121*S3p/(L**4)*sq5) - &
!!$               A1111*(2*A2111*S2p/(L**3)*sq3+6*A2121*S3p/(L**4)*sq5) )
!!$
!!$          ! Fills the surface suceptebilities (10-47)
!!$          surf_const_coef(ienergy)%beta   =  density*Az/(e1**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*Ap
!!$          surf_const_coef(ienergy)%delta  =  -density*(Az10+Ap10-d*Az-d*Ap)/e1
!!$          surf_const_coef(ienergy)%tau    =  -density*(Ap10-d*Ap)
!!$
!!$       Enddo
!!$
!!$    Else
!!$       !------------------------
!!$       !- MP below the substrate 
!!$       !------------------------        
!!$       Do ienergy = 1,param%Numerics%NEnergy
!!$          !Some abreviations
!!$          e2    =     param%Materials%Epsilon%Substrate(ienergy)   !  Misc%Eps_Substrate(ienergy)    
!!$          eps   =     (e1-e2)/(e2+e1)
!!$          S2p   =     S2_mp + eps*S2_imp
!!$          S2m   =     S2_mp - eps*S2_imp      
!!$          S3p   =     S3_mp + eps*S3_imp
!!$          S3m   =     S3_mp - eps*S3_imp
!!$          S4p   =     S4_mp + eps*S4_imp
!!$          S4m   =     S4_mp - eps*S4_imp      
!!$
!!$          ! Polarizabilities of clusters interacting with the substrate
!!$          ! Eqs. (5-67) Bedeaux's book
!!$          A1010 =     alphad(1,2,ienergy)/(4*pi*e2)
!!$          A1111 =     alphad(1,1,ienergy)/(4*pi*e2)
!!$          A2111 =     3*alphad(2,1,ienergy)/(4*pi*e2*Sqrt(5._wp))
!!$          A2010 =     alphad(2,2,ienergy)/(2*pi*e2*Sqrt(5._wp/3._wp))
!!$          A2121 =     3*(alphaq(2,ienergy)+2*alphaq(3,ienergy))/(4*pi*e2)
!!$          A2020 =     (2*alphaq(1,ienergy)+3*alphaq(3,ienergy))/(2*pi*e2)
!!$          A2222 =     3*alphaq(3,ienergy)/(2*pi*e2)
!!$
!!$          ! Equations 10-42 10-45
!!$          Dz = (1._wp-4*A1010*S2p/(L**3)*sq3 - 6*A1020*S3p/(L**4)*sq4)* &
!!$               (1._wp+2*A2010*S3m/(L**4)*sq1 + 4*A2020*S4m/(L**5)*sq2)+ &
!!$               (4*A2010*S2p/(L**3)*sq3 + 6*A2020*S3p/(L**4)*sq4)* &
!!$               (2*A1010*S3m/(L**4)*sq1 + 4*A1020*S4m/(L**5)*sq2)
!!$
!!$          Dp = (1._wp+2*A1111*S2m/(L**3)*sq3 + 6*A1121*S3m/(L**4)*sq5)* &
!!$               (1._wp-2*A2111*S3p/(L**4)*sq6 - 8*A2121*S4p/(L**5)*sq2/3)+ &
!!$               (2*A2111*S2m/(L**3)*sq3 + 6*A2121*S3m/(L**4)*sq5)* &
!!$               (2*A1111*S3p/(L**4)*sq6 + 8*A1121*S4p/(L**5)*sq2/3)
!!$
!!$          ! Equations 10-41 10-42
!!$          Az            =       4*pi*e2/(Dz)* &
!!$               ( A1010*(1._wp+2*A2010*S3m/(L**4)*sq1+4*A2020*S4m/(L**5)*sq2) - &
!!$               A2010*(2*A1010*S3m/(L**4)*sq1+4*A1020*S4m/(L**5)*sq2) ) 
!!$
!!$          Az10  =       2*pi*e2*Sqrt(5._wp/3._wp)/(Dz)* &
!!$               ( A2010*(1._wp-4*A1010*S2p/(L**3)*sq3-6*A1020*S3p/(L**4)*sq4) + &
!!$               A1010*(4*A2010*S2p/(L**3)*sq3+6*A2020*S3p/(L**4)*sq4) )
!!$
!!$          Ap            =       4*pi*e2/(Dp)* &
!!$               ( A1111*(1._wp-2*A2111*S3p/(L**4)*sq6-8*A2121*S4p/(L**5)*sq2/3) + &
!!$               A2111*(2*A1111*S3p/(L**4)*sq6+8*A1121*S4p/(L**5)*sq2/3) )
!!$
!!$          Ap10  =       4*pi*e2*Sqrt(5._wp)/3/(Dp)* &
!!$               ( A2111*(1._wp+2*A1111*S2m/(L**3)*sq3+6*A1121*S3m/(L**4)*sq5) - &
!!$               A1111*(2*A2111*S2m/(L**3)*sq3+6*A2121*S3m/(L**4)*sq5) )
!!$
!!$          ! Fills the surface suceptebilities (10-47)
!!$          surf_const_coef(ienergy)%beta   =  density*Az/(e2**2)
!!$          surf_const_coef(ienergy)%gamma  =  density*Ap
!!$          surf_const_coef(ienergy)%delta  =  -density*(Az10+Ap10+d*Az+d*Ap)/e2
!!$          surf_const_coef(ienergy)%tau    =  -density*(Ap10+d*Ap)*(e1/e2)
!!$
!!$       Enddo
!!$
!!$    Endif
!!$
!!$  End Subroutine  surf_const_coef_quadrupole
!!$
!!$
!!$
!!$
!!$


!!$
!!$  Subroutine surf_const_coef_rough_substrate(surf_const_coef)
!!$    !
!!$    ! Calculates the surface constitutive coefficients of rough substrate
!!$    ! with a gaussian roughness
!!$    !
!!$    ! SEE : Vlieger/Bedeaux book pages 382
!!$    !
!!$    Implicit None
!!$    Type(surface_constitutive_type)    :: surf_const_coef(:)
!!$    ! Local
!!$    integer                            :: Ienergy
!!$    complex(wp )                       :: e1,e2
!!$    Real(wp)                           :: t,xi
!!$
!!$    t  = param%Film%Thickness/param%Island%Radius
!!$    xi = param%Island%Coating_Thickness/param%Island%Radius
!!$    e1 =  eps_vacuum    ! Vacuum
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e2                              =   param%Materials%Epsilon%Substrate_file(ienergy)
!!$       surf_const_coef(ienergy)%gamma  =   - Sqrt(pi)/8._wp*(e1+e2)/(e1*e2)*(e2-e1)**2*t**2/xi
!!$       surf_const_coef(ienergy)%beta   =   Sqrt(pi)/4._wp*(e1+e2)/((e1*e2)**2)*(e2-e1)**2*t**2/xi
!!$       surf_const_coef(ienergy)%tau    =   (e2-e1)*t**2/2._wp
!!$       surf_const_coef(ienergy)%delta  =   (e2**2-e1**2)*t**2/(e1*e2)/2._wp
!!$    Enddo
!!$
!!$  End Subroutine surf_const_coef_rough_substrate
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_film_above(surf_const_coef,thickness,eps_film)
!!$    !
!!$    ! Calculates the surface constitutive coefficients of a thin contiguous film 
!!$    ! shifted just above the substrate
!!$    !
!!$    Implicit None
!!$    Type(surface_constitutive_type)    :: surf_const_coef(:)
!!$    Real(wp)                           :: thickness
!!$    complex(wp )                       :: eps_film(:)
!!$    ! Local
!!$    integer                            :: Ienergy
!!$    complex(wp )                       :: e1,e3
!!$
!!$    e1     =  eps_vacuum    ! Vacuum
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e3                              = eps_film(ienergy)
!!$       surf_const_coef(ienergy)%gamma  = (e3-e1)*thickness/param%Island%Radius    
!!$       surf_const_coef(ienergy)%beta   = (1._wp/e1-1._wp/e3) * thickness/param%Island%Radius
!!$       surf_const_coef(ienergy)%tau    = (thickness/param%Island%Radius)**2*(e3-e1)/2 
!!$       surf_const_coef(ienergy)%delta  = (thickness/param%Island%Radius)**2*(e3/e1-e1/e3)/2
!!$    Enddo
!!$
!!$  End Subroutine surf_const_coef_film_above
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_film_below(surf_const_coef,thickness,eps_film,eps_substrate)
!!$    ! Calculates the surface constitutive coefficients of a thin contiuous film
!!$    ! shifted just below the substrate
!!$    Implicit None
!!$    Type(surface_constitutive_type), intent(out)    :: surf_const_coef(:)
!!$    Real(wp),                        intent(in)     :: thickness
!!$    complex(wp ),                    intent(in)     :: eps_film(:),eps_substrate(:)
!!$    ! Local 
!!$    integer                            :: Ienergy
!!$    complex(wp )                       :: e2,e3
!!$
!!$    Do ienergy = 1,param%Numerics%NEnergy
!!$       e2                              = eps_substrate(ienergy)
!!$       e3                              = eps_film(ienergy)
!!$       surf_const_coef(ienergy)%gamma  = (e3-e2)*thickness/param%Island%Radius    
!!$       surf_const_coef(ienergy)%beta   = (1._wp/e2 - 1._wp/e3)*thickness/param%Island%Radius
!!$       surf_const_coef(ienergy)%tau    = -(thickness/param%Island%Radius)**2*(e3-e2)/2 
!!$       surf_const_coef(ienergy)%delta  = (thickness/param%Island%Radius)**2*(e2/e3-e3/e2)/2
!!$    Enddo
!!$
!!$  End Subroutine surf_const_coef_film_below
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine add_2fims_surf_const_coef(SCC1,SCC2,SCC)
!!$    !
!!$    ! Add the surface constitutive coefficients (formulas 3.63 of Bedeaux's book)
!!$    !
!!$    Implicit None
!!$    Type(surface_constitutive_type), intent(in)    :: SCC1(:),SCC2(:)
!!$    Type(surface_constitutive_type), intent(out)   :: SCC(:)
!!$
!!$    ! Add the layers (formulas 3.63 of Bedeaux's book)
!!$    SCC(:)%gamma =  SCC1(:)%gamma + SCC2(:)%gamma
!!$    SCC(:)%beta  =  SCC1(:)%beta  + SCC2(:)%beta
!!$    SCC(:)%tau   =  SCC1(:)%tau   + SCC2(:)%tau
!!$    SCC(:)%delta =  SCC1(:)%delta + SCC2(:)%delta + &
!!$         (SCC2(:)%gamma*SCC1(:)%beta-SCC1(:)%gamma*SCC2(:)%beta)/2._wp
!!$
!!$  End Subroutine add_2fims_surf_const_coef
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_cap(surf_const_coef)
!!$    !
!!$    ! Calculates the surface constitutive coefficients of a thin 
!!$    ! spherical cap 
!!$    !
!!$    ! SEE : Vlieger/Bedeaux book Section 8.5
!!$    !
!!$    Implicit None    
!!$    Type(surface_constitutive_type)     :: surf_const_coef(:)
!!$    ! --- Local 
!!$    complex(wp )                        :: alphad(2,2,param%Numerics%NEnergy)
!!$
!!$    Write(unit=6,fmt=*)
!!$    Write(unit=6,fmt=*) '-----------------------------------------------------------'
!!$    Write(unit=6,fmt=*) ' Warning (Subroutine : surf_const_coef_cap) !'
!!$    Write(unit=6,fmt=*) ' The formulas for a thin cap are valid Only for '
!!$    Write(unit=6,fmt=*) ' a small effective height tr~-1 !'
!!$    Write(unit=6,fmt=*) '-----------------------------------------------------------'
!!$
!!$    ! Calculate the (dimensionless) surface constitutive coefficients for all energies
!!$    !NICK   Select Case(TRAL(param%Numerics%Island_Island_Interaction))
!!$    Select Case(TRIM(param%Numerics%Island_Island_Interaction))
!!$    Case('none')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies 
!!$       Call polarizabilities_cap(alphad)
!!$       Call surf_const_coef_nointeract(surf_const_coef,alphad)
!!$    Case('dipole')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the dipolar situation
!!$       Call polarizabilities_cap(alphad)
!!$       Call surf_const_coef_dipole(surf_const_coef,alphad)
!!$    Case default
!!$       !NICK     Write(unit=6,fmt=*) 'ERROR (surf_const_coef_cap): Parameter ', &
!!$       !NICK       TRAL(param%Numerics%Island_Island_Interaction),' not supported (yet)'
!!$       Write(unit=6,fmt=*) 'ERROR (surf_const_coef_cap): Parameter ', &
!!$            TRIM(param%Numerics%Island_Island_Interaction),' not supported (yet)'
!!$       Pause
!!$       Stop
!!$    End Select
!!$
!!$  End Subroutine surf_const_coef_cap
!!$  
!!$  !
!!$  !--------------------------------------------------------------------------------------------
!!$  !
!!$
!!$  Subroutine surf_const_coef_island(surf_const_coef,Ad,Aq)
!!$    !
!!$    ! Calculate the surface constitutive coefficients for the island geometry
!!$    ! Generic routine
!!$    !
!!$
!!$    !    Use Size_mod,  Only    :       Size_distribution_main                       
!!$    Implicit None    
!!$    Type(surface_constitutive_type)     :: surf_const_coef(param%Numerics%NEnergy)
!!$    complex(wp )                        :: Ad(2,0:1,param%Numerics%NEnergy)
!!$    complex(wp )                        :: Aq(3,param%Numerics%NEnergy)
!!$    complex(wp )                        :: alphad(2,2,param%Numerics%NEnergy)
!!$    complex(wp )                        :: alphaq(3,param%Numerics%NEnergy)
!!$    Real(wp)                            :: c0,s0,sqr1,sqr2,e1
!!$    complex(wp )                        :: exp0
!!$
!!$    ! Ad : results of the calculations with a constant field
!!$    ! Aq : results for the calculations for a quadrupolar field    
!!$
!!$    ! Calculate the (dimensionless) surface constitutive coefficients for all energies
!!$    !NICK   Select Case(TRAL(param%Numerics%Island_Island_Interaction))
!!$    Select Case(TRIM(param%Numerics%Island_Island_Interaction))
!!$    Case('none')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies 
!!$       Call polarizabilities(alphad,Ad)
!!$       Call surf_const_coef_nointeract(surf_const_coef,alphad)
!!$    Case('dipole')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the dipolar situation
!!$       Call polarizabilities(alphad,Ad)
!!$       Call surf_const_coef_dipole(surf_const_coef,alphad)
!!$       ! Correct to first order the potential development
!!$       e1          = eps_vacuum
!!$       c0          = Cos(param%Source%Derived%Theta0_calc)
!!$       s0          = Sin(param%Source%Derived%Theta0_calc)
!!$       exp0        = Exp(-imu*param%Source%Derived%Phi0_calc)
!!$       sqr1        = Sqrt(pi/3._wp)
!!$       sqr2        = Sqrt(pi/5._wp)
!!$       Ad(1,0,:)   = alphad(1,2,:)*sqr1*c0/(2*pi*e1)
!!$       Ad(2,0,:)   = alphad(2,2,:)*sqr2*c0/(pi*e1)
!!$       Ad(1,1,:)   = -alphad(1,1,:)*Sqrt(2._wp)*sqr1*s0*exp0/(4*pi*e1)
!!$       Ad(2,1,:)   = -alphad(2,1,:)*Sqrt(6._wp)*sqr1*s0*exp0/(4*pi*e1)
!!$    Case('quadrupole')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the quadrupolar situation
!!$       Call polarizabilities(alphad,Ad)
!!$       Call polarizabilities_quadrupole(alphaq,Aq)
!!$       Call surf_const_coef_quadrupole(surf_const_coef,alphad,alphaq)
!!$       !           Case('Size')
!!$       ! Calculate the (dimensionless) polarizabilities (alpha) 
!!$       ! for all energies in the dipolar situation taking into account
!!$       ! a Size distribution
!!$       !                  Call polarizabilities(alphad,Ad)
!!$       !                  e1            = eps_vacuum
!!$       !                  Call Size_distribution_main(alphad)
!!$       !                  density       = param%Interaction%Derived%Density*param%Island%Radius**2
!!$       !                  Do ienergy = 1,param%Numerics%NEnergy
!!$       ! Only the particle polarizabities are taken into account and 
!!$       ! the second order coefficient are set equal to zero
!!$       !                        surf_const_coef(ienergy)%gamma  =   density*alphad(1,1,ienergy) 
!!$       !                        surf_const_coef(ienergy)%beta   =   density*alphad(1,2,ienergy)/(e1**2)   
!!$       !                        surf_const_coef(ienergy)%delta  =   0._wp
!!$       !                        surf_const_coef(ienergy)%tau    =       0._wp   
!!$       !                  Enddo
!!$    End Select
!!$
!!$  End Subroutine surf_const_coef_island
!!$
!!$  !
!!$  !--------------------------------------------------------------------------------------------






