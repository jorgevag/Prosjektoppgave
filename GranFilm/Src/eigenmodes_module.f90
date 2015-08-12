! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!   
!     This module contains routines for the study of the eigenmodes of the island system 
!     at zero incident field. 
!
! 
! --- AUTHOR : Sindre Stavseng, Trondheim, April 2013
!
! ----------------------------------------------------------


!----------------------------------!
Module Eigenmodes_Module
!----------------------------------! 

  ! --- The Use Statements global to the module
  Use Shared, only : wp, param	   
  Use Error_Module,                   only : Error_Warning, Error_Failure

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Calculate_Determinants
  Public :: Calculate_Eigenvalues

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


!-------!
Contains	
!-------!

  !---------------------------------------!
  Subroutine Calculate_Determinants( )
  !---------------------------------------!
    ! This routine calculates the determinant of the matrix A for all 
    ! energies used in the previous calculations and writes it to file.
    !
    ! The calculation is turned on/off using the perform_det_calculation flag
    ! below.
    !
    ! Author: Sindre Stavseng, April 2013, Trondheim
    ! ------------------------------------------------------------------
    
    Use Matrix_System_Sphere_Module,    only : Get_Matrix_System_Sphere
    Use Matrix_System_Spheroid_Module,  only : Get_Matrix_System_Spheroid
    
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Calculate_Determinants"
    Complex(wp), allocatable    :: A(:,:), b(:), Det(:,:)
    Integer                     :: ienergy, m, N
    ! --- Move to param? -----------------------------------
    Logical, parameter          :: perform_det_calculation = .false.
    ! ------------------------------------------------------
    
    If(.not. perform_det_calculation) then
       return
    End if
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    ! --- Allocate the Storage for the matrix A
    ! --- (Note that the b vector is not in use but is needed in order to 
    !      re-use the get_matrix_system routines.)
    N = 2 * param%Numerics%Multipole_Order *  size( Param%Geometry%Radius_Ratios,1)
    if (allocated(A)) deallocate(A);     Allocate( A( N, N ) ) 
    if (allocated(b)) deallocate(b);     Allocate( b( N    ) )
    
    ! --- Allocate storage for the resulting determinants
    if (allocated(Det)) deallocate(Det);     Allocate( Det( size(param%Numerics%Energy,1), 0:1 ) ) 

    ! -------------------
    ! --- Energy loop
    ! -------------------
    Energy_Loop : Do ienergy = 1, size(param%Numerics%Energy,1)
       ! --- The m quantum number
       m_loop : Do m=0,1
          ! Get the matrix A
          if (size(Param%Geometry%Radius)==1) then 
            call Get_Matrix_System_Sphere( A, b, m, ienergy )
          else 
            call Get_Matrix_System_Spheroid( A, b, m, ienergy )
          end if 
          
          Det(ienergy, m) = Determinant(A)

       End Do m_loop
    End Do Energy_Loop
    
    ! --- Write determinants to file:
    call write_determinants_to_file( Det, param%Numerics%Energy, size(param%Numerics%Energy,1) )
    ! --------------------------------------------
    
    ! --- Deallocate storeage
    if (allocated(A))           deallocate(A)
    if (allocated(b))           deallocate(b)
    if (allocated(Det))         deallocate(Det)
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

    
  !-------!
  Contains	
  !-------!
    
    !---------------------------------------!
    Function Determinant(A)
    !---------------------------------------! 
      Implicit None
      Complex(wp), dimension(:,:), intent(In)  :: A
      Complex(wp)                              :: Determinant
      ! --- Local ---
      Integer                                  :: N, info, i
      Integer, allocatable                     :: ipiv(:)
      
      N = size(A, 1)
      if (allocated(ipiv)) deallocate(ipiv); allocate( ipiv(N) ) 

      Determinant = 0.0_wp
      
      ! LU-factorization
      Call zgetrf(N,N,A,N,ipiv,info)
      
      If (info /= 0) then
         call Error_Failure("Determinant(A)","LAPACK zgetrf() returned with an error!")
      End if
      
      Determinant = 1.0_wp

      Do i=1,N
         if ( ipiv(i) /= i ) then
            Determinant = -Determinant*A(i,i)
         else
            Determinant = Determinant*A(i,i)
         end if
      End Do
            
      if (allocated(ipiv)) deallocate(ipiv)
    !---------------------------------------! 
    End Function Determinant
    !---------------------------------------!   
 
  !---------------------------------------!    
  End Subroutine Calculate_Determinants
  !---------------------------------------!

  !---------------------------------------!
  Subroutine Calculate_Eigenvalues( )
  !---------------------------------------!
    ! This routine calculates the eigenvalues and eigenvectors of the matrix A for all 
    ! energies used in the previous calculations. The eigenvalues are written to file.
    ! The calculation is carried out if the perform_ev_calculation flag is set to true.
    !
    ! The potential corresponding to the eigenvectors is evaluated and written to file
    ! if the perform_ev_potential_calculation flag is set to true. This file follows the
    ! conventions of the potential module. Note that in order for the eigenvector potential
    ! calculation to work, a point file and an energy must be specified in the potential namelist
    ! of the parameter file.
    !
    ! Author: Sindre Stavseng, April 2013, Trondheim
    ! ------------------------------------------------------------------
    Use Matrix_System_Sphere_Module,    only : Get_Matrix_System_Sphere
    Use Matrix_System_Spheroid_Module,  only : Get_Matrix_System_Spheroid
    Use Error_Module,                   only : Error_Warning, Error_Failure
    
    Implicit None
    ! --- Local
    Character(len=*), parameter :: routine = "Calculate_Eigenvalues"
    Complex(wp), allocatable    :: A(:,:), b(:)
    Integer                     :: ienergy, m, N
    Complex(wp), allocatable    :: eigenvalues(:,:,:)
    Complex(wp), allocatable    :: eigenvectors(:,:,:,:) 
    ! --- Move to param? -----------------------------------
    Logical, parameter          :: perform_ev_calculation = .false.
    Logical, parameter          :: perform_ev_potential_calculation = .false.
    ! ------------------------------------------------------
    Integer                     :: pot_eval_energy_loc
    Integer, allocatable        :: minloc_eigenvalues_m0(:), minloc_eigenvalues_m1(:)

    If(.not. perform_ev_calculation) then
       return
    End if
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine

    ! --- Allocate the Storage for the matrix A
    ! --- (Note that the b vector is not in use but is needed in order to 
    !      re-use the get_matrix_system routines.)
    N = 2 * param%Numerics%Multipole_Order *  size( Param%Geometry%Radius_Ratios,1)
    if (allocated(A)) deallocate(A);     Allocate( A( N, N ) ) 
    if (allocated(b)) deallocate(b);     Allocate( b( N    ) )
    
    ! --- Allocate storage for the resulting eigenvalues/-vectors
    if (allocated(eigenvalues)) deallocate(eigenvalues)
    Allocate( eigenvalues( size(param%Numerics%Energy,1), 0:1, N ) ) 
    if (allocated(eigenvectors)) deallocate(eigenvectors)
    Allocate( eigenvectors( size(param%Numerics%Energy,1), 0:1, N, N ) ) 

    if (allocated(minloc_eigenvalues_m0)) deallocate(minloc_eigenvalues_m0)
    Allocate( minloc_eigenvalues_m0( size(param%Numerics%Energy,1) ) ) 
    if (allocated(minloc_eigenvalues_m1)) deallocate(minloc_eigenvalues_m1)
    Allocate( minloc_eigenvalues_m1( size(param%Numerics%Energy,1) ) ) 

    ! -------------------
    ! --- Energy loop
    ! -------------------
    Energy_Loop : Do ienergy = 1, size(param%Numerics%Energy,1)
       ! --- The m quantum number
       m_loop : Do m=0,1
          ! Get the matrix A, note that vector b is discarded
          if (size(Param%Geometry%Radius)==1) then 
            call Get_Matrix_System_Sphere( A, b, m, ienergy )
          else 
            call Get_Matrix_System_Spheroid( A, b, m, ienergy )
          end if 
          
          !Det(ienergy, m) = Determinant(A)
          call Get_Eigen(A, eigenvalues(ienergy, m, :), eigenvectors(ienergy, m, :, :) )
          
          If (ienergy==344 .and. m==1) Then
             call write_matrix_to_file(A,N)
          End If
          
          
       End Do m_loop
    End Do Energy_Loop
    
    ! -------------------------------
    ! Write eigenvalues to file
    ! -------------------------------
    call write_eigenvalues_to_file(eigenvalues, param%Numerics%Energy, size(param%Numerics%Energy,1), N)
    ! -------------------------------
    

    If (perform_ev_potential_calculation) Then
       ! ---------------------------------------
       ! Get location of minimum abs(eigenvalue) for each energy
       ! ---------------------------------------
       minloc_eigenvalues_m0 = minloc( abs( eigenvalues(:,0,:) ), 2 )
       minloc_eigenvalues_m1 = minloc( abs( eigenvalues(:,1,:) ), 2 )
       
       ! ---------------------------------------------------------
       ! Get the array position of the energy to evaluate the eigenvector 
       ! potential at. This is stored in ienergy.
       ! ---------------------------------------------------------
       call Get_closest_ienergy_eigenvector(Param%Potential%Energy(1), ienergy)
       
       !pot_eval_energy_loc = 200 ! The array position of the energy to evaluate the eigenvector potential at
       
       ! -------------------------------
       ! Write corresponding potential to file
       ! -------------------------------    
       call Get_Potential_From_Eigenvector( N, &
            eigenvectors( ienergy, 0, :, minloc_eigenvalues_m0(ienergy) ), &
            eigenvectors( ienergy, 1, :, minloc_eigenvalues_m1(ienergy) ), &
            ienergy )
       ! -------------------------------    
    End If
    
    ! --- Deallocate storeage
    if (allocated(A))            deallocate(A)
    if (allocated(b))            deallocate(b)
    if (allocated(eigenvalues))  deallocate(eigenvalues)
    if (allocated(eigenvectors)) deallocate(eigenvectors)
    if (allocated(minloc_eigenvalues_m0)) deallocate(minloc_eigenvalues_m0)
    if (allocated(minloc_eigenvalues_m1)) deallocate(minloc_eigenvalues_m1)
    
    ! --- If verbose
    If (Param%InOut%Verbose) Write(*,*) " --- Leaving  : ", routine

  !---------------------------------------!
  End Subroutine Calculate_Eigenvalues
  !---------------------------------------!
  
  !---------------------------------------------------------------------!
  Subroutine Get_Eigen(A, eigenvalues, eigenvectors)
  !---------------------------------------------------------------------!
    ! ---------------------------------------------------------------------------------
    ! About:
    ! This routine calculates the eigenvalues of the NxN matrix A, as well as
    ! the (right) eigenvectors, using the LAPACK routine ZGEEV. A nice documentation
    ! of zgeev can be found here:
    ! http://www.physics.orst.edu/~rubin/nacphy/lapack/routines/zgeev.html
    !
    ! Author:
    ! Sindre Stavseng, Trondheim, Apr. 2013
    ! ----------------------------------------------------------------------------------
    
    Implicit None
    Complex(wp), dimension(:,:), intent(In)     :: A
    Complex(wp), dimension(:), intent(InOut)    :: eigenvalues
    Complex(wp), dimension(:,:), intent(InOut)  :: eigenvectors
    ! --- Local ---
    Integer                                     :: N, lwork, info
    Complex(wp), allocatable                    :: WORK(:)
    Real(wp), allocatable                       :: rwork(:)
    Complex(wp)                                 :: DUMMY(1,1)
    
    N = size(A, 1)
    lwork = 2*N
    ! --- Allocate storage for the local arrays
    if (allocated(WORK)) deallocate(WORK)
    Allocate( WORK( 2*N ) )
    if (allocated(rwork)) deallocate(rwork)
    Allocate( rwork( 2*N ) )
    
    !w ! array for eigenvalues, dimension N
    !vr ! Array for eigenvectors, dimension ldvr*N
    !WORK ! Complex array, dimension >= 2*N
    !lwork ! integer >= 2*N
    !rwork ! double precision array, dimension 2*N

    ! call zgeev( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
    call zgeev( 'N', 'V', N, A, N, eigenvalues , DUMMY, 1, eigenvectors, N, WORK, lwork, rwork, info )

    If (info /= 0) then
       call Error_Failure("Get_Eigen","LAPACK zgeev() returned with an error!")
    End if

    if (allocated(WORK)) deallocate(WORK)
    if (allocated(rwork)) deallocate(rwork)

  !---------------------------------------------------------------------!
  End Subroutine Get_Eigen
  !---------------------------------------------------------------------!

  !---------------------------------------------------------------------!
  Subroutine Get_Potential_From_Eigenvector(N, eig_vec_m0, eig_vec_m1, iEnergy)
  !---------------------------------------------------------------------!
    ! ABOUT:
    ! This routine takes in two eigenvectors containing mp coefficients and calculates the
    ! potential at points specified by a points file.
    !
    ! Note: A points file must be supplied for this routine to work!
    !
    ! AUTHOR:
    ! Sindre Stavseng, Trondheim, April 2013
    
    Use Potential_Module,            only : Get_points, Cartesian_to_Spherical, &
                                            Get_Medium_Sphere, Get_multipole_pot_sphere, write_to_file, &
                                            Get_Medium_Spheroid, Cartesian_to_Spheroidal_Oblate, &
                                            Cartesian_to_Spheroidal_Prolate, Get_multipole_pot_spheroid_oblate, &
                                            Get_multipole_pot_spheroid_prolate, Get_const_pot_sphere, &
                                            Get_const_pot_spheroid

    implicit none
    integer, Intent(In)                   :: N
    complex(wp), Intent(In)               :: eig_vec_m0(N), eig_vec_m1(N)
    !complex(wp), dimension(:), Intent(In) :: eig_vec_m0, eig_vec_m1
    !real(wp), Intent(In)                  :: eig_energy
    integer, Intent(In)                   :: iEnergy
    ! --- Local --- 
    Character(len=*), parameter           :: routine = "Get_Potential_From_Eigenvector"
    Logical                               :: file_exists
    real(wp), allocatable                 :: x(:),y(:),z(:)
    real(wp), allocatable                 :: r_xi(:),eta_theta(:),phi(:)
    complex(wp), allocatable              :: multipole_pot(:), eps(:,:), coeff(:,:), output_matrix(:,:), &
                                             const_pot(:)
    integer, allocatable                  :: iMedium(:)
    integer                               :: Npts, j, nEnergy_potential,nMedia, system_dim
    real(wp)                              :: a

    If (Param%Potential%Potential_Calculation) then
    !-----------------------------------------------------   
       ! --- Potential Calculation using mp coefficients from eigenvector
       If (Param%InOut%Verbose) Write(*,*) " +++ Entering : ", routine
       
       !nEnergy_potential = size(param%potential%energy)
       nEnergy_potential = 1  ! Potential is only calculated for ONE energy
       nMedia = 2*size(param%geometry%radius_ratios) + 2
       system_dim = 2 * param%numerics%multipole_order * size(param%geometry%radius_ratios)
       allocate(eps(nEnergy_potential,nMedia),coeff(system_dim,0:1))
       coeff(:,0) = eig_vec_m0
       coeff(:,1) = eig_vec_m1
       call Get_Points(x,y,z,Npts)
       allocate(r_xi(Npts),eta_theta(Npts),phi(Npts), output_matrix(Npts,nEnergy_potential), &
                multipole_pot(Npts), const_pot(Npts) )
       allocate(iMedium(Npts))
       !call Get_closest_ienergy_eigenvector(eig_energy, iEnergy)
       ! --- Does point-file exist?
       inquire(file = param%potential%points_file, exist = file_exists)
       
       if (file_exists) then
          ! -------------------------------------------------------------
          Select case( size(param%geometry%radius) )
             ! -----------------------------------------------------------
             Case(1) ! = Sphere
                
                call Cartesian_to_Spherical(x,y,z,Npts,0.0_wp,r_xi,eta_theta,phi)
                call Get_Medium_Sphere(r_xi,eta_theta,phi,Npts,iMedium)
                do j=1,nMedia
                   eps(1,j) = param%media(j)%epsilon(iEnergy)
                end do
                call Get_multipole_pot_sphere(x,y,z,Npts,iMedium,nMedia, eps(1,:), &
                                              system_dim, coeff, multipole_pot)
                call Get_const_pot_sphere( iMedium, Npts, nMedia, system_dim, coeff, eps(1,:), const_pot )

                output_matrix(:,1) = multipole_pot + const_pot

             ! ---------------------------------------------------------------
             Case(2) ! = Spheroid
                
                call Get_Medium_Spheroid(r_xi,eta_theta,phi,Npts,iMedium)

                If ( Param%Geometry%isOblate ) Then
                   a = sqrt(param%geometry%radius(2)**2-param%geometry%radius(1)**2)
                   call Cartesian_to_Spheroidal_Oblate(x,y,z,Npts,a,0.0_wp,r_xi,eta_theta,phi)
                   do j=1,nMedia
                      eps(1,j) = param%media(j)%epsilon(iEnergy)
                   end do
                   call Get_multipole_pot_spheroid_oblate(x,y,z,Npts,iMedium,nMedia, eps(1,:), &
                                                          system_dim, coeff, multipole_pot)
                   call Get_const_pot_spheroid( iMedium, Npts, nMedia, system_dim, coeff, eps(1,:), const_pot )

                   output_matrix(:,1) = multipole_pot + const_pot
                   
                Else if ( Param%Geometry%isProlate ) Then
                   a = sqrt(param%geometry%radius(1)**2-param%geometry%radius(2)**2)
                   call Cartesian_to_Spheroidal_Prolate(x,y,z,Npts,a,0.0_wp,r_xi,eta_theta,phi)
                   do j=1,nMedia
                      eps(1,j) = param%media(j)%epsilon(iEnergy)
                   end do
                   call Get_multipole_pot_spheroid_prolate(x,y,z,Npts,iMedium,nMedia, eps(1,:), &
                                                           system_dim, coeff, multipole_pot)
                   call Get_const_pot_spheroid( iMedium, Npts, nMedia, system_dim, coeff, eps(1,:), const_pot )

                   output_matrix(:,1) = multipole_pot + const_pot
                End If

             ! ----------------------------------------------------------------
             Case default
                call Error_Failure(routine,"Internal Error; Unsupported size(param%geometry%radius)" )
             ! -----------------------------------------------------------------
          End select
          ! ----------------------------------------------------------------------
          
          ! --- Write potential to file 
          call write_eigenmode_potential_to_file(x,y,z,output_matrix,iMedium,Npts,iEnergy, nMedia,eps)
          
       else
          ! --- The point-file does not exist....
          call Error_Failure(routine,&
           "Potential Point-File ("//trim(adjustl(param%potential%points_file))//") does not exist.")
       end if
       
       deallocate(x,y,z,r_xi,eta_theta,phi,iMedium,output_matrix,eps,multipole_pot,coeff,const_pot)
    
       If (Param%InOut%Verbose) Write(*,*) " --- Leaving : ", routine
    Else
       ! --- Do nothing...
    End if
    
  !---------------------------------------------------------------------!
  End Subroutine Get_Potential_From_Eigenvector
  !---------------------------------------------------------------------!

  !----------------------------------------------------------!
  Subroutine Get_closest_ienergy_eigenvector(eigen_energy, iEnergy)
  !----------------------------------------------------------!
    implicit none
    character(len=*), parameter :: routine = "Get_closest_ienergy_eigenvector"
    real(wp), intent(in)        :: eigen_energy
    integer, intent(out)        :: iEnergy
    integer                     :: i,j
    real(wp), parameter         :: warning_threshold = 0.05
    
    write(*,'(A,F10.7)') ' ***Finding match for E = ',eigen_energy
    if (eigen_energy < param%numerics%energy(1) .or. &
         eigen_energy > param%numerics%energy(size(param%numerics%energy))) then
       call Error_Failure(routine,"Energy for potential outside range.")
    end if
    do j=1,size(param%numerics%energy)
       if (param%numerics%energy(j)>eigen_energy) then
          iEnergy = j
          write(*,'(A,F10.7)') " ***Found match: E = ",param%numerics%energy(j)
          if (abs(eigen_energy-param%numerics%energy(j)) > warning_threshold) then
             call Error_Warning(routine, "Matched energy may be far away from &
                  requested energy for potential calculation.")
          end if
          exit
       end if
    end do
    
  End Subroutine Get_closest_ienergy_eigenvector
  !----------------------------------------------------------!

!---------------------------------------------------------------------!
  subroutine write_eigenmode_potential_to_file(x,y,z,Matrix,iMedium,Npts,iEnergy, nMedia,eps)
  !---------------------------------------------------------------------!
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    integer, intent(in)       :: Npts, iMedium(Npts),iEnergy ,nMedia
    complex(wp), intent(in)   :: Matrix(Npts,1), eps(1,nMedia)
    real(wp), intent(in)      :: x(Npts),y(Npts),z(Npts)
    integer                   :: file_id,i,j
    
    ! Writing potential
    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
                                //"_eigenmode_potential.dat")
    write(file_id,fmt='(A)',advance='no') "# x   y   z   Medium    Potential(Re,Im), E = "
    write(file_id,fmt='(F12.6)',advance='no') param%numerics%energy( iEnergy )
    write(file_id,*)
    write(file_id,'(A8,F9.6,A3)',advance='no') "# eps(E=",param%numerics%energy(iEnergy),"): "
    write(file_id,*) (eps(1,j), j=1,nMedia)
    do i=1,Npts
       write(file_id,*) x(i),y(i),z(i),iMedium(i), real(Matrix(i,1)), aimag(Matrix(i,1)) 
    end do
    Close(unit=file_id)

  end subroutine write_eigenmode_potential_to_file
  !--------------------------------------------------------------------------!

  !---------------------------------------------------------------------!
  Subroutine write_determinants_to_file(Det, Energies, nEnergies)
  !---------------------------------------------------------------------!
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    integer, intent(in)           :: nEnergies
    Complex(wp), intent(in)       :: Det(nEnergies, 0:1)
    Real(wp), intent(in)          :: Energies(nEnergies)
    ! --- Local ---
    integer                       :: file_id, i

    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
         //"_determinants.dat")
    write(file_id, fmt='(A)') "#          m = 0                             m = 1                           "
    write(file_id, fmt='(A)') "# Energy   Real( Det(A) )   Imag( Det(A) )   Real( Det(A) )   Imag( Det(A) ) "
    Do i=1, nEnergies
        write(file_id,*) Energies(i), Real(Det(i,0)), Aimag(Det(i,0)), Real(Det(i,1)), Aimag(Det(i,1))
    End do
    Close(unit=file_id)

  !---------------------------------------------------------------------!
  End Subroutine write_determinants_to_file
  !---------------------------------------------------------------------!

  !---------------------------------------------------------------------!
  Subroutine write_eigenvalues_to_file(Evalues, Energies, nEnergies, N)
  !---------------------------------------------------------------------!
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    integer, intent(in)           :: nEnergies, N
    Complex(wp), intent(in)       :: Evalues(nEnergies, 0:1, N)
    Real(wp), intent(in)          :: Energies(nEnergies)
    ! --- Local ---
    integer                       :: file_id, i

    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
         //"_eigenvalues.dat")
    write(file_id, fmt='(A)') "#               m = 0                   m = 1              "
    write(file_id, fmt='(A)') "# Energy        Minval( Abs(evs) )      Minval( Abs(evs) ) "
    Do i=1, nEnergies
        write(file_id,*) Energies(i), minval(abs(Evalues(i,0,:))), minval(abs(Evalues(i,1,:)))
    End do
    Close(unit=file_id)
  !---------------------------------------------------------------------!
  End Subroutine write_eigenvalues_to_file
  !---------------------------------------------------------------------!

  !---------------------------------------------------------------------!
  Subroutine write_matrix_to_file(A, N)
  !---------------------------------------------------------------------!
    Use SFL_Logical_Units,          only : SFL_Get_Free_Unit 
    implicit none
    Integer, Intent(In)             :: N
    Complex(wp), Intent(In)         :: A(N,N)
    
    ! --- Local ---
    integer                       :: file_id, i, j
    
    call SFL_Get_Free_Unit( file_id )
    Open( unit=file_id, file=trim(adjustl(param%InOut%Output_Base_Name))&
         //"_matrix.dat")
    write(file_id,fmt='(A)',advance='no') "# "
    write(file_id,*) N
    Do i = 1,N
       Do j = 1,N
          write(file_id,*) Real(A(i,j)), Aimag(A(i,j))
       End Do
       write(file_id,*)
    End Do
    Close(unit=file_id)
    
  !---------------------------------------------------------------------!
  End Subroutine write_matrix_to_file
  !---------------------------------------------------------------------!


!----------------------------------!
End Module Eigenmodes_Module
!----------------------------------! 

! === ABOUT ============================================================
! The function Determinant(A) is based on the following
! code, found at http://software.intel.com/en-us/forums/topic/309460 :
! -----------------------------------------------------------------------
!
! call dgetrf(n,n,a,n,piv,info)
! det = 0d0
! if (info.ne.0) then
!   return
! endif
! det = 1d0
! do 10,i=1,n
!   if (piv(i).ne.i) then
!     det = -det * a(i,i)
!   else
!     det = det * a(i,i)
!   endif
! 10 continue
! end
!
! =====================================================
