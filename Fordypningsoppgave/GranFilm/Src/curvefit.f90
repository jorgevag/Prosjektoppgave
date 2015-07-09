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
! --- AUTHOR : Eskil Aursand, Trondheim, 2011.
!
! ----------------------------------------------------------
!



!-----------------------------!
Module Curvefit_Module
!-----------------------------!

  ! --- The Use Statements global to the module
  Use Shared
  Use Error_Module,                     only : Error_Failure
  Use Initialize_Module,                only : Initialize_GranFilm

  ! --------------------------------------
  ! --- The Publicly available routines
  ! --------------------------------------
  Public :: Curvefit, N_constrained_param

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private
  
  ! --- Module-global data
  integer, parameter        :: N_param = 5
  ! Number of variable parameters in the optimization
  integer                   :: N_fit
  integer, parameter        :: broadening_params = 2
  ! Verbosity of curve fitting procedure 
  logical, parameter        :: verbose_curvefit = .true.
  ! Arrays for the experimental curve in its interpolated form, the 
  ! simulated curve, the difference between these two, and the 
  ! energies.
  real(wp), allocatable     :: R_exp_interp(:), R_sim(:), E(:)
  ! Constraints:
  ! xl, xr: Lower and upper bounds for the parameters 
  ! A,b: Linear constraints Ax>b
  real(wp)                  :: xl_constr(N_param), xr_constr(N_param),&
                               A_constr(N_param), b_constr
  ! Large value to used for divergence when constraints are broken:
  real(wp), parameter       :: largeval = huge(1.0_wp)
  !
  

!-------!
Contains
!-------!
 
  !--------------------------------!
  Subroutine Curvefit()
  !--------------------------------!
  use Integrals_Module,                 only : Allocate_Integrals
  use Physical_Observables_Module,      only : Allocate_Results

  implicit none
  real(wp), allocatable             :: E_exp(:), R_exp(:),fjac(:,:),wa4(:),&
                                       fvec(:), jacobian(:,:),cov_mat(:,:)
  integer                           :: M_exp,M,i,j,ierr,res,info,&
                                       maxfev,mode,nprint,nfev,ldfjac,&
                                       ipvt(N_param)
  real(wp)                          :: Emin,Emax,x(N_param),x_l(N_param),&
                                       x_r(N_param),A(N_param),b,&
                                       ftol,xtol,gtol,epsfcn,diag(N_param),&
                                       factor,qtf(N_param), wa1(N_param),&
                                       wa2(N_param),wa3(N_param),&
                                       density,coverage,eq_thickness,&
                                       contact_angle
  character(len=*), parameter       ::  routine = "Curvefit"

  character(len=*), parameter       :: parameter_outfile = "fitted_pars.dat"
  character(len=*), parameter       :: derived_parameter_outfile =& 
                                                           "derived_pars.dat"
  character(len=*), parameter       :: info_outfile = 'curvefit_info.dat'
  character(len=*), parameter       :: covariance_outfile = 'covariance.dat'



  ! Number of variable parameters in curve fitting
  ! If freeze_broadening, only R,t,L will be allowed to vary.
  if (param%curvefitting%freeze_broadening) then
    N_fit = N_param - broadening_params
  else
    N_fit = N_param
  end if 
  allocate(cov_mat(N_fit,N_fit))

  ! --- Read experimental data file.
  write(*,*) 'Reading file with experimental data..'
  call Read_Datafile(Param%InOut%Experimental_File_Name,E_exp,R_exp,M_exp)
  write(*,*) 'Loaded experimental data:'
  write(*,'(I10,X, A,F6.3,X,A,F6.3)') M_exp, "points from", E_exp(1),&
                                  "to",E_exp(M_exp)

  ! --- Getting energy values from simulation config
  M = param%Numerics%No_Energy_Points
  Emin = param%Source%Energy_Range(1)
  Emax = param%Source%Energy_Range(2) 

  write(*,*) 'Allocating storage for shared arrays of dimension M...'
  allocate(R_sim(M),E(M),R_exp_interp(M),jacobian(M,N_fit))
  
  write(*,*) 'Simulation settings:'
  write(*,'(I10,X, A,F6.3,X,A,F6.3)') M, "points from", Emin,"to",Emax
  write(*,*) 'Interpolating experimental data to fit...'
  ! --- Interpolate experimental data to match the calculated data points
  call Interpolate_Experimental(E_exp, R_exp)
  
  ! --- Get constraints for the optimization. (Box and linear)
  call Get_Constraints()

  ! --- Get initial guess
  call Get_Initial_Guess(x)
  
  ! --- Set fitting-algorithm parameters
  call Set_lmdif_parameters(M,N_fit,fvec,ftol,xtol,gtol,maxfev,epsfcn,diag,&
                                  mode,factor,nprint,fjac,ldfjac,wa4)
  
  ! --- Do curve fitting (lmdif from MINPACK)
  write(*,*) "Calling lmdif"
  write(*,*) "N_fit=",N_fit
  call lmdif(fcn,M,N_fit,x(:N_fit),fvec,ftol,xtol,gtol,maxfev,epsfcn, diag,&
            mode,factor,nprint,info,nfev,fjac,ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)
 
  ! --- Getting derived parameters: density,coverage,eq_thickness
  call Get_Derived_Parameters(x(1),x(2),x(3),contact_angle,density,&
                              coverage,eq_thickness)

  
  ! Getting Jacobian matrix. 
  epsfcn = 0.0_wp
  ! Removing the artificial plateau set by Param%Curvefitting%broadening_zero, 
  ! by setting it to zero.
  Param%Curvefitting%broadening_zero = 0.0_wp
  call fdjac2(fcn,M,N_fit,x(:N_fit),fvec,jacobian,ldfjac,info,epsfcn,wa4)
  
  ! Getting covariance matrix. 
  ! Noise in experimental data = Param%Curvefitting%Sigma
  call Get_Covariance_Matrix(N_fit,M,jacobian,cov_mat)
 
  ! --- Finalize 
  call Curvefit_Report(x,contact_angle,density,coverage,eq_thickness,info,&
                            nfev,fvec,M,cov_mat,N_fit)

  ! --- Run a final simulation with these parameters, as GranFilm would, to 
  ! make the regular output file.
  call Update_Parameters(x)
  call Run_Simulation()

  
  ! --- Write parameters to parameter_outfile
  call Save_Parameters(x,parameter_outfile)
  ! --- Write derived parameters to derived_parameter_outfile
  call Save_Derived_Parameters(contact_angle,density,coverage,eq_thickness,&
                                    derived_parameter_outfile)
  ! --- Write extra info to info_outfile
  call Save_Info(info,fvec,M,N_fit,info_outfile)
  
  ! --- Write covariance matrix to covariance_outfile
  call Save_Covariance(N_fit,cov_mat,covariance_outfile)
     
  End Subroutine Curvefit
  !---------------------!
   

  !---------------------!
  subroutine fcn(M,N,x,fvec,iflag)
  !---------------------!
  ! Objective function to be passed to lmdif according to precise 
  ! specifications (see lmdif doc)
  !
    implicit none
    integer,intent(in)    :: M,N
    integer               :: iflag,i
    real(wp)              :: x(N_param),fvec(M)
    

    !write(*,*) "-------------------------------"
    !write(*,*) "x =",x
    if (iflag==0) then
      write(*,*) "-------------------------------"
      !write(*,'(A30F8.3)') "x =",x(1)
      write(*,'(A4)',advance='no') " x ="
      do i=1,N_param
        write(*,'(F10.6)',advance='no') x(i)
      end do
      write(*,*) ""
      write(*,'(A25,F12.6)') "Sum of squared errors =",sum(fvec**2)
      write(*,*) "-------------------------------"
    !else
    !  infeas = infeasibility(x)
    !  if (infeas < 0.0_wp) then
    !    !write(*,*) "Feasible, infeas = ", infeas 
    !    call Curvediff(x,fvec,.true.)
    !    !fvec = diff
    !  else
    !    write(*,*) "UNFeasible, infeas = ", infeas
    !    fvec = infeas
    !  end if
    !end if

    else
      if (feasible(x)) then
        !write(*,*) "Feasible"
        call Curvediff(x,fvec,.true.)
        !fvec = diff
      else
        !write(*,*) "UNFeasible"
        fvec = largeval
      end if
    end if


    !write(*,*) "Sum of squares(fvec) =",sum(fvec**2)
    return
  !---------------------!
  end subroutine fcn
 
  !--------------------------------!
  Subroutine Read_Datafile(path,x,y,N)
  !--------------------------------!
    !First column -> x
    !Second column -> y
    !Number of elements -> N
    !
    !Ignores lines starting with <commentsymbol>
    !Also ignores lines with only whitespace
    !
    use SFL_Logical_Units,       only :  SFL_Get_Free_Unit 

    implicit none
    character(len=*)                  ::  path
    real(wp), allocatable             ::  x(:)
    real(wp), allocatable             ::  y(:)
    character, parameter               :: commentsymbol = "#"
    logical                           ::  exi
    integer                           ::  N,ifile,i
    character(len=*), parameter       ::  routine = "Read_datafile"
    character(len=200) :: line
    integer :: iostatvar

    Inquire(file=path, exist = exi)
    if ( .not. exi) then
      call Error_Failure(routine,("File not found: "//path))
    end if
    call SFL_Get_Free_Unit(ifile)
    open(unit=ifile,file=path,status='old',action='read')
    !Count number of lines without commentsymbol
    N=0
    do while (.true.)
      read(unit=ifile,fmt='(A)',iostat=iostatvar) line
      line = trim(adjustl(line))

      if ((iostatvar==0) .and. (line(1:1)/=commentsymbol) .and. &
                                (line/='')) then
        N = N+1
      else if (iostatvar>0) then
        call Error_failure(routine,("Input error from: "//path))
      else if (iostatvar<0) then
        !End of file
        exit
      end if
    end do
    ! Allocate according to count, and rewind file..
    allocate(x(N),y(N))
    rewind(unit=ifile)
    !Read and store data in x and y
    i=1
    do while (.true.)
      read(unit=ifile,fmt='(A)',iostat=iostatvar) line
      line = trim(adjustl(line))
      if ((iostatvar==0) .and. (line(1:1)/=commentsymbol) .and. &
                                (line/='')) then
        read(line,*) x(i),y(i)
        i=i+1
      else if (iostatvar>0) then
        call Error_failure(routine,("Input error from: "//path))
      else if (iostatvar<0) then
        !End of file
        exit
      end if
    end do
    close(unit=ifile)
    
    return
  end subroutine Read_Datafile
  !--------------------------------!

  !--------------------------------!
  Subroutine Interpolate_Experimental(E_exp, R_exp)
  !--------------------------------!
    ! Interpolate data E_exp,R_exp to match the data which will be 
    ! produced by the simulation (as given in the loaded parameters in param)
    ! Results are stored in E and R_exp_interp.
    ! E_exp and R_exp will be deallocated!
    !
    use SFL_Interpolation,          only  : SFL_AkimaSpline 
    implicit none

    integer                    :: N,ierr
    real(wp), allocatable      :: E_exp(:),R_exp(:)
    character(len=*), parameter       ::  routine = "Interpolate_Experimental"
    
    ! Reading energy vector from param into module-global array "E"
    E = param%numerics%energy
    ! Interpolate the data E_exp,R_exp based on E
    call SFL_AkimaSpline(E_exp,R_exp,ierr,E,R_exp_interp)
    if (ierr<0) then
      call Error_Failure(routine, "SFL_AkimaSpline reported an error") 
    end if
    ! E_exp and R_exp will no longer be needed..
    deallocate(E_exp,R_exp) 

    return
  end subroutine Interpolate_Experimental
  !--------------------------------!


  !--------------------------------!
  Subroutine Update_Parameters(x)
  !--------------------------------!
    ! Update simulation settings to use the given parameters
    !
    ! x = [radius, truncation_ratio, lattice constant,broadening_par, 
    !       broadening_perp]
    !
    ! Will also recalculate derived parameters, and run 
    ! checking routines.
    !
    
    use Post_Process_Parameters_Module, only : Post_Process_Parameters
    use Check_Parameters_Module,        only : Check_Input_Parameters, &
                                               Check_Derived_Parameters

    implicit none
    real(wp),intent(in)              :: x(N_param)
    
    ! Update base parameters:
    param%Geometry%Radius(1) = x(1)
    param%Geometry%Truncation_Ratio = x(2)
    param%Interaction%Lattice_Constant = x(3)

    if (.not. param%curvefitting%freeze_broadening) then
      param%Geometry%Broadening_Par = x(4)
      param%Geometry%Broadening_Perp = x(5)
    end if

    ! Do the things in Initialize_Granfilm which are 
    ! after Read_Input_Parameter_File:
    call Check_Input_Parameters()
    call Post_Process_Parameters()
    call Check_Derived_Parameters()

    return
  end subroutine Update_Parameters
  !--------------------------------!


  !--------------------------------!
  Subroutine Get_Simulated_Curve(recalc_integrals)
  !--------------------------------!
    ! Run simulation based on the current parameters, and returns 
    ! deltaR/R curve in module-module-global array "R_sim"
    !
    ! Assuming:
    !   Main parameters have been set.
    !   Derived parameters have been calculated.
    !   Space for integrals and results have been allocated.
    !
    use Integrals_Module,               only : Get_Integrals 
    use MultiPole_Coefficient_Module,   only : Get_Multipole_Coefficients
    use Polarizabilities_Module,        only : Get_Polarizabilities
    use Ref_Trans_Amplitudes_Module,    only : &
                                        Get_Reflection_Transmission_Amplitudes  
    use Susceptibilities_Module,        only : Get_Surface_Susceptabilities
    use Physical_Observables_Module,    only : Get_Physical_Observables 
    Use Broadening_Module,              only : Broaden_Alpha
    Use Epsilon_Corrections_Module,     only : Epsilon_Corrections

    implicit none
    logical, intent(in)       :: recalc_integrals                   
    
    call Epsilon_Corrections()
    if (recalc_integrals) then
      call Get_Integrals()
    end if
    call Get_Multipole_Coefficients()   
    call Get_Polarizabilities()
    call Broaden_Alpha()
    call Get_Surface_Susceptabilities() 
    call Get_Reflection_Transmission_Amplitudes()
    call Get_Physical_Observables() 
    
    R_sim = Results%Observables%Delta_R_over_R(:,1)

    return
  end subroutine Get_Simulated_Curve
  !--------------------------------!

  
  !--------------------------------!
  subroutine Curvediff(x,fvec,recalc_integrals)
  !--------------------------------!
  ! Finds the difference between the (interpolated) experimental curve 
  ! and the simulated curve, given the parameters "x",
  ! and stores it in fvec
  ! 
  ! x: Parameter vector
  ! fvec: Vector of experimental minus simulated.
  !
  ! recalc_integrals: Logical passed on to Get_Simulated_Curve(). 
  ! Specifies whether the integrals, which only depend on the 
  ! truncation ratio, should be calculated again.
  !
    implicit none
    real(wp), intent(in)              :: x(N_param)
    character(len=*), parameter       ::  routine = "Curvediff" 
    logical, intent(in)               :: recalc_integrals                   
    real(wp), intent(out)             :: fvec(:)
    
    call Update_Parameters(x)
    call Get_Simulated_Curve(recalc_integrals)
    

    fvec = R_exp_interp-R_sim

  end subroutine Curvediff
  !--------------------------------!


  !--------------------------------!
  !subroutine Bruteforce(x,resol)
  !--------------------------------!
  !Curve fitting by searching the parameter space brute-force.
  !Will only recalculate integrals when truncation ratio is changing.
  !
  ! x: Storage for final solution
  ! x_l, x_r: Lower and upper bounds for the parameters 
  ! A,b: Linear constraints Ax>b
  ! res: Number of points to test along each dimension
  !
  !implicit none
  !integer, intent(in)           :: resol
  !real(wp)                      :: x(N_param), dx(N_param),lowest,meansquare,&
  !                                 lowest_x(N_param)
  !character(len=*), parameter   :: routine = "Bruteforce"
  !logical                       :: recalc_integrals

  !! Hardcoded for three parameters (i,j,k)... (t,r,a) OBS: trunc_ratio first!
  !! x = (r,t,a)!
  !integer                       :: i,j,k
   
  !lowest = 1e100_wp

  !dx = (xr_constr-xl_constr)/(resol-1)
  !do i=0,(resol-1)
  !  x(2) = xl_constr(2) + i*dx(2)
  !  !!!
  !  recalc_integrals = .true.
  !  !!!
  !  do j=0,(resol-1)
  !    x(1) = xl_constr(1) + j*dx(1)
  !    do k=0,(resol-1)
  !      x(3) = xl_constr(3) + k*dx(3)
  !      write(*,*) x 
  !      if (feasible(x)) then
  !        call Curvediff(x,recalc_integrals)
  !        meansquare = dot_product(diff,diff)/size(diff)
  !        if (meansquare < lowest) then
  !          lowest = meansquare
  !          lowest_x = x
  !        end if
  !      end if
  !      !!!
  !      recalc_integrals = .false.
  !      !!!
  !    end do
  !  end do
  !end do
  !
  !write(*,*) "BRUTEFORCE DONE"
  !write(*,*) lowest_x
  !write(*,*) "value: ",lowest
  !end subroutine Bruteforce
  !--------------------------------!


  !--------------------------------!
  function feasible(x) result(res)
  !--------------------------------!
  ! Checks if a location x in parameter space is feasible, i.e. if it 
  ! satisfies the constraints given by the module-global variables:
  ! xl_constr, xr_constr : Box constraints, lower and upper.
  ! A_constr, b_constr: Linear constrains ( Ax > b )
  !
    implicit none

    logical               :: res
    real(wp), intent(in)  :: x(N_fit)
    integer               :: i
    
    !write(*,*) "-----"
    !write(*,*) size(x)
    !write(*,*) x

    res = .true.
    do i=1,N_fit 
      !Checking box constraints
      if ((x(i) < xl_constr(i)) .or. (x(i) > xr_constr(i))) then
        res = .false.
      end if
    end do 
    !Checking linear constraints
    if (dot_product(A_constr(1:N_fit),x) <= b_constr) then
      res = .false.
    end if
  !--------------------------------!
  end function feasible
 
  !--------------------------------!
  function infeasibility(x) result(res)
  !--------------------------------!
  !Returns -1.0 if feasible.
  !If infeasible, return a positive number which should be larger than 
  !fvec at any feasible point, and larger the further outside the constraints 
  !x is.
  implicit none
  real(wp), intent(in)  :: x(N_param)
  real(wp)              :: res, infeas_factor,dotprod
  integer               :: i
  logical               :: feasible
  

  feasible = .true.
  infeas_factor = 0.0_wp
  
  do i=1,N_param
    !Checking box constraints
    if (x(i) < xl_constr(i)) then
      feasible = .false.
      infeas_factor = infeas_factor + xl_constr(i)-x(i)
    else if (x(i) > xr_constr(i)) then 
      feasible = .false.
      infeas_factor = infeas_factor + x(i) - xr_constr(i)
    end if
  end do 
  !Checking linear constraints

  dotprod = dot_product(A_constr,x) 
  if (dotprod <= b_constr) then
    feasible = .false.
    infeas_factor = infeas_factor + b_constr - dotprod
  end if

  if (feasible) then 
    res = -1.0_wp
  else
    res = largeval*(infeas_factor+1.0_wp)/(infeas_factor+2.0_wp)
  end if
  !--------------------------------!
  end function infeasibility  

  !--------------------------------!
  subroutine Curvefit_Report(x,contact_angle, density,coverage,&
                              eq_thickness,info,nfev,fvec,M,cov_mat,N_fit)
  !--------------------------------!
  ! Writes info about the finished curve fitting.
  !
    implicit none
    integer, intent(in)       :: info,nfev,M,N_fit
    real(wp), intent(in)      :: x(N_param),contact_angle,density,&
                                 coverage,eq_thickness,fvec(:),cov_mat(:,:)
    integer                   :: i,j
    real(wp)                  :: sigma

    sigma = Param%Curvefitting%Sigma

    write(*,*) "" 
    write(*,*) "-------------------------------------"
    write(*,*) "---------CURVE FITTING INFO----------" 
    write(*,*) "-------------------------------------"
    write(*,'(A30)') "PRIMARY VARIABLES:" 
    write(*,'(A30F8.4)') "Radius = ",x(1) 
    write(*,'(A30F8.4)') "Truncation ratio = ",x(2)
    write(*,'(A30F8.4)') "Lattice constant = ",x(3)
    write(*,'(A30F8.4)') "Broadening_Par = ",x(4)
    write(*,'(A30F8.4)') "Broadening_Perp = ",x(5)
    write(*,*) ""
    write(*,'(A30)') "DERIVED PARAMETERS("//&
                      trim(param%interaction%lattice_type)//"):"
    write(*,'(A30F8.4)') "Contact angle = ",contact_angle 
    write(*,'(A30F8.4)') "Particle density = ",density 
    write(*,'(A30F8.4)') "Coverage = ",coverage
    write(*,'(A30F8.4)') "Equivalent thickness = ",eq_thickness
    write(*,*) ""

    write(*,'(A30)') "EXTRA INFO:" 
    write(*,'(A30F8.5)') "Sum of squared errors: ", sum(fvec**2)
    write(*,'(A30I8)') "Number of data points used: ", M
    write(*,'(A30I8)') "Number of variable parameters: ", N_fit
    write(*,'(A30I8)') "Number of simulations made: ",nfev 
    write(*,'(A26I1A)') "Why it stopped (info=",info,"):"
   
    ! Negative info comes from user termination from within fcn()
    if (info<0) then
      write(*,*) "User-terminated execution"
      return
    end if
    ! Explanations from lmdif documentation
    select case (info)
      case(0)  
        write(*,*) "---Improper input parameters."
      case(1)  
        write(*,*) "---Both actual and predicted relative reductions &
                    in the sum of squares are at most ftol."
      case(2)  
        write(*,*) "---Relative error between two consecutive iterates &
                    is at most xtol."
      case(3)  
        write(*,*) "---Both 1 and 2 hold:"
        write(*,*) "---Both actual and predicted relative reductions &
                    in the sum of squares are at most ftol."
        write(*,*) "---Relative error between two consecutive iterates &
                    is at most xtol."
      case(4)  
        write(*,*) "---The cosine of the angle between fvec and any &
                   column of the jacobian is at most gtol in &
                   absolute value."
      case(5)  
        write(*,*) "---Number of calls to fcn has reached or exceeded maxfev."
      case(6)  
        write(*,*) "---ftol is too small. No further reduction in &
                   the sum of squares is possible."
      case(7)  
        write(*,*) "---xtol is too small. No further improvement in &
                   the approximate solution x is possible."
      case(8)  
        write(*,*) "---gtol is too small. fvec is orthogonal to the &
                   columns of the jacobian to machine precision."
    end select
    write(*,*) "-------------------------------------"

    write(*,'(A30)') "COVARIANCE MATRIX:"
    write(*,'(A30F8.6)') "Based on sigma_{exp} = ",sigma
    write(*,*) ""
    do i=1,N_fit
      do j=1,N_fit
        write(*,'(F15.10)',advance='no') cov_mat(i,j)
      end do
      write(*,*) ""
    end do
    write(*,*)
    write(*,'(A18 F15.10)') 'sigma_R =',          sqrt(cov_mat(1,1))
    write(*,'(A18 F15.10)') 'sigma_t =',          sqrt(cov_mat(2,2))
    write(*,'(A18 F15.10)') 'sigma_L =',          sqrt(cov_mat(3,3))
    write(*,'(A18 F15.10)') 'sigma_broadpar =',   sqrt(cov_mat(4,4))
    write(*,'(A18 F15.10)') 'sigma_broadperp =',  sqrt(cov_mat(5,5))
    
    write(*,*) ""
    write(*,'(A28 F15.10)') 'Corr(R,t) =', cov_mat(1,2)/&
                                      sqrt(cov_mat(1,1)*cov_mat(2,2)) 
    write(*,'(A28 F15.10)') 'Corr(R,L) =', cov_mat(1,3)/&
                                      sqrt(cov_mat(1,1)*cov_mat(3,3)) 
    write(*,'(A28 F15.10)') 'Corr(t,L) =', cov_mat(2,3)/&
                                      sqrt(cov_mat(2,2)*cov_mat(3,3))
    write(*,'(A28 F15.10)') 'Corr(R,broadpar) =', cov_mat(1,4)/&
                                      sqrt(cov_mat(1,1)*cov_mat(4,4))
    write(*,'(A28 F15.10)') 'Corr(R,broadperip) =', cov_mat(1,5)/&
                                      sqrt(cov_mat(1,1)*cov_mat(5,5))
    write(*,'(A28 F15.10)') 'Corr(t,broadpar) =', cov_mat(2,4)/&
                                      sqrt(cov_mat(2,2)*cov_mat(4,4))
    write(*,'(A28 F15.10)') 'Corr(t,broadperp) =', cov_mat(2,5)/&
                                      sqrt(cov_mat(2,2)*cov_mat(5,5))
    write(*,'(A28 F15.10)') 'Corr(L,broadpar) =', cov_mat(3,4)/&
                                      sqrt(cov_mat(3,3)*cov_mat(4,4))
    write(*,'(A28 F15.10)') 'Corr(L,broadperp) =', cov_mat(3,5)/&
                                      sqrt(cov_mat(3,3)*cov_mat(5,5))
    write(*,'(A28 F15.10)') 'Corr(broadpar,broadperp) =', cov_mat(4,5)/&
                                      sqrt(cov_mat(4,4)*cov_mat(5,5))


    write(*,*) "-------------------------------------"
    return


  !--------------------------------!
  end subroutine Curvefit_Report
  


  !--------------------------------!
  subroutine Set_lmdif_parameters(M,N,fvec,ftol,xtol,gtol,maxfev,epsfcn,diag,&
                                  mode,factor,nprint,fjac,ldfjac,wa4)
  !--------------------------------!
  ! Sets the parameters for the lmdif routine, and allocates arrays 
  ! which have dimensions depending on M. 
  ! (Since M is not known at compile-time.)
  !
    implicit none
    integer, intent(in)           :: M,N
    integer                       :: maxfev,mode,nprint,ldfjac
    real(wp)                      :: ftol,xtol,gtol,epsfcn,diag(N),factor 
    real(wp), allocatable         :: fjac(:,:), wa4(:),fvec(:)

    ! Explanation for lmdif parameters 
    ! (not all of them need to be treated in this routine):
    !
    ! --- fcn: Subroutine as specified in doc.
    !
    ! --- M: Number of functions to minimize the sum of squares of.
    !Already set to number of energy points in the simulation.
    
    ! --- N: Number of variables
    !Already set to 3 (N_param)
    
    ! --- x: Real array of length N. Set to initial estimate, will 
    ! contain solution when lmdif finishes. 
    
    ! --- fvec: Real array of length M, containing the functions evaluated at x.
    allocate(fvec(M))

    ! --- ftol: Relative error desired in the sum of squares
    ftol = 1e-6_wp
    
    ! --- xtol: Relative error desired in the solution x.
    xtol = 1e-6_wp

    ! --- gtol: Orthogonality desired between the function vector and the 
    ! columns of the jacobian.
    gtol = 0.0_wp

    ! --- maxfev: Maximum number of calls to fcn.
    maxfev = 200*(N + 1)

    ! --- epsfcn: Used in determining a suitable step length for the 
    !forward-difference approximation. This approximation assumes that the 
    !relative errors in the functions are of the order of epsfcn.
    !if epsfcn is less than the machine precision, it is assumed that 
    !the relative errors in the functions are of the order of the machine
    !precision.
    epsfcn = 0.0_wp

    ! --- diag: Real array of length N. if mode = 1 diag is internally set. 
    !If mode = 2, diag must contain positive entries that serve as 
    !multiplicative scale factors for the variables. 
    diag = 1.0_wp

    ! --- mode: If mode = 1, the variables will be scaled internally. 
    ! If mode = 2, the scaling is specified by the input diag.
    ! OBS: If mode=2, ensure scale invariance in diag (see Minpack docs)
    ! Minpack docs recommends mode=1 for least squares fitting. 
    mode = 1 !Internal scaling

    ! --- factor: Used in determining the initial step bound. This bound is set 
    !to the product of factor and the euclidean norm of diag*x if nonzero, 
    !or else to factor itself. Smaller factor -> Smaller steps.
    factor = 100.0_wp

    ! --- nprint: Tell fcn to print every nprint iterations.
    if (verbose_curvefit) then 
      nprint = 1
    else
      nprint = 0
    end if
    ! --- info: Output variable (see doc). Negative if user terminated from fcn.

    ! --- nfev: Output variable, set to number of calls to fcn.

    ! --- fjac: Output M by N array. (see doc)
    allocate(fjac(M,N))

    ! --- ldfjac: Input variable not less than m which specifies the leading 
    !dimension of the array fjac.
    ldfjac = M

    ! --- ipvt: Output array of length N. (see doc)
    
    ! --- qtf: Output array of length N. (see doc)
    
    ! --- wa1,wa2,wa3: Work arrays of length N
    
    ! --- wa4: Work arrays of length M
    allocate(wa4(M))

  !--------------------------------!
  end subroutine Set_lmdif_parameters


  !--------------------------------!
  Subroutine Run_Simulation()
  !--------------------------------!
    ! Run simulation based on the current parameters, 
    ! and finalize like the main GranFilm program would.   
    ! Assuming:
    !   Main parameters have been set.
    !   Derived parameters have been calculated.
    !   Space for integrals and results have been allocated.
    use Finalize_Module,                only : Finalize_GranFilm
    use Integrals_Module,               only : Get_Integrals 
    use MultiPole_Coefficient_Module,   only : Get_Multipole_Coefficients
    use Physical_Observables_Module,    only : Get_Physical_Observables 
    use Polarizabilities_Module,        only : Get_Polarizabilities
    use Ref_Trans_Amplitudes_Module,    only : &
                                        Get_Reflection_Transmission_Amplitudes  
    use Susceptibilities_Module,        only : Get_Surface_Susceptabilities
    use Broadening_Module,              only : Broaden_alpha
    use Epsilon_Corrections_Module,     only : Epsilon_Corrections
    implicit none

    call Epsilon_Corrections()
    call Get_Integrals()
    call Get_Multipole_Coefficients()  
    call Get_Polarizabilities()
    call Broaden_alpha()
    call Get_Surface_Susceptabilities() 
    call Get_Reflection_Transmission_Amplitudes()
    call Get_Physical_Observables() 
    call Finalize_GranFilm()
  
    return
  !--------------------------------!
  end subroutine Run_Simulation

  !--------------------------------!
  subroutine Get_Initial_Guess(x)
  !--------------------------------!
  ! Gets the initial guess for the parameters array, and stores 
  ! them in x.
  !
  ! Get_Constraints() must have been called before this.
  !
  ! Guess is read from the values in the shared structure Param, whose 
  ! values initially come from the input file.
  !
  ! x = [radius, truncation ratio, lattice constant, Broadening_Par, 
  !         Broadening_Perp]
  !
    implicit none
    real(wp)          :: x(N_param) 
    character(len=*), parameter       ::  routine = "Get_Initial_Guess"

    ! Factor to multiply the initial broadening with if it is exactly at 
    ! or below Param%Curvefitting%broadening_zero. 
    ! This might happen when fitting to a series of data 
    ! where the broadening is practically zero. The variable then has to 
    ! be increased slightly because the fitting shouldn't start 
    ! at the plateau. If it does start there, the fitting will never allow 
    ! the broadening to increase again, even if it would make for a better 
    ! fit, since the gradient is artificially zero in the plateau.
    real(wp), parameter               :: broadening_increasefactor = 1.1

    x(1) = Param%Geometry%Radius(1)
    x(2) = Param%Geometry%Truncation_Ratio
    x(3) = Param%Interaction%Lattice_Constant 
    x(4) = Param%Geometry%Broadening_Par
    x(5) = Param%Geometry%Broadening_Perp
    
    if (.not. param%curvefitting%freeze_broadening) then
      if (x(4) <= Param%Curvefitting%broadening_zero) then 
        x(4) = Param%Curvefitting%broadening_zero*broadening_increasefactor
      end if
      if (x(5) <= Param%Curvefitting%broadening_zero) then 
        x(5) = Param%Curvefitting%broadening_zero*broadening_increasefactor
      end if
    end if

    if (.not. feasible(x)) then
      call Error_Failure(routine,"Initial guess is outside constraints!")
    end if
    
    ! Initializing parameters to initial guess
    param%Geometry%Radius(1) = x(1)
    param%Geometry%Truncation_Ratio = x(2)
    param%Interaction%Lattice_Constant = x(3)
    param%Geometry%Broadening_Par = x(4)
    param%Geometry%Broadening_Perp = x(5)


  !--------------------------------!
  end subroutine Get_Initial_Guess


  !--------------------------------!
  subroutine Get_Constraints()
  !--------------------------------!
  ! xl, xr: Lower and upper bounds for the parameters 
  ! A,b: Linear constraints Ax>b
  !
  ! OBS: xl and xr should later be input parameters, not hard coded here...
  

    implicit none
    real(wp), parameter       :: min_broadening = 0.0_wp

    xl_constr(1:3) = param%curvefitting%lower_constraint
    xr_constr(1:3) = param%curvefitting%upper_constraint

    ! Set constraints
    ! Radius
    !xl_constr(1) = 1.0_wp
    !xr_constr(1) = 15.0_wp
    ! Truncation ratio
    !xl_constr(2) = 0.01_wp
    !xr_constr(2) = 0.9_wp
    ! Lattice constant
    !xl_constr(3) = 2.0_wp
    !xr_constr(3) = 30.0_wp
    ! Broadening
    if (param%curvefitting%freeze_broadening) then 
      xl_constr(4) = 0.0_wp
      xl_constr(5) = 0.0_wp
    else
      xl_constr(4) = min_broadening
      xl_constr(5) = min_broadening
    end if
    xr_constr(4) = huge(1.0_wp)
    xr_constr(5) = huge(1.0_wp)
    ! Linear constraint (Ax > b)
    A_constr(1) = -2.0_wp
    A_constr(2) = 0.0_wp
    A_constr(3) = 1.0_wp
    A_constr(4) = 0.0_wp
    A_constr(5) = 0.0_wp
    b_constr = 0.0_wp

    !write(*,*) xl_constr
    !write(*,*) xr_constr
    !!call exit()
  !--------------------------------!
  end subroutine Get_Constraints

  
  !--------------------------------!
  subroutine Save_Parameters(x,parameter_outfile)
  !--------------------------------!
    use SFL_Logical_Units,       only :  SFL_Get_Free_Unit 

    implicit none
    real(wp), intent(in)          :: x(N_param)
    character(len=*), intent(in)  :: parameter_outfile
    integer                       :: i,ifile

    call SFL_Get_Free_Unit(ifile)
    open(unit=ifile,file=parameter_outfile,status='replace',action='write')    
    do i=1,N_param
      write(ifile,*) x(i)
    end do
    close(ifile)

  !--------------------------------!
  end subroutine 

  !--------------------------------!
  subroutine Save_Derived_Parameters(contact_angle,density,coverage,&
                                                        eq_thickness,filename)
  !--------------------------------!
  use SFL_Logical_Units,       only :  SFL_Get_Free_Unit 
  implicit none
  real(wp), intent(in)         :: contact_angle,density,coverage,eq_thickness
  character(len=*), intent(in) :: filename
  integer                      :: ifile

  call SFL_Get_Free_Unit(ifile)
  open(unit=ifile,file=filename,status='replace',action='write')    
  
  write(ifile,*) "contact_angle", contact_angle
  write(ifile,*) "density", density
  write(ifile,*) "coverage", coverage
  write(ifile,*) "eq_thickness", eq_thickness
  close(ifile)

  !--------------------------------!
  end subroutine Save_Derived_Parameters


  !--------------------------------!
  subroutine Save_Info(info,fvec,M,N,filename)
  !--------------------------------!
  use SFL_Logical_Units,       only :  SFL_Get_Free_Unit 
  implicit none

  integer, intent(in)       :: info, M, N
  real(wp), intent(in)      :: fvec(:)
  character(len=*), intent(in) :: filename
  integer                   :: ifile
  real(wp)                  :: sum_of_squares
  
  sum_of_squares = sum(fvec**2)



  call SFL_Get_Free_Unit(ifile)
  open(unit=ifile,file=filename,status='replace',action='write')    
  write(ifile,*) "info",info
  write(ifile,*) "M",M
  write(ifile,*) "N",N
  write(ifile,*) "sum_of_squares", sum_of_squares
  write(ifile,*) "sigma_exp", Param%Curvefitting%Sigma

  close(ifile)
  !--------------------------------!
  end subroutine Save_Info

  !--------------------------------!
  subroutine Save_Covariance(N_fit,cov_mat,filename)
  !--------------------------------!
  use SFL_Logical_Units,       only :  SFL_Get_Free_Unit 
  implicit none
  integer, intent(in)           :: N_fit
  real(wp), intent(in)          :: cov_mat(N_fit,N_fit)
  character(len=*), intent(in)  :: filename
  integer                       :: ifile, i,j

  call SFL_Get_Free_Unit(ifile)
  open(unit=ifile,file=filename,status='replace',action='write')    
  do i=1,N_fit
    do j=1,N_fit
      write(ifile,'(ES30.10)',advance='no') cov_mat(i,j)
    end do
    write(ifile,*) ""
  end do

  close(ifile)



  !--------------------------------!
  end subroutine Save_Covariance

  !--------------------------------!
  subroutine Get_Covariance_Matrix(N,M,jacobian,cov_mat)
  !--------------------------------!
  implicit none
  integer, intent(in)     :: N,M
  real(wp), intent(in)    :: jacobian(M,N)
  real(wp)                :: cov_mat(N,N), alpha(N,N), AP(N*(N+1)/2),det,sigma
  integer                 :: i,j,k,info,job


  sigma = Param%Curvefitting%Sigma

  ! Getting curvature matrix alpha
  alpha = 0.0_wp 
  do i=1,N
    do j=1,N
      do k = 1,M
        alpha(i,j) = alpha(i,j) + jacobian(k,i)*jacobian(k,j)
      end do
    end do
  end do
  alpha = alpha/(sigma**2)
  
  ! Building packed form of alpha (AP)
  k = 0
  do j=1,N
    do i=1,j
      k = k+1
      AP(k) = alpha(i,j)
    end do
  end do


  ! Inverting alpha to get covariance matrix
  call dppfa(AP,N,info)
  job = 1 !Determinant not needed
  call dppdi(AP,N,det,job)
  ! AP now has upper triangle of cov_mat. AP is in packed form.
  ! Building covariance matrix from AP
  k = 0
  do j=1,N
    do i=1,j
      k = k+1
      cov_mat(i,j) = AP(k)
      cov_mat(j,i) = AP(k)
    end do
  end do 

  !--------------------------------!
  end subroutine Get_Covariance_Matrix

  !--------------------------------!
  subroutine Get_Derived_Parameters(R,t,L,&
                                  contact_angle,density,coverage,eq_thickness)
  !--------------------------------!
  Use Supported_Options_Module,         only : LATTICE_TYPE_OPTION_TABEL

  real(wp), intent(in)         :: R,t,L
  real(wp), intent(out)        :: contact_angle,density,coverage,eq_thickness
  character(len=*), parameter       ::  routine = "Get_Derived_Parameters"

  
  contact_angle = acos(-t)*180.0_wp/pi

  select case(Param%Interaction%Lattice_Type)
  case(LATTICE_TYPE_OPTION_TABEL(1))
    !Square
    density = L**(-2)

  case(LATTICE_TYPE_OPTION_TABEL(2))
    !Hexagonal
    density = 2.0_wp/(sqrt(3.0_wp)*L**2)
  
  case default
    call Error_Failure(routine,&
                  ("(Curvefit module) Lattice type not supported!"))
  end select

  coverage = pi * R**2 * density
  eq_thickness = density * pi * R**3 * (2.0_wp/3.0_wp + t - t**3/3.0_wp)

  end subroutine Get_Derived_Parameters  
  !--------------------------------!


End Module Curvefit_Module
!-------------------------!

