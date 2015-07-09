
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




!---------------!
Program GranFilm
!---------------!

  Use Shared
  Use Error_Module
  Use Initialize_Module,                only : Initialize_GranFilm
  Use Finalize_Module,                  only : Finalize_GranFilm
  Use Integrals_Module,                 only : Allocate_Integrals, Get_Integrals 
  Use MultiPole_Coefficient_Module,     only : Get_Multipole_Coefficients
  Use Physical_Observables_Module,      only : Allocate_Results, Get_Physical_Observables 
  Use Polarizabilities_Module,          only : Get_Polarizabilities
  Use Ref_Trans_Amplitudes_Module,      only : Get_Reflection_Transmission_Amplitudes  
  Use Susceptibilities_Module,          only : Get_Surface_Susceptabilities
  Use Fit_Experimental_Data_Module,     only : Fit_Experimental_Data
  Use Potential_module,                 only : Get_Potential 
  ! ------------------------------------------------------------------------------------------
  Use Eigenmodes_Module,                only : Calculate_Determinants, Calculate_Eigenvalues
  ! ------------------------------------------------------------------------------------------
  Implicit None


  ! --- Integral type
  !       1st index  : No. of interfaces
  !       2nd index  : truncation ratio index 1:2     
  !                    2nd colum corresponds to tr=1
  !Type(GranFilm_Integral_Type), dimension(:,:), allocatable :: Integrals   



  ! --- Initialize (including reading input parameters)
  !     and defining an calculating parameters in the
  !     PARAM derived type (that is shared)....
  call Initialize_GranFilm()


  ! --- Allocate storage for the Results and needed Integrals 
  call Allocate_Integrals( )
  call Allocate_Results()
  

  ! --- Do curve fitting if "-f <filename>" was among the command line parameters.
  call Fit_Experimental_Data()


  ! --- Calculate the integrals
  call Get_Integrals(  )

  
  ! --- Solve for the MP coefficients of a single island .....
  !     and calculates the single island polarizabilities 
  !     that are stured in Results%Polarizabilities
  !call Get_Multipole_Coefficients( Integrals, Results)  
  call Get_Multipole_Coefficients( )  
  

  ! --- Calculate the potential if needed
  call Get_Potential( )
  
  ! === TESTING ==============================================================
  ! Added by Sindre Stavseng, May 2013
  !
  ! --- Calculate the determinants of the matrix A for all energies if needed
  call Calculate_Determinants( )
  ! --- Calculate the eigenvalues and eigenvectors of the matrix A
  call Calculate_Eigenvalues()
  !
  ! ==========================================================================
  
  ! --- Calculate the polarizabilities for a single particle from the calculated multipoles
  call Get_Polarizabilities()
  

  ! --- Calculate teh surface susceptibilities for the film
  !     This includes taking into acount island-island interaction
  call Get_Surface_Susceptabilities() 
  

  ! --- Calculate the Reflection Transmission Amplitudes
  call Get_Reflection_Transmission_Amplitudes( )
  

  ! --- Calculate the Physics Observables....
  if (.not. Param%Inout%Gui_Mode) &
       call Get_Physical_Observables( ) 


  ! --- Finalize (including writing results to file...)
  call Finalize_GranFilm()
  


End Program GranFilm
!-------------------!
