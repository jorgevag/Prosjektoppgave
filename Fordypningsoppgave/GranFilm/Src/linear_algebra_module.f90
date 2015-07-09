
! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!       This module handles the linear algebra issues needed
!       for GranFilm. 
!       This is done by wrappers to LAPACK.
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!
  

!---------------------------!
Module Linear_Algebra_Module
!---------------------------!


  ! --- The Use Statements global to the module
  Use Shared, only : wp
  Use SFL_Precision, only : sp, dp


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Solve_Linear_System
  Public :: Invert_Matrix

  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private
  




!-------!
Contains
!-------!




  !-------------------------------------------------------------------------


  !------------------------------------------------------!
  Subroutine Solve_Linear_System( A, b, condition_number )
  !------------------------------------------------------!
    !
    ! --- Solves the matrix system Ax=b by the LU decomposition method.
    !     The obtained solution is improved if the condition number is large
    !
    !     
    !Use SFL_Precision, only : sp, dp
    Implicit None
    complex(wp ), Intent(In)            ::  A(:,:)
    complex(wp ), Intent(InOut)         ::  b(:)
    Real(wp),     optional              ::  condition_number
    ! --- Local
    complex(wp ), Allocatable           ::  AA(:,:)
    complex(wp ), Allocatable           ::  bb(:)
    Integer                             ::  m,n
    Integer,     Allocatable            ::  pivot(:)
    Integer                             ::  info(4), istat
    Real(wp)                            ::  rcond
    complex(wp ), Allocatable           ::  work(:)
    Real(wp), Allocatable               ::  rwork(:)
    Real(wp)                            ::  ferr(1),berr(1)
    Real(wp)                            ::  error

    rcond = 0._wp

    ! --- Some initial abbreviations
    m      =  Size(A,1)
    n      =  Size(A,2)

    ! --- Allocation of arrays
    Allocate( AA(m,n), bb(n), pivot(n), work(2*n), rwork(2*n) ) 

    ! Working matrix and coulumns    
    AA=A
    bb=b

    select case(wp)
    case(sp)
       ! LU-factorization
       Call cgetrf(m,n,AA,m,pivot,info(1))
       ! LU back susbtitution
       Call cgetrs('n',n,1,AA,m,pivot,bb,n,info(2))
       ! Evaluation of the condition number
       Call cgecon('1',n,AA,m,1._wp,rcond,work,rwork,info(3))
       ! Improvement of the calculated solution
       Call cgerfs('n',n,1,A,m,AA,m,pivot,b,n,bb,n,ferr,berr,work,rwork,info(4))

    case(dp)
       ! LU-factorization          
       Call zgetrf(m,n,AA,m,pivot,info(1))
       ! LU back susbtitution
       Call zgetrs('n',n,1,AA,m,pivot,bb,n,info(2))
       ! Evaluation of the condition number
       Call zgecon('1',n,AA,m,1._wp,rcond,work,rwork,info(3))
       ! Improvement of the calculated solution
       Call zgerfs('n',n,1,A,m,AA,m,pivot,b,n,bb,n,ferr,berr,work,rwork,info(4))
    End select




    ! --- Error codes in the Lapack routines
    If(info(1)/=0) Then
       Write(unit=6,fmt='(a,i2)') 'Error code in LU decomposition : ',info(1)
    Endif
    If(info(2)/=0) Then
       Write(unit=6,fmt='(a,i2)') 'Error code in LU backsubstitution : ',info(2)
    Endif
    If(info(3)/=0) Then
       Write(unit=6,fmt='(a,i2)') 'Error code in condition number evaluation : ',info(3)
    Endif
    If(info(4)/=0) Then
       Write(unit=6,fmt='(a,i2)') 'Error code in iterative improvment of solution : ',info(4)
    Endif

    ! --- An estimation of the error
    error = Sqrt(Sum(Abs(Matmul(A(:,:),bb(:)) - b(:))**2)) 
    error = error/Sqrt(Sum(Abs(b(:))**2))

    !   Open(unit=15,file='error_linsys.dat',status='unknown')
    ! 1/rcond is the condition number of the matrix (ratio of max and min singular values)
    ! ferr(1) is a upper bound of the relative error in the solution of the system
    !   Write(unit=15,fmt='(a,i2)') 1._wp/rcond,ferr(1),error


    ! Output of the solution
    b = bb

    ! --- Deallocate storage
    Deallocate( AA, pivot, work, rwork, stat=ISTAT) 
    Deallocate( bb, stat = ISTAT)


  End Subroutine Solve_Linear_System
  !---------------------------------!







  !--------------------------!
  Subroutine Invert_Matrix(A)
  !--------------------------!
    !Use SFL_Precision, only : sp, dp	   
    Implicit None
    complex(wp), Intent(InOut)     ::  A(:,:)
    ! --- Local
    Integer                        ::  n, istat
    Integer,Allocatable            ::  pivot(:)
    Integer                        ::  info(2)
    complex(wp),Allocatable        ::  work(:)

    ! --- Some initial abbreviations
    n  =  Size(A,1)

    ! --- Allocation of arrays
    Allocate(pivot(n),work(n)) 

    Select case(wp)
    case(sp)
       ! LU-factorization
       Call cgetrf(n,n,A,n,pivot,info(1))
       ! Inversion using the LU-factorization
       Call cgetri(n,A,n,pivot,work,n,info(2))

    case(dp)
       ! LU-factorization
       Call zgetrf(n,n,A,n,pivot,info(1))
       ! Inversion using the LU-factorization
       Call zgetri(n,A,n,pivot,work,n,info(2))                     
    End Select

    ! --- Error codes in the Lapack routines
    If(info(1)/=0) Then
       Write(unit=6,fmt='(a,i2)') 'Error code in LU factorization : ',info(1)
    Endif
    If(info(2)/=0) Then
       Write(unit=6,fmt='(a,i2)') 'Error code in LU inversion : ',info(2)
    Endif


    ! --- Deallocate storage
    Deallocate(pivot,work,stat=istat)

  End Subroutine Invert_Matrix
  !--------------------------!







End Module Linear_Algebra_Module
!-------------------------------!
