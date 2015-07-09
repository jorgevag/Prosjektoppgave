! ----------------------------------------------------------
! $Id:$
! ----------------------------------------------------------

!
! ----------------------------------------------------------
! 
! --- PURPOSE
!
!     Contains the routine for calculating the Associated 
!     Legendre Polynomilas and realted functions
!
!     SEE
!
!     http://en.wikipedia.org/wiki/Associated_Legendre_polynomials
!
! 
! --- AUTHOR : Ingve Simosnen, Paris, Jul 2010.
!
! ----------------------------------------------------------
!

Module Legendre_Module



  ! --- The Use Statements global to the module
  Use Shared, only : wp	   
  !Use SFL_Precision, only : qp 

  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Legendre_Plm
  Public :: Legendre_Plm_vec
  Public :: Legendre_Plm_Diff     


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private


  
contains
  


  !----------------------------------------!
  Function Legendre_Plm(l,m,x)  Result(Plm)
  !----------------------------------------!    
    ! ----------------------------------------------------------
    ! ---  Routines for the calculation of the assosiated 
    ! ---  Legendre polynomials with real input parameter
    ! ----------------------------------------------------------
    Use Error_Module, only : Error_Failure
    Implicit None
    ! Global
    Integer,      Intent(In)    :: l
    Integer,      Intent(In)    :: m
    Real(wp),     Intent(In)    :: x
    Real(wp)                    :: Plm
    ! Local
    character(len=*),  parameter :: routine = "Legendre_Plm"
    Integer                     :: ll
    Real(wp)                    :: pll,pmm,pmmp1,somx2
    Character(len=100)          :: str

    ! --- Error Checking
    If(.not.(m >= 0.and.m <= l .and. Abs(x) <= 1.0_wp)) Then
       !Write(unit=6,fmt='(a)') 'Error in AssLegPoly1 arguments'
       !Write(unit=6,fmt='(a,i2,a,e25.15)') 'm = ',m,'  x-1 = ',Abs(x)-1.0_wp
       !Stop
       Write(str,fmt='(a,i2,a,i2,a,e25.15)') 'm = ',m,';   l= ',l, ';   x= ',x
       call Error_Failure( routine, "Argument error : "//trim(adjustl(str)) )
    Endif
    !
    ! --- The main calculation
    pmm = 1.0_wp
    If(m > 0) Then
       somx2 = Sqrt((1.0_wp-x)*(1.0_wp+x))
       pmm   = product(arth(1.0_wp,2.0_wp,m))*somx2**m
       !If(mod(m,2) == 1) pmm = -pmm
    Endif
    If(l == m) Then
       Plm = pmm
    Else
       pmmp1 = x*(2*m+1)*pmm
       If(l == m+1) Then
          Plm = pmmp1
       Else
          Do ll = m+2,l
             pll   = (x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
             pmm   = pmmp1
             pmmp1 = pll
          end do
          Plm = pll
       Endif
    Endif

  contains

    Function arth(first,increment,n)
      Implicit None
      Real(wp)                 ::      first,increment
      Integer                  ::      n,i
      Real(wp),Dimension(n)    ::      arth
      arth(1) = first
      Do i=2,n
         arth(i) = arth(i-1)+increment
      Enddo
    End Function arth
    
  End Function Legendre_Plm
  !------------------------!




  
  !--------------------------------------------!  
  Function Legendre_Plm_vec(l,m,x)   Result(Plm)
  !--------------------------------------------!  
    ! ----------------------------------------------------------
    ! ---  Routines for the calculation of the Assosiate 
    ! ---  Legendre polynomials with vector input arguments
    ! ----------------------------------------------------------
    Implicit None
    ! Global
    Integer,                     Intent(In)   :: l
    Integer,                     Intent(In)   :: m
    Real(wp), Dimension(:),      Intent(In)   :: x
    Real(wp), Dimension(Size(x))              :: Plm
    ! Local
    Integer    :: i

    Do i=1,Size(x)
       Plm(i) =  Legendre_Plm(l,m,x(i))    
    Enddo

  End Function Legendre_Plm_vec
  !-----------------------------!





  !---------------------------------------------!
  Function Legendre_Plm_Diff(l,m,x) Result(dalp)
  !---------------------------------------------!
    !
    ! Calculates the first derivative of the Associate Legendre polynomials
    !
    Implicit None
    Integer,   Intent(In)   :: l, m
    Real(wp),  Intent(in)   :: x
    Real(wp)                :: dalp

    ! Recursive relation
    dalp  = (l-m+1)*Legendre_Plm(l+1,m,x)-(l+1)*x*Legendre_Plm(l,m,x)
    dalp  = dalp/(x**2-1)

  End Function  Legendre_Plm_Diff
  !------------------------------!



End Module Legendre_Module
!-------------------------!
