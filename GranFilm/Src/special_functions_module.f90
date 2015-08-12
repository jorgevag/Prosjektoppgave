

!------------------------------!
Module Special_Functions_Module
!------------------------------!

  Use Shared, only : wp

  Implicit None


Contains



  !------------------------------------------------------------------------------------------  
  Function AssLegPoly(l,m,x)   Result(Plm)
    ! ----------------------------------------------------------
    ! ---  Routines for the calculation of the Assosiate 
    ! ---  Legendre polynomials with vector input arguments
    ! ----------------------------------------------------------
    ! Double precision version
    Implicit None
    ! Global
    Integer,                     Intent(In)   :: l
    Integer,                     Intent(In)   :: m
    Real(wp), Dimension(:),      Intent(In)   :: x
    Real(wp), Dimension(Size(x))              :: Plm
    ! Local
    Integer    :: i

    Do i=1,Size(x)
       Plm(i) =  AssLegPoly1(l,m,x(i))    
    Enddo

  End Function AssLegPoly

  !------------------------------------------------------------------------------------------


  Function AssLegPoly1(l,m,x)  Result(Plm)
    ! ----------------------------------------------------------
    ! ---  Routines for the calculation of the assosiated 
    ! ---  Legendre polynomials with real input parameter
    ! ----------------------------------------------------------
    Implicit None
    ! Global
    Integer,      Intent(In)    :: l
    Integer,      Intent(In)    :: m
    Real(wp),     Intent(In)    :: x
    Real(wp)                    :: Plm
    ! Local
    Integer                     :: ll
    Real(wp)                    :: pll,pmm,pmmp1,somx2

    If(.not.(m >= 0.and.m <= l.and. Abs(x) <= 1.0_wp)) Then
       Write(unit=6,fmt='(a)') 'Error in AssLegPoly1 arguments'
       Write(unit=6,fmt='(a,i2,a,e25.15)') 'm = ',m,'  x-1 = ',Abs(x)-1.0_wp
       !Pause
       Stop
    Endif
    pmm=1.0_wp
    If(m > 0) Then
       somx2 = Sqrt((1.0_wp-x)*(1.0_wp+x))
       pmm   = product(arth(1.0_wp,2.0_wp,m))*somx2**m
       If(mod(m,2) == 1) pmm=-pmm
    Endif
    If(l == m) Then
       Plm=pmm
    Else
       pmmp1=x*(2*m+1)*pmm
       If(l == m+1) Then
          Plm=pmmp1
       Else
          Do ll=m+2,l
             pll=(x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
             pmm=pmmp1
             pmmp1=pll
          end do
          Plm=pll
       Endif
    Endif

  End Function AssLegPoly1

  !-------------------------------------------------------------------

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

  !----------------------------------------------------------------------

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

End Module Special_Functions_module

!----------------------------------!


