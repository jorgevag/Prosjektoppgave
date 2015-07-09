! -----------------------------------------------------------------------
! $Id:$
! -----------------------------------------------------------------------
!
! This module is an adapted version of the Numerical Recipes module 
! NRUTIL. The needed content of NRTYPE has been included. 
! 
! Ingve Simonsen, Paris, Apr, 2007
!
! -----------------------------------------------------------------------


!
! MODIFICATION
!
!   * Added a DP version of cumsum  (Aug 3, 2007)
!
!




Module SFL_NR_Util


  ! --------------------------------------
  ! --- The Needed use statments.....
  ! --------------------------------------
  Use SFL_Precision
  Implicit None



  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  !
  !  .... One should maybe control the publicly available routines here......
  !
  !Public :: SFL_TimeStamp      ! Write a timestanmp on a output unit



  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  !Private



  ! --------------------------------------
  ! --- Parameters .....
  ! --------------------------------------
  Integer(I4B), Parameter :: NPAR_ARTH=16,NPAR2_ARTH=8
  Integer(I4B), Parameter :: NPAR_GEOP=4,NPAR2_GEOP=2
  Integer(I4B), Parameter :: NPAR_CUMSUM=16
  Integer(I4B), Parameter :: NPAR_CUMPROD=8
  Integer(I4B), Parameter :: NPAR_POLY=8
  Integer(I4B), Parameter :: NPAR_POLYTERM=8



  ! --------------------------------------
  ! --- Type definitions
  ! --------------------------------------
  Type sprs2_sp
     Integer(I4B) :: n,len
     Real(SP),     Dimension(:), Pointer :: val   ! For reasons of efficiancy these might be  
     Integer(I4B), Dimension(:), Pointer :: irow  !   allocatable arrays if the compiler 
     Integer(I4B), Dimension(:), Pointer :: jcol  !   supports it
  End Type sprs2_sp
  Type sprs2_dp
     Integer(I4B) :: n,len
     Real(DP),     Dimension(:), Pointer :: val
     Integer(I4B), Dimension(:), Pointer :: irow
     Integer(I4B), Dimension(:), Pointer :: jcol
  End Type sprs2_dp



  ! --------------------------------------
  ! --- Interfaces
  ! --------------------------------------
  Interface array_copy
     Module Procedure array_copy_r, array_copy_d, array_copy_i
  End Interface
  Interface swap
     Module Procedure swap_i,swap_r,swap_rv,swap_c, &
          swap_cv,swap_cm,swap_z,swap_zv,swap_zm, &
          masked_swap_rs,masked_swap_rv,masked_swap_rm
  End Interface
  Interface reallocate
     Module Procedure reallocate_rv,reallocate_rm,&
          reallocate_iv,reallocate_im,reallocate_hv
  End Interface
  Interface imaxloc
     Module Procedure imaxloc_r, imaxloc_d, imaxloc_i
  End Interface
  Interface iminloc
     Module Procedure iminloc_r, iminloc_d, iminloc_i
  End Interface
  Interface assert
     Module Procedure assert1,assert2,assert3,assert4,assert_v
  End Interface
  Interface assert_eq
     Module Procedure assert_eq2,assert_eq3,assert_eq4,assert_eqn
  End Interface
  Interface arth
     Module Procedure arth_r, arth_d, arth_i
  End Interface
  Interface geop
     Module Procedure geop_r, geop_d, geop_i, geop_c, geop_dv
  End Interface
  Interface cumsum
     Module Procedure cumsum_r,cumsum_d,cumsum_i
  End Interface
  Interface poly
     Module Procedure poly_rr,poly_rrv,poly_dd,poly_ddv,&
          poly_rc,poly_cc,poly_msk_rrv,poly_msk_ddv
  End Interface
  Interface poly_term
     Module Procedure poly_term_rr,poly_term_cc
  End Interface
  Interface outerprod
     Module Procedure outerprod_r,outerprod_d
  End Interface
  Interface outerdiff
     Module Procedure outerdiff_r,outerdiff_d,outerdiff_i
  End Interface
  Interface scatter_add
     Module Procedure scatter_add_r,scatter_add_d
  End Interface
  Interface scatter_max
     Module Procedure scatter_max_r,scatter_max_d
  End Interface
  Interface diagadd
     Module Procedure diagadd_rv,diagadd_r
  End Interface
  Interface diagmult
     Module Procedure diagmult_rv,diagmult_r
  End Interface
  Interface get_diag
     Module Procedure get_diag_rv, get_diag_dv
  End Interface
  Interface put_diag
     Module Procedure put_diag_rv, put_diag_r
  End Interface
  
  
  
Contains

  !BL
  Subroutine array_copy_r(src,dest,n_copied,n_not_copied)
    Real(SP), Dimension(:), Intent(IN) :: src
    Real(SP), Dimension(:), Intent(OUT) :: dest
    Integer(I4B), Intent(OUT) :: n_copied, n_not_copied
    n_copied=Min(Size(src),Size(dest))
    n_not_copied=Size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
  End Subroutine array_copy_r
  !BL
  Subroutine array_copy_d(src,dest,n_copied,n_not_copied)
    Real(DP), Dimension(:), Intent(IN) :: src
    Real(DP), Dimension(:), Intent(OUT) :: dest
    Integer(I4B), Intent(OUT) :: n_copied, n_not_copied
    n_copied=Min(Size(src),Size(dest))
    n_not_copied=Size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
  End Subroutine array_copy_d
  !BL
  Subroutine array_copy_i(src,dest,n_copied,n_not_copied)
    Integer(I4B), Dimension(:), Intent(IN) :: src
    Integer(I4B), Dimension(:), Intent(OUT) :: dest
    Integer(I4B), Intent(OUT) :: n_copied, n_not_copied
    n_copied=Min(Size(src),Size(dest))
    n_not_copied=Size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
  End Subroutine array_copy_i
  !BL
  Subroutine swap_i(a,b)
    Integer(I4B), Intent(INOUT) :: a,b
    Integer(I4B) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_i
  !BL
  Subroutine swap_r(a,b)
    Real(SP), Intent(INOUT) :: a,b
    Real(SP) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_r
  !BL
  Subroutine swap_rv(a,b)
    Real(SP), Dimension(:), Intent(INOUT) :: a,b
    Real(SP), Dimension(Size(a)) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_rv
  !BL
  Subroutine swap_c(a,b)
    Complex(SPC), Intent(INOUT) :: a,b
    Complex(SPC) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_c
  !BL
  Subroutine swap_cv(a,b)
    Complex(SPC), Dimension(:), Intent(INOUT) :: a,b
    Complex(SPC), Dimension(Size(a)) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_cv
  !BL
  Subroutine swap_cm(a,b)
    Complex(SPC), Dimension(:,:), Intent(INOUT) :: a,b
    Complex(SPC), Dimension(Size(a,1),Size(a,2)) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_cm
  !BL
  Subroutine swap_z(a,b)
    Complex(DPC), Intent(INOUT) :: a,b
    Complex(DPC) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_z
  !BL
  Subroutine swap_zv(a,b)
    Complex(DPC), Dimension(:), Intent(INOUT) :: a,b
    Complex(DPC), Dimension(Size(a)) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_zv
  !BL
  Subroutine swap_zm(a,b)
    Complex(DPC), Dimension(:,:), Intent(INOUT) :: a,b
    Complex(DPC), Dimension(Size(a,1),Size(a,2)) :: dum
    dum=a
    a=b
    b=dum
  End Subroutine swap_zm
  !BL
  Subroutine masked_swap_rs(a,b,mask)
    Real(SP), Intent(INOUT) :: a,b
    Logical(LGT), Intent(IN) :: mask
    Real(SP) :: swp
    If (mask) Then
       swp=a
       a=b
       b=swp
    End If
  End Subroutine masked_swap_rs
  !BL
  Subroutine masked_swap_rv(a,b,mask)
    Real(SP), Dimension(:), Intent(INOUT) :: a,b
    Logical(LGT), Dimension(:), Intent(IN) :: mask
    Real(SP), Dimension(Size(a)) :: swp
    Where (mask)
       swp=a
       a=b
       b=swp
    End Where
  End Subroutine masked_swap_rv
  !BL
  Subroutine masked_swap_rm(a,b,mask)
    Real(SP), Dimension(:,:), Intent(INOUT) :: a,b
    Logical(LGT), Dimension(:,:), Intent(IN) :: mask
    Real(SP), Dimension(Size(a,1),Size(a,2)) :: swp
    Where (mask)
       swp=a
       a=b
       b=swp
    End Where
  End Subroutine masked_swap_rm
  !BL
  !BL
  Function reallocate_rv(p,n)
    Real(SP), Dimension(:), Pointer :: p, reallocate_rv
    Integer(I4B), Intent(IN) :: n
    Integer(I4B) :: nold,ierr
    Allocate(reallocate_rv(n),stat=ierr)
    If (ierr /= 0) Call &
         nrerror('reallocate_rv: problem in attempt to allocate memory')
    If (.Not. Associated(p)) Return
    nold=Size(p)
    reallocate_rv(1:Min(nold,n))=p(1:Min(nold,n))
    Deallocate(p)
  End Function reallocate_rv
  !BL
  Function reallocate_iv(p,n)
    Integer(I4B), Dimension(:), Pointer :: p, reallocate_iv
    Integer(I4B), Intent(IN) :: n
    Integer(I4B) :: nold,ierr
    Allocate(reallocate_iv(n),stat=ierr)
    If (ierr /= 0) Call &
         nrerror('reallocate_iv: problem in attempt to allocate memory')
    If (.Not. Associated(p)) Return
    nold=Size(p)
    reallocate_iv(1:Min(nold,n))=p(1:Min(nold,n))
    Deallocate(p)
  End Function reallocate_iv
  !BL
  Function reallocate_hv(p,n)
    Character(1), Dimension(:), Pointer :: p, reallocate_hv
    Integer(I4B), Intent(IN) :: n
    Integer(I4B) :: nold,ierr
    Allocate(reallocate_hv(n),stat=ierr)
    If (ierr /= 0) Call &
         nrerror('reallocate_hv: problem in attempt to allocate memory')
    If (.Not. Associated(p)) Return
    nold=Size(p)
    reallocate_hv(1:Min(nold,n))=p(1:Min(nold,n))
    Deallocate(p)
  End Function reallocate_hv
  !BL
  Function reallocate_rm(p,n,m)
    Real(SP), Dimension(:,:), Pointer :: p, reallocate_rm
    Integer(I4B), Intent(IN) :: n,m
    Integer(I4B) :: nold,mold,ierr
    Allocate(reallocate_rm(n,m),stat=ierr)
    If (ierr /= 0) Call &
         nrerror('reallocate_rm: problem in attempt to allocate memory')
    If (.Not. Associated(p)) Return
    nold=Size(p,1)
    mold=Size(p,2)
    reallocate_rm(1:Min(nold,n),1:Min(mold,m))=&
         p(1:Min(nold,n),1:Min(mold,m))
    Deallocate(p)
  End Function reallocate_rm
  !BL
  Function reallocate_im(p,n,m)
    Integer(I4B), Dimension(:,:), Pointer :: p, reallocate_im
    Integer(I4B), Intent(IN) :: n,m
    Integer(I4B) :: nold,mold,ierr
    Allocate(reallocate_im(n,m),stat=ierr)
    If (ierr /= 0) Call &
         nrerror('reallocate_im: problem in attempt to allocate memory')
    If (.Not. Associated(p)) Return
    nold=Size(p,1)
    mold=Size(p,2)
    reallocate_im(1:Min(nold,n),1:Min(mold,m))=&
         p(1:Min(nold,n),1:Min(mold,m))
    Deallocate(p)
  End Function reallocate_im
  !BL
  Function ifirstloc(mask)
    Logical(LGT), Dimension(:), Intent(IN) :: mask
    Integer(I4B) :: ifirstloc
    Integer(I4B), Dimension(1) :: loc
    loc=Maxloc(Merge(1,0,mask))
    ifirstloc=loc(1)
    If (.Not. mask(ifirstloc)) ifirstloc=Size(mask)+1
  End Function ifirstloc
  !BL
  Function imaxloc_r(arr)
    Real(SP), Dimension(:), Intent(IN) :: arr
    Integer(I4B) :: imaxloc_r
    Integer(I4B), Dimension(1) :: imax
    imax=Maxloc(arr(:))
    imaxloc_r=imax(1)
  End Function imaxloc_r
  ! Added by IS Mar 2008; Double precission version
  Function imaxloc_d(arr)
    Real(DP), Dimension(:), Intent(IN) :: arr
    Integer(I4B) :: imaxloc_d
    Integer(I4B), Dimension(1) :: imax
    imax=Maxloc(arr(:))
    imaxloc_d=imax(1)
  End Function imaxloc_d
  !BL
  Function imaxloc_i(iarr)
    Integer(I4B), Dimension(:), Intent(IN) :: iarr
    Integer(I4B), Dimension(1) :: imax
    Integer(I4B) :: imaxloc_i
    imax=Maxloc(iarr(:))
    imaxloc_i=imax(1)
  End Function imaxloc_i
  !BL
  ! Chaged by IS Mar 2008; Multiple precission supported
  Function iminloc_r(arr)
    Real(SP), Dimension(:), Intent(IN) :: arr
    Integer(I4B), Dimension(1) :: imin
    Integer(I4B) :: iminloc_r
    imin=Minloc(arr(:))
    iminloc_r=imin(1)
  End Function iminloc_r
  !
  Function iminloc_d(arr)
    Real(DP), Dimension(:), Intent(IN) :: arr
    Integer(I4B), Dimension(1) :: imin
    Integer(I4B) :: iminloc_d
    imin=Minloc(arr(:))
    iminloc_d=imin(1)
  End Function iminloc_d
  !
  Function iminloc_i(iarr)
    Integer(I4B), Dimension(:), Intent(IN) :: iarr
    Integer(I4B), Dimension(1) :: imin
    Integer(I4B) :: iminloc_i
    imin=Minloc(iarr(:))
    iminloc_i=imin(1)
  End Function iminloc_i
  !BL
  Subroutine assert1(n1,string)
    Character(LEN=*), Intent(IN) :: string
    Logical, Intent(IN) :: n1
    If (.Not. n1) Then
       Write (*,*) 'SFL Error: an assertion failed with this tag:', &
            string
       Stop 'program terminated by assert1'
    End If
  End Subroutine assert1
  !BL
  Subroutine assert2(n1,n2,string)
    Character(LEN=*), Intent(IN) :: string
    Logical, Intent(IN) :: n1,n2
    If (.Not. (n1 .And. n2)) Then
       Write (*,*) 'SFL Error: an assertion failed with this tag:', &
            string
       Stop 'program terminated by assert2'
    End If
  End Subroutine assert2
  !BL
  Subroutine assert3(n1,n2,n3,string)
    Character(LEN=*), Intent(IN) :: string
    Logical, Intent(IN) :: n1,n2,n3
    If (.Not. (n1 .And. n2 .And. n3)) Then
       Write (*,*) 'SFL Error: an assertion failed with this tag:', &
            string
       Stop 'program terminated by assert3'
    End If
  End Subroutine assert3
  !BL
  Subroutine assert4(n1,n2,n3,n4,string)
    Character(LEN=*), Intent(IN) :: string
    Logical, Intent(IN) :: n1,n2,n3,n4
    If (.Not. (n1 .And. n2 .And. n3 .And. n4)) Then
       Write (*,*) 'SFL Error: an assertion failed with this tag:', &
            string
       Stop 'program terminated by assert4'
    End If
  End Subroutine assert4
  !BL
  Subroutine assert_v(n,string)
    Character(LEN=*), Intent(IN) :: string
    Logical, Dimension(:), Intent(IN) :: n
    If (.Not. All(n)) Then
       Write (*,*) 'SFL Error: an assertion failed with this tag:', &
            string
       Stop 'program terminated by assert_v'
    End If
  End Subroutine assert_v
  !BL
  Function assert_eq2(n1,n2,string)
    Character(LEN=*), Intent(IN) :: string
    Integer, Intent(IN) :: n1,n2
    Integer :: assert_eq2
    If (n1 == n2) Then
       assert_eq2=n1
    Else
       Write (*,*) 'SFL Error: an assert_eq failed with this tag:', &
            string
       Stop 'program terminated by assert_eq2'
    End If
  End Function assert_eq2
  !BL
  Function assert_eq3(n1,n2,n3,string)
    Character(LEN=*), Intent(IN) :: string
    Integer, Intent(IN) :: n1,n2,n3
    Integer :: assert_eq3
    If (n1 == n2 .And. n2 == n3) Then
       assert_eq3=n1
    Else
       Write (*,*) 'SFL Error: an assert_eq failed with this tag:', &
            string
       Stop 'program terminated by assert_eq3'
    End If
  End Function assert_eq3
  !BL
  Function assert_eq4(n1,n2,n3,n4,string)
    Character(LEN=*), Intent(IN) :: string
    Integer, Intent(IN) :: n1,n2,n3,n4
    Integer :: assert_eq4
    If (n1 == n2 .And. n2 == n3 .And. n3 == n4) Then
       assert_eq4=n1
    Else
       Write (*,*) 'SFL Error: an assert_eq failed with this tag:', &
            string
       Stop 'program terminated by assert_eq4'
    End If
  End Function assert_eq4
  !BL
  Function assert_eqn(nn,string)
    Character(LEN=*), Intent(IN) :: string
    Integer, Dimension(:), Intent(IN) :: nn
    Integer :: assert_eqn
    If (All(nn(2:) == nn(1))) Then
       assert_eqn=nn(1)
    Else
       Write (*,*) 'SFL Error: an assert_eq failed with this tag:', &
            string
       Stop 'program terminated by assert_eqn'
    End If
  End Function assert_eqn
  !BL
  Subroutine nrerror(string)
    Character(LEN=*), Intent(IN) :: string
    Write (*,*) 'SFL Error: ',string
    Stop 'program terminated by SFL Error'
  End Subroutine nrerror
  !BL
  Function arth_r(first,increment,n)
    Real(SP), Intent(IN) :: first,increment
    Integer(I4B), Intent(IN) :: n
    Real(SP), Dimension(n) :: arth_r
    Integer(I4B) :: k,k2
    Real(SP) :: temp
    If (n > 0) arth_r(1)=first
    If (n <= NPAR_ARTH) Then
       Do k=2,n
          arth_r(k)=arth_r(k-1)+increment
       End Do
    Else
       Do k=2,NPAR2_ARTH
          arth_r(k)=arth_r(k-1)+increment
       End Do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       Do
          If (k >= n) Exit
          k2=k+k
          arth_r(k+1:Min(k2,n))=temp+arth_r(1:Min(k,n-k))
          temp=temp+temp
          k=k2
       End Do
    End If
  End Function arth_r
  !BL
  Function arth_d(first,increment,n)
    Real(DP), Intent(IN) :: first,increment
    Integer(I4B), Intent(IN) :: n
    Real(DP), Dimension(n) :: arth_d
    Integer(I4B) :: k,k2
    Real(DP) :: temp
    If (n > 0) arth_d(1)=first
    If (n <= NPAR_ARTH) Then
       Do k=2,n
          arth_d(k)=arth_d(k-1)+increment
       End Do
    Else
       Do k=2,NPAR2_ARTH
          arth_d(k)=arth_d(k-1)+increment
       End Do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       Do
          If (k >= n) Exit
          k2=k+k
          arth_d(k+1:Min(k2,n))=temp+arth_d(1:Min(k,n-k))
          temp=temp+temp
          k=k2
       End Do
    End If
  End Function arth_d
  !BL
  Function arth_i(first,increment,n)
    Integer(I4B), Intent(IN) :: first,increment,n
    Integer(I4B), Dimension(n) :: arth_i
    Integer(I4B) :: k,k2,temp
    If (n > 0) arth_i(1)=first
    If (n <= NPAR_ARTH) Then
       Do k=2,n
          arth_i(k)=arth_i(k-1)+increment
       End Do
    Else
       Do k=2,NPAR2_ARTH
          arth_i(k)=arth_i(k-1)+increment
       End Do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       Do
          If (k >= n) Exit
          k2=k+k
          arth_i(k+1:Min(k2,n))=temp+arth_i(1:Min(k,n-k))
          temp=temp+temp
          k=k2
       End Do
    End If
  End Function arth_i
  !BL
  !BL
  Function geop_r(first,factor,n)
    Real(SP), Intent(IN) :: first,factor
    Integer(I4B), Intent(IN) :: n
    Real(SP), Dimension(n) :: geop_r
    Integer(I4B) :: k,k2
    Real(SP) :: temp
    If (n > 0) geop_r(1)=first
    If (n <= NPAR_GEOP) Then
       Do k=2,n
          geop_r(k)=geop_r(k-1)*factor
       End Do
    Else
       Do k=2,NPAR2_GEOP
          geop_r(k)=geop_r(k-1)*factor
       End Do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       Do
          If (k >= n) Exit
          k2=k+k
          geop_r(k+1:Min(k2,n))=temp*geop_r(1:Min(k,n-k))
          temp=temp*temp
          k=k2
       End Do
    End If
  End Function geop_r
  !BL
  Function geop_d(first,factor,n)
    Real(DP), Intent(IN) :: first,factor
    Integer(I4B), Intent(IN) :: n
    Real(DP), Dimension(n) :: geop_d
    Integer(I4B) :: k,k2
    Real(DP) :: temp
    If (n > 0) geop_d(1)=first
    If (n <= NPAR_GEOP) Then
       Do k=2,n
          geop_d(k)=geop_d(k-1)*factor
       End Do
    Else
       Do k=2,NPAR2_GEOP
          geop_d(k)=geop_d(k-1)*factor
       End Do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       Do
          If (k >= n) Exit
          k2=k+k
          geop_d(k+1:Min(k2,n))=temp*geop_d(1:Min(k,n-k))
          temp=temp*temp
          k=k2
       End Do
    End If
  End Function geop_d
  !BL
  Function geop_i(first,factor,n)
    Integer(I4B), Intent(IN) :: first,factor,n
    Integer(I4B), Dimension(n) :: geop_i
    Integer(I4B) :: k,k2,temp
    If (n > 0) geop_i(1)=first
    If (n <= NPAR_GEOP) Then
       Do k=2,n
          geop_i(k)=geop_i(k-1)*factor
       End Do
    Else
       Do k=2,NPAR2_GEOP
          geop_i(k)=geop_i(k-1)*factor
       End Do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       Do
          If (k >= n) Exit
          k2=k+k
          geop_i(k+1:Min(k2,n))=temp*geop_i(1:Min(k,n-k))
          temp=temp*temp
          k=k2
       End Do
    End If
  End Function geop_i
  !BL
  Function geop_c(first,factor,n)
    Complex(SP), Intent(IN) :: first,factor
    Integer(I4B), Intent(IN) :: n
    Complex(SP), Dimension(n) :: geop_c
    Integer(I4B) :: k,k2
    Complex(SP) :: temp
    If (n > 0) geop_c(1)=first
    If (n <= NPAR_GEOP) Then
       Do k=2,n
          geop_c(k)=geop_c(k-1)*factor
       End Do
    Else
       Do k=2,NPAR2_GEOP
          geop_c(k)=geop_c(k-1)*factor
       End Do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       Do
          If (k >= n) Exit
          k2=k+k
          geop_c(k+1:Min(k2,n))=temp*geop_c(1:Min(k,n-k))
          temp=temp*temp
          k=k2
       End Do
    End If
  End Function geop_c
  !BL
  Function geop_dv(first,factor,n)
    Real(DP), Dimension(:), Intent(IN) :: first,factor
    Integer(I4B), Intent(IN) :: n
    Real(DP), Dimension(Size(first),n) :: geop_dv
    Integer(I4B) :: k,k2
    Real(DP), Dimension(Size(first)) :: temp
    If (n > 0) geop_dv(:,1)=first(:)
    If (n <= NPAR_GEOP) Then
       Do k=2,n
          geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
       End Do
    Else
       Do k=2,NPAR2_GEOP
          geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
       End Do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       Do
          If (k >= n) Exit
          k2=k+k
          geop_dv(:,k+1:Min(k2,n))=geop_dv(:,1:Min(k,n-k))*&
               Spread(temp,2,Size(geop_dv(:,1:Min(k,n-k)),2))
          temp=temp*temp
          k=k2
       End Do
    End If
  End Function geop_dv
  !BL
  !BL
  Recursive Function cumsum_r(arr,seed) Result(ans)
    Real(SP), Dimension(:), Intent(IN) :: arr
    Real(SP), Optional, Intent(IN) :: seed
    Real(SP), Dimension(Size(arr)) :: ans
    Integer(I4B) :: n,j
    Real(SP) :: sd
    n=Size(arr)
    If (n == 0_i4b) Return
    sd=0.0_sp
    If (Present(seed)) sd=seed
    ans(1)=arr(1)+sd
    If (n < NPAR_CUMSUM) Then
       Do j=2,n
          ans(j)=ans(j-1)+arr(j)
       End Do
    Else
       ans(2:n:2)=cumsum_r(arr(2:n:2)+arr(1:n-1:2),sd)
       ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
    End If
  End Function cumsum_r
  !BL
  Recursive Function cumsum_d(arr,seed) Result(ans)
    Real(DP), Dimension(:), Intent(IN) :: arr
    Real(DP), Optional, Intent(IN) :: seed
    Real(DP), Dimension(Size(arr)) :: ans
    Integer(I4B) :: n,j
    Real(DP) :: sd
    n=Size(arr)
    If (n == 0_i4b) Return
    sd=0.0_sp
    If (Present(seed)) sd=seed
    ans(1)=arr(1)+sd
    If (n < NPAR_CUMSUM) Then
       Do j=2,n
          ans(j)=ans(j-1)+arr(j)
       End Do
    Else
       ans(2:n:2)=cumsum_d(arr(2:n:2)+arr(1:n-1:2),sd)
       ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
    End If
  End Function cumsum_d
  !BL
  Recursive Function cumsum_i(arr,seed) Result(ans)
    Integer(I4B), Dimension(:), Intent(IN) :: arr
    Integer(I4B), Optional, Intent(IN) :: seed
    Integer(I4B), Dimension(Size(arr)) :: ans
    Integer(I4B) :: n,j,sd
    n=Size(arr)
    If (n == 0_i4b) Return
    sd=0_i4b
    If (Present(seed)) sd=seed
    ans(1)=arr(1)+sd
    If (n < NPAR_CUMSUM) Then
       Do j=2,n
          ans(j)=ans(j-1)+arr(j)
       End Do
    Else
       ans(2:n:2)=cumsum_i(arr(2:n:2)+arr(1:n-1:2),sd)
       ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
    End If
  End Function cumsum_i
  !BL
  !BL
  Recursive Function cumprod(arr,seed) Result(ans)
    Real(SP), Dimension(:), Intent(IN) :: arr
    Real(SP), Optional, Intent(IN) :: seed
    Real(SP), Dimension(Size(arr)) :: ans
    Integer(I4B) :: n,j
    Real(SP) :: sd
    n=Size(arr)
    If (n == 0_i4b) Return
    sd=1.0_sp
    If (Present(seed)) sd=seed
    ans(1)=arr(1)*sd
    If (n < NPAR_CUMPROD) Then
       Do j=2,n
          ans(j)=ans(j-1)*arr(j)
       End Do
    Else
       ans(2:n:2)=cumprod(arr(2:n:2)*arr(1:n-1:2),sd)
       ans(3:n:2)=ans(2:n-1:2)*arr(3:n:2)
    End If
  End Function cumprod
  !BL
  !BL
  Function poly_rr(x,coeffs)
    Real(SP), Intent(IN) :: x
    Real(SP), Dimension(:), Intent(IN) :: coeffs
    Real(SP) :: poly_rr
    Real(SP) :: pow
    Real(SP), Dimension(:), Allocatable :: vec
    Integer(I4B) :: i,n,nn
    n=Size(coeffs)
    If (n <= 0) Then
       poly_rr=0.0_sp
    Else If (n < NPAR_POLY) Then
       poly_rr=coeffs(n)
       Do i=n-1,1,-1
          poly_rr=x*poly_rr+coeffs(i)
       End Do
    Else
       Allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       Do
          vec(n+1)=0.0_sp
          nn=Ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          If (nn == 1) Exit
          pow=pow*pow
          n=nn
       End Do
       poly_rr=vec(1)
       Deallocate(vec)
    End If
  End Function poly_rr
  !BL
  Function poly_dd(x,coeffs)
    Real(DP), Intent(IN) :: x
    Real(DP), Dimension(:), Intent(IN) :: coeffs
    Real(DP) :: poly_dd
    Real(DP) :: pow
    Real(DP), Dimension(:), Allocatable :: vec
    Integer(I4B) :: i,n,nn
    n=Size(coeffs)
    If (n <= 0) Then
       poly_dd=0.0_dp
    Else If (n < NPAR_POLY) Then
       poly_dd=coeffs(n)
       Do i=n-1,1,-1
          poly_dd=x*poly_dd+coeffs(i)
       End Do
    Else
       Allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       Do
          vec(n+1)=0.0_dp
          nn=Ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          If (nn == 1) Exit
          pow=pow*pow
          n=nn
       End Do
       poly_dd=vec(1)
       Deallocate(vec)
    End If
  End Function poly_dd
  !BL
  Function poly_rc(x,coeffs)
    Complex(SPC), Intent(IN) :: x
    Real(SP), Dimension(:), Intent(IN) :: coeffs
    Complex(SPC) :: poly_rc
    Complex(SPC) :: pow
    Complex(SPC), Dimension(:), Allocatable :: vec
    Integer(I4B) :: i,n,nn
    n=Size(coeffs)
    If (n <= 0) Then
       poly_rc=0.0_sp
    Else If (n < NPAR_POLY) Then
       poly_rc=coeffs(n)
       Do i=n-1,1,-1
          poly_rc=x*poly_rc+coeffs(i)
       End Do
    Else
       Allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       Do
          vec(n+1)=0.0_sp
          nn=Ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          If (nn == 1) Exit
          pow=pow*pow
          n=nn
       End Do
       poly_rc=vec(1)
       Deallocate(vec)
    End If
  End Function poly_rc
  !BL
  Function poly_cc(x,coeffs)
    Complex(SPC), Intent(IN) :: x
    Complex(SPC), Dimension(:), Intent(IN) :: coeffs
    Complex(SPC) :: poly_cc
    Complex(SPC) :: pow
    Complex(SPC), Dimension(:), Allocatable :: vec
    Integer(I4B) :: i,n,nn
    n=Size(coeffs)
    If (n <= 0) Then
       poly_cc=0.0_sp
    Else If (n < NPAR_POLY) Then
       poly_cc=coeffs(n)
       Do i=n-1,1,-1
          poly_cc=x*poly_cc+coeffs(i)
       End Do
    Else
       Allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       Do
          vec(n+1)=0.0_sp
          nn=Ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          If (nn == 1) Exit
          pow=pow*pow
          n=nn
       End Do
       poly_cc=vec(1)
       Deallocate(vec)
    End If
  End Function poly_cc
  !BL
  Function poly_rrv(x,coeffs)
    Real(SP), Dimension(:), Intent(IN) :: coeffs,x
    Real(SP), Dimension(Size(x)) :: poly_rrv
    Integer(I4B) :: i,n,m
    m=Size(coeffs)
    n=Size(x)
    If (m <= 0) Then
       poly_rrv=0.0_sp
    Else If (m < n .Or. m < NPAR_POLY) Then
       poly_rrv=coeffs(m)
       Do i=m-1,1,-1
          poly_rrv=x*poly_rrv+coeffs(i)
       End Do
    Else
       Do i=1,n
          poly_rrv(i)=poly_rr(x(i),coeffs)
       End Do
    End If
  End Function poly_rrv
  !BL
  Function poly_ddv(x,coeffs)
    Real(DP), Dimension(:), Intent(IN) :: coeffs,x
    Real(DP), Dimension(Size(x)) :: poly_ddv
    Integer(I4B) :: i,n,m
    m=Size(coeffs)
    n=Size(x)
    If (m <= 0) Then
       poly_ddv=0.0_dp
    Else If (m < n .Or. m < NPAR_POLY) Then
       poly_ddv=coeffs(m)
       Do i=m-1,1,-1
          poly_ddv=x*poly_ddv+coeffs(i)
       End Do
    Else
       Do i=1,n
          poly_ddv(i)=poly_dd(x(i),coeffs)
       End Do
    End If
  End Function poly_ddv
  !BL
  Function poly_msk_rrv(x,coeffs,mask)
    Real(SP), Dimension(:), Intent(IN) :: coeffs,x
    Logical(LGT), Dimension(:), Intent(IN) :: mask
    Real(SP), Dimension(Size(x)) :: poly_msk_rrv
    poly_msk_rrv=Unpack(poly_rrv(Pack(x,mask),coeffs),mask,0.0_sp)
  End Function poly_msk_rrv
  !BL
  Function poly_msk_ddv(x,coeffs,mask)
    Real(DP), Dimension(:), Intent(IN) :: coeffs,x
    Logical(LGT), Dimension(:), Intent(IN) :: mask
    Real(DP), Dimension(Size(x)) :: poly_msk_ddv
    poly_msk_ddv=Unpack(poly_ddv(Pack(x,mask),coeffs),mask,0.0_dp)
  End Function poly_msk_ddv
  !BL
  !BL
  Recursive Function poly_term_rr(a,b) Result(u)
    Real(SP), Dimension(:), Intent(IN) :: a
    Real(SP), Intent(IN) :: b
    Real(SP), Dimension(Size(a)) :: u
    Integer(I4B) :: n,j
    n=Size(a)
    If (n <= 0) Return
    u(1)=a(1)
    If (n < NPAR_POLYTERM) Then
       Do j=2,n
          u(j)=a(j)+b*u(j-1)
       End Do
    Else
       u(2:n:2)=poly_term_rr(a(2:n:2)+a(1:n-1:2)*b,b*b)
       u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
    End If
  End Function poly_term_rr
  !BL
  Recursive Function poly_term_cc(a,b) Result(u)
    Complex(SPC), Dimension(:), Intent(IN) :: a
    Complex(SPC), Intent(IN) :: b
    Complex(SPC), Dimension(Size(a)) :: u
    Integer(I4B) :: n,j
    n=Size(a)
    If (n <= 0) Return
    u(1)=a(1)
    If (n < NPAR_POLYTERM) Then
       Do j=2,n
          u(j)=a(j)+b*u(j-1)
       End Do
    Else
       u(2:n:2)=poly_term_cc(a(2:n:2)+a(1:n-1:2)*b,b*b)
       u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
    End If
  End Function poly_term_cc
  !BL
  Function zroots_unity(n,nn)
    Integer(I4B), Intent(IN) :: n,nn
    Complex(SPC), Dimension(nn) :: zroots_unity
    Integer(I4B) :: k
    Real(SP) :: theta
    zroots_unity(1)=1.0
    theta= 8*atan(1._sp)/n   !theta=TWOPI/n
    k=1
    Do
       If (k >= nn) Exit
       zroots_unity(k+1)=Cmplx(Cos(k*theta),Sin(k*theta),SPC)
       zroots_unity(k+2:Min(2*k,nn))=zroots_unity(k+1)*&
            zroots_unity(2:Min(k,nn-k))
       k=2*k
    End Do
  End Function zroots_unity
  !BL
  Function outerprod_r(a,b)
    Real(SP), Dimension(:), Intent(IN) :: a,b
    Real(SP), Dimension(Size(a),Size(b)) :: outerprod_r
    outerprod_r = Spread(a,dim=2,ncopies=Size(b)) * &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerprod_r
  !BL
  Function outerprod_d(a,b)
    Real(DP), Dimension(:), Intent(IN) :: a,b
    Real(DP), Dimension(Size(a),Size(b)) :: outerprod_d
    outerprod_d = Spread(a,dim=2,ncopies=Size(b)) * &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerprod_d
  !BL
  Function outerdiv(a,b)
    Real(SP), Dimension(:), Intent(IN) :: a,b
    Real(SP), Dimension(Size(a),Size(b)) :: outerdiv
    outerdiv = Spread(a,dim=2,ncopies=Size(b)) / &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerdiv
  !BL
  Function outersum(a,b)
    Real(SP), Dimension(:), Intent(IN) :: a,b
    Real(SP), Dimension(Size(a),Size(b)) :: outersum
    outersum = Spread(a,dim=2,ncopies=Size(b)) + &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outersum
  !BL
  Function outerdiff_r(a,b)
    Real(SP), Dimension(:), Intent(IN) :: a,b
    Real(SP), Dimension(Size(a),Size(b)) :: outerdiff_r
    outerdiff_r = Spread(a,dim=2,ncopies=Size(b)) - &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerdiff_r
  !BL
  Function outerdiff_d(a,b)
    Real(DP), Dimension(:), Intent(IN) :: a,b
    Real(DP), Dimension(Size(a),Size(b)) :: outerdiff_d
    outerdiff_d = Spread(a,dim=2,ncopies=Size(b)) - &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerdiff_d
  !BL
  Function outerdiff_i(a,b)
    Integer(I4B), Dimension(:), Intent(IN) :: a,b
    Integer(I4B), Dimension(Size(a),Size(b)) :: outerdiff_i
    outerdiff_i = Spread(a,dim=2,ncopies=Size(b)) - &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerdiff_i
  !BL
  Function outerand(a,b)
    Logical(LGT), Dimension(:), Intent(IN) :: a,b
    Logical(LGT), Dimension(Size(a),Size(b)) :: outerand
    outerand = Spread(a,dim=2,ncopies=Size(b)) .And. &
         Spread(b,dim=1,ncopies=Size(a))
  End Function outerand
  !BL
  Subroutine scatter_add_r(dest,source,dest_index)
    Real(SP), Dimension(:), Intent(OUT) :: dest
    Real(SP), Dimension(:), Intent(IN) :: source
    Integer(I4B), Dimension(:), Intent(IN) :: dest_index
    Integer(I4B) :: m,n,j,i
    n=assert_eq2(Size(source),Size(dest_index),'scatter_add_r')
    m=Size(dest)
    Do j=1,n
       i=dest_index(j)
       If (i > 0 .And. i <= m) dest(i)=dest(i)+source(j)
    End Do
  End Subroutine scatter_add_r
  Subroutine scatter_add_d(dest,source,dest_index)
    Real(DP), Dimension(:), Intent(OUT) :: dest
    Real(DP), Dimension(:), Intent(IN) :: source
    Integer(I4B), Dimension(:), Intent(IN) :: dest_index
    Integer(I4B) :: m,n,j,i
    n=assert_eq2(Size(source),Size(dest_index),'scatter_add_d')
    m=Size(dest)
    Do j=1,n
       i=dest_index(j)
       If (i > 0 .And. i <= m) dest(i)=dest(i)+source(j)
    End Do
  End Subroutine scatter_add_d
  Subroutine scatter_max_r(dest,source,dest_index)
    Real(SP), Dimension(:), Intent(OUT) :: dest
    Real(SP), Dimension(:), Intent(IN) :: source
    Integer(I4B), Dimension(:), Intent(IN) :: dest_index
    Integer(I4B) :: m,n,j,i
    n=assert_eq2(Size(source),Size(dest_index),'scatter_max_r')
    m=Size(dest)
    Do j=1,n
       i=dest_index(j)
       If (i > 0 .And. i <= m) dest(i)=Max(dest(i),source(j))
    End Do
  End Subroutine scatter_max_r
  Subroutine scatter_max_d(dest,source,dest_index)
    Real(DP), Dimension(:), Intent(OUT) :: dest
    Real(DP), Dimension(:), Intent(IN) :: source
    Integer(I4B), Dimension(:), Intent(IN) :: dest_index
    Integer(I4B) :: m,n,j,i
    n=assert_eq2(Size(source),Size(dest_index),'scatter_max_d')
    m=Size(dest)
    Do j=1,n
       i=dest_index(j)
       If (i > 0 .And. i <= m) dest(i)=Max(dest(i),source(j))
    End Do
  End Subroutine scatter_max_d
  !BL
  Subroutine diagadd_rv(mat,diag)
    Real(SP), Dimension(:,:), Intent(INOUT) :: mat
    Real(SP), Dimension(:), Intent(IN) :: diag
    Integer(I4B) :: j,n
    n = assert_eq2(Size(diag),Min(Size(mat,1),Size(mat,2)),'diagadd_rv')
    Do j=1,n
       mat(j,j)=mat(j,j)+diag(j)
    End Do
  End Subroutine diagadd_rv
  !BL
  Subroutine diagadd_r(mat,diag)
    Real(SP), Dimension(:,:), Intent(INOUT) :: mat
    Real(SP), Intent(IN) :: diag
    Integer(I4B) :: j,n
    n = Min(Size(mat,1),Size(mat,2))
    Do j=1,n
       mat(j,j)=mat(j,j)+diag
    End Do
  End Subroutine diagadd_r
  !BL
  Subroutine diagmult_rv(mat,diag)
    Real(SP), Dimension(:,:), Intent(INOUT) :: mat
    Real(SP), Dimension(:), Intent(IN) :: diag
    Integer(I4B) :: j,n
    n = assert_eq2(Size(diag),Min(Size(mat,1),Size(mat,2)),'diagmult_rv')
    Do j=1,n
       mat(j,j)=mat(j,j)*diag(j)
    End Do
  End Subroutine diagmult_rv
  !BL
  Subroutine diagmult_r(mat,diag)
    Real(SP), Dimension(:,:), Intent(INOUT) :: mat
    Real(SP), Intent(IN) :: diag
    Integer(I4B) :: j,n
    n = Min(Size(mat,1),Size(mat,2))
    Do j=1,n
       mat(j,j)=mat(j,j)*diag
    End Do
  End Subroutine diagmult_r
  !BL
  Function get_diag_rv(mat)
    Real(SP), Dimension(:,:), Intent(IN) :: mat
    Real(SP), Dimension(Size(mat,1)) :: get_diag_rv
    Integer(I4B) :: j
    j=assert_eq2(Size(mat,1),Size(mat,2),'get_diag_rv')
    Do j=1,Size(mat,1)
       get_diag_rv(j)=mat(j,j)
    End Do
  End Function get_diag_rv
  !BL
  Function get_diag_dv(mat)
    Real(DP), Dimension(:,:), Intent(IN) :: mat
    Real(DP), Dimension(Size(mat,1)) :: get_diag_dv
    Integer(I4B) :: j
    j=assert_eq2(Size(mat,1),Size(mat,2),'get_diag_dv')
    Do j=1,Size(mat,1)
       get_diag_dv(j)=mat(j,j)
    End Do
  End Function get_diag_dv
  !BL
  Subroutine put_diag_rv(diagv,mat)
    Real(SP), Dimension(:), Intent(IN) :: diagv
    Real(SP), Dimension(:,:), Intent(INOUT) :: mat
    Integer(I4B) :: j,n
    n=assert_eq2(Size(diagv),Min(Size(mat,1),Size(mat,2)),'put_diag_rv')
    Do j=1,n
       mat(j,j)=diagv(j)
    End Do
  End Subroutine put_diag_rv
  !BL
  Subroutine put_diag_r(scal,mat)
    Real(SP), Intent(IN) :: scal
    Real(SP), Dimension(:,:), Intent(INOUT) :: mat
    Integer(I4B) :: j,n
    n = Min(Size(mat,1),Size(mat,2))
    Do j=1,n
       mat(j,j)=scal
    End Do
  End Subroutine put_diag_r
  !BL
  Subroutine unit_matrix(mat)
    Real(SP), Dimension(:,:), Intent(OUT) :: mat
    Integer(I4B) :: i,n
    n=Min(Size(mat,1),Size(mat,2))
    mat(:,:)=0.0_sp
    Do i=1,n
       mat(i,i)=1.0_sp
    End Do
  End Subroutine unit_matrix
  !BL
  Function upper_triangle(j,k,extra)
    Integer(I4B), Intent(IN) :: j,k
    Integer(I4B), Optional, Intent(IN) :: extra
    Logical(LGT), Dimension(j,k) :: upper_triangle
    Integer(I4B) :: n
    n=0
    If (Present(extra)) n=extra
    upper_triangle=(outerdiff(arth_i(1,1,j),arth_i(1,1,k)) < n)
  End Function upper_triangle
  !BL
  Function lower_triangle(j,k,extra)
    Integer(I4B), Intent(IN) :: j,k
    Integer(I4B), Optional, Intent(IN) :: extra
    Logical(LGT), Dimension(j,k) :: lower_triangle
    Integer(I4B) :: n
    n=0
    If (Present(extra)) n=extra
    lower_triangle=(outerdiff(arth_i(1,1,j),arth_i(1,1,k)) > -n)
  End Function lower_triangle
  !BL
  Function vabs(v)
    Real(SP), Dimension(:), Intent(IN) :: v
    Real(SP) :: vabs
    vabs=Sqrt(Dot_product(v,v))
  End Function vabs
  !BL



End Module SFL_NR_Util
