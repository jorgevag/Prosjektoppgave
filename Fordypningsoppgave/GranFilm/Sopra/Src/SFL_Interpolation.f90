
!----------------------------------------------------------------------------
!  $Id: AkimaSplineMod.f90,v 1.2 2004/02/02 13:02:26 ingves Exp ingves $
!----------------------------------------------------------------------------


!
! NOTICE : A potential BUG in the original version was corrected in the routine 
!           akimamod_driver (writing outside vectors) 
!


Module SFL_Interpolation



  Use SFL_Precision, only : sp, dp
  ! integer, parameter :: sp = kind(1.0)
  ! integer, parameter :: dp = kind(1.d0)
 

 !  Create generic name 'SFL_AkimaSpline'
  interface SFL_AkimaSpline
     module procedure &
          AkimaSpline_Vector_sp, AkimaSpline_Vector_dp,                    &
          AkimaSpline_Vector_Complex_sp, AkimaSpline_Vector_Complex_dp,    &
          AkimaSpline_Scalar_sp, AkimaSpline_Scalar_dp,                    &
          AkimaSpline_Scalar_Complex_sp, AkimaSpline_Scalar_Complex_dp
  end interface
  !---------------------------------------------------------------------
  ! ROUTINE :  AkimaSpline   (generic routine)
  ! TYPE    :  Subroutine
  ! SYSTEM  :  Fortran 90
  !
  ! PURPOSE :  To to Akima Spline
  !         
  !
  ! SYNTAX  :    
  !    AkimaSpline(Xinput,Yinput,ierr,X,Y,dYdx)
  !     real, intent(in)            :: XInput(:), Yinput(:)  
  !     Integer,  intent(InOut)     :: ierr
  !     real, intent(InOut)         :: X(:)     (or X)
  !     real, intent(Out)           :: Y(:)     (or Y)
  !     real, intent(out), optional :: dYdX(:)  (or dYdX)
  ! 
  !        XInput, Yinput  : Data to be interpolated (ASSUMED ORDERED ....)
  !        ierr            : error flag (<0 in case of an error)
  !        X               : Interpolated abscissas values 
  !                          X values should be in the range of Xinput
  !                           ( i.e. minval(X)<=minval(XInput) or 
  !                              maxval(X)>=maxval(Xinput)
  !                          This varaible is provided by the user as a 
  !                           scalar or a vector (Generic Interface)
  !        Y               : Resulting intepolated values (size(X))
  !        dYdX            : Optional derivative information at values 
  !                          specified by X
  !---------------------------------------------------------------------
  ! USES      : 
  !---------------------------------------------------------------------
  ! DATE      : 29-Jan-2004
  ! LAST MOD  :  
  ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
  ! E-mail    : ingves@phys.unit.no
  !---------------------------------------------------------------------




  !-------------------------
  ! Public
  !-------------------------
  Public  :: SFL_AkimaSpline

  !-------------------------
  ! Privat
  !-------------------------
  Private :: AkimaSpline_Vector_dp,   &
             AkimaSpline_Vector_sp,   &
             AkimaSpline_Scalar_sp,   &
             AkimaSpline_Scalar_dp,   &
             akimamod_driver,         &
             boundary_conditions,     &
             derivative_volatility,   &
             distance

contains


  !--------------------------------------------
  ! Singel Precision
  !--------------------------------------------

  Subroutine AkimaSpline_Vector_sp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the double precision version
    Implicit None
    real(sp), intent(in)              :: XInput(:), Yinput(:)
    Integer,  intent(InOut)           :: ierr
    real(sp), intent(In)              :: X(:)
    real(sp), intent(Out)             :: Y(:)
    real(sp), intent(out), optional   :: dYdX(:)
    ! Local
    real(dp)   :: XInput_dp(size(XInput)), YInput_dp(size(YInput))
    real(dp)   :: X_dp(size(X)), Y_dp(size(Y)), dYdX_dp(size(X))
    
    ierr =  0
    ! Convert input data to double precision 
    XInput_dp = real(XInput,dp)
    YInput_dp = real(YInput,dp)
    X_dp      = real(X,dp)
    ! Wrapper to the double precision routine
    if (present(dYdx)) then
       call AkimaSpline_Vector_dp(Xinput_dp,Yinput_dp,ierr,X_dp,Y_dp,dYdX_dp)
    else
       call AkimaSpline_Vector_dp(Xinput_dp,Yinput_dp,ierr,X_dp,Y_dp)
    end if
    ! Convert the result to single precision 
    Y = real(Y_dp,sp)
    if (present(dYdX)) then
       if (size(dYdX)/=size(X)) then
          ierr=-1
          return
       else
          dYdX = real(dYdX_dp,sp)
       end if
    end if

    return

  End Subroutine AkimaSpline_Vector_sp

  ! ------------------------------------------------------------------

  Subroutine AkimaSpline_Vector_Complex_sp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the real version
    Implicit None
    real(sp),    intent(in)            :: XInput(:)
    complex(sp), intent(in)            :: Yinput(:)
    Integer,     intent(InOut)         :: ierr
    real(sp),    intent(In)            :: X(:)
    complex(sp), intent(Out)           :: Y(:)
    complex(sp), intent(out), optional :: dYdX(:)
    ! Local
    complex(sp), parameter :: imu=(0._sp, 1._sp)
    real(sp)   :: Y_real(size(X))
    real(sp)   :: dYdX_real(size(X))
    
    ierr =  0
    ! Wrapper to the real  routine
    if (present(dYdx)) then 
      call AkimaSpline_Vector_sp(Xinput,real(Yinput,sp),ierr,X,Y_real, dYdX_real)
      Y    = Y_real
      dYdX = dYdX_real
      call AkimaSpline_Vector_sp(Xinput,real(-imu*Yinput,sp),ierr,X,Y_real,dYdX_real)
      Y    = Y + imu*Y_real
      dYdX = dYdX + imu*dYdX_real
    else
      call AkimaSpline_Vector_sp(Xinput,real(Yinput,sp),ierr,X,Y_real)
      Y    = Y_real
      call AkimaSpline_Vector_sp(Xinput,real(-imu*Yinput,sp),ierr,X,Y_real)
      Y    = Y + imu*Y_real
    end if

    return

  End Subroutine AkimaSpline_Vector_Complex_sp

  !-----------------------------------------------------

  Subroutine AkimaSpline_Scalar_sp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the single precision vector routine
    Implicit None
    real(sp), intent(in)              :: XInput(:), Yinput(:)
    Integer,  intent(InOut)           :: ierr
    real(sp), intent(In)              :: X
    real(sp), intent(Out)             :: Y
    real(sp), intent(out), optional   :: dYdX
    ! Local
    real(sp), dimension(1) :: Xvec, Yvec, dYdXvec

    ierr =  0
    ! Convert input data to double precision 
    Xvec(1)      =  real(X,dp)
    ! Wrapper to the double precision routine
    if (present(dYdx)) then
       call AkimaSpline_Vector_sp(Xinput,Yinput,ierr,Xvec,Yvec,dYdXvec)
    else
       call AkimaSpline_Vector_sp(Xinput,Yinput,ierr,Xvec,Yvec)
    end if
    ! Convert the result to single precision 
    Y = real(Yvec(1),sp)
    if (present(dYdX)) then
       dYdX =  dYdXvec(1)
    end if

    return

  End Subroutine AkimaSpline_Scalar_sp

  !-----------------------------------------------------

  Subroutine AkimaSpline_Scalar_Complex_sp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the real version
    Implicit None
    real(sp),    intent(in)            :: XInput(:)
    complex(sp), intent(in)            :: Yinput(:)
    Integer,     intent(InOut)         :: ierr
    real(sp),    intent(In)            :: X
    complex(sp), intent(Out)           :: Y
    complex(sp), intent(out), optional :: dYdX
    ! Local
    real(sp),     dimension(1) :: Xvec
    complex(sp),  dimension(1) :: Yvec, dYdXvec

    
    ierr =  0
    ! Copy the input argument
    Xvec(1) = X
    ! Wrapper to the real routine
    if (present(dYdx)) then
       call AkimaSpline_Vector_Complex_sp(Xinput,Yinput,ierr,Xvec,Yvec,dYdXvec)
    else
       call AkimaSpline_Vector_Complex_sp(Xinput,Yinput,ierr,Xvec,Yvec)
    end if
    ! Convert the result to scalar 
    Y = Yvec(1)
    if (present(dYdX)) then
       dYdX =  dYdXvec(1)
    end if

    return

  End Subroutine AkimaSpline_Scalar_Complex_sp



  !--------------------------------------------
  ! Double Precision
  !--------------------------------------------

  Subroutine AkimaSpline_Vector_dp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! This is the main routine !!!!
    Implicit None
    real(dp), intent(InoUT)              :: XInput(:), Yinput(:)
    Integer,  intent(InOut)           :: ierr
    real(dp), intent(In)              :: X(:)
    real(dp), intent(Out)             :: Y(:)
    real(dp), intent(out), optional   :: dYdX(:)
    ! Local
    real(dp)                           :: dYdX_local(size(X))
    Integer :: i
    
    ierr = 0
    ! Error check if the INPUT arrays are of the correct size
    if (size(XInput)/=size(YInput)) then
       Write(*,*) "WARNING : vector sizes do NOT match ..... "
       ierr=-1
       return
    endif
    ! Error check if the OUTPUT arrays are of the correct size
    if (size(X)/=size(Y)) then
       Write(*,*) "WARNING : vector sizes do NOT match ..... "
       ierr=-1
       return
    endif
    ! Check if X is in range of XInput (assumed to be in increasing order)
    if ( (minval(X)<=minval(XInput)) .or. (maxval(X)>=maxval(Xinput)) ) then
       Write(*,*) "WARNING : data out of range......"
       ierr=-1
       return
    end if
    if (present(dYdX)) then
       if (size(X)/=size(dYdX)) then
          ierr=-1
          return
       endif
    endif

    ! Call the wrapper
    call akimamod_driver(size(XInput),size(X),Y,dYdX_local,Xinput,YInput,X)
    ! Copy the derivative array
    if (present(dYdX)) dYdX = dYdX_local
    return
 
  End Subroutine AkimaSpline_Vector_dp

  !------------------------------------------


  Subroutine AkimaSpline_Vector_Complex_dp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the real version
    Implicit None
    real(dp),    intent(in)            :: XInput(:)
    complex(dp), intent(in)            :: Yinput(:)
    Integer,     intent(InOut)         :: ierr
    real(dp),    intent(In)            :: X(:)
    complex(dp), intent(Out)           :: Y(:)
    complex(dp), intent(Out), optional :: dYdX(:)
    ! Local
    complex(dp), parameter :: imu=(0._dp, 1._dp)
    real(dp)   :: Y_real(size(X))
    real(dp)   :: dYdX_real(size(X))


    ierr =  0
    ! Wrapper to the real  routine
    if (present(dYdx)) then 
       call AkimaSpline_Vector_dp(Xinput,real(Yinput,dp),ierr,X,Y_real,dYdX_real)
       Y    = Y_real
       dYdX = dYdX_real
       call AkimaSpline_Vector_dp(Xinput,real(-imu*Yinput,dp),ierr,X,Y_real,dYdX_real)
       Y    = Y + imu*Y_real
       dYdX = dYdX + imu*dYdX_real
    else
       call AkimaSpline_Vector_dp(Xinput,real(Yinput,dp),ierr,X,Y_real)
       Y    = Y_real
       call AkimaSpline_Vector_dp(Xinput,real(-imu*Yinput,dp),ierr,X,Y_real)
       Y    = Y + imu*Y_real
    end if
    
    return
    
  End Subroutine AkimaSpline_Vector_Complex_dp



  !-----------------------------------------------------

  Subroutine AkimaSpline_Scalar_dp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the double precision version
    Implicit None
    real(dp), intent(in)              :: XInput(:), Yinput(:)
    Integer,  intent(InOut)           :: ierr
    real(dp), intent(In)              :: X
    real(dp), intent(Out)             :: Y
    real(dp), intent(out), optional   :: dYdX
    ! Local
    real(dp), dimension(1) :: Xvec, Yvec, dYdXvec

    
    ierr =  0
    ! Convert input data to double precision 
    Xvec(1)      =  real(X,dp)
    ! Wrapper to the double precision routine
    if (present(dYdx)) then
       call AkimaSpline_Vector_dp(Xinput,Yinput,ierr,Xvec,Yvec,dYdXvec)
    else
       call AkimaSpline_Vector_dp(Xinput,Yinput,ierr,Xvec,Yvec)
    end if
    ! Convert the result to single precision 
    Y = real(Yvec(1),sp)
    if (present(dYdX)) then
       dYdX =  dYdXvec(1)
    end if

    return

  End Subroutine AkimaSpline_Scalar_dp

  ! ----------------------------------------------------

  Subroutine AkimaSpline_Scalar_Complex_dp(Xinput,Yinput,ierr,X,Y,dYdx)
    ! Wrapper to the real version
    Implicit None
    real(dp),    intent(in)            :: XInput(:)
    complex(dp), intent(in)            :: Yinput(:)
    Integer,     intent(InOut)         :: ierr
    real(dp),    intent(In)            :: X
    complex(dp), intent(Out)           :: Y
    complex(dp), intent(out), optional :: dYdX
    ! Local
    real(dp),     dimension(1) :: Xvec
    complex(dp),  dimension(1) :: Yvec, dYdXvec
    
    ierr =  0
    ! Copy the input argument
    Xvec(1) = X 
    ! Wrapper to the real routine
    if (present(dYdx)) then
       call AkimaSpline_Vector_Complex_dp(Xinput,Yinput,ierr,Xvec,Yvec,dYdXvec)
    else
       call AkimaSpline_Vector_Complex_dp(Xinput,Yinput,ierr,Xvec,Yvec)
    end if
    ! Convert the result to scalar 
    Y = Yvec(1)
    if (present(dYdX)) then
       dYdX =  dYdXvec(1)
    end if

    return

  End Subroutine AkimaSpline_Scalar_Complex_dp

  !-----------------------------------------------------


  !------------------------------------------------
  ! UTILITY ROUTINES
  !------------------------------------------------


  !     
  !     My modified akima spline routine.
  !     call akimamod(n,ni,yi,gi,x,y,xi). See below.
  !     
  !     Uses Lagrange polynomial formulas for quadratics and cubics.
  !     Sun Feb  9 15:00:30 PST 2003
  !     


  subroutine akimamod_driver(n,ni,yi,gi,x,y,xi)
    !     
    !     /***
    !     * Akima spline from the 1991 method of Hiroshi Akima
    !     * ACM Trans. on Math. Software Vol. 17, No. 3, Sept. 1991, p. 341.
    !     * Modified to use different volatility measure and more
    !     * derivative estimates.
    !     *
    !     * n is number of knots, minumum of three, located at (x,y); ni is
    !     * the size of the xi, yi and gi arrays -- the interpolated
    !     * coordinate, value and derivative.
    !     ***/
    implicit none
    integer     ::  i,ii,k,j,n,ni
    real(dp)    ::  yi(ni),gi(ni),x(n),y(n),xi(ni)
    real(dp)    ::  w(6),mw,sw,f(6),v(6),d(6)
    real(dp)    ::  hk,hi,m,p0,p1,p2,p3
    real(dp)    ::  xaug(n+6),yaug(n+6),t(n)              ! *** NOTE : Is this correct ?    
    !!!    Orriginal reads : real(dp)    ::  xaug(1006),yaug(1006),t(1000)  
    Logical     :: Reordered = .false.

    if(n .lt. 3) then
       write(*,*) 'too few points to spline'
       stop
    endif


    ! ---- Do we need to reorder the data....?
    !      Added by IS
    Reordered = .false.
    if (x(1)>x(2)) then      ! Data are assumed to be ordered....
       ! Reverse the order of the data.....
       Reordered = .true.
       x = x(  [ (i,i=size(x),1,-1) ] )
       y = y(  [ (i,i=size(x),1,-1) ] )
    endif

    !     /* add three points on either side -- x -> xaug */
    call boundary_conditions(n,xaug,yaug,x,y)

    !do i=4,n+4   IS THIS A BUG ? 
    !             One access outside the xarg/yarg vector....
    do i=4,n+3 
       ii=i-3
       call derivative_volatility(f(1),v(1),xaug(i),xaug(i-1),     &
               xaug(i-2),xaug(i-3),yaug(i),yaug(i-1),yaug(i-2),    &
               yaug(i-3))
       call derivative_volatility(f(2),v(2),xaug(i),xaug(i-1),     &
                xaug(i+1),xaug(i-3),yaug(i),yaug(i-1),yaug(i+1),   &
                yaug(i-3))
       call derivative_volatility(f(3),v(3),xaug(i),xaug(i-1),     &
                xaug(i+1),xaug(i-2),yaug(i),yaug(i-1),yaug(i+1),   &
                yaug(i-2))
       call derivative_volatility(f(4),v(4),xaug(i),xaug(i-1),     &
                xaug(i+1),xaug(i+2),yaug(i),yaug(i-1),yaug(i+1),   &
                yaug(i+2))
       call derivative_volatility(f(5),v(5),xaug(i),xaug(i-1),     &
                xaug(i+1),xaug(i+3),yaug(i),yaug(i-1),yaug(i+1),   &
                yaug(i+3))
       call derivative_volatility(f(6),v(6),xaug(i),xaug(i+1),     &
                xaug(i+2),xaug(i+3),yaug(i),yaug(i+1),yaug(i+2),   &
                yaug(i+3))
       call distance(d(1),xaug(i),xaug(i-1),xaug(i-2),xaug(i-3))
       call distance(d(2),xaug(i),xaug(i-1),xaug(i+1),xaug(i-3))
       call distance(d(3),xaug(i),xaug(i-1),xaug(i+1),xaug(i-2))
       call distance(d(4),xaug(i),xaug(i-1),xaug(i+1),xaug(i+2))
       call distance(d(5),xaug(i),xaug(i-1),xaug(i+1),xaug(i+3))
       call distance(d(6),xaug(i),xaug(i+1),xaug(i+2),xaug(i+3))

       !  /* max(w) */
       mw=0._dp
       do k=1,6
          !  /* in case some of the v's are zero */
          v(k)=v(k)+1.0d-200  !
          w(k)=1./v(k)
          mw=max(mw,w(k))
       end do
       !  /* sum(w) */
       sw=0._dp 
       do k=1,6
          !  /* renormalize to get rid of 1e+200 and add in 1/dist */
          w(k)=w(k)/(mw*d(k))
          sw=sw+w(k)
       end do
       t(ii)=0._dp
       do k=1,6
          t(ii)=t(ii)+f(k)*w(k)/sw
       end do
    end do

    do j=1,ni
       do ii=2,n
          i=ii-1
          if(xi(j) .lt. x(ii)) goto 99
       end do
99     continue
       hk=x(i+1)-x(i)
       hi=xi(j)-x(i)
       m=(y(i+1)-y(i))/(x(i+1)-x(i))
       p0=y(i)
       p1=t(i)
       p2=(3._dp*m-2._dp*t(i)-t(i+1))/hk
       p3=(t(i)+t(i+1)-2._dp*m)/hk**2
       yi(j)=p0+p1*hi+p2*hi**2+p3*hi**3
       gi(j)=p1+2._dp*p2*hi+3._dp*p3*hi**2
    end do


    ! --- Do we need to reorder the data back ....?
    !     Added by IS
    if (ReOrdered) then
       ! Reverse the order of the data.....
       x = x(  [ (i,i=size(x),1,-1) ] )
       y = y(  [ (i,i=size(x),1,-1) ] )
    endif

    return
  end subroutine akimamod_driver


  !--------------------------------------------------

  subroutine boundary_conditions(n,xaug,yaug,x,y)
    !     
    !     /***
    !     * use parabolic extrapolation from the three points adjacent the
    !     * ends to add three more 'ghost' points on either side.
    !     ***/
    !
    implicit none

    integer     :: i,n
    real(dp)    :: xaug(n+6),yaug(n+6),x(n),y(n)
    real(dp)    :: Lg1,Lg2,Lg3,coef0,coef1,coef2

    ! /* get coefs by Lagrange polynomial formula
    !  y=coef0+coef1*x+coef2*x^2 */
    Lg1=y(1)/(x(1)-x(2))/(x(1)-x(3))
    Lg2=y(2)/(x(2)-x(1))/(x(2)-x(3))
    Lg3=y(3)/(x(3)-x(1))/(x(3)-x(2))
    coef0=x(2)*x(3)*Lg1+x(1)*x(3)*Lg2+x(1)*x(2)*Lg3
    coef1=-(x(2)+x(3))*Lg1-(x(1)+x(3))*Lg2-(x(1)+x(2))*Lg3
    coef2=Lg1+Lg2+Lg3

    xaug(1)=3._dp*x(1)-2._dp*x(3)
    xaug(2)=2._dp*x(1)-x(3)
    xaug(3)=2._dp*x(1)-x(2)
    yaug(1)=coef0+coef1*xaug(1)+coef2*xaug(1)*xaug(1)
    yaug(2)=coef0+coef1*xaug(2)+coef2*xaug(2)*xaug(2)
    yaug(3)=coef0+coef1*xaug(3)+coef2*xaug(3)*xaug(3)

    Lg1=y(n)/(x(n)-x(n-1))/(x(n)-x(n-2));
    Lg2=y(n-1)/(x(n-1)-x(n))/(x(n-1)-x(n-2));
    Lg3=y(n-2)/(x(n-2)-x(n))/(x(n-2)-x(n-1));
    coef0=x(n-1)*x(n-2)*Lg1+x(n)*x(n-2)*Lg2+x(n)*x(n-1)*Lg3
    coef1=-(x(n-1)+x(n-2))*Lg1-(x(n)+x(n-2))*Lg2                &
          -(x(n)+x(n-1))*Lg3
    coef2=Lg1+Lg2+Lg3

    xaug(3+n+1)=2._dp*x(n)-x(n-1)
    xaug(3+n+2)=2._dp*x(n)-x(n-2)
    xaug(3+n+3)=3._dp*x(n)-2._dp*x(n-2)
    yaug(3+n+1)=coef0+coef1*xaug(3+n+1)+coef2*xaug(3+n+1)*xaug(3+n+1)
    yaug(3+n+2)=coef0+coef1*xaug(3+n+2)+coef2*xaug(3+n+2)*xaug(3+n+2)
    yaug(3+n+3)=coef0+coef1*xaug(3+n+3)+coef2*xaug(3+n+3)*xaug(3+n+3)

    do i=1,n
       xaug(3+i)=x(i)
       yaug(3+i)=y(i)
    end do

    return
  end subroutine boundary_conditions
  

  !------------------------------------


  subroutine derivative_volatility(f,v,x1,x2,x3,x4,y1,y2,y3,y4)
    !     
    !     /***
    !     * f is the slope of the cubic at point x1, and v is the
    !     * "volatility" measure which, as implemented here, is the square of
    !     * the maximum second derivative on the cubic between the endpoints.
    !     ***/
    implicit none
    real(dp)    ::   f,v,x1,x2,x3,x4,y1,y2,y3,y4
    real(dp)    ::   coef1,coef2,coef3,mx,MMx,ypp1,ypp2
    real(dp)    ::   Lg1,Lg2,Lg3,Lg4
    
    !      /* y=coef0+coef1*x+coef2*x^2+coef3*x^3
    !      but coef0 is not needed since only want y' and y'' */

    Lg1=y1/(x1-x2)/(x1-x3)/(x1-x4)
    Lg2=y2/(x2-x1)/(x2-x3)/(x2-x4)
    Lg3=y3/(x3-x1)/(x3-x2)/(x3-x4)
    Lg4=y4/(x4-x1)/(x4-x2)/(x4-x3)
    coef1=(x2*x4+x3*x4+x2*x3)*Lg1        &
          +(x1*x4+x3*x4+x1*x3)*Lg2       & 
          +(x1*x4+x2*x4+x1*x2)*Lg3       &
          +(x1*x3+x2*x3+x1*x2)*Lg4       
    coef2=-(x2+x3+x4)*Lg1-(x1+x3+x4)*Lg2 &
          -(x1+x2+x4)*Lg3-(x1+x2+x3)*Lg4
    coef3=Lg1+Lg2+Lg3+Lg4

    f=coef1+2._dp*coef2*x1+3*coef3*x1*x1

    mx  = min(x1,x2)
    mx  = min(mx,x3)
    mx  = min(mx,x4)
    MMx = max(x1,x2)
    MMx = max(MMx,x3)
    MMx = max(MMx,x4)

    ypp1 = 2._dp*coef2+6._dp*coef3*mx
    ypp2 = 2._dp*coef2+6._dp*coef3*MMx

    v=max(ypp1**2,ypp2**2)

    return
  end subroutine derivative_volatility
  
  !----------------------------

  subroutine distance(d,x1,x2,x3,x4)
    !     
    !     /***
    !     * cumulative distance measure from x1 to the others
    !     ***/
    implicit none
    real(dp)    :: d,x1,x2,x3,x4
    d = (x2-x1)*(x2-x1)+(x3-x1)*(x3-x1)+(x4-x1)*(x4-x1)
    return
  end subroutine distance
  
  !----------------------------


End Module SFL_Interpolation


!!$
!!$
!!$
!!$!===========0000000000====================
!!$
!!$
!!$
!!$
!!$
!!$PROGRAM Akima
!!$  USE SFL_Interpolation, only : wp=>sp
!!$  USE SFL_Interpolation
!!$  Implicit None
!!$  integer, parameter :: SIZE=14
!!$  real(wp)    :: X(SIZE), Y(SIZE)
!!$  complex(wp) :: YC(Size)
!!$  real(wp)    :: Xint, Xint1(1), Yint,dYdXInt  
!!$  complex(wp) :: YCInt, dYdXCInt
!!$  complex(wp) :: YCInt1(1), dYdXCInt1(1)
!!$  real(wp)    :: Xvec(15), Yvec(15),dYdXVec(15), XXX(14,2)
!!$  complex(wp) :: YCVec(15), dYdXCVec(15)
!!$  real(wp)    :: a,b,xx,yy,yyy
!!$  integer     :: i,iv,n,ierr
!!$
!!$  print *,' '
!!$  print *,'Akima spline fitting of SIN(X):'
!!$  ! Input sine table
!!$  !-----------------------------------------------------------------
!!$  !Sine table values from  Handbook of mathematical functions
!!$  !by M. Abramowitz and I.A. Stegun, NBS, june 1964
!!$  !-----------------------------------------------------------------
!!$  X(1)=0.000_wp; Y(1)=0.00000000; X(2)=0.125_wp; Y(2)=0.12467473;
!!$  X(3)=0.217_wp; Y(3)=0.21530095; X(4)=0.299_wp; Y(4)=0.29456472;
!!$  X(5)=0.376_wp; Y(5)=0.36720285; X(6)=0.450_wp; Y(6)=0.43496553;
!!$  X(7)=0.520_wp; Y(7)=0.49688014; X(8)=0.589_wp; Y(8)=0.55552980;
!!$  X(9)=0.656_wp; Y(9)=0.60995199; X(10)=0.721_wp; Y(10)=0.66013615;
!!$  X(11)=0.7853981634_wp; Y(11)=0.7071067812;
!!$  X(12)=0.849_wp; Y(12)=0.75062005; X(13)=0.911; Y(13)=0.79011709;
!!$  X(14)=0.972_wp; Y(14)=0.82601466;
!!$
!!$
!!$  y  = sin(x)
!!$  yc = y
!!$
!!$  XXX(:,1) = X
!!$  XXX(:,2) = Y
!!$
!!$  !-----------------------------------------------------------------
!!$  ! Test the scalar interface  
!!$  Write(*,*)
!!$  write(*,*) "Testing the scalar interface : "
!!$  print *,' '
!!$  print *,'  X   SIN(X) HANDBOOK         Int:Y          ERROR           Int:dy/dx         ERROR'
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  ! main loop
!!$  Xint=0._wp
!!$  do i = 1,15
!!$     Xint = Xint + 0.01_wp
!!$     call SFL_AkimaSpline(X,Y,ierr,Xint,Yint,dYdXInt)
!!$     if (ierr<0) write(*,*) " An ERROR Occured ! "
!!$     write(*,50) Xint,sin(Xint),          &
!!$                 Yint,sin(Xint)-Yint,  &
!!$                 dYdXint,cos(Xint)-dYdXInt
!!$  end do
!!$
!!$  !print footer
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  print *,' '
!!$50 format(' ',F4.2,'    ',F9.7,'          ',F9.7,'      ',F10.7,'         ',F9.7,'      ',F10.7,'       ',F10.7)
!!$
!!$
!!$
!!$  !-----------------------------------------------------------------
!!$  ! Test the complex scalar interface  
!!$  Write(*,*)
!!$  write(*,*) "Testing the complex scalar interface : "
!!$  print *,' '
!!$  print *,'  X   SIN(X) HANDBOOK         Int:Y          ERROR           Int:dy/dx         ERROR'
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  ! main loop
!!$  Xint=0._wp
!!$  do i = 1,15
!!$     Xint = Xint + 0.01_wp
!!$     call SFL_AkimaSpline(X,YC,ierr,Xint,YCint,dYdXCInt)
!!$     if (ierr<0) write(*,*) " An ERROR Occured ! "
!!$     write(*,52) Xint,sin(Xint),          &
!!$                 real(YCint),sin(Xint)-real(YCint),  &
!!$                 real(dYdXCInt),cos(Xint)-real(dYdXCInt)
!!$  end do
!!$
!!$  !print footer
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  print *,' '
!!$52 format(' ',F4.2,'    ',F9.7,'          ',F9.7,'      ',F10.7,'         ',F9.7,'      ',F10.7,'       ',F10.7)
!!$
!!$
!!$  !-----------------------------------------------------------------
!!$  ! Test the Complex vector interface; Test 1 
!!$  write(*,*) "Testing the complex vector interface :"
!!$  XVec = (/ (i*0.01_wp,i=1,15) /)
!!$  call  SFL_AkimaSpline(XXX(:,1),YC,ierr,XVec,YCVec,dYdXCVec)
!!$  if (ierr<0) write(*,*) " An ERROR Occured ! "
!!$  print *,' '
!!$  print *,'  X   SIN(X) HANDBOOK         Int:Y          ERROR           Int:dy/dx         ERROR'
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  do i = 1,15
!!$     write(*,63) XVec(i),sin(XVec(i)),          &
!!$                 YVec(i),sin(XVec(i))-real(YCVec(i)),  &
!!$                 dYdXVec(i),cos(XVec(i))-real(dYdXCVec(i))
!!$  end do
!!$
!!$  !print footer
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  print *,' '
!!$63 format(' ',F4.2,'    ',F9.7,'          ',F9.7,'      ',F10.7,'         ',F9.7,'      ',F10.7,'       ',F10.7)
!!$
!!$
!!$
!!$  !-----------------------------------------------------------------
!!$  ! Test the vector interface; Test 1 
!!$  write(*,*) "Testing the vector interface :"
!!$  XVec = (/ (i*0.01_wp,i=1,15) /)
!!$  call  SFL_AkimaSpline(XXX(:,1),XXX(:,2),ierr,XVec,YVec,dYdXVec)
!!$  if (ierr<0) write(*,*) " An ERROR Occured ! "
!!$  print *,' '
!!$  print *,'  X   SIN(X) HANDBOOK         Int:Y          ERROR           Int:dy/dx         ERROR'
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  do i = 1,15
!!$     write(*,60) XVec(i),sin(XVec(i)),          &
!!$                 YVec(i),sin(XVec(i))-YVec(i),  &
!!$                 dYdXVec(i),cos(XVec(i))-dYdXVec(i)
!!$  end do
!!$
!!$  !print footer
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  print *,' '
!!$60 format(' ',F4.2,'    ',F9.7,'          ',F9.7,'      ',F10.7,'         ',F9.7,'      ',F10.7,'       ',F10.7)
!!$
!!$
!!$
!!$
!!$  !-----------------------------------------------------------------
!!$  ! Test the vector interface; Test 2
!!$  write(*,*) "Testing the vector interface :"
!!$  XVec = (/ (i*0.01_wp,i=1,15) /)
!!$  call  SFL_AkimaSpline(X,Y,ierr,XVec,YVec,dYdXVec)
!!$  if (ierr<0) write(*,*) " An ERROR Occured ! "
!!$  print *,' '
!!$  print *,'  X   SIN(X) HANDBOOK         Int:Y          ERROR           Int:dy/dx         ERROR'
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  do i = 1,15
!!$     write(*,65) XVec(i),sin(XVec(i)),          &
!!$                 YVec(i),sin(XVec(i))-YVec(i),  &
!!$                 dYdXVec(i),cos(XVec(i))-dYdXVec(i)
!!$  end do
!!$
!!$  !print footer
!!$  print *,'------------------------------------------------------------------------------------------'
!!$  print *,' '
!!$65 format(' ',F4.2,'    ',F9.7,'          ',F9.7,'      ',F10.7,'         ',F9.7,'      ',F10.7,'       ',F10.7)
!!$
!!$
!!$end PROGRAM Akima
!!$
!!$
