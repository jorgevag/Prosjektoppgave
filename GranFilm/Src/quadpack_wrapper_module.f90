
!
! NOTE : The toutines in this section is not consistantly using single and double precissions......
!        since the code contains manu constructions like 1.1e-1 etc. insted of 0.11_wp.
!  
!        In the future this should be fixed.....
!
!        Ingve Simonsen, Paris, Jun 2010
!

!------------------------------!
Module Quadpack_Wrapper_Module
!------------------------------!


  
  ! ---------------------------
  ! --- The use statement
  ! ---------------------------
  Use Shared,        only : wp
  Use SFL_Precision, only : sp, dp

  ! --------------------------------------
  ! --- The Publicly avaiable quantities
  ! --------------------------------------
  Public :: Quadpack
  Public :: quadpack_I
  Public :: Set_QuadPack_Error_Limits
  
  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private



  ! --- Some private constants....
  Real(wp), Parameter  :: zero_wp  = 0.0_wp
  Real(wp), Parameter  :: one_wp   = 1.0_wp

  
  ! --- Some numerical parameters (used by the QuadPack integration method)
  !Real(wp),      Parameter         ::  EpsAbs = 1.e-6_wp                           ! Absolute precison in integral
  !Real(wp),      Parameter         ::  EpsRel = 1.e-4_wp                           ! Relative precision in integral
  Real(wp), save   :: EpsAbs = 0.00001_wp    !1.e-5_wp           ! Absolute precison in integral
  Real(wp), save   :: EpsRel = 0.0001_wp     !1.e-4_wp           ! Relative precision in integral




Contains 



  !-----------------------------------------------------------------------!
  Subroutine Set_QuadPack_Error_Limits( Absolute_Error, Relative_Error )
    !-----------------------------------------------------------------------!
    !
    ! --- PURPOSE 
    !    
    !     Set the absolute and relative error limits for the integration routinbes 
    !     to use by GranFilm
    !
    !     AUTHOR  
    !     Ingve Simonse, Paris, Jul 2010
    !
    Implicit None
    Real(wp), Intent(In) :: Absolute_Error
    Real(wp), Intent(In) :: Relative_Error 
    
    EpsAbs  =  Absolute_Error
    EpsRel  =  Relative_Error 

  End Subroutine Set_QuadPack_Error_Limits
  !---------------------------------------!




  !-------------------------------------------------------------------------------------------------------


  !----------------------------------!
  Subroutine Quadpack(F, x1, x2, Int)
  !----------------------------------!
    !
    ! PURPOSE :  package for quadrature adaptative integration
    !
    Use SFL_Precision,         only  : i4b 
    !Use Share_Parameters_Mod,  only  : EpsAbs, EpsRel
    Implicit None
    Real(wp),External         ::    F
    Real(wp), Intent(In)      ::    x1, x2
    Real(wp), Intent(InOut)   ::    Int
    ! --- Locals
    ! ... Parameters
    Integer(i4b), Parameter   ::    limit = 30
    Integer(i4b), Parameter   ::    lenw  = 4*limit    ! lenw  = 4*limit  
    Integer(i4b), Parameter   ::    key   = 4      ! Type of quadrature
    Integer(i4b)              ::    neval,ier,last, istat
    Real(wp)                  ::    abserr
    Integer(i4b), Dimension(:), Allocatable ::  iwork
    Real(wp),     Dimension(:), Allocatable ::  work

    ! Allocation of the working arrays
    Allocate( iwork(limit) )
    Allocate( work(lenw)   )

    select case(wp)
    case(sp)
       Call qag(F,x1,x2,EpsAbs,EpsRel,key,Int,abserr,neval,ier,limit,lenw,last,iwork,work)
       !Call qag(F,x1,x2,EpsAbs,EpsRel,key,Int,abserr,neval,ier)

    case(dp)
       !       print *,'Using single precision, dqag not ready !!!'
       !Call  qag(F,x1,x2,EpsAbs,EpsRel,key,Int,abserr,neval,ier)
       Call dqag(F,x1,x2,EpsAbs,EpsRel,key,Int,abserr,neval,ier,limit,lenw,last,iwork,work)

    End select



    ! --- Check for Errors
    if (ier>0) then

       if (wp==sp) Write(*,*) " --- An error accured in the QuadPack routine QAG!"
       if (wp==dp) Write(*,*) " --- An error accured in the QuadPack routine DQAG!"

       if (ier == 1 ) then
          Write(*,*) "     Maximum number of subdivisions allowed has been achieved."    
       endif

       if (ier == 2) then
          Write(*,*) "     The occurrence of roundoff error is detected, which prevents "
          Write(*,*) "     the requested tolerance from being achieved."
       endif

       if (ier == 3) then
          Write(*,*) "     Extremely bad integrand behaviour occurs at some points "
          Write(*,*) "     of the integration interval."
       endif

       if (ier == 6 ) then
          Write(*,*) "     The input is invalid. Chack documentation!"   
       endif
    endif


   ! Error code in Quadpack
    !   If(ier/=0) Then
    !   ! Output of some control Parameters
    !    Open(unit=53,file='error_int.dat',status='unknown')
    !    Write(unit=53,fmt='(i2,3(e15.5),2i5)')  ier,Int,abserr/abs(Int),abserr,neval,last
    !   Endif

    ! Deallocation of arrays
    Deallocate( iwork, stat=ISTAT )
    Deallocate(  work, stat=ISTAT )

  End Subroutine Quadpack
  !----------------------------------!


  !-------------------------------------------------------------------------------------------------------

!!$
!!$  Subroutine Quadpack_I(F,bound,Int)
!!$    Use SFL_Precision,         only  : i4b 
!!$    !Use Share_Parameters_Mod,  only  : EpsAbs, EpsRel
!!$    Implicit None
!!$    ! Subroutine for Calling the package of quadrature adaptative 
!!$    ! integration over infinite interval : a,+inf
!!$
!!$    Real(wp),External                 ::      F
!!$    Real(wp), Intent(In)              ::      bound
!!$    Real(wp), Intent(InOut)           ::      Int
!!$    ! Local
!!$    Real(wp)                          ::      abserr
!!$    Integer(i4b), Parameter           ::      limit = 50
!!$    Integer(i4b), Parameter           ::      lenw  = 200 !   lenw  = 4*limit
!!$    Integer(i4b)                      ::      neval,ier,last, istat
!!$    Integer(i4b),Allocatable          ::      iwork(:)
!!$    Real(wp),Allocatable              ::      work(:)
!!$
!!$
!!$    ! Allocation of the working arrays
!!$    Allocate(iwork(limit),work(lenw))
!!$
!!$
!!$    select case(wp)
!!$    case(sp)
!!$       !NICK   Call qagi(F,bound,1,EpsAbs$,EpsRel$,Int,abserr,neval,ier,limit,lenw,last,iwork,work)
!!$       Call qagi(F,bound,1,EpsAbs,EpsRel,Int,abserr,neval,ier)
!!$    case(dp)
!!$       !       print *,'Using single precision, dqagi not ready !!!'
!!$       Call  qagi(F,bound,1,EpsAbs,EpsRel,Int,abserr,neval,ier)
!!$       !       Call dqagi(F,bound,1,EpsAbs$,EpsRel$,Int,abserr,neval,ier,limit,lenw,last,iwork,work)
!!$    End select
!!$
!!$    ! Error code in Quadpack
!!$    !   If(ier/=0) Then
!!$    !   ! Output of some control Parameters
!!$    !    Open(unit=53,file='error_int_inf.dat',status='unknown')
!!$    !    Write(unit=53,fmt='(i2,3(E15.5),2i5)') &
!!$    !      ier,Int,abserr/abs(Int),abserr,neval,last
!!$    !   Endif
!!$
!!$    ! Deallocation of arrays
!!$    Deallocate(iwork,work,stat=ISTAT)
!!$
!!$  End Subroutine Quadpack_I


End Module Quadpack_Wrapper_Module
