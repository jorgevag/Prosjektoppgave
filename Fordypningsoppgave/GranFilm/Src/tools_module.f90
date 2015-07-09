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


!-------------------!
Module Tools_Module
!-------------------!



  ! --- The Use Statements global to the module
  Use Shared, only : wp	   


  ! --------------------------------------
  ! --- The Publicly avaiable routines
  ! --------------------------------------
  Public :: Get_Amp_Sq_Phase
  Public :: Deg2Rad
  Public :: Rad2Deg
  Public :: Get_Phase
  Public :: Get_Phase_in_Degrees
  Public :: tolower


  ! --------------------------------------
  ! --- Everyting is Private by default
  ! --------------------------------------
  Private

  

!-------!
Contains	
!-------!



  !-----------------------------------------------!
  Function Get_Amp_sq_Phase( Data ) Result( res )
  !-----------------------------------------------!
    !
    ! --- Returns the amplitude and phase of 
    !     the complex input array
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    Use shared, only : pi, imu
    Implicit None
    Complex(wp), dimension(:), intent(in)   :: Data
    Real(wp),   dimension(Size(Data,1),2)   :: res
    ! --- Local

    ! --- The amplitude
    res(:,1)  = abs( Data )**2
    ! --- The phase
    res(:,2)  = atan2( real(-imu*Data, wp), Real(Data,wp) ) *  rad2deg()

  End Function Get_Amp_sq_Phase
  !---------------------------------------------------------------!

  



  !-------------------------------------------------!
  Elemental Function Get_Phase( z )  Result(res)
  !-------------------------------------------------!
    !
    ! --- Defines the phase in radians of the 
    !     complex number z
    !     
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    Use shared, only : pi,imu
    Implicit None
    Complex(wp),   Intent(In)  :: z
    Real(wp)                   :: res
    
    res  = atan2( real(-imu*z, wp), Real(z,wp) ) 

  End Function Get_Phase
  !--------------------------------------------------!




  !-------------------------------------------------------!
  Elemental Function Get_Phase_in_Degrees( z )  Result(res)
  !--------------------------------------------------------!
    !
    ! --- Defines the phase in degrees of the 
    !     complex number z
    !     
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    Use shared, only : pi,imu
    Implicit None
    Complex(wp),   Intent(In)  :: z
    Real(wp)                   :: res
    
    res  = atan2( real(-imu*z, wp), Real(z,wp) ) *  rad2deg()

  End Function Get_Phase_in_Degrees
  !--------------------------------------------------!
  




  !-------------------------------------------------!
  Pure Function Deg2Rad( )  Result(res)
  !-------------------------------------------------!
    !
    ! --- The multiplicative factor used to convert an
    !     angle from degrees to radians
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    Use shared, only : pi
    Implicit None
    Real(wp)               :: res
    
    Res = pi / 180._wp 

  End Function Deg2Rad
  !--------------------------------------------------!





  !-------------------------------------------------!
  Pure Function Rad2Deg( )  Result(res)
  !-------------------------------------------------!
    !
    ! --- The multiplicative factor used to convert an
    !     angle from radians to degree
    !
    !     Ingve Simonsen, Paris, Jul 2010
    !
    Use shared, only : pi
    Implicit None
    Real(wp)               :: res

    Res =  180._wp / pi

  End Function Rad2Deg
  !--------------------------------------------------!



  !---------------------------------------------!
  function tolower(in_string) result(out_string)
  !---------------------------------------------!
     !---------------------------------------------------------------------
     ! ROUTINE   :  Upper2lower 
     ! TYPE      :  Function
     ! SYSTEM    :  Fortran 90
     !
     ! PURPOSE   :  Tranlate a character variable to all lowercase letters.
     !              Non-alphabetic characters are not affected.  
     !
     ! SYNTAX    :  tolower
     !---------------------------------------------------------------------
     ! USES  Lib :  
     !       Mod : 
     !---------------------------------------------------------------------
     ! DATE      : 98.08.18
     ! LAST MOD  :  
     ! AUTHER    : Ingve Simonsen, Theoreticcal Physics, NTNU
     ! E-mail    : ingves@phys.ntnu.no
     !---------------------------------------------------------------------
     implicit none
     character(len=*)               :: in_string
     character(len=len(in_string))  :: out_string
     integer                        :: i,ascii
     do i=1,len(in_string)
        ascii = ichar(in_string(i:i))
        if ((ascii>64).and.(ascii<91)) THEN
           out_string(i:i) = char(ascii+32)
        else
           out_string(i:i) = in_string(i:i)
        endif
     enddo
     return
   end function tolower
   !---------------------------------------------!



End Module Tools_Module
!----------------------!
