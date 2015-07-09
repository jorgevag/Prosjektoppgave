module SFL_ISO_Fortran_Env

  ! Non-Intrinsic version for Lahey/Fujitsu Fortran for Linux.
  ! See Subsection 13.8.2 of the Fortran 2003 standard.
  !
  ! Comments by Ingve Simonsen (Mar 2007)
  !
  ! NOTE : Before F2003, the module ISO_FORTRAN_ENV is NOT standard.
  !
  !
  ! NOTE : From Metcalf et al. say in sect. 9.7 regarding F95:
  !        The F95 standrd says:
  !              iostat = 0    : NO error
  !              iostat > 0    : An error occurred
  !              iostat < 0    : An end of file of recored occurred 
  !
  !        Hence, the standard specifies not the values, but only the
  !        sign of iostat......!!!!
  !
  !
  ! Therefore the values in this routine will be Compiler dependent before 
  ! they are standardiced in F2003. this should be taken into consideration 
  ! when using these routines.... 
  !
  ! In particular the calies for IOSTAT_END and IOSTAT_EOR have been found to 
  ! varry greatly, while those for Error_Unit, Input_Unit, and Output_Unit, 
  ! seem to be quite starndard.
  !
  ! 

  implicit NONE
  public

  integer, parameter :: Error_Unit = 0             ! StdError unit  
  integer, parameter :: Input_Unit = 5             ! StdIn unit 
  integer, parameter :: Output_Unit = 6            ! StdOut unit 

  integer, parameter :: Numeric_Storage_Size = 32  ! bit size of a scalar numeric varable  
  integer, parameter :: Character_Storage_Size = 8 ! bit size of a single character varable  
  integer, parameter :: File_Storage_Size = 8      ! bit size of a file storage unit   

  integer, parameter :: IOSTAT_END = -1            ! value assinged to ISOSTAT= on end-of-file
  integer, parameter :: IOSTAT_EOR = -2            ! value assinged to ISOSTAT= on end-of-record


end module SFL_ISO_FORTRAN_ENV
