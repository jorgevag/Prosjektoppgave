module ISO_FORTRAN_ENV

  ! Nonintrinsic version for Lahey/Fujitsu Fortran for Linux.
  ! See Subclause 13.8.2 of the Fortran 2003 standard.
  
  ! Comments by Ingve Simonsen (Mar 2007)
  
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


end module ISO_FORTRAN_ENV 
