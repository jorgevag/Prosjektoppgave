Module SFL_Precision
  ! 
  ! This module defines common precissions 
  ! It is modelled after Numerical Recipes nrtype.
  !

  Integer, Parameter :: I4B = Selected_int_kind(9)
  Integer, Parameter :: I2B = Selected_int_kind(4)
  Integer, Parameter :: I1B = Selected_int_kind(2)
  Integer, Parameter :: SP  = Kind(1.0)
  Integer, Parameter :: DP  = Kind(1.0D0)
  Integer, Parameter :: SPC = Kind((1.0,1.0))
  Integer, Parameter :: DPC = Kind((1.0D0,1.0D0))
  Integer, Parameter :: LGT = Kind(.True.)

End Module SFL_Precision
