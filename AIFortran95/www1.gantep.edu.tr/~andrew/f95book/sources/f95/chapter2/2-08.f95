PROGRAM Feet_to_Metres
!------------------------------------------------
! This program inputs a length in feet, converts
! the length to metres, and outputs the result.
!------------------------------------------------

  IMPLICIT NONE
  REAL :: Feet, Metres

  PRINT *, "Type the length in Feet"
  READ *, Feet

  Metres = Feet * 0.3048

  PRINT *, Feet, " feet = ", Metres, " metres."

END PROGRAM Feet_to_Metres

