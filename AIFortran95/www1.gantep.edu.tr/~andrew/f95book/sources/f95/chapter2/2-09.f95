PROGRAM Degress_to_Radians
!---------------------------------------------------
! This program inputs an angle in degrees, converts
! the angle to  radians, and  outputs  the  result.
! Note the use of a "named constant", PI
!---------------------------------------------------

  IMPLICIT NONE
  REAL, PARAMETER :: PI = 3.141593
  REAL :: Degrees, Radians

  PRINT *, "Type the angle in degrees"
  READ *, Degrees

  Radians = Degrees * PI/180.

  PRINT *, Degrees, " degrees = ", Radians, " radians."

END PROGRAM Degress_to_Radians

