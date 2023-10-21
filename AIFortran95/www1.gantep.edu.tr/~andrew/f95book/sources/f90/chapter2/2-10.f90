PROGRAM Trigonometric_Functions
!-------------------------------------------------------
! This program inputs an angle in  degrees and  outputs
! the  sine,  cosine  and  tangent  of the angle  using
! intrinsic functions SIN, COS and TAN. These functions
! assume the angle  to be in radians so a conversion is
! performed before the functions are called.
!-------------------------------------------------------

  IMPLICIT NONE
  REAL, PARAMETER :: PI = 3.141593
  REAL :: Degrees, Radians

  PRINT *, "Type the angle in degrees"
  READ *, Degrees

  Radians = Degrees * PI/180.

  PRINT *, "   The sine of ", Degrees, " degrees is ", SIN(Radians)
  PRINT *, " The cosine of ", Degrees, " degrees is ", COS(Radians)
  PRINT *, "The tangent of ", Degrees, " degrees is ", TAN(Radians)

END PROGRAM Trigonometric_Functions

