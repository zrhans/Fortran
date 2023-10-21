PROGRAM Coordinates
!--------------------------------------------------------
! Program implementing internal functions for converting
! Polar coordinates to Cartesian coordinates. 
!--------------------------------------------------------

  IMPLICIT NONE
  REAL :: Radius, Angle, X, Y
  REAL, PARAMETER :: PI = 3.14159265

  PRINT *, "Enter radius and angle(in degrees) of the point."
  READ *,  Radius, Angle

  PRINT *, "The point is (", Radius, ",", Angle,&
           ") in polar coordinates"

  Angle = Angle * PI / 180.
  X = Cartesian_X(Radius, Angle)
  Y = Cartesian_Y(Radius, Angle)

  PRINT *, "The point is (", X, ",", Y,&
           ") in cartesian coordinates"

CONTAINS

  REAL FUNCTION Cartesian_X(Radius, Angle)
    REAL, INTENT(IN) :: Radius, Angle
    Cartesian_X = Radius * COS(Angle)
  END FUNCTION Cartesian_X

  REAL FUNCTION Cartesian_Y(Radius, Angle)
    REAL, INTENT(IN) :: Radius, Angle
    Cartesian_Y = Radius * SIN(Angle)
  END FUNCTION Cartesian_Y

END PROGRAM Coordinates
