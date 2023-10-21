MODULE Polar_to_Cart

  PRIVATE :: PI
  REAL, PARAMETER :: PI = 3.14159265

CONTAINS

  REAL FUNCTION Cartesian_X(Radius, Angle)
    REAL, INTENT(IN) :: Radius, Angle
    Cartesian_X = Radius * COS(Angle*PI/180.)
  END FUNCTION Cartesian_X

  REAL FUNCTION Cartesian_Y(Radius, Angle)
    REAL, INTENT(IN) :: Radius, Angle
    Cartesian_Y = Radius * SIN(Angle*PI/180.)
  END FUNCTION Cartesian_Y

END MODULE Polar_to_Cart

PROGRAM Coordinates
!--------------------------------------------------------
! Program implementing module functions for converting
! Polar coordinates to Cartesian coordinates. 
!--------------------------------------------------------

  USE Polar_to_Cart

  IMPLICIT NONE
  REAL :: Radius, Angle, X, Y

  PRINT *, "Enter radius and angle(in degrees) of the point."
  READ *,  Radius, Angle

  PRINT *, "The point is (", Radius, ",", Angle,&
           ") in polar coordinates"

  X = Cartesian_X(Radius, Angle)
  Y = Cartesian_Y(Radius, Angle)

  PRINT *, "The point is (", X, ",", Y,&
           ") in cartesian coordinates"

END PROGRAM Coordinates
