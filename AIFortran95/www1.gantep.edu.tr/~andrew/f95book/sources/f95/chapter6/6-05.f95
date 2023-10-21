MODULE Cartesian_Mod

  IMPLICIT NONE

CONTAINS

  REAL FUNCTION Cartesian_X(Radius, Angle)
    REAL, INTENT(IN) :: Radius, Angle
    Cartesian_X = Radius * COS(Angle)
  END FUNCTION Cartesian_X

  REAL FUNCTION Cartesian_Y(Radius, Angle)
    REAL, INTENT(IN) :: Radius, Angle
    Cartesian_Y = Radius * SIN(Angle)
  END FUNCTION Cartesian_Y

END MODULE Cartesian_Mod

PROGRAM COORDINATES

!------------------------------------------------------
! Program implementing module functions for converting
! Polar coordinates to Cartesian coordinates. 
!------------------------------------------------------

  USE Cartesian_Mod, C_X => Cartesian_X, C_Y => Cartesian_Y

  IMPLICIT NONE
  REAL :: Radius, Angle, X, Y
  REAL, PARAMETER :: PI = 3.14159265

  PRINT *, "Enter radius and angle(in degrees) of the point."
  READ *,  Radius, Angle

  PRINT *, "The point is (", Radius, ",", Angle,&
           ") in polar coordinates"

  Angle = Angle * PI / 180.
  X = C_X(Radius, Angle)
  Y = C_Y(Radius, Angle)

  PRINT *, "The point is (", X, ",", Y,&
           ") in cartesian coordinates"

END PROGRAM COORDINATES
