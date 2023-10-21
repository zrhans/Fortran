PROGRAM Coordinates
!--------------------------------------------------------
! Program implementing  explicit external  functions for
! converting Polar coordinates to Cartesian coordinates. 
!--------------------------------------------------------

  IMPLICIT NONE

  INTERFACE Cartesian_X_Interface
    FUNCTION Cartesian_X(Radius, Angle)
      REAL :: Cartesian_X
      REAL, INTENT(IN) :: Radius, Angle
    END FUNCTION Cartesian_X
  END INTERFACE Cartesian_X_Interface

  INTERFACE Cartesian_Y_Interface
    FUNCTION Cartesian_Y(Radius, Angle)
      REAL :: Cartesian_Y
      REAL, INTENT(IN) :: Radius, Angle
    END FUNCTION Cartesian_Y
  END INTERFACE Cartesian_Y_Interface

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

END PROGRAM Coordinates

FUNCTION Cartesian_X(Radius, Angle)
  IMPLICIT NONE 
  REAL :: Cartesian_X
  REAL, INTENT(IN) :: Radius, Angle
  Cartesian_X = Radius * COS(Angle)
END FUNCTION Cartesian_X

FUNCTION Cartesian_Y(Radius, Angle)
  IMPLICIT NONE 
  REAL :: Cartesian_Y
  REAL, INTENT(IN) :: Radius, Angle
  Cartesian_Y = Radius * SIN(Angle)
END FUNCTION Cartesian_Y
