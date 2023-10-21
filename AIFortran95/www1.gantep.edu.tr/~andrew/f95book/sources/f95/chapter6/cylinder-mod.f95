MODULE Cylinder_Mod

  USE Constants_Mod, ONLY: PI

  IMPLICIT NONE

CONTAINS

  PURE SUBROUTINE Cylinder(H, R, A, V)
    REAL, INTENT(IN) :: H, R
    REAL, INTENT(OUT) :: A, V
    A = Cyl_Area(H, R)
    V = Cyl_Volume(H, R)
  END SUBROUTINE Cylinder

  PURE FUNCTION Cyl_Area(H, R)
    REAL :: Cyl_Area
    REAL, INTENT(IN) :: H, R
    Cyl_Area = 2.*Base(R) + Side(R,H)

    CONTAINS

      PURE FUNCTION Side(R,H)
        REAL :: Side
        REAL, INTENT(IN) :: H, R
        Side = 2.*PI*R*H
      END FUNCTION Side

  END FUNCTION Cyl_Area

  PURE FUNCTION Cyl_Volume(H, R)
    REAL :: Cyl_Volume
    REAL, INTENT(IN) :: H, R
    Cyl_Volume = Base(R)*H
  END FUNCTION Cyl_Volume

  PURE FUNCTION Base(R)
    REAL :: Base
    REAL, INTENT(IN) :: R
    Base = PI*R**2
  END FUNCTION Base

END MODULE Cylinder_Mod
