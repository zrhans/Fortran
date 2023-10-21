PROGRAM Resultant_Force
!-------------------------------------------------------------------
! The resultant magnitude and direction of four forces is computed.
!
! Initial Data:
!
!    Force - A vector of the 4 forces (magnitudes).
!    Angle - A vector of the 4 force angles.
!
! Derived values:
!
!   Fx, Fy - The 4 component forces.
!   Rx, Ry - The 4 resultant component forces.
!  R_Force - The resultant force (magnitude).
!  R_Angle - The resultant force (direction).
!-------------------------------------------------------------------

  IMPLICIT NONE
  INTEGER :: I
  REAL,PARAMETER :: PI = 3.14159265
  REAL,DIMENSION(4) :: Force, Angle
  REAL              :: Fx, Fy
  REAL              :: Rx, Ry
  REAL              :: R_Force, R_Angle

  ! Initial data: the magnitudes and directions of the 4 forces.
  Force = (/ 150., 120., 100., 115. /)
  Angle = (/  30., 340.,  90., 180. /)

  ! Determine the component forces and sum them.
  Rx = 0. ; Ry = 0.
  DO I = 1 , 4
   Fx = Force(I) * COS( Angle(I) * PI/180.)
   Fy = Force(I) * SIN( Angle(I) * PI/180.)
   Rx = Rx + Fx
   Ry = Ry + Fy
  END DO

  ! Determine the magnitude of the resultant force and its direction.
  R_Force = SQRT( Rx**2 + Ry**2 )
  R_Angle = 180./PI * ATAN(Ry/Rx)

  ! Output the results
  PRINT *, "The magnitude of resultant force is ", R_Force
  PRINT *, "The angle of resultant force is ", R_Angle

END PROGRAM Resultant_Force
