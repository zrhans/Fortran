PROGRAM Cylinder_Area_Volume
!--------------------------------------------------
! This program computes the total surface area and
! the  volume of a cylinder  given the height, and
! radius of the base, is input from the keyboard.
! An (explicit) external subroutine is used.
!--------------------------------------------------

  IMPLICIT NONE
  
  INTERFACE Cylinder
    SUBROUTINE Cylinder(H, R, A, V)
      REAL, INTENT(IN) :: H, R
      REAL, INTENT(OUT) :: A, V
    END SUBROUTINE Cylinder
  END INTERFACE Cylinder

  REAL :: Height, Radius ! the inputs
  REAL :: Area,Volume    ! the results

  PRINT *, "Input the height of the cylinder."
  READ *, Height
  PRINT *, "Input the radius of the cylinder."
  READ *, Radius

  CALL Cylinder(Height, Radius, Area, Volume)

  PRINT *, "The total surface area is ", Area 
  PRINT *, "The volume is ", Volume

END PROGRAM Cylinder_Area_Volume

SUBROUTINE Cylinder(H, R, A, V)

  IMPLICIT NONE

  REAL, PARAMETER :: PI = 3.14159265
  REAL, INTENT(IN) :: H, R
  REAL, INTENT(OUT) :: A, V
  A = 2*PI*R**2 + 2*PI*R*H
  V = PI*R**2*H

END SUBROUTINE Cylinder
