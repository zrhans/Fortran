PROGRAM Cylinder_Area_Volume
!--------------------------------------------------
! This program computes the total surface area and
! the  volume of a cylinder  given the height, and
! radius of the base, is input from the keyboard.
! An internal subroutine is used.
!--------------------------------------------------

  IMPLICIT NONE
  
  REAL, PARAMETER :: PI = 3.14159265
  REAL :: Height, Radius ! the inputs
  REAL :: Area,Volume    ! the results

  PRINT *, "Input the height of the cylinder."
  READ *, Height
  PRINT *, "Input the radius of the cylinder."
  READ *, Radius

  CALL Cylinder(Height, Radius, Area, Volume)

  PRINT *, "The total surface area is ", Area 
  PRINT *, "The volume is ", Volume

CONTAINS

  SUBROUTINE Cylinder(H, R, A, V)
    REAL, INTENT(IN) :: H, R
    REAL, INTENT(OUT) :: A, V
    A = 2.*PI*R**2 + 2.*PI*R*H
    V = PI*R**2*H
  END SUBROUTINE Cylinder

END PROGRAM Cylinder_Area_Volume
