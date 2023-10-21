PROGRAM Cylinder_Area_Volume
!--------------------------------------------------
! This program computes the total surface area and
! the  volume of a cylinder  given the height, and
! radius of the base, is input from the keyboard.
!--------------------------------------------------

  USE Cylinder_Mod

  IMPLICIT NONE
  
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
