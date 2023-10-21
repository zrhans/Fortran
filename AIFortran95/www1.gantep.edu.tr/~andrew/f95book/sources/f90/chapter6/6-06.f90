MODULE Area_Module

  IMPLICIT NONE
  REAL,PARAMETER :: PI = 3.14159265

CONTAINS

  REAL FUNCTION Circle(R)
    REAL,INTENT(IN) :: R
    Circle = PI*R**2
  END FUNCTION circle

  REAL FUNCTION Triangle(B, H)
    REAL,INTENT(IN) :: B, H
    Triangle = 0.5*B*H
  END FUNCTION Triangle

  REAL FUNCTION Rectangle(S1, S2)
    REAL,INTENT(IN) :: S1,S2
    Rectangle = S1*S2
  END FUNCTION Rectangle

END MODULE Area_Module

PROGRAM Composite_Area
!-----------------------------------------------
! Program to compute the total area of a member
! composed of N parts.  The dimensions  of each
! part are input. Parts may be of type "circle"
! "rectangle" and "triangle".
!----------------------------------------------

  USE Area_Module

  IMPLICIT NONE
  REAL :: Circle_Radius
  REAL :: Triangle_Base, Triangle_Height
  REAL :: Rectangle_Side1, Rectangle_Side2
  REAL :: Area, Total_Area
  INTEGER :: N, I
  CHARACTER(LEN=1) :: Type

  WRITE(UNIT=*,FMT='(A)',ADVANCE="NO") &
  "Enter the number parts of the composite member: "
  READ *, N

  PRINT *, " "
  PRINT *, "Available part types are:"
  PRINT *, " "
  PRINT *, " C - for a circle   "
  PRINT *, " R - for a rectangle"
  PRINT *, " T - for a triangle "

  Total_Area = 0.

  DO I = 1, N

    5 PRINT *, " "
    WRITE(UNIT=*,FMT='(A,I2,A)',ADVANCE="NO") &
    "Enter part type code (C, R, or T) for part ", I, " : "
    READ *, Type

    IF ( Type == "C" .OR. Type == "c" ) THEN

      WRITE(UNIT=*,FMT='(A)',ADVANCE="NO") &
      "Enter the radius of the circle : "
      READ *, Circle_Radius
      Area = Circle(Circle_Radius)

    ELSE IF ( Type == "T" .OR. type == "t" ) THEN

      WRITE(UNIT=*,FMT='(A)',ADVANCE="NO") &
      "Enter the base and height of the triangle : "
      READ *, Triangle_Base, Triangle_Height
      Area = Triangle(Triangle_Base, Triangle_Height)

    ELSE IF ( Type == "R" .OR. Type == "r" ) THEN

      WRITE(UNIT=*,FMT='(A)',ADVANCE="NO") &
      "Enter the side 1 and side 2 of the rectangle : "
      READ *, Rectangle_Side1, Rectangle_Side2
      Area = Rectangle(Rectangle_Side1, Rectangle_Side2)

    ELSE

      PRINT *, "You entered wrong type code! Try again."
      GOTO 5

    END IF

    Total_Area = Total_Area + Area

  END DO

  PRINT *, " "
  PRINT *, "The total area of the composite member is ", Total_Area

END PROGRAM Composite_Area
