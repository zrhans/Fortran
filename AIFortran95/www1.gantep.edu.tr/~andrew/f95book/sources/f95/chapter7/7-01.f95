PROGRAM Static_Arrays
!----------------------------------------
! Program to demonstrate the declaration
! and assignment of static arrays.
!----------------------------------------

  IMPLICIT NONE

  INTEGER :: I
  INTEGER :: Numbers(10) = (/ (I, I=1,10) /)
  CHARACTER(LEN=7) :: Names(4)
  REAL :: Alpha(3) = (/ 1.143, 1.178, 1.237 /)

  Names = (/ "   Ayse", " Atakan", "  Fatih", "Yasemin" /)

  PRINT *, Numbers
  PRINT *, Names
  PRINT *, Alpha

END PROGRAM Static_Arrays

