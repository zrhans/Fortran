PROGRAM Semi_Dynamic_Arrays
!----------------------------------------
! Subprogram that swaps  the contents of
! two type integer arrays of equal size.
!----------------------------------------

  IMPLICIT NONE

  INTEGER :: A(5) = (/ 37, 12, 67, 18, 4 /)
  INTEGER :: B(5) = (/ 14, 15, 85,  9, 8 /)

  PRINT *, "Before swapping:"
  PRINT *, "A = ", A
  PRINT *, "B = ", B

  CALL Swap(A,B)

  PRINT *, "After swapping:"
  PRINT *, "A = ", A
  PRINT *, "B = ", B

CONTAINS

  SUBROUTINE Swap(A,B)

    INTEGER, INTENT(INOUT) :: A(:), B(:)
    INTEGER :: Temporary(SIZE(A))

    Temporary = A
    A = B
    B = Temporary

  END SUBROUTINE Swap

END PROGRAM Semi_Dynamic_Arrays

