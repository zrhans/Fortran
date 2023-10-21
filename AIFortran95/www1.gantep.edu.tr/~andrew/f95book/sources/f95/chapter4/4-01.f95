PROGRAM Summation
!----------------------------
! Summation of the values 28
! to 1012 using a DO loop.
!----------------------------

  IMPLICIT NONE
  INTEGER :: I, S

  S = 0
  DO I = 28, 1012
    S = S + I
  END DO

  PRINT *, "The Sum is ", S

END PROGRAM Summation
