PROGRAM If_Else
!--------------------------------------------------------
! Program to input a value of x and output the evaluated
! function f = x^2 / (x-5).  A check is performed, using
! an  IF-ELSE  statement,  for the  infinite   condition
! x-5 = 0 (we cannot divide by zero!).
!--------------------------------------------------------

  IMPLICIT NONE
  REAL :: X

  PRINT *, "Enter the value of x"
  READ *, X

  IF ( X-5. == 0. ) THEN
    PRINT *, "x^2 / (x-5) = infinity!"
  ELSE
    PRINT *, "x^2 / (x-5) = ", X**2/(X-5.)
  END IF

END PROGRAM If_Else
