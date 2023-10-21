PROGRAM Nested_If_Else
!---------------------------------------------
! Program to evaluate the composite function:
!
!         x < -1    f(x) = 0
!   -1 <= x <= 0    f(x) = 1+x
!         x > 0     f(x) = 1/|x-1|
!
!---------------------------------------------

  IMPLICIT NONE
  REAL :: X, F

  PRINT *, "Enter the value of x"
  READ *, X

  IF ( X > 0. ) THEN

    IF ( X-1. == 0. ) THEN
      PRINT *, "When x = ", X, " f(x) is infinite" 
    ELSE 
      F = 1. / ABS(X-1.)
      PRINT *, "When x = ", X, " f(x) = ", F 
    END IF

  ELSE

    IF ( X < -1. ) THEN
      PRINT *, "When x = ", X, " f(x) = ", 0.
    ELSE 
      F = 1.+X
      PRINT *, "When x = ", X, " f(x) = ", F 
    END IF

  END IF

END PROGRAM Nested_If_Else
