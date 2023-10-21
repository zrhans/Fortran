PROGRAM Evaluate
!--------------------------------------------------
! Program to evaluate a function of two variables.
!--------------------------------------------------

  IMPLICIT NONE
  REAL :: X, Y, F

  PRINT *, "Enter the values of x and y" 
  READ *, X, Y

  F = EXP(X) / (Y**2 + X**2) - SQRT(X**4 + ABS(Y))

  PRINT *, "When x = ", X, " and y = ", Y
  PRINT *, "f(x,y) = ", F

END PROGRAM Evaluate

