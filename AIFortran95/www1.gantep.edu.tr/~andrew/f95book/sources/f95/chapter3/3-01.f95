PROGRAM Simple_If
!--------------------------------------------------------
! Program to input a value of x and output the evaluated
! function f = x^2 / (x-5).  A check is performed, using
! a simple  IF  statement,  for the  infinite  condition
! x-5 = 0 (we cannot divide by zero!).  
!--------------------------------------------------------

  IMPLICIT NONE
  REAL :: X

  PRINT *, "Enter the value of x"
  READ *, X

  IF ( X-5.0 == 0. ) PRINT *, "x^2 / (x-5) = infinity!"
  IF ( X-5.0 /= 0. ) PRINT *, "x^2 / (x-5) = ", X**2/(X-5.)

END PROGRAM Simple_If
