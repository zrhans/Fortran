PROGRAM Numerical_Differentiation
!------------------------------------------------
! Program to approximate the first derivative of
! a function f(x) using the  "central-difference
! approximation" (CDA) method.
!------------------------------------------------
  IMPLICIT NONE
  REAL(KIND=8) :: x, h         ! Inputs
  REAL(KIND=8) :: CDA          ! Estimate of the first derivative

  PRINT *, "Input x "
  READ *, x
  PRINT *, "Input h "
  READ *, h

  CDA = (f(x+h)-f(x-h))/(2*h) ! CDA
  PRINT *, "The derivative at ", x, " is ", CDA

CONTAINS

  REAL(KIND=8) FUNCTION f(x)
  REAL(KIND=8), INTENT(IN) :: x
  f = 2*x**3 - 3*x**2 - 9*x + 10
  END FUNCTION f

END PROGRAM Numerical_Differentiation
