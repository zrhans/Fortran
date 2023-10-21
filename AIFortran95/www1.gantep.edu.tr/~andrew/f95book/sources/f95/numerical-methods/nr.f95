PROGRAM Root_Finding
!-------------------------------------
! Newton-Raphon iterative formula for
! finding the roots of a function.
!-------------------------------------
  IMPLICIT NONE
  REAL (KIND=8) :: x, Error, Tolerance

  PRINT *, "Input an initial root estimate."
  READ *, x
  
  PRINT *, "Input the tolerance."
  READ *, Tolerance

  DO
    Error = F(x) / D(x)                  ! The error estimate
    IF ( ABS(Error) < Tolerance ) EXIT   ! Terminate if tolerance is satisfied
    x = x - Error                        ! Subtract the error estimate
  END DO

  PRINT *, "A root is ", x
  PRINT *, "The estimated error is ", Error

CONTAINS

  REAL (KIND=8) FUNCTION f(x)
  REAL (KIND=8), INTENT(IN) :: x
  f = 2*x**3 - 3*x**2 - 9*x + 10
  END FUNCTION f

  REAL (KIND=8) FUNCTION d(x)
  REAL (KIND=8), INTENT(IN) :: x
  d = 6*x**2 - 6*x - 9
  END FUNCTION d

END PROGRAM Root_Finding
