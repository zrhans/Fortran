PROGRAM MC_Int
!----------------------------------------
! Monte-Carlo integration of a function.
!----------------------------------------
  IMPLICIT NONE
  INTEGER :: I, M, N=10000
  REAL :: X, Y, Ymax, a, b

  Ymax = 14.26013  ! f(-0.82287566)
  a = -1.0
  b = +1.0

  M = 0
  DO I = 1, N
    CALL RANDOM_NUMBER(X)
    CALL RANDOM_NUMBER(Y)
    X = a + (b-a)*X
    Y = Ymax*Y
    IF ( Y < f(X) ) M=M+1
  END DO

  PRINT *, (b-a)*Ymax*M/N

CONTAINS

  REAL FUNCTION f(x)
  REAL, INTENT(IN) :: x
  f = 2*x**3 - 3*x**2 - 9*x + 10
  END FUNCTION f

END PROGRAM MC_Int
