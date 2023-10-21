PROGRAM Circle
!----------------------------------------
! Calculation of the area of a circle of
! unit radius by the Monte-Carlo method.
!----------------------------------------
  IMPLICIT NONE
  REAL :: X, Y
  INTEGER :: M, N, I

  N=10000
  M=0

  DO I = 1, N
    CALL RANDOM_NUMBER(X)
    CALL RANDOM_NUMBER(Y)
    IF ( X**2+Y**2 < 1.0 ) M=M+1 
  END DO

  PRINT *, 4.0*M/N

END PROGRAM Circle

