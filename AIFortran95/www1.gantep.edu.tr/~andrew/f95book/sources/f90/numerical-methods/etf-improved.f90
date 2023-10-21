PROGRAM Numerical_Integration
!------------------------------------------------------
! Program to perform  numerical integration of a
! function f(x) between the limits a and b using
! the "Extended Trapezoidal Formula" (ETF).
!
! Three features are to be noted:
!
! 1. The integral is repeated for 2n intervals until
!    the estimated error is less than a tolerance.
! 2. The program does not repeat previous function
!    evaluations: DO L=1,n-1,2
! 3. A final step improves the integral estimate by
!    subtracting the estimated error: (ETF_n-ETF_2n)/3
!------------------------------------------------------
  IMPLICIT NONE
  INTEGER :: I, N
  REAL(KIND=8) :: a, b, h, x, etf, etf2, error, Tolerance
 
  PRINT *, "Input the limits 'a' and 'b'"
  READ *, a, b
  PRINT *, "Input the tolerance."
  READ *, Tolerance

  n=1
  etf = 0.0_8
  etf2 = ( f(a) + f(b) ) / 2     ! Sum the end points

  DO

    n=n*2                        ! Double the number of intervals.
    h = (b-a) / n                ! The interval size.

    DO i = 1, n-1,2
      x = a + i*h                ! Calculate the evaluation position.
      etf2 = etf2 + f(x)         ! Sum over the remaining points.
    END DO

    error = (etf-etf2*h)/3       ! Compute the error.
    etf=etf2*h                   ! Save the result.

    IF ( ABS(error) < Tolerance ) EXIT ! Terminate is error < tolerance.

  END DO

  PRINT *, " The integral estimate is ", etf
  PRINT *, " The error estimate is    ", error
  PRINT *, " Subtracting the error => ", etf-error

CONTAINS

  REAL(KIND=8) FUNCTION f(x)
  REAL(KIND=8), INTENT(IN) :: x
  f = 2*x**3 - 3*x**2 - 9*x + 10
  END FUNCTION f

END PROGRAM Numerical_Integration
