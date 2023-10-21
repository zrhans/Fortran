PROGRAM Numerical_Integration
!------------------------------------------------
! Program to perform  numerical integration of a
! function f(x) between the limits a and b using
! the "Extended Trapezoidal Formula" (ETF).
!------------------------------------------------
  IMPLICIT NONE

  INTEGER :: I, N
  REAL(KIND=8) :: a, b, h, x, etf
 
  PRINT *, "Input the limits 'a' and 'b'"
  READ *, a, b

  PRINT *, "Input the number of trapezoidal sections, n"
  READ *, N

  h = (b-a) / N                    ! The section size
  etf = ( f(a) + f(b) ) / 2        ! Sum the end points

  DO I = 1, N-1
    x = a + I*h                    ! Calculate the evaluation position
    etf = etf + f(x)               ! Sum over the remaining points
  END DO
  etf = etf*h                      ! Complete the ETF

  PRINT *, "The integral = ", etf  ! Output the result
 
CONTAINS 

  REAL(KIND=8) FUNCTION f(x)
  REAL(KIND=8), INTENT(IN) :: x
  f = 2*x**3 - 3*x**2 - 9*x + 10
  END FUNCTION f

END PROGRAM Numerical_Integration
