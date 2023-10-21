PROGRAM N_Factorial
!---------------------------------------------------------
! Program to compute the factorial of a positive integer.
! A recursive function is used.
!---------------------------------------------------------
 
  IMPLICIT NONE
  INTEGER :: N

  DO N = 0, 9
    PRINT *, N, " Factorial = ", Factorial(N) 
  END DO

CONTAINS

  RECURSIVE FUNCTION Factorial(N) RESULT(NFact)

    INTEGER, INTENT(IN)  :: N
    INTEGER :: NFact
 
    IF ( N > 1 ) THEN
      NFact = N * Factorial(N-1)
    ELSE 
      NFact = 1
    END IF

  END FUNCTION Factorial

END PROGRAM N_Factorial
