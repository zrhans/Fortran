PROGRAM N_Factorial
!---------------------------------------------------------
! Program to compute the factorial of a positive integer.
! A recursive internal subroutine is used.
!---------------------------------------------------------
 
  IMPLICIT NONE
  INTEGER :: N, NFact

  DO N = 0, 9
    CALL Factorial(N, NFact)
    PRINT *, N, " Factorial = ", NFact 
  END DO

CONTAINS

  RECURSIVE SUBROUTINE Factorial(N, NFact)

    INTEGER, INTENT(IN)  :: N
    INTEGER, INTENT(OUT) :: NFact
    INTEGER :: I
 
    IF ( N > 1 ) THEN
      CALL Factorial(N-1, I)
      NFact = N*I
    ELSE 
      NFact = 1
    END IF

  END SUBROUTINE Factorial

END PROGRAM N_Factorial
