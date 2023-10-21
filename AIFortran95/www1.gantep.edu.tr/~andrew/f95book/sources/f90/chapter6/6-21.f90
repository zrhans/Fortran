PROGRAM N_Factorial
!---------------------------------------------------------
! Program to compute the factorial of a positive integer.
! A non-recursive internal subroutine is used.
!---------------------------------------------------------
 
  IMPLICIT NONE
  INTEGER :: N, NFact

  DO N = 0, 9
    CALL Factorial(N, NFact)
    PRINT *, N, " Factorial = ", NFact
  END DO

CONTAINS

  SUBROUTINE Factorial(N, NFact)

    INTEGER, INTENT(IN)  :: N
    INTEGER, INTENT(OUT) :: NFact
    INTEGER :: I
 
    NFact = 1
    DO I = 2,N
      NFact = NFact*I
    END DO

  END SUBROUTINE Factorial

END PROGRAM N_Factorial
