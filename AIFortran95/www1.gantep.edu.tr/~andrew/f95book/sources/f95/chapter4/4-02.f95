PROGRAM Series_Sum
!----------------------------------------------------------------
! The series summation 1 + 2 + 3 + .... + N is performed using a
! DO loop. The value of N is input by the user and the result is 
! compared to N(N+1)/2.
!----------------------------------------------------------------

  IMPLICIT NONE
  INTEGER :: I, N, S

  PRINT *, "Input N"
  READ *, N

  S = 0
  DO I = 1, N
    S = S + I
  END DO

  PRINT *, "The series sum is ", S
  PRINT *, "This should equal ", N, "(", N+1, ")/2 = ", N*(N+1)/2

END PROGRAM Series_Sum
