PROGRAM Positive_Integers
!---------------------------------------------
! Input of N positive integers into an array.
! A static array is used.
! N is unknown and is limited to Nmax.
!---------------------------------------------
  IMPLICIT NONE
  INTEGER, PARAMETER :: Nmax=10000
  INTEGER :: Numbers(Nmax)
  INTEGER :: I, N

  PRINT *, "Enter a list of positive integers;"
  PRINT *, "terminate the list with a negative value."

  DO I = 1, Nmax
    READ *, Numbers(I)
    IF ( Numbers(I) < 0 ) EXIT    
  END DO
  N = I-1
  PRINT *, "Number of values in the list is ", N
  IF ( N > 0 ) PRINT *, "The list of numbers is ", Numbers(1:N)

END PROGRAM Positive_Integers
