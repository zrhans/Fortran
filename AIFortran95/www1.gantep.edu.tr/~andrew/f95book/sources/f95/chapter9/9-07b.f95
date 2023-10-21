PROGRAM Positive_Integers
!---------------------------------------------
! Input of N positive integers into an array.
! N is unknown.  Two dynamic arrays are used.
!---------------------------------------------
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: Numbers(:), NumbersCopy(:)
  INTEGER :: Num, N

  ALLOCATE( Numbers(0), NumbersCopy(0) )

  PRINT *, "Enter a list of positive integers;"
  PRINT *, "terminate the list with a negative value."

  N=0
  DO
    READ *, Num
    IF ( Num < 0 ) EXIT
    NumbersCopy=Numbers    
    N=N+1
    DEALLOCATE(Numbers)
    ALLOCATE ( Numbers(N) )
    Numbers(1:N-1)=NumbersCopy
    Numbers(N)=Num
    DEALLOCATE(NumbersCopy)    
    ALLOCATE ( NumbersCopy(N) )
  END DO
  
  DEALLOCATE(NumbersCopy)

  PRINT *, "Number of values in the list is ", N
  IF ( N > 0 ) PRINT *, "The list of numbers is ", Numbers

END PROGRAM Positive_Integers
