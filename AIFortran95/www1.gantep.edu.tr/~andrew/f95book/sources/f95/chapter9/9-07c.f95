PROGRAM Positive_Integers
!--------------------------------------------
! Input of N positive integers into a linked
! structure. N is unknown.
!--------------------------------------------
  IMPLICIT NONE

  TYPE Node
    INTEGER :: Value
    TYPE (Node), POINTER :: Next
  END TYPE Node

  INTEGER :: Num, N
  TYPE (Node), POINTER :: Numbers, Current_Number
  NULLIFY(Numbers)

  PRINT *, "Enter a list of positive integers;"
  PRINT *, "terminate the list with a negative value."

  N=0
  DO
    READ *, Num
    IF ( Num < 0 ) EXIT
    N=N+1
    ALLOCATE(Current_Number)
    Current_Number%value = Num
    Current_Number%next => Numbers
    Numbers => Current_Number
  END DO
  Current_number => Numbers

  PRINT *, "Number of values in the list is ",N
  IF ( N > 0 ) PRINT *, "The list of numbers (in reverse order) is"
  DO
    IF ( .NOT. ASSOCIATED(Current_Number) ) EXIT
    PRINT *, Current_Number%Value
    Current_Number => Current_Number%Next
  END DO

END PROGRAM Positive_Integers
