PROGRAM Invalid_Logicals_and_Operators
!---------------------------------------------
! Examples of *invalid* LOGICAL type variable
! assignments using relational operators.
!---------------------------------------------

  REAL :: A, B, C
  LOGICAL :: D, E

  A = 5.2 ; B = 3.5 ; C = 4.

  D = A < = = B     !  An invalid relational operator is used. 
  E = B = B         !  An invalid relational operator is used.
  A = B > C         !  Attempt to assign a logical to a real variable.

  PRINT *, D, E

END PROGRAM Invalid_Logicals_and_Operators


