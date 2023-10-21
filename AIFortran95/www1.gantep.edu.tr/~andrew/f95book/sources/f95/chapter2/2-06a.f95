PROGRAM Valid_Logicals_and_Operators
!------------------------------------------------------
! Examples of valid LOGICAL type variable declarations
! and assignments using relational operators.
!------------------------------------------------------

  REAL :: A, B, C
  LOGICAL :: D, E, F

  A = 5.2 ; B = 3.5 ; C = 4.
  D = .TRUE.
  E = B == B
  F = B > C

  PRINT *, D, E, F

END PROGRAM Valid_Logicals_and_Operators
