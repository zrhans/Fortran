PROGRAM Valid_Character_Assignments
!-------------------------------------------
! Examples of valid CHARACTER type variable
! declarations and assignments.
!-------------------------------------------

  CHARACTER :: A*5, B*6, C*7, D*13

  A = "don't"
  B = 'Samsun'
  C = "Fortran"
  D = B // C

  PRINT *, A, B, C, D

END PROGRAM Valid_Character_Assignments
