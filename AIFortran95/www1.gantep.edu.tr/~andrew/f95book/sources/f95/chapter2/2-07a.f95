PROGRAM Valid_Compound_Logicals
!------------------------------------------------
! Example of valid compound logical assignments.
!------------------------------------------------

  REAL :: A, B
  LOGICAL :: C, D, E, F, G

  A = 5.2 ; B = 3.5 

  C = .TRUE.
  D = .FALSE.
  E = C.AND.D
  F = A < B .OR. E
  G = .NOT. F

  PRINT *, A, B, C, D, E, F, G

END PROGRAM Valid_Compound_Logicals

