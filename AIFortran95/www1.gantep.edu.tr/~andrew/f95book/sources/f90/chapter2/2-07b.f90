PROGRAM Invalid_Compound_Logicals
!----------------------------------------------------
! Example of *invalid* compound logical assignments.
!----------------------------------------------------

  REAL :: A, B
  LOGICAL :: C, D, E, F, G

  A = 5.2 ; B = 3.5 

  C = .TRUE.
  D = .FALSE.
  E = A .AND. D      !  A is not a logical variable. 
  F = A < B .OR. B   !  Second B is not a logical variable. 
  G = C .NOT. D      !  The NOT logical operator can only be used
                     !  with a single logical. 

  PRINT *, A, B, C, D, E, F, G

END PROGRAM Invalid_Compound_Logicals
