PROGRAM Invalid_Character_Assignments
!------------------------------------------------------------
! Examples of *invalid* CHARACTER type variable assignments.
! Single- and double- quotes are mismatched.
!------------------------------------------------------------

  CHARACTER :: A*5, B*6, C*7, D*13

  A = 'mehmet"        ! final quote should be an apostrophe
  B = "samsun'        ! final apostrophe should be a quote
  C = "don't'         ! final apostrophe should be a quote
  D = 'can't"         ! first apostrophe should be a quote

  PRINT *, A, B, C, D

END PROGRAM Invalid_Character_Assignments


