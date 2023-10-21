PROGRAM Invalid_Logicals
!----------------------------------------------------------
! Examples of *invalid* LOGICAL type variable assignments.
!----------------------------------------------------------

  LOGICAL :: A, B, C

  A = .TRUE    !  Logical constants should be given between two dots
  B = FALSE    !  Logical constants should be given between two dots
  C = .F.      !  Only .TRUE. and .FALSE. are valid logical constants

  PRINT *, A, B, C

END PROGRAM Invalid_Logicals


