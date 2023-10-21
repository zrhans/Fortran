PROGRAM Valid_Reals
!--------------------------------------------------------------------
! Examples of valid REAL type variable declarations and assignments.
!--------------------------------------------------------------------

  REAL :: A, B, C, Gaziantep, Ali

  A = +1.2456
  B = -0.00056
  C = 3.7E+08           ! same as 370 000 000.0
  Gaziantep = -5.7E-4   ! same as -0.000 57
  Ali = 2.5E3           ! same as 2500.0

  PRINT *, A, B, C, Gaziantep, Ali
  
END PROGRAM Valid_Reals
