PROGRAM Invalid_Reals
!-------------------------------------------------------
! Examples of *invalid* REAL type variable assignments.
!-------------------------------------------------------

  REAL :: A, B, C, Gaziantep, Ali

  A = +2,314           !  Reals do not contain commas
  B = -0.222E+1.2      !  Exponential part must contain an integer 
  C = -4.8+E08         !  Plus sign must be placed after E
  Gaziantep = -5,7     !  Reals do not contain commas  
  Ali = 2.5E3,1        !  Exponential part does not contain commas 

  PRINT *, A, B, C, Gaziantep, Ali

END PROGRAM Invalid_Reals



