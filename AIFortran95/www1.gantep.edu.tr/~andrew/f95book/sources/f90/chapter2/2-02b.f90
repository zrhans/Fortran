PROGRAM Invalid_Integers
!----------------------------------------------------------
! Examples of *invalid* INTEGER type variable assignments.
!----------------------------------------------------------

  INTEGER :: A, B, Gaziantep, Ali

  A = 1,1111        !  Commas can not be used in integer type variables
  B = 17.30         !  Assignment will be truncated to 17
  Gaziantep = 3-0.9 !  Assignment will be truncated to 2
  Ali = 4-          !  Minus sign must be in front

  PRINT *, A, B, Gaziantep, Ali

END PROGRAM Invalid_Integers
