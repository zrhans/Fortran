PROGRAM A_Descriptor_Read
!-----------------------------------------
! Program to illustrate the  A descriptor
! in a READ statement.  The data is input
! from the keyboard in the format:
! --------------
!   2nd ME240 AA  
! An example T descriptor is also shown.
!-----------------------------------------

  IMPLICIT NONE
  CHARACTER(LEN=5) :: Year, Course, Grade

  READ '(2X, A3, T7, A, 1X, A2)', Year, Course, Grade
  PRINT *, Year, Course, Grade

END PROGRAM A_Descriptor_Read

