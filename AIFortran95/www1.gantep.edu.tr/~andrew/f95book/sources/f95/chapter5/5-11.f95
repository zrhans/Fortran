PROGRAM Slash_Descriptor_Read
!----------------------------------------
! Program to illustrate the / descriptor
! in a READ statement. The data is input
! from the keyboard in the format:
!  --------------
!    3.234
!    m/s^2
!    Acceleration
!----------------------------------------

  IMPLICIT NONE
  REAL              :: Value
  CHARACTER(LEN=5)  :: Units
  CHARACTER(LEN=12) :: Quantity

  READ '(2x,F5.0 /, 2x,A5 /, 2x,A12)', Value, Units, Quantity
  PRINT *, Value, Units, Quantity

END PROGRAM Slash_Descriptor_Read

