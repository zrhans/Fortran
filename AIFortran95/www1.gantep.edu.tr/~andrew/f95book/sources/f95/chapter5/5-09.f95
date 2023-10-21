PROGRAM I_Descriptor_Read
!----------------------------------------
! Program to illustrate the I descriptor
! in a READ statement.
!----------------------------------------

  IMPLICIT NONE
  INTEGER :: Number, Group, X, Y

  READ '(2(2X,I4))', Number, Group
  READ '(I3,I2)', X, Y

  PRINT *, Number, Group, X, Y

END PROGRAM I_Descriptor_Read

