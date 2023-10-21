PROGRAM Pointer_Assignment_1
!--------------------------------
! Example of pointer assignment.
!--------------------------------

  IMPLICIT NONE

  INTEGER, POINTER :: Ip           ! Variables
  REAL, POINTER :: Rp              ! Ip, Rp, and Cp
  CHARACTER(LEN=5), POINTER :: Cp  ! are pointers.

  INTEGER, TARGET :: It            ! Variables
  REAL, TARGET :: Rt               ! It, Rt, and Ct
  CHARACTER(LEN=5), TARGET :: Ct   ! are targets.

  It = 7
  Rt = -0.785
  Ct = "Me240"

  Ip => It
  Rp => Rt
  Cp => Ct

  PRINT   '(2(A,I2,2X))',  "It  =",It, "Ip = ",Ip  
  PRINT '(2(A,F6.2,2X))',  "Rt = ",Rt, "Rp = ",Rp
  PRINT    '(2(A,A,2X))',  "Ct = ",Ct, "Cp = ",Cp

END PROGRAM Pointer_Assignment_1
