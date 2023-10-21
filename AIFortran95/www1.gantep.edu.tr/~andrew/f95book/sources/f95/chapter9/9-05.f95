PROGRAM Array_Pointers
!---------------------------------------
! Program to illustrate array pointers.
!---------------------------------------

  IMPLICIT NONE
  INTEGER :: k
  INTEGER, DIMENSION(12), TARGET :: numbers = (/ (k, k=1,12) /)
  INTEGER, DIMENSION(:), POINTER :: pl, p2, p3, p4, p5

  pl => numbers
  p2 => pl(4::1)
  p3 => p2(4::1)
  p4 => p3(4::1)
  p5 => p4(4::1)

  PRINT '(A,12I3)', " pl = ", pl
  PRINT '(A,12I3)', " p2 = ", p2
  PRINT '(A,12I3)', " p3 = ", p3
  PRINT '(A,12I3)', " p4 = ", p4
  PRINT '(A,12I3)', " p5 = ", p5

END PROGRAM Array_Pointers
