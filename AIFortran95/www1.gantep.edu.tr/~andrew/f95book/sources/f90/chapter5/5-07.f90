PROGRAM T_Descriptor
!---------------------------
! Example of the use of the
! T formatting descriptor.
!---------------------------

  IMPLICIT NONE
  CHARACTER :: A*12, B*13, C*14
  REAL :: ss, ns, bs

  A = "shear stress"
  B = "normal stress"
  C = "bearing stress"

  ss = -4.23
  ns = 1.54
  bs = 0.78

  PRINT *, "--- 1 ---"
  PRINT '(2X,A,4X,F5.2)', A, ss
  PRINT '(2X,A,4X,F5.2)', B, ns
  PRINT '(2X,A,4X,F5.2)' ,C, bs
  PRINT *, "--- 2 ---"
  PRINT '(T3,A,T20,F5.2)', A, ss
  PRINT '(T3,A,T20,F5.2)', B, ns
  PRINT '(T3,A,T20,F5.2)' ,C, bs

END PROGRAM T_Descriptor
