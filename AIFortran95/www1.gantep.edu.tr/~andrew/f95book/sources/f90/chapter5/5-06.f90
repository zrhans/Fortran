PROGRAM A_Descriptor
!---------------------------
! Example of the use of the
! A formatting descriptor.
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
  PRINT '(2X,A,2X,F5.2)', A, ss
  PRINT '(2X,A,2X,F5.2)', B, ns
  PRINT '(2X,A,2X,F5.2)' ,C, bs
  PRINT *, "--- 2 ---"
  PRINT '(2X,A14,2X,F5.2)', A, ss
  PRINT '(2X,A14,2X,F5.2)', B, ns
  PRINT '(2X,A14,2X,F5.2)' ,C, bs
  PRINT *, "--- 3 ---"
  PRINT '(2X,A13,2X,F5.2)', A, ss
  PRINT '(2X,A13,2X,F5.2)', B, ns
  PRINT '(2X,A13,2X,F5.2)', C, bs
  PRINT *, "--- 4 ---"
  PRINT '(2X,A,2X,F5.2)', "  shear stress", ss
  PRINT '(2X,A,2X,F5.2)', " normal stress", ns
  PRINT '(2X,A,2X,F5.2)', "bearing stress", bs
  PRINT *, "--- 5 ---"
  PRINT '(2X,"  shear stress",2X,F5.2)', ss
  PRINT '(2X," normal stress",2X,F5.2)', ns
  PRINT '(2X,"bearing stress",2X,F5.2)', bs

END PROGRAM A_Descriptor
