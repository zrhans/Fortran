PROGRAM Slash_Descriptor
!---------------------------
! Example of the use of the
! slash (/) descriptor.
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

  PRINT '(3(2X,A14,4X,F5.2 /))', A, ss, B, ns, C, bs

END PROGRAM Slash_Descriptor
