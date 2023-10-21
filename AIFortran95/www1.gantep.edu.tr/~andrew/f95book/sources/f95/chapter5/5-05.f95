PROGRAM EN_Descriptor
!---------------------------
! Example of the use of the
! EN formatting descriptor.
!---------------------------

  IMPLICIT NONE
  REAL :: Force, Moment

  Force = 177777.355
  Moment = 25.788555

  PRINT '(EN11.3,EN11.3)', Force, Moment
  PRINT 10, Force, Moment
  PRINT 20, Force, Moment
  PRINT 30, Force, Moment
  PRINT 40, Force, Moment

  10 FORMAT(2EN9.2)
  20 FORMAT(2EN15.5E2)
  30 FORMAT(2EN15.5E4)
  40 FORMAT(2EN15.8E5)

END PROGRAM EN_Descriptor
