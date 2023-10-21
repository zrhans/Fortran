PROGRAM F_Descriptor
!---------------------------
! Example of the use of the
! F formatting descriptor.
!---------------------------

  IMPLICIT NONE
  REAL :: Force, Moment

  Force = 17.355
  Moment = 25.788555
          
  PRINT '(F9.3,F9.3)', Force, Moment          
  PRINT 10, Force, Moment          
  PRINT 20, Force, Moment          
  PRINT '(F12.11,F12.6)', Force, Moment

  10 FORMAT(2F9.2)         
  20 FORMAT(2F11.5)

END PROGRAM F_Descriptor
