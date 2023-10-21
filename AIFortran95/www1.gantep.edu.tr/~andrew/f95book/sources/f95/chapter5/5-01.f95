PROGRAM I_Descriptor
!---------------------------------------------------
! Program to demonstrate three forms for specifying
! format descriptors for type integer values.
!---------------------------------------------------

  IMPLICIT NONE
  INTEGER :: Mass, Time, Acceleration

  Mass = 22
  Time = 4
  Acceleration = -43

  PRINT *, "    mass", "     time", "     acc."     
  PRINT *, "    ====", "     ====", "     ===="     

  ! First form
  PRINT *, Mass, Time, Acceleration

  ! Second Form
  PRINT '(I9, I9, I9)', Mass, Time, Acceleration

  ! Third form
  PRINT 10, Mass, Time, Acceleration
  PRINT 20, Mass, Time, Acceleration
    
  10 FORMAT(3I9)     
  20 FORMAT(3I9.5)

END PROGRAM I_Descriptor
