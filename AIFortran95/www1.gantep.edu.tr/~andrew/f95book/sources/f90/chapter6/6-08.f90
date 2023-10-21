PROGRAM Acceleration
!------------------------------------------
! Program  to  illustrate  the  use  of an
! explicit external function. The function
! f(t) = 55+22t is implemented.
!------------------------------------------

  IMPLICIT NONE
  
  INTERFACE Force_Interface
    REAL FUNCTION Force(Time)
      REAL, INTENT(IN) :: Time
    END FUNCTION Force
  END INTERFACE Force_Interface

  INTEGER :: I
  REAL :: T, A
  REAL, PARAMETER :: Mass = 6.

  DO I = 0, 5
    T = REAL(I)
    A = Force(T) / Mass
    PRINT *, "At t = ", T, " s, acceleration = ", A, " m/s/s"
  END DO

END PROGRAM Acceleration

REAL FUNCTION Force(Time)

  IMPLICIT NONE
  REAL, INTENT(IN) :: Time
  Force = 55. + 22. * Time

END FUNCTION Force
