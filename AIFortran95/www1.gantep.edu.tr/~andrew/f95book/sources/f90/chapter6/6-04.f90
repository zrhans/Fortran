MODULE Force_Module

  IMPLICIT NONE

CONTAINS	

  REAL FUNCTION FORCE(Time)	

    REAL, INTENT(IN) :: Time
    Force = 55. + 22. * Time

  END FUNCTION FORCE

END MODULE Force_Module

PROGRAM Acceleration
!------------------------------------
! Program  to  illustrate the use of
! a module function. The function 
! f(t) = 55+22t is implemented. 
!------------------------------------

  USE Force_Module

  IMPLICIT NONE
  INTEGER :: I
  REAL :: T, A
  REAL, PARAMETER :: Mass = 6.

  DO I = 0, 5
    T = REAL(I)
    A = Force(T) / Mass
    PRINT *, "At t = ", T, " s, acceleration = ", A, " m/s/s"
  END DO

END PROGRAM Acceleration
