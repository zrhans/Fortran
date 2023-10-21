PROGRAM Cool
!-------------------------------------------------------------
! Program to simulate the cooling of a coffee cup assuming
! Newton's law of cooling. A Euler method is used.
! This cup of coffee takes 10 minutes to cool from 80 Celcius
! to 51 Celcius given that the temperature of the environment
! is 20 Celcius and the cooling constant k=0.0011
!--------------------------------------------------------------
  IMPLICIT NONE

  REAL, PARAMETER :: T_Env=20.0, T_0=80.0, K=0.0011, dt=10.0
  REAL :: t=0.0, T_Cup

  T_Cup=T_0
  
  DO
    t=t+dt
    T_Cup = T_Cup - k*(T_Cup-T_Env)*dt
    PRINT '(3(2x,F5.1))', t, T_Cup, T_Env+(T_0-T_Env)*EXP(-k*t)

    IF ( t >= 600. ) EXIT

  END DO

END PROGRAM Cool
