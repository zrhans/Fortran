PROGRAM Discharge
!--------------------------------------------------------------
! Program to simulate the discharge of an R-C circuit.
! A Euler method is used.
! The current in the circuit is dq/dt = i = V/R and the
! voltage is V = q/C
!
! In this example
!
!    R = 1000 Ohms        Resistance.
!    C = 1 micro Farad    Capacitance.
!   V0 = 12 Volts         Initial voltage.
!   q0 = V0*C             Initial charge.
!
! The simulation proceeds in time steps of 10E-5 seconds for a
! total of 0.001 seconds.
!
! The result for V(t) is compared with the analytical solution
! V(t) = V0 exp(-t/RC)
!--------------------------------------------------------------
  IMPLICIT NONE
  REAL, PARAMETER :: R=1000., C=1.0E-6, dt=1.0E-5
  REAL :: V=12.0, q, t=0.0, i=0.0

  q=V*C ! inital charge

  DO 

    t = t + dt       ! Evolve time.
    i = V/R          ! Recalculate the current in the circuit.
    q = q - i*dt     ! Remove some charge from the capacitor.
    V = q/C          ! Recalculate the voltage across the capacitor.

    PRINT '(2x,F7.5,2x,F5.2,2x,F5.2)', t, V, 12*EXP(-t/(R*C))

    IF ( t >= 0.001 ) EXIT  ! Terminate after 1 ms.

  END DO

END PROGRAM Discharge
