PROGRAM Marble_in_Oil
!-----------------------------------------------------------------
! Program to simulate the motion of a marble falling through oil.
! A Euler method is used. The acceleration of the marble is given
! by a = g - b*v/m.
! 
! In this example 
!
!      g = 9.81 m/s^2   Acceleration due to gravity. 
!      b = 0.1 Ns/m     Drag coefficient.
!      m = 0.010 kg     Mass of the marble.
!
! The marble is initially at rest, the simulation proceeds in
! time steps of 0.001 seconds for a total of 0.1 seconds.
!
! The result for v(t) is compared with the analytical solution
! (m*g/b)*(1-EXP( -b*t/m ))
!
!-----------------------------------------------------------------
  IMPLICIT NONE
  REAL, PARAMETER ::  g=9.81, b=0.1, m=0.01, dt=0.001
  REAL :: v=0., t=0, a

  DO 

    t = t + dt         ! Evolve time.
    a = g - b*v/m      ! Recalculate the acceleration.
    v = v + a*dt       ! Evolve velocity.

    PRINT '(3(2x,F5.3))', t,v, (m*g/b)*(1-EXP( -b*t/m ))

    IF ( t >= 0.1 ) EXIT  ! Terminate after 0.1 seconds

  END DO

END PROGRAM Marble_in_Oil
