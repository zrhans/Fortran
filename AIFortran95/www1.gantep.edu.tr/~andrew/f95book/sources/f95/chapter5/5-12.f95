PROGRAM Particle_Motion_Formatted
! ------------------------------------------------------------
! The displacement of a particle is described by the equation
! s = 2 t^3 - 24 t + 6,  the  velocity  is therefore given by
! v = 6 t^2 - 24 and the acceleration by a = 12 t.
! This  program  outputs the values of  s, v, and a,  in  the
! period 0 < t < 4 in 0.5 second intervals.
! ------------------------------------------------------------

  IMPLICIT NONE
  REAL :: Time, Displacement, Velocity, Acceleration
  INTEGER :: I

  DO I = 0, 40, 5

    Time = 0.1 * I

    Displacement =  2 * Time**3 - 24 * Time + 6
        Velocity =  6 * Time**2 - 24
    Acceleration = 12 * Time

    WRITE(6,'(4(A,F6.2,A))')              &
        " At t =",         Time, " s ",   &
    " Displac. =", Displacement, " m ",   &
    " Velocity =",     Velocity, " m/s ", &
        " Acc. =", Acceleration, " m/s/s"

  END DO

END PROGRAM Particle_Motion_Formatted
