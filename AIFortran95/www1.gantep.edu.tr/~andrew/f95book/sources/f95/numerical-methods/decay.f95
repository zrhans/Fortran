PROGRAM Decay
!----------------------------------------------
! Simulation of the decay of  Molybdenum-99 to
! Technetium-99m.  The decay constant  of 99Mo
! is 2.87x10^-6 per second  (its  half-life is
! 67 hours).  For a good representation of the
! random  nature  of  the  decay process,  the
! condition Lamda x dt << 1 must be satisfied.
!----------------------------------------------
  IMPLICIT NONE

  REAL, PARAMETER :: Lamda = 2.87E-6 , dt = 3600.
  INTEGER, PARAMETER :: N0 = 1000000
  REAL :: R, t
  INTEGER :: I, N

  t=0.
  N=N0

  DO
    t = t + dt                         ! Evolve time by dt seconds.
    DO I = 1, N                        ! Loop over every surviving nuclei.
      CALL RANDOM_NUMBER(R)            ! Generate a random number.
      IF ( R < Lamda*dt ) N=N-1        ! Test for a decay.
    END DO
    IF ( t >= 67*3600 ) EXIT           ! Terminate after 67 hours.
  END DO

  PRINT *, t, N, N0*EXP(-Lamda*t)  ! Output the result, compare with theory.

END PROGRAM Decay
