PROGRAM Cantilever_Beam
!-----------------------------------------------
! A cantilever  beam  is loaded at its end. The
! deflection y (down is negative) is calculated
! for any position x along the beam.
!-----------------------------------------------

  IMPLICIT NONE

  REAL :: Moment_of_Inertia, Elasticity_modulus
  REAL :: Beam_length, Load, X, Y
  LOGICAL :: Length_Check

  Moment_of_Inertia = 8.333E-5 ! m^4
  Elasticity_Modulus = 210.0E9 ! Pa
  Beam_length=1.               ! m
  Load = 1000.                 ! N

  PRINT *, "Enter the location x at which the value"
  PRINT *, "of deflection is required."
  READ *, X

  Length_Check = X > Beam_Length .OR. X < 0.

  IF ( Length_Check ) THEN
    PRINT *, "That position is not located on the beam!"
    PRINT *, "Please try again."
  ELSE
    Y = Load * ( X**3 - 3. * Beam_length * X**2 )&
        / ( 6. * Elasticity_Modulus * Moment_of_Inertia)
    PRINT *, "At point x= ", X, "  m, deflection is  ", Y, " m."
  END IF

END PROGRAM Cantilever_Beam
