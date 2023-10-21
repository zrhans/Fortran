PROGRAM Cantilever_Beam_2
!---------------------------------------------------------
! A  cantilever beam  is  loaded at position given by the
! user. The deflection y (down is negative) is calculated
! for any position x along the beam.
!---------------------------------------------------------

  IMPLICIT NONE
  REAL :: Moment_of_Inertia, Elasticity_Modulus
  REAL :: Beam_Length, Load, Load_Position, X, Y

  Moment_of_Inertia = 8.333E-5
  Elasticity_Modulus = 210.0E9
  Beam_length=1.
  Load = 1000.

  PRINT *, "Enter the position of the load:"
  READ *, Load_Position 

  IF ( Load_Position < 0. ) THEN
    PRINT *, "Negative position! Please try again." ; STOP
    ELSE IF ( Load_Position > Beam_Length ) THEN
      PRINT *, "That position is not located on the beam! &
               &Please try again." ; STOP
  END IF

  PRINT *, "Enter the position at which the value of &
           &deflection is required."
  READ *, X 

  IF ( X < 0. ) THEN
    PRINT *, "Negative position! Please try again." ; STOP
    ELSE IF ( X > Beam_Length ) THEN
      PRINT *, "That position is not locate on the beam, &
               &please try again." ; STOP
      ELSE IF ( X < Load_Position ) THEN
        Y = Load * X**2 * ( X - 3. * Load_Position )&
            / ( 6. * Elasticity_Modulus * Moment_of_Inertia )
  ELSE
    Y = Load * Load_Position**2&
        * ( Load_Position - 3. * X )&
        / ( 6. * Elasticity_Modulus * Moment_of_Inertia )
  END IF

  PRINT *, "At point x= ", X, "  m, deflecton is  ", Y, " m."

END PROGRAM Cantilever_Beam_2
