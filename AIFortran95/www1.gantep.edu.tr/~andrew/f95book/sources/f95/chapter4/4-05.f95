PROGRAM Cantilever_Beam
!---------------------------------------------------------
! A cantilever beam is loaded at its end.  The deflection
! (down is negative) at the end of the beam is calculated
! for  increasing loads  using an "endless DO loop".  The 
! program terminates when the deflection exceeeds 0.026 m
!---------------------------------------------------------

  IMPLICIT NONE
  REAL :: Moment_of_Inertia, Elasticity_modulus
  REAL :: Beam_length, Load, Deflection

  Moment_of_Inertia = 8.333E-5 ! m^4
  Elasticity_Modulus = 210.0E9 ! Pa
  Beam_length=1.               ! m
  Load = 0.                    ! N (initially zero)

  DO

    Deflection = - Load * Beam_length**3 &
    / ( 3. * Elasticity_Modulus * Moment_of_Inertia)

    IF ( Deflection < -0.026 ) EXIT

    PRINT *, "For a load of ", Load,&
             " N the deflection is ", Deflection, " m"

    Load = Load + 20.E3  

  END DO

END PROGRAM Cantilever_Beam
