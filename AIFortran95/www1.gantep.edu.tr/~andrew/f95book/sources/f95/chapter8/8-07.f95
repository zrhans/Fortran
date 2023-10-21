PROGRAM Quadratic_Roots
!--------------------------------------------------
! Computation of the roots of a quadratic equation
!
!          f(x) = a x^2 + b x + c
!
!  Inputs: the coefficients a, b, and c.
! Outputs: the roots x1 and x2 (real or complex)
!---------------------------------------------------

  IMPLICIT NONE
  REAL :: A, B, C
  COMPLEX :: Discriminant, X1, X2

  PRINT *, "Enter the coefficients a, b, and c of the"
  PRINT *, "quadratic equation f(x) = a x^2 + b x + c"
  READ *, A, B, C

  Discriminant = CMPLX(B**2-4*A*C, 0.0)

  X1 = ( -B + SQRT(Discriminant) ) / (2*A)
  X2 = ( -B - SQRT(Discriminant) ) / (2*A)

  PRINT *, "The roots are:"

  IF ( REAL(Discriminant) < 0. ) THEN
    PRINT *, X1
    PRINT *, X2
  ELSE 
    PRINT *, REAL(X1)
    PRINT *, REAL(X2)
  END IF

END PROGRAM Quadratic_Roots
