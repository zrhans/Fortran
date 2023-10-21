PROGRAM Quadratic_Roots
!--------------------------------------------------
! Computation of the roots of a quadratic equation
!
!          f(x) = a x^2 + b x + c
!
!  Inputs: the coefficients a, b, and c.
! Outputs: the roots x1 and x2 if they are real.
!--------------------------------------------------

  IMPLICIT NONE
  REAL :: A, B, C
  REAL :: Discriminant, X1, X2

  PRINT *, "Enter the coefficients a, b, c,"
  PRINT *, "of the quadratic equation."
  READ *, A, B, C

  Discriminant = B**2 - 4*A*C

  ! two real roots
  IF ( Discriminant > 0. ) THEN
    PRINT *, "There are two real roots."
    X1 = ( -B + SQRT(Discriminant) ) / (2*A)
    X2 = ( -B - SQRT(Discriminant) ) / (2*A)
    PRINT *, "The roots are, x = ", X1, " and x = ", X2
  END IF

  ! one real root
  IF ( Discriminant == 0. ) THEN
    PRINT *, "There is one real root."
    X1 = -B / (2*A)
    PRINT *, "The root is x = ", X1
  END IF

  ! imaginary roots
  IF ( Discriminant < 0. ) THEN
    PRINT *, "The roots are complex!"
  END IF

END PROGRAM Quadratic_Roots
