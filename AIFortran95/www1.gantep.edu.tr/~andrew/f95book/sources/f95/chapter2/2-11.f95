PROGRAM Triangle
!------------------------------------------
! The area of a triangle of sides a, b, c,
! and  inner angles alpha, beta, gamma, is
! calculated from two sides and one angle.
!------------------------------------------

  IMPLICIT NONE
  REAL :: A, B, C, Alpha, Beta, Gamma, Area

  PRINT *, "Enter triangle lengths a and b"
  READ *, A, B
  PRINT *, "Enter inner angle, alpha (radians)"
  READ *, Alpha

  Beta = ASIN(B * SIN(Alpha) / A)
  Gamma = 3.14159265 - Alpha - Beta
  C = A * SIN(Gamma) / SIN(Alpha)
  Area = 0.5 * C * B * SIN(Alpha)

  PRINT *, "a=", A, "  b=", B, "  c=", C
  PRINT *, "Alpha=", Alpha, "  Beta=", beta, "  Gamma=", Gamma
  PRINT *, "Triangle area=", Area
  
END PROGRAM Triangle

