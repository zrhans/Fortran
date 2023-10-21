PROGRAM Explicit_Shape

  IMPLICIT NONE
  INTEGER, PARAMETER :: Num_Values = 6
  REAL :: Values(Num_Values) = (/ 1.2, 3.2, 1.7, 8.3, 4.5, 6.8 /)

  PRINT *, "The mean is ", Mean(Values, Num_Values)

CONTAINS

  FUNCTION Mean(V, N)

    REAL :: Mean
    INTEGER, INTENT(IN) :: N
    REAL, DIMENSION(N), INTENT(IN) :: V

    Mean = SUM(V)/REAL(N)

  END FUNCTION Mean

END PROGRAM Explicit_Shape
