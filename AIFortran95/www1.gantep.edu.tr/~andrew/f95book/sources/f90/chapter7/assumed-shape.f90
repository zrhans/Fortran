PROGRAM Assumed_Shape

  IMPLICIT NONE
  INTEGER, PARAMETER :: Num_Values = 6
  REAL :: Values(Num_Values) = (/ 1.2, 3.2, 1.7, 8.3, 4.5, 6.8 /)

  PRINT *, "The mean is ", Mean(Values)

CONTAINS

  FUNCTION Mean(V)

    REAL :: Mean
    REAL, DIMENSION(:), INTENT(IN) :: V

    Mean = SUM(V)/REAL(SIZE(V))

  END FUNCTION Mean

END PROGRAM Assumed_Shape
