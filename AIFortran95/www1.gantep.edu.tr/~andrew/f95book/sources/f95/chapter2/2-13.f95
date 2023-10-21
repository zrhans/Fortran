PROGRAM Average_Temperature
!------------------------------------------
! Calculation of the minimum, average, and
! maximum, values of five temperatures.
! Intrinsic functions  MINVAL, SUM, MAXVAL
! are used.
!------------------------------------------

  IMPLICIT NONE
  REAL, DIMENSION(5) :: T
  REAL :: Minimum_T, Average_T, Maximum_T

  T = (/ 23., 26., 21., 25., 18. /)

  Minimum_T = MINVAL(T)
  Average_T = SUM(T)/5.
  Maximum_T = MAXVAL(T)

  PRINT *, "Minimum temperature is ", Minimum_T
  PRINT *, "Average temperature is ", Average_T
  PRINT *, "Maximum temperature is ", Maximum_T

END PROGRAM Average_Temperature
