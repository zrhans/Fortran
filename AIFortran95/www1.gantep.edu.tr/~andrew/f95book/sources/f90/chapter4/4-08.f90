PROGRAM Temperature_Sets
! ---------------------------------------------------
! The products of the values in two temperature data
! sets are calculated using named nested DO loops.
! ---------------------------------------------------

  IMPLICIT NONE

  REAL :: First_Temperature(8), Second_Temperature(6)
  INTEGER :: I, J

  ! Define the data
   First_Temperature = (/ 21., 25., 12., 19., 18., 15., 22., 13. /)
  Second_Temperature = (/ 27., 26., 29., 32., 30., 33. /)

  ! Form the products

  First: DO I = 1 , 8

    Second: DO J = 1 , 6

      PRINT *,  First_Temperature(I), " * ",&
               Second_Temperature(J), " = ",&
                First_Temperature(I) * Second_Temperature(J)

    END DO Second

  END DO First

END PROGRAM Temperature_Sets
