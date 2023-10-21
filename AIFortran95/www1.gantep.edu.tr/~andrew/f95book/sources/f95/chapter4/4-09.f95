PROGRAM Temperature_Sets
! ---------------------------------------------------
! The products of the values in two temperature data
! sets are calculated using nested endless DO loops.
! ---------------------------------------------------

  IMPLICIT NONE
  REAL :: First_Temperature(8), Second_Temperature(6)
  INTEGER :: I, J

  ! Define the data
   First_Temperature = (/ 21., 25., 12., 19., 18., 15., 22., 13. /)
  Second_Temperature = (/ 27., 26., 29., 32., 30., 33. /)

  ! Form the products

  I = 0
  First: DO
    I = I + 1

    J = 0 
    Second: DO
      J = J + 1

      PRINT *,  First_Temperature(I), " * ",&
               Second_Temperature(J), " = ",&
                First_Temperature(I) * Second_Temperature(J)

      IF ( J == 6 ) EXIT Second 
    END DO Second

    IF ( I == 8 ) EXIT First 
  END DO First

END PROGRAM Temperature_Sets
