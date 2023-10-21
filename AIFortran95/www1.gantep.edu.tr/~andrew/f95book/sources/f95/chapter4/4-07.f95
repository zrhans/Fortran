PROGRAM Simple_Product_List
!-------------------------------------------------------
! This program uses a nest loop  to output the products
! 1x1, 1x2, 1x3, ...., 5x5. Note how the statements are
! indented to emphasise the nesting of the loops.
!-------------------------------------------------------

  IMPLICIT NONE

  INTEGER :: I, J

  DO I = 1, 5
    DO J = 1, 5
      PRINT *, I, " times ", J, " = ", I*J
    END DO
  END DO

END PROGRAM Simple_Product_List
