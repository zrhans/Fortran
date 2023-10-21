PROGRAM Coins
!-----------------------------------------
! Simulation of the tossing of ten coins.
!-----------------------------------------
  IMPLICIT NONE
  REAL :: R
  INTEGER :: I

  DO I = 1, 10
    CALL RANDOM_NUMBER(R)
    IF ( R < 0.5 ) THEN
      PRINT *, "Heads"
    ELSE 
      PRINT *, "Tails"
    END IF
  END DO
  
END PROGRAM Coins

