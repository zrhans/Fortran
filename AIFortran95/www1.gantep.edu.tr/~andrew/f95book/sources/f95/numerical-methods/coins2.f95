PROGRAM Coins2
!------------------------------------------
! Simulation of the tossing of many coins.
!------------------------------------------
  IMPLICIT NONE
  REAL :: R
  INTEGER :: I, N=100, Heads=0, Tails=0

  DO I = 1, N
    CALL RANDOM_NUMBER(R)
    IF ( R < 0.5 ) THEN
      Heads=Heads+1
    ELSE 
      Tails=Tails+1
    END IF
  END DO
  
  PRINT *, "Number of heads = ", Heads, " (", 100*Heads/REAL(N), "%)"
  PRINT *, "Number of tails = ", Tails, " (", 100*Tails/REAL(N), "%)"

END PROGRAM Coins2

