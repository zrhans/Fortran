PROGRAM Dice2
!------------------------------------------
! Simulation of the throwing of many dice.
!------------------------------------------
  IMPLICIT NONE
  REAL :: R
  INTEGER :: I, Throw, Count(6)=0

  DO I = 1, 600
    CALL RANDOM_NUMBER(R)
    Throw = INT(R*6)+1
    IF ( Throw > 6 ) Throw=6
    Count(Throw) = Count(Throw) + 1
  END DO

  PRINT *, Count  

END PROGRAM Dice2

