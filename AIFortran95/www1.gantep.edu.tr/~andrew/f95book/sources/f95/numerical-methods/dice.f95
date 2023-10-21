PROGRAM Dice
!-----------------------------------------
! Simulation of the throwing of ten dice.
!-----------------------------------------
  IMPLICIT NONE
  REAL :: R(10)
  INTEGER :: Throw(10)

  CALL RANDOM_NUMBER(R)
  Throw = INT(R*6)+1
  PRINT *, Throw
  
END PROGRAM Dice

