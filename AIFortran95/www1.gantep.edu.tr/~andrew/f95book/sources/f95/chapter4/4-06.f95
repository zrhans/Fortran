PROGRAM Do_Loop_with_Cycle
!------------------------------------------------------------
! The  function  f(x) = SQRT( x**2 + 2 * x ) / ( 2 * x - 5 )
! is evaluated  for increasing  values of x from 0. to 5. in
! steps of 0.25.  When x = 2.5 an infinite result will ok so
! at this point the program CYCLEs (skips) to the next value
! of x.  The program uses an  endless DO loop  with  an EXIT
! statement executed when the final value of x is reached.
!------------------------------------------------------------ 

  IMPLICIT NONE
  INTEGER :: I
  REAL :: x, f

  I = -25 ! we want the first value to be 0.
  DO

    I = I + 25
    x = 0.01*I

    IF ( I == 250 ) THEN ! x = 2.5
      PRINT *, "x = ", x, " => f(x) is infinite."
      CYCLE
    END IF

    f = SQRT( x**2 + 2. * x ) / ( 2. * x - 5. )
    PRINT *, "x = ", x, " => f(x) = ", f

    IF ( I == 500 ) THEN ! x = 5.0
      PRINT *, "x has reached it's final value."
      EXIT
    END IF

  END DO

END PROGRAM Do_Loop_with_Cycle
