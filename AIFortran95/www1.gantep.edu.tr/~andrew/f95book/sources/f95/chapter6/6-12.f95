PROGRAM Pollution
!------------------------------------------
! Program to index  air pollution  values.
!  Inputs: measured value of air pollution
! Outputs: graduated air pollution index.
! An internal subroutine is used.
!------------------------------------------

  IMPLICIT NONE

  REAL :: Value
  CHARACTER(LEN=10) :: Index = "NONE"

  DO
    WRITE(UNIT=*, FMT='(A)', ADVANCE="NO")&
    "Input the measured value of pollution: "
    READ *, Value
    IF ( Value < 0. ) EXIT
    CALL Pollution_Index(Value, Index)
  END DO

CONTAINS

  SUBROUTINE Pollution_Index(V, I)

    REAL, INTENT(IN) :: V
    CHARACTER(LEN=10), INTENT(INOUT) :: I
    CHARACTER(LEN=10) :: Previous_Index

    Previous_Index = I

    IF      (V < 0.5 ) THEN; I = "Negligible"
    ELSE IF (V < 5.0 ) THEN; I = "Low"
    ELSE IF (V < 20. ) THEN; I = "Medium"
    ELSE IF (V < 50. ) THEN; I = "High"
    ELSE                   ; I = "Extreme!"
    END IF

    PRINT *, "Pollution index is ", I

    IF ( Previous_Index /= I .AND.  &
         Previous_Index /= "NONE" ) &
         PRINT *,"*** index has changed ***"

  END SUBROUTINE Pollution_Index

END PROGRAM Pollution
