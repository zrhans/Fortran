PROGRAM Automatic

  IMPLICIT NONE
  INTEGER, DIMENSION(6) :: A, B

  A = (/ 3, 2, 6, 0, 1, 4  /)
  B = (/ 4, 0, 2, 6, 2, 8  /)

  PRINT *, "Before processing"
  PRINT *, "A = ",A
  PRINT *, "B = ",B

  CALL Process(A,B)

  PRINT *, "After processing"
  PRINT *, "A = ",A
  PRINT *, "B = ",B

CONTAINS

  SUBROUTINE Process(A,B)

    INTEGER, DIMENSION(:), INTENT(INOUT) :: A, B
    INTEGER, DIMENSION(SIZE(A)) :: Large, Small
    ! or as INTEGER :: Large(SIZE(A)), Small(SIZE(A))
    INTEGER I

    DO I = 1, SIZE(A)
      IF ( A(I) > B(I) ) THEN
        Large(I)=A(I)
        Small(I)=B(I)
      ELSE 
        Large(I)=B(I)
        Small(I)=A(I)
      END IF
    END DO

    A=Large
    B=Small

  END SUBROUTINE Process

END PROGRAM Automatic
