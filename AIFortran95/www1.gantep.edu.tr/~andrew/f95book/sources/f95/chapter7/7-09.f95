PROGRAM Matrix_Math
!--------------------------------------------
! Computation of the matrix: A = 2*(B+C) - D
! the  extent of the 2-dimensional arrays is
! input from the keyboard,  followed  by the
! values of the arrays B, C, and D.
! Although a  whole array  assignment can be
! used, array A is calculated using a nested
! do-loop.
!---------------------------------------------

  IMPLICIT NONE
  REAL, DIMENSION(:,:), ALLOCATABLE :: A, B, C, D
  INTEGER Status, Rows, Cols, I, J

  PRINT *, "Computation of the matrix: A = 2*(B+C) - D"
  PRINT *, "Enter the number of rows and columns of the matrices."
  READ *, Rows, Cols

  ALLOCATE( A(Rows,Cols), B(Rows,Cols),&
            C(Rows,Cols), D(Rows,Cols), STAT=Status)

  IF ( Status /= 0 ) THEN
    PRINT*,"Not enough memory to allocate matrices!"
    STOP
  END IF

  PRINT *, "For matrix B:"
  CALL Input(B, Rows, Cols)
  PRINT *, "For matrix C:"
  CALL Input(C, Rows, Cols)
  PRINT *, "For matrix D:"
  CALL Input(D, Rows, Cols)

  ! Element-by-element assignment
  DO J = 1, Cols
    DO I = 1, Rows
     A(I,J) = 2 * ( B(I,J) + C(I,J) ) - D(I,J)
    END DO
  END DO

  ! The whole array assignment would simply be
  ! A = 2 * ( B + C ) - D

  PRINT *, "2 times"
  CALL Output(B, Rows, Cols)
  PRINT *, "plus"
  PRINT *, "2 times"
  CALL Output(C, Rows, Cols)
  PRINT *, "minus"
  CALL Output(D, Rows, Cols)
  PRINT *,"equals"
  CALL Output(A, Rows, Cols)

CONTAINS

  SUBROUTINE Input(R, Rows, Cols)
  !-----------------------------
  ! Column-wise input of values
  ! to be placed into a matrix.
  !-----------------------------
    INTEGER, INTENT(IN) :: Rows, Cols
    REAL, INTENT(OUT) :: R(Rows, Cols)
    INTEGER I,J
    DO J = 1, Cols
      DO I = 1, Rows
        WRITE(*,'(A,I2,A,I2,A)',ADVANCE="NO")&
        "Enter element (",I,",",J,") : "
        READ(*,*) R(I,J)
      END DO
    END DO
  END SUBROUTINE Input

  SUBROUTINE Output(R, Rows, Cols)
  !-----------------------------
  ! Tabulated output of values
  ! from a matrix.
  !-----------------------------
    INTEGER, INTENT(IN) :: Rows, Cols
    REAL :: R(Rows, Cols)
    INTEGER I
    DO I = 1, Rows
      PRINT *, R(I,:)
    END DO
  END SUBROUTINE Output

END PROGRAM Matrix_Math
