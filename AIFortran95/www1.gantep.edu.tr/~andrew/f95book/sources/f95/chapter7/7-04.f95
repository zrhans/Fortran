PROGRAM Bubble_Sort
!-----------------------------------
! Program to 'bubble' sort an array
! of integers into numerical order. 
!-----------------------------------

  IMPLICIT NONE
  INTEGER, PARAMETER :: N=10
  INTEGER :: V(N)
  REAL :: Swap
  INTEGER :: I
  LOGICAL :: Flag
 
  V = (/ 3, 7, 2, -4, 12, 6, -23, 45, 4, 14/)

  PRINT '(100(1x,I3))', V ! the initial array

  DO

    FLAG=.TRUE.

    DO I=1,N-1
      IF ( V(I) .GT. V(I+1) ) THEN
        SWAP = V(I)
        V(I) = V(I+1)
        V(I+1) = SWAP
        FLAG = .FALSE.
        PRINT '(100(1x,I3))', V ! the array after a swap
      END IF
    END DO

    IF ( FLAG ) EXIT

  END DO

END PROGRAM Bubble_Sort
