PROGRAM Dynamic_Arrays
!---------------------------------------
! Program to input n values and outputs
! the mean and standard deviation.  The
! program uses a dynamic array.
!--------------------------------------

  IMPLICIT NONE
  REAL, ALLOCATABLE :: V(:)
  INTEGER :: N, I, Allocate_Status
  REAL :: Mean, Sd

  PRINT *, "How many values are to be input?"
  READ *, N

  ! Allocate V and check for an error condition
  ALLOCATE( V(N), STAT=Allocate_Status)

  IF ( Allocate_Status /= 0 ) THEN
    PRINT*,"Not enough memory to allocate array V!"
    STOP
  END IF

  ! Input array V
  DO I = 1, N
    WRITE(UNIT=*,FMT='("Input value ",I2, ": ")',ADVANCE="NO") I
    READ *, V(I)
  END DO

  ! Form the Mean and standard deviation
  Mean = SUM(V)/REAL(N)
  Sd = SQRT( SUM((V-Mean)**2) / REAL(N-1) )
 
  ! array V is not required anymore
  DEALLOCATE(V)

  ! Output the results
  PRINT *, "The mean is ", Mean
  PRINT *, "The standard deviation is", sd

END PROGRAM Dynamic_Arrays
