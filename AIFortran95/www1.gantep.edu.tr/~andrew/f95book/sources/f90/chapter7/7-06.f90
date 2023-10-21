PROGRAM Whole_Array_Read
!--------------------------------------
! This program reads data  from a file
! into  an  array  using a whole-array
! READ statement. The minimum, maximum
! and mean of the values are output.
!--------------------------------------

  IMPLICIT NONE
  INTEGER :: N, Allocate_Status
  REAL :: Mean
  REAL, ALLOCATABLE :: V(:)

  ! Open the data file and read the number of values.
  OPEN(UNIT=2, FILE="values.dat", ACTION="READ")
  READ (2,*) N

  ! Allocate and input the whole array.
  ALLOCATE( V(N), STAT=Allocate_Status)
  IF ( Allocate_Status /= 0 ) THEN
    PRINT*,"Not enough memory to allocate array V!"
    STOP
  END IF
  READ (2,*) V

  ! Compute the mean value
  Mean = SUM(V)/REAL(N)

  ! Output the results
  PRINT *, "The minimum value is ", MINVAL(V)
  PRINT *, "   The mean value is ", MEAN
  PRINT *, "The maximum value is ", MAXVAL(V)

  ! Deallocate the array, and close the file.
  DEALLOCATE(V)
  CLOSE(2)

END PROGRAM Whole_Array_Read

