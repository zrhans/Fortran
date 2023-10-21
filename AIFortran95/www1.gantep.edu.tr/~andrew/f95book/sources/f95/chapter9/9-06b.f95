PROGRAM No_Memory_Leak
!--------------------------------------
! The program  calculates  the mean of
! the integer numbers  between 1 and n 
! up to n=100000.
!
! To avoid a memory leak the statement
! DEALLOCATE must be used.
!--------------------------------------
  IMPLICIT NONE
  INTEGER(KIND=8), POINTER :: p(:)
  INTEGER :: i, j, memory
  REAL :: Mean

  DO i=1,100000

    ALLOCATE(p(1:i),STAT=memory)
    IF ( memory /= 0 )THEN
      PRINT *, "Memory is not enough for allocating the pointer!!!"
      STOP
    END IF

    p = (/ (j,j=1,i) /)
    Mean = SUM(p)/REAL(i)

    PRINT '(A,I8,A,F10.3))', &
    "The mean of the numbers between 1 and ", i, " is ",Mean

    DEALLOCATE(p) ! no memory leaks

  END DO

END PROGRAM No_Memory_Leak
