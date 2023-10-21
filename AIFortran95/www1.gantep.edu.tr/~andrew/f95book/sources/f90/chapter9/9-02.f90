PROGRAM Pointer_Assignment_2
!---------------------------------
! Example of pointer assignment.
!---------------------------------

  IMPLICIT NONE

  REAL, POINTER :: Ptr1,Ptr2
  REAL, TARGET  :: Trg1,Trg2

  Trg1 = -0.784
  Trg2 = 4.321

  Ptr1 => Trg1
  Ptr2 => Ptr1

  PRINT *, "Ptr1 = ", Ptr1
  PRINT *, "Ptr2 = ", Ptr2

  Ptr1 => Trg2

  PRINT *, "Ptr1 = ", Ptr1
  PRINT *, "Ptr2 = ", Ptr2

  Trg1 = Trg1 + 1

  PRINT *, "Ptr1 = ", Ptr1
  PRINT *, "Ptr2 = ", Ptr2

END PROGRAM Pointer_Assignment_2
