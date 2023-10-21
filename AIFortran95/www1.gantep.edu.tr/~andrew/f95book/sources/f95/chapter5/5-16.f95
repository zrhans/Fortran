PROGRAM Student_Scores
!---------------------------------------
! Program to read student records  from 
! a file, and output the weighted total
! score to another file.
!   INPUT FILE: student.records UNIT=2
!  OUTPUT FILE: student.scores  UNIT=3
! Exam weights: 30% 30% 40%
!---------------------------------------

  IMPLICIT NONE 
  CHARACTER(LEN=18) :: Student_Name
  INTEGER :: I, Score_1, Score_2, Score_3
  REAL :: Weighted_Score

  OPEN(UNIT=2, FILE="student.records", ACTION="READ")
  OPEN(UNIT=3, FILE="student.scores", ACTION="WRITE")

  DO I = 1 , 12
    READ (2,'(A18,3(I3))') Student_Name, Score_1, Score_2, Score_3 
    Weighted_Score = 0.3*Score_1 + 0.3*Score_2 + 0.4*Score_3
    WRITE(3,'(A18,1X,F5.1)') Student_Name, Weighted_Score
  END DO

END PROGRAM Student_Scores

