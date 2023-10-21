PROGRAM Average_Score
!--------------------------------
! Example of component selection
! for derived data types.
!--------------------------------

  IMPLICIT NONE

  TYPE Scores
    INTEGER :: MT1, MT2, Final
  END TYPE Scores

  TYPE ( Scores ) :: Exams
  REAL :: Average

  PRINT *, "Input the first mid-term exam score (%)."
  READ *, Exams%MT1
  PRINT *, "Input the second mid-term exam score (%)."
  READ *, Exams%MT2
  PRINT *, "Input the final exam score (%)."
  READ *, Exams%Final

  Average = 0.3*Exams%MT1 + 0.3*Exams%MT2 + 0.4*Exams%Final

  PRINT '(A,F5.1,A)', "The weighted average is ", Average,  " %"

END PROGRAM Average_Score
