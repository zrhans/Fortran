PROGRAM Grade_Calculation
!----------------------------------------------------------
! Grade calculation from the weighted average     0-39  FF
! of  three exams.  The  first,  second,  and    40-49  FD
! final exam  scores  are  weighted  by  0.3,    50-59  DD
! 0.3,  and  0.4  respectively.  The  average    60-69  DC
! score is  converted  to a  grade  from  the    70-74  CC
! grade table (right). A  'sanity'  check  is    75-79  CB
! performed  to  check  whether  the  average    80-84  BB
! score is negative or greater than 100%.        85-89  BA
!                                                90-100 AA
!----------------------------------------------------------

  IMPLICIT NONE
  REAL :: First_Score, Second_Score, Final_Score, Average_Score
 
  PRINT *, "Enter the results of the first, second, &
           &and final examinations."
  READ *, First_Score, Second_Score, Final_Score

  Average_Score = 0.3 * First_Score &
                + 0.3 * Second_Score &
                + 0.4 * Final_Score

  PRINT *, "The weighted average score is ", Average_Score, "%"

  SELECT CASE ( NINT(Average_Score) )

    CASE(:-1)
      PRINT*, "Negative score! there must have been an input error."
    CASE(0:39)
      PRINT *, "The grade is FF"
    CASE(40:49)
      PRINT *, "The grade is FD"
    CASE(50:59)
      PRINT *, "The grade is DD"
    CASE(60:69)
      PRINT *, "The grade is DC"
    CASE(70:74)
      PRINT *, "The grade is CC"
    CASE(75:79)
      PRINT *, "The grade is CB"
    CASE(80:84)
      PRINT *, "The grade is BB"
    CASE(85:89)
      PRINT *, "The grade is BA"
    CASE(90:100)
      PRINT *, "The grade is AA"
    CASE DEFAULT
      PRINT *, "Average score is greater than 100%!"
      PRINT *, "Maybe there is an input error."

  END SELECT

END PROGRAM Grade_Calculation
