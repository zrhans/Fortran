PROGRAM Students_and_Courses
!-----------------------------------------------
! Program to search for a student in a database
! and, if found, output the list of courses the
! student is taking.
! Each record contains a  colon-delimited  list
! of course codes for each student.
!-----------------------------------------------
  IMPLICIT NONE
  INTEGER :: InputStatus, ColonPosition
  CHARACTER(LEN=70) :: Record
  CHARACTER(LEN=30) :: SearchString

  PRINT *, " Enter a student's name."
  READ *, SearchString

  OPEN(UNIT=1,FILE="students.dat",ACTION="READ")      ! Open the database file
  DO                                                  ! Loop over each record.
    READ(1,FMT='(A)',IOSTAT=InputStatus) Record       ! Read a record.
    IF ( InputStatus < 0 ) EXIT                       ! End of the input file.
    IF ( INDEX(Record,TRIM(SearchString)) > 0 ) THEN  ! A match is found.
      ColonPosition=INDEX(Record,":")                 ! Where is the next colon?
      PRINT *, "Student: ", Record(1:ColonPosition-1) ! Output the student name.
      DO                                              ! Loop over every course code. 
        Record=Record(ColonPosition+1:)               ! Remove the last substring.
        ColonPosition=INDEX(Record,":")               ! Where is the next colon?
        IF ( ColonPosition == 0 ) EXIT                ! No more colons => end of record.
        PRINT *, Record(1:ColonPosition-1)            ! Output the next course code.
      END DO                                          ! End of course code loop.
    END IF                                            ! End of matched string block.
  END DO                                              ! End of record loop.
  CLOSE(1)                                            ! Close the database file.

END PROGRAM Students_and_Courses
