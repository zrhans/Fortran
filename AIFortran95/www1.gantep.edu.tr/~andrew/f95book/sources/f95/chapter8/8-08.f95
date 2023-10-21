PROGRAM Hello_World
!--------------------------------------------------
! Program write "Hello world!" to a file where the 
! base-name of the file is input by the user.
!--------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=20) :: BaseFileName
  CHARACTER(LEN=24) :: FullFileName

  PRINT *, "Input the filename base"
  READ *, BaseFileName

  FullFileName = TRIM(BaseFileName) // ".txt"

  OPEN(UNIT=1,FILE=FullFileName)
  WRITE(1,*) "Hello world!"
  CLOSE(1)

END PROGRAM Hello_World

