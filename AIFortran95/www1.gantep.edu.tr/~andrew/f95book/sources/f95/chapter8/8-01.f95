PROGRAM Derived_Data_Type
!-------------------------------------
! Example  of  the   declaration  and
! assignment of a  derived data type.
!
! Variable structure: Date
! has the data type named: Birth_Date
! having components: Day, Month, Year
!-------------------------------------
  IMPLICIT NONE

  TYPE Birth_Date
    INTEGER :: Day
    CHARACTER(LEN=8) :: Month
    INTEGER :: Year
  END TYPE Birth_Date

  TYPE ( Birth_Date ) :: Date

  Date = Birth_Date( 01, "July", 1972 )

  PRINT '(A,I2,2X,A4,2X,I4)', "My date of birth is ", Date

END PROGRAM Derived_Data_Type
