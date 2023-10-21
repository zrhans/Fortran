PROGRAM Computer_Accounts
!----------------------------------
! Account information is processed
! using a derived data type.
!----------------------------------
  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 12
  INTEGER            :: I, Value, ID
  CHARACTER(LEN=8)   :: User,Group
  CHARACTER(LEN=8)   :: Groups(4) = &
  (/ "operator", "staff   ", "student ", "web     " /)

  Type Account
    INTEGER :: UID, GID
    CHARACTER(LEN=8) :: Username
  END TYPE Account

  TYPE(Account) :: User_Info(N)

  User_Info(1)  = Account(  10, 1, "oper1")
  User_Info(2)  = Account(  11, 1, "oper2")
  User_Info(3)  = Account(  12, 1, "oper3")
  User_Info(4)  = Account( 101, 2, "fatih")
  User_Info(5)  = Account( 102, 2, "yilmaz")
  User_Info(6)  = Account( 103, 2, "hdeniz")
  User_Info(7)  = Account( 104, 2, "akoksal")
  User_Info(8)  = Account( 105, 2, "nkoc")
  User_Info(9)  = Account(1001, 3, "ab34753")
  User_Info(10) = Account(1002, 3, "yy23453")
  User_Info(11) = Account(1003, 3, "nk73426")
  User_Info(12) = Account(8005, 4, "physics")

  ! Search
  DO
    PRINT *, " "
    PRINT *, "Search for computer users by"
    PRINT *, "1 - username"
    PRINT *, "2 - group name"
    PRINT *, "3 - User ID"
    PRINT *, "any other value quits."
    READ *, Value

    SELECT CASE (Value)

      CASE (1)
        PRINT *,"Enter username"
        READ *, User
        DO I = 1 ,N
          IF ( User == User_Info(I)%Username ) CALL Show_User(I)
        END DO

      CASE (2)
        PRINT *,"Group Name"
        READ *, Group
        DO I = 1 ,N
          IF ( Group == Groups(User_Info(I)%GID) ) CALL Show_User(I)
        END DO

      CASE (3)
        PRINT *,"User ID"
        READ *, ID
        DO I = 1 ,N
          IF ( ID == User_Info(I)%UID ) CALL Show_User(I)
        END DO

      CASE DEFAULT
        EXIT

    END SELECT

  END DO

CONTAINS

  SUBROUTINE Show_User(I)
    INTEGER, INTENT(IN) :: I
    PRINT *, "-----------------"
    PRINT *, "Username: ", User_Info(I)%Username
    PRINT *, " User ID:", User_Info(I)%UID
    PRINT *, "   Group: ", Groups(User_Info(I)%GID)
    PRINT *, "   Email: ", TRIM(User_Info(I)%Username),"@gantep.edu.tr"
  END SUBROUTINE Show_User

END PROGRAM Computer_Accounts

