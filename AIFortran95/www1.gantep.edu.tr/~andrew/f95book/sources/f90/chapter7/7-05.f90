PROGRAM Computer_Accounts

  IMPLICIT NONE

  ! Named Constants
  INTEGER, PARAMETER :: N = 12

  ! Arrays
  INTEGER          :: UID(N), GID(N)
  CHARACTER(LEN=8) :: Username(N)
  CHARACTER(LEN=8) :: Groups(4)

  ! Scalars
  CHARACTER(LEN=8)  :: User,Group
  INTEGER           :: I, Value, ID

  ! Data
  Groups = (/ "operator", "staff   ", "student ", "web     " /)
  GID(1:3) = 1; GID(4:8) = 2; GID(9:11) = 3; GID(12) = 4
  UID = (/ 10,11,12, 101,102,103,104,105, 1001,1002,1003, 8005/)
  Username = (/ "oper1   ", "oper2   ", "oper3   ", &
                "fatih   ", "yilmaz  ", "hdeniz  ", &
                "akoksal ", "nkoc    ", &
                "ab34753 ", "yy23453 ", "nk73426 ", &
                "physics " /)

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
          IF ( User == Username(I) ) CALL Show_User(I)
        END DO

      CASE (2)
        PRINT *,"Group Name"
        READ *, Group
        DO I = 1 ,N
          IF ( Group == Groups(GID(I)) ) CALL Show_User(I)
        END DO

      CASE (3)
        PRINT *,"User ID"
        READ *, ID
        DO I = 1 ,N
          IF ( ID == UID(I) ) CALL Show_User(I)
        END DO

      CASE DEFAULT
        EXIT

    END SELECT

  END DO

CONTAINS

  SUBROUTINE Show_User(I)
    INTEGER, INTENT(IN) :: I
    PRINT *, "-----------------"
    PRINT *, "Username: ", Username(I)
    PRINT *, " User ID:", UID(I)
    PRINT *, "   Group: ", Groups(GID(I))
    PRINT *, "   Email: ", TRIM(Username(I)),"@gantep.edu.tr"
  END SUBROUTINE Show_User

END PROGRAM Computer_Accounts
