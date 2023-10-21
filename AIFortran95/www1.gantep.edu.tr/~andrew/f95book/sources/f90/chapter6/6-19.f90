PROGRAM Annual_Repayment
!--------------------------------------------------
! Program to calculate the  annual repayment  of a
! loan with annual interest rate given in percent.
! (Explicit) external subroutines are used.
!--------------------------------------------------

  IMPLICIT NONE

  INTERFACE Repayment
    SUBROUTINE Repayment(L,I,N,A)
      REAL,    INTENT(IN)  :: L ! loan
      REAL,    INTENT(IN)  :: I ! annual interest (%)
      INTEGER, INTENT(IN)  :: N ! replayment period (whole years)
      REAL,    INTENT(OUT) :: A ! annual repayment
    END SUBROUTINE Repayment
  END INTERFACE Repayment

  INTERFACE Payment_List
    SUBROUTINE Payment_List(L,I,N,A)
      REAL,    INTENT(IN)  :: L ! loan
      REAL,    INTENT(IN)  :: I ! annual interest (%)
      INTEGER, INTENT(IN)  :: N ! replayment period (whole years)
      REAL,    INTENT(IN)  :: A ! annual repayment
    END SUBROUTINE Payment_List
  END INTERFACE Payment_List

  REAL :: Loan, Interest, Annual
  INTEGER :: Years

  PRINT *, "Input the loan"
  READ *, Loan

  PRINT *, "Input the annual interest rate (percent)"
  READ *, Interest
  Interest = Interest / 100.

  PRINT *, "Input period of repayment (years)"
  READ *, Years

  CALL Repayment(Loan, Interest, Years, Annual)
  CALL Payment_List(Loan, Interest, Years, Annual)

  WRITE (*,'(A,1X,F10.2)') " Annual payment is ", Annual
  WRITE (*,'(A,1X,F10.2)') "  Total payment is ", Annual*Years

END PROGRAM Annual_Repayment

SUBROUTINE Repayment(L,I,N,A)

  IMPLICIT NONE

  REAL,    INTENT(IN)  :: L ! loan
  REAL,    INTENT(IN)  :: I ! annual interest (%)
  INTEGER, INTENT(IN)  :: N ! replayment period (whole years)
  REAL,    INTENT(OUT) :: A ! annual repayment

  A = ( I * (1+I)**N * L ) / ( (1+I)**N - 1 )

END SUBROUTINE Repayment

SUBROUTINE Payment_List(L,I,N,A)

  IMPLICIT NONE

  REAL,    INTENT(IN)  :: L ! loan
  REAL,    INTENT(IN)  :: I ! annual interest (%)
  INTEGER, INTENT(IN)  :: N ! replayment period (whole years)
  REAL,    INTENT(IN)  :: A ! annual repayment
  REAL    :: Remaining_Loan
  INTEGER :: Year

  PRINT *, " "
  WRITE (*,'(A,I2,1X,F10.2)') " Year ", 0, L ! the initial loan

  Remaining_Loan = L
  DO Year = 1, N
    Remaining_Loan = Remaining_Loan*(1+I)  ! add the interest
    Remaining_Loan = Remaining_Loan - A    ! deduct the payment
    WRITE (*,'(A,I2,1X,F10.2)') " Year ", Year, Remaining_Loan
  END DO

  PRINT *, " "

END SUBROUTINE Payment_List
