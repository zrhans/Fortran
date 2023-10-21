PROGRAM Inventory
!---------------------------------------
! Program to read a list of prices from
! a file and output the total number of
! items and the total price.
! INPUT FILE: inventory.dat UNIT=8
!---------------------------------------

  IMPLICIT NONE 
  INTEGER :: Number_of_Items
  REAL :: Item_Price, Total_Price

  OPEN(UNIT=8, FILE="inventory.dat", ACTION="READ")

  Number_of_Items = 0
  Total_Price = 0.

  DO

    READ (UNIT=8, FMT='(7X,F6.2)', END=10) Item_Price

    Number_of_Items = Number_of_Items + 1
    Total_Price = Total_Price + Item_Price    

  END DO

  10 CONTINUE

  PRINT '(A18,I3)', "Number of items = ", Number_of_Items
  PRINT '(A14,F7.2,1X,A5)', "Total price = ", Total_Price, "Euros"

END PROGRAM Inventory
