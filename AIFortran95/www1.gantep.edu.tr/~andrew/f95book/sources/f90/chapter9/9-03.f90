PROGRAM Pointer_Dereference
!---------------------------------
! Example of pointer dereference.
!---------------------------------

  IMPLICIT NONE

  REAL, POINTER :: Px, Py, Pz
  REAL,  TARGET :: Tx = 1.1, Ty = -1.1, Tz

  Px => Tx; Py => Ty; Pz => Tz
  Pz = Px - Py
  PRINT *, "Pz = ", Pz

  Py => Px
  Pz = Px*Py
  PRINT *, "Pz = ", Pz

  Pz = Px
  Pz => Px

  PRINT *, "Pz = ", Pz
  PRINT *, "Tx = ", Tx
  PRINT *, "Ty = ", Ty
  PRINT *, "Tz = ", Tz

END PROGRAM Pointer_Dereference
