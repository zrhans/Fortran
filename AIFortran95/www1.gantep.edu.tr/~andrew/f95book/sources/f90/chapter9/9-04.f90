PROGRAM Pointer_Association
!----------------------------------------------
! Program to illustration pointer association.
!----------------------------------------------

  IMPLICIT NONE

  REAL, POINTER :: Px => NULL( ), Py => NULL( ), Pz => NULL( )
  REAL, TARGET :: Tx = 1.1, Ty = -1.1, Tz
  
  PRINT *, ASSOCIATED(Px), ASSOCIATED(Py), ASSOCIATED(Pz)

  Px => Tx
  Py => Ty
  Pz => Tz

  PRINT *, ASSOCIATED(Px), ASSOCIATED(Py), ASSOCIATED(Pz)

  NULLIFY(Px, Py, Pz)

  PRINT *, ASSOCIATED(Px), ASSOCIATED(Py), ASSOCIATED(Pz)

END PROGRAM Pointer_Association
