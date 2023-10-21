PROGRAM Particle_Detector
!------------------------------------
! A list of 24 particles from a high
! energy  reaction  is input  from a 
! data file  to an  array structure.
! The data  contains each particle's
! species name, and 3-momentum.
!------------------------------------

  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 24

  Type Particle
    CHARACTER(LEN=3) :: Species
    REAL :: PX, PY, PZ
  END TYPE Particle

  TYPE(Particle) :: Tracks(N)

  OPEN(UNIT=2, FILE='particles.dat',&
       STATUS='OLD', ACTION='READ')

  READ (2,*) Tracks

  CLOSE(2)

  PRINT '((A3,3(1X,F7.3)))', Tracks

END PROGRAM Particle_Detector
