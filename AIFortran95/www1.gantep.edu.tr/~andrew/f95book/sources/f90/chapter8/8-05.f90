PROGRAM Particle_Detector
!-------------------------------------------------------
! A list of  24 particles  from a  high energy reaction
! is input  from a  data file  to  an  array structure.
! The data contains  each particle's species name,  and
! 3-momentum. A new data structure is created including
! additional information  derived  from the first data.
!-------------------------------------------------------

  IMPLICIT NONE
  INTEGER, PARAMETER :: N = 24
  INTEGER :: I
  CHARACTER(LEN=3) :: Name

  Type Particle
    CHARACTER(LEN=3) :: Species
    REAL :: PX, PY, PZ
  END TYPE Particle

  Type Particle2
    CHARACTER(LEN=3) :: Species
    REAL :: PX, PY, PZ
    REAL :: P, M,  E
  END TYPE Particle2

  TYPE(Particle) :: Tracks(N)
  TYPE(Particle2) :: Tracks2(N)

  OPEN(UNIT=2, FILE='particles.dat',&
       STATUS='OLD', ACTION='READ')

  READ (2,*) Tracks

  CLOSE(2)

  ! Whole-array copy of Tracks to Tracks2.
  Tracks2%Species = Tracks%Species
  Tracks2%PX = Tracks%PX
  Tracks2%PY = Tracks%PY
  Tracks2%PZ = Tracks%PZ

  ! loop over each particle and compute M, P ,E
  DO I = 1 , N

    ! Compute the momentum
    Tracks2(I)%P = SQRT( Tracks2(I)%PX**2 + &
                         Tracks2(I)%PY**2 + &
                         Tracks2(I)%PZ**2 )

    ! Determine the mass
    Tracks2(I)%M = 0.
    Name = Tracks2(I)%Species
    IF ( Name=="E+ " .OR. Name=="E- " ) Tracks2(I)%M = 0.000511
    IF ( Name=="MU+" .OR. Name=="MU-" ) Tracks2(I)%M = 0.10566
    IF ( Name=="PI+" .OR. Name=="PI-" ) Tracks2(I)%M = 0.13957
    IF ( Name=="K+ " .OR. Name=="K- " ) Tracks2(I)%M = 0.49365
    IF ( Name=="P+ " .OR. Name=="P- " ) Tracks2(I)%M = 0.93827
    IF ( Tracks2(I)%M==0. ) &
       PRINT *, "No mass assignment for track ",I

    ! Compute the energy
    Tracks2(I)%E = SQRT( Tracks2(I)%P**2 + Tracks2(I)%M**2 )

  END DO

  ! Output the result
  PRINT '(A)', "Name  Px      Py      Pz      Mom     Mass   Energy"
  PRINT '((A3,6(1X,F7.3)))', Tracks2

END PROGRAM Particle_Detector

