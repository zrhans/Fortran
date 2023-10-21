PROGRAM Stresses
!----------------------------------
! Program to illustrate  the input
! of data from a file. The Data is
! stored in the file stresses.dat
!----------------------------------

  IMPLICIT NONE
  REAL :: T_Force, T_Moment, T_Torque
  REAL :: Area, Inertia, P_Inertia, R, C
  REAL :: N_Stress_f, N_Stress_m, S_Stress

  OPEN (UNIT=2, FILE="stresses.dat",&
        STATUS="OLD", ACTION="READ",&
        POSITION="REWIND")

  ! or the following would be enough:
  ! OPEN (UNIT=2, FILE="stresses.dat")

  READ(2,'(2X,F3.0,3X,F3.0)') R, C
  READ(2,'(8X,F7.0)') Area
  READ(2,'(8X,F7.0 /, 14X, F7.0)') Inertia, P_Inertia
  READ(2,'(8X,F6.0)') T_Force, T_Moment, T_Torque

  N_Stress_f = T_Force / Area
  N_Stress_m = T_Moment * c /Inertia
  S_Stress = T_Torque * r / P_Inertia

  WRITE(6,30) "normal stress by force  =", N_Stress_f, " Pa"
  WRITE(6,30) "normal stress by moment =", N_Stress_m, " Pa"
  WRITE(6,30) " shear stress by torque =", S_Stress,   " Pa"
  30 FORMAT(A,F10.2,A)

END PROGRAM Stresses
