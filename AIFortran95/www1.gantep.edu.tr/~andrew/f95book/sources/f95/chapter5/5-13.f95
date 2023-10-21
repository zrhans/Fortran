PROGRAM Stresses
!-----------------------------------
! Program to illustrate the general
! READ statement. Values are input
! in the following format:
! -----------------------
! r=0.1 c=0.1
! area   =0.0314
! inertia=4.91E-6
! polar inertia=9.82E-6
! tforce =45.254
! tmoment=22.550
! ttorque=13.780
!
! Different format specifiers are
! used for demonstration purposes.
!-----------------------------------

  IMPLICIT NONE
  REAL :: T_Force, T_Moment, T_Torque
  REAL :: Area, Inertia, P_Inertia, R, C
  REAL :: N_Stress_f, N_Stress_m, S_Stress

  WRITE(6,'(A)') "Enter r, c , area, inertia, &
                & p_inertia, t_force, t_moment, t_torque"

  READ(*,'(2X,F3.0,3X,F3.0)') R, C
  READ(5,'(T9,F7.0)') Area
  READ(5,10) Inertia, P_Inertia
  READ(5,20) T_Force, T_Moment, T_Torque

  N_Stress_f = T_Force / Area
  N_Stress_m = T_Moment * c /Inertia
  S_Stress = T_Torque * r / P_Inertia

  WRITE(6,30) "normal stress by force  =", N_Stress_f, " Pa"
  WRITE(6,30) "normal stress by moment =", N_Stress_m, " Pa"
  WRITE(6,30) " shear stress by torque =", S_Stress,   " Pa"

  10 FORMAT(T9,F7.0 / T15,F7.0)
  20 FORMAT(8X,F6.4)
  30 FORMAT(A,F10.2,A)

END PROGRAM Stresses
