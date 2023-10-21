PROGRAM Valid_Reals
!--------------------------------------------------------------------
! Examples of valid REAL type variable declarations and assignments.
!--------------------------------------------------------------------

  REAL :: A, B, C, Gaziantep, Ali

  A = +1.2456
  B = -0.00056
  C = 3.7E+08           ! same as 370 000 000.0
  Gaziantep = -5.7E-4   ! same as -0.000 57
  Ali = 2.5E3           ! same as 2500.0

  PRINT *, A, B, C, Gaziantep, Ali
  
END PROGRAM Valid_Reals

! Resultado após compilar:
!---------------------------------
! zrhans:~/workspace $ gfortran reais.f90 
! zrhans:~/workspace $ ./a.out 
!   1.24559999      -5.60000015E-04   370000000.      -5.69999975E-04   2500.00000  

!Chute gustavo
!  ⁠⁠⁠⁠⁠+1.2456  -0.00056  3.7E+08  -5.7E-4  2.5E3?