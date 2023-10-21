PROGRAM Temperatures
!-------------------------------------------
! Data  representing  a set of  temperature
! measurements are assigned to an array and
! then  output in an appropriate  tabulated
! format.
!-------------------------------------------

  IMPLICIT NONE

  REAL :: T(4,5)

  T = RESHAPE((/ 8.0, 19.3, 33.3,  9.0,&
                11.0, 17.6, 18.2, 22.5,&
                13.0, 15.2, 22.1, 33.0,&
                16.0, 13.1, 12.5, 17.3,&
                18.2, 10.0, 12.5, 22.0/), (/4,5/))

  PRINT '(5(1x,F4.1))', T(1,:)
  PRINT '(5(1x,F4.1))', T(2,:)
  PRINT '(5(1x,F4.1))', T(3,:)
  PRINT '(5(1x,F4.1))', T(4,:)

END PROGRAM Temperatures
