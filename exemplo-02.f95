 program exemplo2
 implicit none
 
 REAL             :: r = 1.5
 DOUBLE PRECISION :: d = 1.5
 INTEGER          :: i = 1
 PRINT *, DBLE(r), DBLE(d), DBLE(i)   ! Convert number to a double precision
 PRINT *, REAL(r), REAL(d), REAL(i)   ! Convert number to a single precision (REAL)
 PRINT *, INT(r), INT(d), INT(i)      ! Convert number to an integer
 PRINT *, ceil(r), INT(d), INT(i)      ! Convert number to an integer
 
 stop '>>> Programa finalizado!'
 end program exemplo2