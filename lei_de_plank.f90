!---------------------------------------------------
! Calcula e gera arquivos com valores de Radiância
! em função do comprimento de onda para um 
! Corpo Negro.
!---------------------
! Autor: Hans Zimermann
! Data: 09-10-2016 Alt:
!---------------------------------------------------
program curvas_de_radiancia
implicit none

real, parameter :: c1 = 3.74e-16 ! [W][m^2] ou [J/s][m^2]
real, parameter :: c2 = 1.44e-2  ! [m][K]
integer, parameter :: sp = selected_real_kind(6, 37) ! Ian Chivers,Jane Sleightholme
real :: lambda = 0. , T = 0.
real :: radiancia = 0.
integer :: i, N = 20 ! N numero de pontos da função
real :: dl

!--- @begin Benckmarking
INTEGER :: clock_start,clock_end,clock_rate
REAL(KIND=sp) :: elapsed_time
!--- @end Benckmarking
T =7000.

! Relação entre lambda e N contador  incremento = (b - a) / n
dl = ( 2e-6 - 0.)/ N

open(unit=1, file="radiacia.txt", status="replace") 
open(unit=2, file="radiacia-x.txt", status="replace") 

CALL SYSTEM_CLOCK(COUNT=clock_start)
do i = 1,N
    lambda = lambda + dl
    radiancia = (c1)*(lambda**(-5.0))*( 1 / (exp(c2/(lambda*T))-1.0) )
    print 100,lambda, radiancia
    write(1,100) radiancia
    write(2,100) lambda
end do
CALL SYSTEM_CLOCK(COUNT=clock_end)

 ! Calculate the elapsed time in seconds:
  elapsed_time=REAL((clock_end-clock_start)/clock_rate,sp)
  print *, elapsed_time

100 format (es12.2)

 close(1)
 close(2)
 stop('Programma finalizado!')
end program curvas_de_radiancia