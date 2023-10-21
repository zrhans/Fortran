!--------------------------------------------------------
! Arquivo: aula-31052017.f90
! Programa para calcular f(x) = x^2 no intervamo x[-5,5]
!
! Autor: Hans Z
! Data: 31-05-2017
!------------------------------------------------------
program aula_31052017
implicit none
! declaração de constantes e variáveis
integer :: t
real :: ft = 0.0

! Criando o arquivo
open(15, file='dados.txt')

! Repetindo no intervalo -5 até 5
do t = -50, 50

  !Calculando a função x²
  ft = t**2

  ! Escrevendo x² no arquivo
  write(15,100) t, ft
  ! Mostrando na tela o valor de x²
  print 100, t, ft

end do

!100 FORMAT(I3,',',F8.2)
100 FORMAT(I3,',',F8.2)

! Fechando o arquivo
close(15)

end program aula_31052017
