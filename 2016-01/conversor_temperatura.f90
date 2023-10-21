!---------------------------------------------------
! Este programa le uma lista de temperaturas em um
! arquivo no disco e converte para Fahreneheit e para
! Kelvin.
!
! FORMULAS
! C = 9/5 * (F -32)
! K = C + 273.15
!
! autor: hans
! data: 19/05/2016
!---------------------------------------------------
program converte
implicit none

! Declaração de constantes e variáveis

real      :: Temp_C = 0.0, Temp_F = 0., Temp_K = 0.0 

integer   :: i

! Inincio 

! Abrindo o arquivo de dados
open(10,file="celcius.txt")

open(11,file="temp.txt")


do i = 1, 10

  ! Lendo o valor da temperatura em Celcius
  read(10, *) Temp_C

  ! conversoes
  Temp_F = 1.8 * Temp_C + 32
  Temp_K = Temp_C + 273.15

  !Mostrando o valor na tela
  print *, Temp_C, Temp_F, Temp_K

  ! Escrevendo (salvando) os valores em arquivo
  write(11, *) Temp_C, Temp_F, Temp_K


end do



! Fechando a unidade lógica numero 10 
! associada ao arquivo celcius.txt
close(10)

close(11)

end program converte