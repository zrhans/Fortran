!Sintaxe
! open([unit=]<unidade>,file=<"nome_do_arquivo">)
! read(<unidade>, <formato>) lista_de_variaveis
! write(<unidade>, <formato>) lista_de_variaveis
! close(<unidade>)
!------------------------------------------------------------
!
! Este programa escreve uma lista co os 10 primeiros
! valores da função y(x) = 2 + x^2; onde x é inteiro
!
! autor: hans
! data: 19/05/2016
!--------------------------------------------------------------

program aula_arquvios
implicit none

! Declaração de variáveis
integer :: i
real    :: y

! Inicio do Programa

open(unit=10, file="dados.txt")

do i = 1, 10    ! Estrutura de repetição (repete 10 vezes)

  y = 2 + i**2
  print *, y    ! Mostrando o valor de y no display
  
  ! Escrevendo (salvado) no arquivo dados.txt
  write(10, *) i, y

end do

close(10)



end program aula_arquvios
