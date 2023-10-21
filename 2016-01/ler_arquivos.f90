!Sintaxe
! open([unit=]<unidade>,file=<"nome_do_arquivo">)
! read(<unidade>, <formato>) lista_de_variaveis
! write(<unidade>, <formato>) lista_de_variaveis
! close(<unidade>)
!------------------------------------------------------------
!
! Este programa le uma lista com os 10 primeiros
! valores da função y(x) = 2 + x^2; 
!
! autor: hans
! data: 19/05/2016
!--------------------------------------------------------------

program le
implicit none

  ! Declaração de variáveis
  integer :: i
  real    :: primeira_coluna, segunda_coluna
  
  ! Inicio do Programa
  
  open(unit=10, file="dados.txt")
  
  do i = 1, 10    ! Estrutura de repetição (repete 10 vezes)
    
    ! Escrevendo (salvado) no arquivo dados.txt
    
    read(10, *) primeira_coluna, segunda_coluna
    
    print *, primeira_coluna, segunda_coluna
    
  end do

  close(10)



end program le
