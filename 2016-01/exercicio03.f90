!----------------------------------------------------------------------
! Escreva um programa para ler o nome e nota final dos integrantes de 
! uma turma de 10 alunos. O programa deve imprimir o número e a média
! das notas e o nome e a nota do aluno que tem a maior nota. 
! Neste programa, uma estrutura de repetição contável deve ser utilizada
! e os dados de entrada devem estar em colunas de um arquivo externo
! passado como parâmetro na linha de comando de execução do programa. 
!
!
! autor: hans
! data: 12-05-2016
!----------------------------------------------------------------------

program exerc_03
implicit none


! Declaração de variáveis

integer(kind=1), parameter  :: num_alunos = 3
character(len=50)           :: nome, nome_maior_nota
real                        :: nota = 0.0, soma = 0.0, media = 0.0
real                        :: maior_nota = 0.0
integer(kind=1)             :: i, notas_validas = 0

  ! Criando uma estrutura de repetição contável
  ! Repete enquanto i for menor que o valor da variável num_alunos
  do_classe: do i = 1, num_alunos 
    ! Mensagem para o usuário
    
    ! Lendo o nome digitado
    read *, nome, nota
    
    !print *, "Digite a nota do aluno: "
    ! Lendo a nota do aluno
      do
        ! Testando validade do valor digitado
        if ((nota > 10.0) .or. ( nota < 0.0)) then
          print*,'Nota inválida.'
          cycle do_classe
        else 
          exit
        end if
      end do
    
    ! estrutura condicional - 
    if (nota > maior_nota) then
      nome_maior_nota = nome ! Atualiza o nome do aluno com maior nota
      maior_nota = nota      ! Atualiza o valor da maior nota
    endif

    soma = soma + nota
    notas_validas = notas_validas + 1
  end do do_classe

  media = soma / notas_validas

  ! Mostrando dados do exercício
  print *, '--------------------------------------------'
  print *, 'Total de notas          : ', i-1
  print *, 'Total de notas válidas  : ', notas_validas
  print *, 'Média das notas         : ', media
  print *, 'Aluno classe A          : ', nome_maior_nota
  print *, 'Nota                    : ', maior_nota
  print *, '--------------------------------------------'

end program exerc_03




