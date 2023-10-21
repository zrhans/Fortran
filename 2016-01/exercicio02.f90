!----------------------------------------------------------------------
! Escreva um programa para ler o nome e nota final dos integrantes de 
! uma turma de 10 alunos. O programa deve imprimir a média das notas e 
! o nome do aluno que tem a maior nota. Neste programa, uma estrutura 
! de repetição contável deve ser utilizada. 
!
!
! autor: hans
! data: 12-05-2016
!----------------------------------------------------------------------

program exerc_01
implicit none


! Declaração de variáveis

integer(kind=1), parameter  :: num_alunos = 3
character(len=50)           :: nome, nome_maior_nota
real                        :: nota = 0.0, soma = 0.0, media = 0.0
real                        :: maior_nota = 0.0
integer(kind=1)             :: i

  ! Criando uma estrutura de repetição contável
  ! Repete enquanto i for menor que o valor da variável num_alunos
  do i = 1, num_alunos 
    ! Mensagem para o usuário
    print *, "Digite o nome do aluno: "
    ! Lendo o nome digitado
    read *, nome
    
    print *, "Digite a nota do aluno: "
    ! Lendo a nota do aluno
      do
        read *, nota
        ! Testando validade do valor digitado
        if ((nota > 10.0) .or. ( nota < 0.0)) then
          print*,'Nota inválida.'
          cycle
        else 
          exit
        end if
      end do
    
    ! estrutura condicional - 
    if (nota > maior_nota) then
      nome_maior_nota = nome ! Nome do aluno com maior nota
      maior_nota = nota
    endif

    soma = soma + nota

  end do

  media = soma / num_alunos

  ! Mostrando a media das notas e o nome do alunmo com maior nota
  print *, '--------------------------------------------'
  print *, 'Média das notas : ', media
  print *, 'Aluno classe A  : ', nome_maior_nota
  print *, '--------------------------------------------'

end program exerc_01




