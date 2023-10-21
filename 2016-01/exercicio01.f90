!----------------------------------------------------------------------
! Escreva um programa para ler valores inteiros.
! O programa termina quando o valor 0 (zero) for lido. 
! Ao final, o programa deve imprimir quantos números lidos são negativos
! e quantos são positivos.
!
!
! autor: hans
! data: 12-05-2016
!----------------------------------------------------------------------

program exerc_01
implicit none


! Declaração de variáveis
integer   :: numero = 1, negativos = 0, positivos = 0

  ! Criando um laço
  do while (numero .ne. 0) !Enquando o numero for diferente de zero
    ! Mensagerm para o usuário
    print *, "Digite um número inteiro: "
    ! Lendo o valor digitado
    read *, numero 
    
    ! estrutura condicional
    if (numero > 0) then
      positivos = positivos + 1 ! Contador de numeros positivos
    else if (numero < 0 ) then
      negativos = negativos + 1  ! Contador de numeros negativos
    endif
  
  end do

  ! Mostrando quantos números lidos são negativos e positivos
  print *, '--------------------------------------------'
  print *,  'Qtd de números negativos: ', negativos
  print *,  'Qtd de números positivos: ', positivos
  print *, '--------------------------------------------'

end program exerc_01




