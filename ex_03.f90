program ex3 !Programa solicita 2 valores reais e exibe a soma deles
implicit none
! Declaracao de variaveis

character(len=10):: a,b,c

!character*20 :: c

! Fornecimento de dados pelo usuario

print*,"Digite a primeira palavra:"
read*, a
print*,"Digite a segundo palavra:"
read*, b

!Definindo a soma dos valores

c = trim(a) // " " // trim(b)

!Exibe os valores dos numeros

print*, c

end program ex3