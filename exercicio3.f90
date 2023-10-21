!Fazer um programa que leia dez numeros inteiros, armazena-los em um vetor e
!escreve-los na ordem inversa de sua leitura.

!WILLIAN GRECILLO DOS SANTOS

program leiturainvertida

implicit none
integer num(10), i !informa ao compilador que a vaiavel num eh um vetor com 10 elementos


write(*,*)
write(*,*)'-----------------------------------------'
write(*,*)'   ESCREVA DEZ NUMEROS QUE O PROGRAMA'
write(*,*)'   IRA REORGANIZA-LOS NA ORDEM INVERSA'      !explica ao operador como
write(*,*)'    (apos digitar um numero pressione'       !funciona o programa
write(*,*)'      enter para digitar o proximo)'
write(*,*)'-----------------------------------------'
write(*,*)

do 1, i=1,10   !repete o comando 10x para ler os 10 numeros
  write(*,10)'Informe o numero ',i,': '
  read*,num(i)
  1 continue
  
write(*,*)
write(*,*)'--------------------------'
write(*,*)'NUMEROS NA ORDEM INVERTIDA'
write(*,*)'--------------------------'
do 2, i=10,1,-1  !reorganiza os numeros na ordem invertida da informada pelo operador
write(*,20)num(i)
write(*,*)
  2 enddo
write(*,*)'--------------------------'
write(*,*)
write(*,*)
write(*,*)'-----------------------------------------'
write(*,*)'PRESSIONE ENTER PARA FINALIZAR O PROGRAMA'
write(*,*)'-----------------------------------------'
read*,

stop
10 format(a,i2,a,$)
20 format(i16.2,2x,$)

end program leiturainvertida
