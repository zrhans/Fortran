!Fazer um programa que leia dez numeros inteiros, armazena-los em um vetor e escreve-los na ordem inversa de sua leitura.

program vetor
implicit none

! numero inteiro
integer num(10), i !

!
write(*,*)' ESCREVA 10 NUMEROS PARA O PROGRAA COLOCAR NA ORDEM INVERTIDA '



!  armazenar em um vetor
do 1, i=1,10
  write(*,10)' Digite um numero ',i,': '
  read*,num(i)
  1 continue


write(*,*)' NUMEROS NA ORDEM INVERTIDA '


! escrever na ordem inversa
do 2, i=10,1,-1
  write(*,20)num(i)
  write(*,*)
  2 enddo

write(*,*)' APERTE ENTER PARA TERMINAR O PROGRAMA '


read*,

stop
10 format(a,i2,a,$)
20 format(i16.2,2x,$)

end program vetor
