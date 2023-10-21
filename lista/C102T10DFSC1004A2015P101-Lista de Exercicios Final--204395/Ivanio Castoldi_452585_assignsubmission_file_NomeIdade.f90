program NomeIdade

implicit none
character nome(50) * 10   ! Vetor de 10 caracteres por elemento
integer idade(50), i, j, soma, maiorida, menorida, maior, menor, media

i = 1; soma = 0; maiorida = 0; menorida = 99

write(*,10) 'Nome: '
read(*,*) nome(i)

do while(nome(i)/='fim')
   write(*,10) 'Idade: '
   read(*,*) idade(i)

   soma = soma + idade(i)

   if (idade(i) > maiorida) then
      maiorida = idade(i); maior = i
   end if
   
   if (idade(i) < menorida) then
      menorida = idade(i); menor = i
   end if
   
   i = i + 1
   
   write(*,10) 'Nome: '
   read(*,*) nome(i)
end do

media = soma / (i - 1)

write(*,*)   ! Imprime uma linha em branco
write(*,*) 'Maior idade: ', nome(maior)
write(*,*) 'Menor idade: ', nome(menor)
write(*,20) 'Idade maior que a m‚dia: ', media

do 100 j = 1, i - 1
   if (idade(j) > media) then
      write(*,*) nome(j)
   end if
100   continue

read*,
stop

10    format(a)
20    format(/, a, i2)

end program NomeIdade
