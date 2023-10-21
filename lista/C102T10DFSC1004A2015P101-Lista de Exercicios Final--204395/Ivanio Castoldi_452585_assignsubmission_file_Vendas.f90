program vendas

implicit none
real venda(5,3), soma, media
integer i, j, k

soma = 0; k = 0

do 200 i = 1, 5
   do 150 j = 1, 3
      write(*,10) 'Insira o valor da venda - ', &
      & 'Vendedor: ', (i*10), ' mˆs: ', j, ' : '
      read(*,*) venda(i,j)
      soma = soma + venda(i,j)
150   continue
200   continue

media = soma / 5.0

write(*,20) 'M‚dia: ', media
do 300 i = 1, 5
   soma = 0
   do 250 j = 1, 3
      soma = soma + venda(i,j)
250   continue

   if (soma > media) then
      write(*,30) 'Vendedor: ', i*10, 'Valor: ', soma
   end if
300   continue

read*,

stop
10    format(a, a, i3, a, i2, a)
20    format(/, a, f7.2, /)
30    format(a, i2, a, f7.2)

end program vendas
