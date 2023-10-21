program MediaDe2Provas

implicit none
real prova1(3), prova2(3) !Vetores
integer i

do 100, i = 1, 3
   write(*,10) 'Informe as duas notas do ', i, 'o aluno: '
   read(*,*) prova1(i), prova2(i)
100   continue

write(*,*) !Escreve 1 linha em branco

do 200, i = 1, 3
   write(*,20) 'M‚dia do ', i, 'o aluno: ', (prova1(i) + prova2(i)) / 2
200   continue

read*,

stop
10    format(a, i2, a)
20    format(a9, i2, a, f4.1)

end program MediaDe2Provas
