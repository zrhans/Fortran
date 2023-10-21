program mediade2provas
implicit none
real prova1(3), prova2(3)
integer i
do 100,i=1, 3
write(*,10)'informe as duas notas do ', i, 'o aluno: '
read (*,*)prova1(i), prova2(i)
100 continue
write(*,*)
do 200,i=1,3
write(*,20)'media do ', i, 'o aluno: ',(prova1(i)+prova2(i))/2
200 continue
stop
10 format (a,i2,a,$)
20 format (a9,i2,a,f4.1)
end

