program matriculas
implicit none
integer inf(100), alg (100), i, j, k, m
i=1
write(*,10)'Matriculas dos alunos informatica'
write(*,20)'Entre com a', i, ' A matricula: '
read(*,*)inf(i)
do while(inf(i)<999)
i=i+1
write(*,20)'Entre com a ',i, 'a matricula: '
read (*,*)inf(i)
enddo
j=1
write(*,10)'Matricula dos alunos de algebra linear'
write(*,20)'Entre com a ', j, 'a matricula: '
read(*,*)alg(j)
do while (alg(j)<999)
j=j+1
write (*,20)'Entre com a', j, ' A matricula: '
read(*,*)alg(j)
enddo
write(*,*)
do 150 k=1, 1-i
do 100 m=1, j-1
if (inf(k)==alg(m)) then
write(*,30)inf(k)
exit
endif
100 continue
150 continue
stop
10 format(/,a)
20 format(a,i2,a,$)
30 format(i3)
end
