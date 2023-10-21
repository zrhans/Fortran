program matriculas

implicit none
integer inf(100), alg(100), i, j, k, m

i = 1

write(*,10) 'Matr¡culas dos alunos de Inform tica'
write(*,20) 'Entre com a ', i, 'a. matr¡cula: '
read(*,*) inf(i)

do while(inf(i) < 999)
   i = i + 1
   write(*,20) 'Entre com a ', i, 'a. matr¡cula: '
   read(*,*) inf(i)
end do

j = 1

write(*,10) 'Matr¡culas dos alunos de µlgebra Linear'
write(*,20) 'Entre com a ', j, 'a. matr¡cula: '
read(*,*) alg(j)

do while(alg(j) < 999)
   j = j + 1
   write(*,20) 'Entre com a ', j, 'a. matr¡cula: '
   read(*,*) alg(j)
end do

write(*,*)  ! Escreve 1 linha em branco

do 150 k = 1, i-1
   do 100 m = 1, j - 1
      if (inf(k) == alg(m)) then
         write(*,30) inf(k)
         exit
      end if
100   continue
150   continue

read*,
stop

10    format(/, a)
20    format(a, i2, a)
30    format(i3)

end program matriculas
