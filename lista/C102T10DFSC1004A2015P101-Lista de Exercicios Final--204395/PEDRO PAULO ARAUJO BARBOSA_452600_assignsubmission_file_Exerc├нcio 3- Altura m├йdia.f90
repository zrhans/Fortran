program alturamedia
implicit none
real alt(100), soma, media
character nome (100)*10
integer i,j
i=1; soma=0
write(*,10)'entre com o ',i, 'o nome: '
read(*,*)nome(i)
do while (nome(i)/='fim')
write(*,20)'Entre com a altura: '
read(*,*)alt(i)
soma=soma+alt(i)
i=i+1
write(*,10)'Entre com o ', i,' o nome: '
read(*,*)nome(i)
end do
media=soma/(i-1)
write(*,30)'Altura m‚dia: ', media
do 100,j=1,i-1
if (alt(j)>media) then
write(*,40)nome(j), '  ', alt(j)
end if
100 continue
stop
10 format(a,i2,a,$)
20 format(a, $)
30 format(/, a, f4.2, /)
40 format(a,a,f4.2)
end
