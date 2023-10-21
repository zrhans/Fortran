program inverte
implicit none
integer:: num(10), i
do 100,i=1,10
write(*,10)'informe o ', i, 'o n£mero: '
read*, num(i)
100 continue
write(*,*)
do 200, i=10,1,-1
write(*,20)num (i)
200    continue
stop
10 format (a, i2, a, $)
20 format (i2, 2x, $)
end
