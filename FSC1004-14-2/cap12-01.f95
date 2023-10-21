program entrada_formatada_01
implicit none

integer m, n
real x, y

write(*,*)""
write(*,100,advance='no') "Digite m e n inteiros: "
100 format (A)
read(*,*) m, n
write(*,100,advance="no") "Digite x e y reais   : "
read(*,*) x, y

print*,'Valores gigitados'
print*,'================='
print*,'m e n: ', m,n
print*,'x e y: ', x,y
print*,''
stop '>>> Programa Finalizado'
end