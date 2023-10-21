program raizes_de_equacao_de_segundo_grau

implicit none

real :: x, a, b, c, delta, x1, x2

print*, "selecione um valor para a"

read*, a

print*, "selecione um valor para b"

read*, b

print*, "selecione um valor para c"

read*, c

delta = b**2 - 4*a*c

print*, "o valor de delta é", delta

if (delta < 0) then

print*, "x nao tem solucao real"

else if (delta == 0) then

print*, "x tem uma unica solucao real"

else  if (delta > 0) then

print*, "x tem duas solucoes reais distintas"

end if

x1 = (-b + sqrt(delta))/(2*a)

x2 = (-b - sqrt(delta))/(2*a)

print*, "o valor de x1 é", x1

print*, "o valor de x2 é", x2

end program raizes_de_equacao_de_segundo_grau