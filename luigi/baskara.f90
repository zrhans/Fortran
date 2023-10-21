! Fazer um program para para encontrar as raizes de uma equação de segundo grau.
! Use a fórmula de báskara.
!
! Requisitos:
! Dizer quantas e que tipo são as raízes da equação quadrática.
! 
! delta > 0  2 raízes reais 
!             x1 = (-b + raiz(delta) ) / 2a
!             x2 = (-b - raiz(delta) ) / 2a
! delta = 0  1 raiz x1 = x2 = -b / 2a
! delta < 0  não há raízes reais. Há 2 raízes complexas 
!            x1 = (-b + i raiz(delta) ) / 2a
!            x2 = (-b - i raiz(delta) ) / 2a
!  --------------------------------------------------------------------------
program baskara
 real :: a
 real :: b
 real :: c
 print*, "resolvendo equacao de segundo grau"
 print*, "ax^2 + bx + c = 0 "
 print*, "fornecer os coeficientes dos termos da equacao"
 read*, a,b,c
delta = b**2-4*a*c

if (delta>0) then

x1= (-b +sqrt(delta))/2*a
x2= (+b +sqrt(delta))/2*a

end if

if (delta>0) then

print*"duas raizes reais"
print*, delta, "delta ="
print*,x1, x2 "raizes:"

else if (delta<0) then
print*, "duas raizes complexas"
print*, delta, "delta:"
print*, "x1, raizes x1=x2"

else
print*, "raizes iguais"
print*, x1, "x1 = x2"


end program
