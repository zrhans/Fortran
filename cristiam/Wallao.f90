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
implicit none

real :: a,b,c,x1,x2

print*, "=================================="
print*, " Solver para a equacao quadratica"
print*, "----------------------------------"
print*, "     ax^2 + bx + c = 0"
print*, "__________________________________"
print*, "Forneca os coeficientes a, b, c : "
read *, a,b,c

!vamos verificar o delta

delta = (b²-4*a*c)

if (delta > 0) then
f (delta >=0) then
 x1 = (-b + sqrt(delta) ) / 2*a
 x2 = (-b - sqrt(delta) ) / 2*a
endif 
print*,"2 raizes reais"
print*,"Delta = ", delta

else if (delta < 0) then
print*, "2 raizes complexas"
print*, "Delta = ", delta
else
print*, "As duas raizes sao iguais"
prinit*,"Delta =",delta
print*,"x1=x2=",x1
end if
end program baskar
