! Fazer um program para para encontrar as raizes de uma equação de segundo grau.
! Use a fórmula de báskara.
!
! Requisitos:
! Dizer quantas e que tipo são as raízes da equação quadrática.
! 
!  delta = sqrt(b**2 - 4*a*c)
!
! delta > 0  2 raízes reais 
!             x1 = (-b + sqrt(delta) ) / 2*a
!             x2 = (-b - sqrt(delta) ) / 2*a
! delta = 0  1 raiz x1 = x2 = -b / 2a
! delta < 0  não há raízes reais. Há 2 raízes complexas 
!            x1 = (-b + i*sqrt(delta) ) / 2*a
!            x2 = (-b - i*sqrt(delta) ) / 2*a
!  --------------------------------------------------------------------------

program baskara
implicit none

real :: a,b,c, delta, x1, x2

print*, "=================================="
print*, " Solver para a equacao quadratica"
print*, "----------------------------------"
print*, "     ax^2 + bx + c = 0"
print*, "__________________________________"
print*, " "
read *, a,b,c

! Verificando o delta

delta = b**2 - 4*a*c

if (delta >=0) then
 x1 = (-b + sqrt(delta) ) / 2*a
 x2 = (-b - sqrt(delta) ) / 2*a
endif 
 
if (delta > 0 ) then
 print*, "2 raizes reais"
 print*, "Delta = ", delta
 print *, "x1 = ", x1
 print *, "x2 = ", x2

else if (delta < 0) then
 print*, "2 Raizes complexas"
 print *, "Delta = ", delta

else
    print*, "As duas raizes são iguais."
    print*,  "Delta = ", delta
    print *, "x1 = x2 = ", x1
endif

end program baskara
