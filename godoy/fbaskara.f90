program bask

real :: a, b, c, delta, x1, x2
asdfasdfasdfasdfsdaf
print *, "Solver para a equacao quadratica"

print *, "ax^2 + bx + c = 0"

print *,"Forneça os coeficientes a, b, c :"

read *, a, b, c

!verificando o delta

delta = sqrt(b**2 - 4*a*c)

if (delta > 0) then 
x1 = (-b + sqr(delta) ) / 2*a
x2 = (-b - sqr(delta)) / 2*a

print *, "2 raizes reais"
print *, "delta=", delta
print *, "X1=", X1
print *, "x2=", x2

end program bask