program baskara
implicit none

real :: a,b,c,delta,x1,x2

    print*, "--------------------------------"
    print*, "Resolvendo equacao segundo grau"
    print*, "--------------------------------"

    print*, "--------------------------------"
    print*, "     ax^2 + bx + c = 0          "
    print*, "--------------------------------"

    print*, "Fornecendo os coeficientes dos termos da equacao"
    read *, a,b,c

delta = b**2 - 4*a*c

if (delta >=0) then

x1 = (-b +sqrt(delta)) / 2*a
x2 = (+b +sqrt(delta)) / 2*a

end if

if (delta >0 ) then

    print*, "2 Raizes reais"
    print*, delta, "Delta ="
    print*, x1, x2, "Raizes:" 
    
else if (delta <0 ) then

    print*, "2 Raizes complexas"
    print*, delta ,"Delta"
    print*, x1 , "Raizes x1 = x2"
    
    
else

    print*, "Raizes iguais"
    print*,x1 , "x1 = x2"
    

end program baskara