program segundograu
implicit none
real:: a, b, c, x1, x2, delta

write(*,*) "Inserir coeficientes a b c :"
read(*,*) a, b, c

delta = b**2-4.0*A*C

if (delta>0) then
        ! 2 soluções
        x1 = (-b + sqrt(delta))/(2*a)
        x2 = (-b - sqrt(delta))/(2*a)
        write(*,*) "x1 = ",x1
        write(*,*) "x2 = ",x2
else if (delta == 0) then 
        ! 1 solução
        x1 = -b/(2*a)
        write(*,*) "x1 = x2 = ",x1
else 
        write(*,*) "Delta < 0. A equação não tem solução real"
        stop 1
        ! não tem solução real
end if

end program