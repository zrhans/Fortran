program meuprog
real pi, g
parameter (pi = 3.1415927, g = 9.80665)

integer x1

x1 = 2 + 3

if ( x1 .GT. 5) then
     print*, 'maior que 5'
else
    print*,'x1=', x1
end if
read *
end program


