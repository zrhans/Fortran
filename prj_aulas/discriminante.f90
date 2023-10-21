program discriminante
real a, b, c, delta
print*, 'Programa de calculo do Discriminante'
print*, 'a='
read*,  a
print*, 'b='
read*, b
print*, 'c='
read*, c
if (a .ne. 0) then
   delta =  b ** 2  -  4 * a * c
   print *, 'delta =', delta
else
    print *,  ' a equa‡Æo ‚ linear '
end if

stop 'Fim programa disciminante'
end program
