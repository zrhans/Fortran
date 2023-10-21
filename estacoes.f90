program teste_logico
implicit none

real :: t

! Suponha uma escala de temperaturas que indique o padrão para estações do ano

! t abaixo de 10 graus Inverno
! t entre 11 e 15 graus Primavera
! t entre 16 e 25 graus Outono
! t acima de 25 verão
print *,"Tempertura :"
read *, t

if (t <= 10.99) then       
    print *, "Inverno" 
else if (t <= 15.99) then       
    print *, "Primavera"
else if (t <= 25.) then 
    print *, "Outono"
else       
   ! block 
   print *, "Verão !!"
end if

end program teste_logico