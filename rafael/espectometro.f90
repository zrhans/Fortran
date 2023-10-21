program espectometro
implicit none

real :: lambda, frequencia, 

! Supondo um comprimento de onda e frequencia 

lambda

print *,"Faixa do comprimento: "
read *, lambda

if (lambda 380:450) then       
    print *, "Violeta" 
else if (lambda 450:495) then       
    print *, "Azul"
else if (lambda 495:570) then 
    print *, "Verde"
else if (lambda 570:590) then
    print *, "Amarelo"
else if (lambda 590:620) then
    print *, "Laranja"
else if (lambda 620:750) then
    print *, "Vermelho"

   ! block 

   print *, " "
end if

end program espectometro