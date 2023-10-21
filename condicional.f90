program cond
implicit none

integer :: var

var = 30


if (var < 30 ) then
    ! comandos se a condicao for verdadeira
    print*, "Var vale menos do que 30"
else
    ! comandos se a condicao for falsa
    print*, "Var eh igual ou maior que 30"
 
endif 



end program  cond