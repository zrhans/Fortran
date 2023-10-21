program lambda

implicit none

!declarar variaveis

real :: lambda , frequencia , energia


print *, "comprimento de onda"

read *, lambda
if (390<= lambda => 450) then
print*, "violeta"